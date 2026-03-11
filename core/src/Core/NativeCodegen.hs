{-# LANGUAGE OverloadedStrings #-}
module Core.NativeCodegen (nativeCodegen) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.List (intercalate, nub, partition)
import Control.Monad (when, unless, forM, foldM_)
import Core.Syntax
import Core.Codegen (emitPreamble, CGState(..), cfunctionToC, cRetTypeName)
import Core.Reduce (exprFreeVars)
import System.IO (Handle, hPutStr, hPutStrLn)

-- | Inferred native type for specialized code generation
data NType = NInt | NFloat | NString | NRecord Text | NClosure | NUnknown
  deriving (Show, Eq)

-- | State for native code generation
data NCGState = NCGState
  { ncgNextId   :: IORef Int
  , ncgTopDefs  :: IORef [String]    -- top-level C function/struct defs (reversed)
  , ncgIncludes :: IORef [String]    -- #include directives (reversed)
  , ncgScope    :: Map.Map Text String  -- milang name → C variable name
  , ncgGlobals  :: Set.Set Text        -- names that are static globals (don't capture)
  , ncgSelfNames :: Set.Set Text       -- self-recursive binding names (reconstruct from fn+env)
  , ncgKnownTypes :: Map.Map Text NType -- names with known types at compile time
  , ncgForwardDecls :: Set.Set Text    -- names that are forward-declared but not yet assigned
  , ncgEnvPatches :: IORef [(String, String, String)]  -- deferred env patches: (envTypeName, fieldName, valueExpr)
  , ncgTags     :: IORef (Set.Set String) -- interned record tag strings
  }

newNCGState :: IO NCGState
newNCGState = do
  nid <- newIORef 0
  defs <- newIORef []
  incs <- newIORef []
  patches <- newIORef []
  tags <- newIORef Set.empty
  pure NCGState
    { ncgNextId = nid
    , ncgTopDefs = defs
    , ncgIncludes = incs
    , ncgScope = Map.empty
    , ncgGlobals = Set.empty
    , ncgSelfNames = Set.empty
    , ncgKnownTypes = Map.empty
    , ncgForwardDecls = Set.empty
    , ncgEnvPatches = patches
    , ncgTags = tags
    }

freshId :: NCGState -> IO Int
freshId st = do
  n <- readIORef (ncgNextId st)
  writeIORef (ncgNextId st) (n + 1)
  pure n

addTopDef :: NCGState -> String -> IO ()
addTopDef st s = modifyIORef (ncgTopDefs st) (s :)

addInclude :: NCGState -> String -> IO ()
addInclude st s = modifyIORef (ncgIncludes st) (s :)

withScope :: NCGState -> Map.Map Text String -> NCGState
withScope st extra = st { ncgScope = Map.union extra (ncgScope st) }

withGlobals :: NCGState -> Set.Set Text -> NCGState
withGlobals st gs = st { ncgGlobals = Set.union gs (ncgGlobals st) }

-- | Get the set of names known to be int (backward compat with isKnownInt/isIntBody)
knownIntSet :: NCGState -> Set.Set Text
knownIntSet st = Map.keysSet $ Map.filter (== NInt) (ncgKnownTypes st)

-- | Intern a record tag, returning the C variable name for the interned string.
internTag :: NCGState -> String -> IO String
internTag st tag = do
  modifyIORef (ncgTags st) (Set.insert tag)
  pure $ "_mi_tag_" ++ sanitizeName (T.pack tag)

-- | Emit static tag string declarations for all interned tags.
emitTagDecls :: NCGState -> IO [String]
emitTagDecls st = do
  tags <- readIORef (ncgTags st)
  pure [ "static const char " ++ "_mi_tag_" ++ sanitizeName (T.pack t) ++ "[] = " ++ cStringLit t ++ ";"
       | t <- Set.toList tags ]

-- | Get the set of names known to be float
knownFloatSet :: NCGState -> Set.Set Text
knownFloatSet st = Map.keysSet $ Map.filter (== NFloat) (ncgKnownTypes st)

-- | Infer the compile-time type of an expression from its structure.
inferType :: NCGState -> Expr -> NType
inferType _  (IntLit _)    = NInt
inferType _  (SizedInt {}) = NInt
inferType _  (FloatLit _)  = NFloat
inferType _  (SizedFloat {}) = NFloat
inferType _  (StringLit _) = NString
inferType _  (Lam _ _)     = NClosure
inferType st (Name n)      = Map.findWithDefault NUnknown n (ncgKnownTypes st)
inferType st (BinOp op l r)
  | op `elem` ["==", "/=", "<", ">", "<=", ">="] = NInt  -- comparisons always return 0/1
  | op `elem` ["+", "-", "*", "/", "%"]
  , inferType st l == NInt, inferType st r == NInt = NInt
  | op `elem` ["+", "-", "*", "/"]
  , inferType st l == NFloat || inferType st r == NFloat = NFloat
  | op == "+" , inferType st l == NString || inferType st r == NString = NString
inferType _  (Record tag _) = NRecord tag
inferType st (FieldAccess e _) = case inferType st e of
  NRecord _ -> NUnknown  -- fields could be any type
  _         -> NUnknown
inferType st (Case _ alts) = case alts of
  -- If all alt bodies have the same type, the case has that type
  (Alt _ _ body : rest)
    | let bt = inferType st body
    , bt /= NUnknown
    , all (\(Alt _ _ b) -> inferType st b == bt) rest -> bt
  _ -> NUnknown
inferType st (With body _) = inferType st body
inferType _ _ = NUnknown

-- | Add type knowledge for a name
withKnownType :: NCGState -> Text -> NType -> NCGState
withKnownType st name typ = st { ncgKnownTypes = Map.insert name typ (ncgKnownTypes st) }

-- | Add type knowledge for multiple names
withKnownTypes :: NCGState -> [(Text, NType)] -> NCGState
withKnownTypes st pairs = st { ncgKnownTypes = Map.union (Map.fromList pairs) (ncgKnownTypes st) }

-- | Sanitize a milang name for use as a C identifier
sanitizeName :: Text -> String
sanitizeName t = case T.unpack t of
  [] -> "_empty"
  s  -> let name = concatMap sanitizeChar s
        in if name `Set.member` cReservedWords then "_mi_" ++ name else name
  where
    sanitizeChar c
      | c >= 'a' && c <= 'z' = [c]
      | c >= 'A' && c <= 'Z' = [c]
      | c >= '0' && c <= '9' = [c]
      | c == '_' = [c]
      | c == '#' = "_hash_"
      | c == '\'' = "_q"
      | c == '!' = "_bang"
      | c == '?' = "_p"
      | c == '-' = "_"
      | otherwise = "_" ++ show (fromEnum c) ++ "_"

-- | C reserved words that cannot be used as identifiers
cReservedWords :: Set.Set String
cReservedWords = Set.fromList
  [ "auto", "break", "case", "char", "const", "continue", "default", "do"
  , "double", "else", "enum", "extern", "float", "for", "goto", "if", "int"
  , "long", "register", "return", "short", "signed", "sizeof", "static"
  , "struct", "switch", "typedef", "union", "unsigned", "void", "volatile"
  , "while", "inline", "restrict", "_Bool", "_Complex", "_Imaginary"
  ]

-- | Make a C variable name from a milang name
cVarName :: Text -> String
cVarName n = "_v_" ++ sanitizeName n

-- | Escape a string for C
cStringLit :: String -> String
cStringLit s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc '\r' = "\\r"
    esc '\0' = "\\0"
    esc c    = [c]

-- ── Native Expression Code Generation ──────────────────────────────

-- | Generate a C expression that directly computes a MiVal (no expr trees).
nExprToC :: NCGState -> Expr -> IO String
nExprToC _ (IntLit n)
  | n > 9223372036854775807 || n < (-9223372036854775808) =
    pure $ "mi_sized_big(mi_bn_from_str(" ++ cStringLit (show n) ++ "), 1)"
  | otherwise = pure $ "mi_int(" ++ show n ++ ")"

nExprToC _ (FloatLit d) = pure $ "mi_float(" ++ show d ++ ")"

nExprToC _ (SizedInt n w s)
  | w == 0 && (n > 9223372036854775807 || n < (-9223372036854775808)) =
    pure $ "mi_sized_big(mi_bn_from_str(" ++ cStringLit (show n) ++ "), " ++ showBool s ++ ")"
  | otherwise = pure $ "mi_sized_int(" ++ show n ++ ", " ++ show w ++ ", " ++ showBool s ++ ")"
  where showBool True = "1"; showBool False = "0"

nExprToC _ (SizedFloat d w)
  | w == 32 = pure $ "(MiVal){.type = MI_FLOAT32, .as.f32 = " ++ show (realToFrac d :: Float) ++ "f}"
  | otherwise = pure $ "mi_float(" ++ show d ++ ")"

nExprToC _ (StringLit s) = pure $ "mi_string(" ++ cStringLit (T.unpack s) ++ ")"

nExprToC st (Name n) =
  case Map.lookup n (ncgScope st) of
    Just cname -> pure cname
    Nothing
      -- Auto-constructor for capitalized names (Just, Cons, etc.)
      | not (T.null n) && let c = T.head n in c >= 'A' && c <= 'Z' ->
        pure $ "mi_auto_ctor(" ++ cStringLit (T.unpack n) ++ ")"
      | otherwise ->
        -- Variable not in scope — should have been captured.
        -- Emit a runtime error for debugging.
        pure $ "(mi_error(NULL, \"native codegen: unbound variable: " ++ T.unpack n ++ "\"), mi_int(0))"

nExprToC st (BinOp op l r)
  | isBuiltinOp op = do
    let lt = inferType st l
        rt = inferType st r
    -- When both operands are known-int, skip type check entirely
    if lt == NInt && rt == NInt && op `elem` ["+","-","*","/","%","==","/=","<",">","<=",">="]
      then do
        lc <- nExprToC st l
        rc <- nExprToC st r
        pure $ "mi_int((" ++ lc ++ ").as.i " ++ intCOp op ++ " (" ++ rc ++ ").as.i)"
      else do
        lc <- nExprToC st l
        rc <- nExprToC st r
        pure $ nativeBinop op lc rc
  | otherwise = do
    -- Custom operator: treat as function application
    fc <- nExprToC st (Name op)
    lc <- nExprToC st l
    rc <- nExprToC st r
    pure $ "mi_apply(mi_apply(" ++ fc ++ ", " ++ lc ++ "), " ++ rc ++ ")"

-- Closure inlining: App (Lam param body) arg → let-bind instead of mi_apply
nExprToC st (App (Lam param body) x) = do
  let realName = lamParamName param
      cvar = cVarName realName
  xc <- nExprToC st x
  let xType = inferType st x
      st' = withScope st (Map.singleton realName cvar)
      st'' = if xType /= NUnknown then withKnownType st' realName xType else st'
  bodyC <- nExprToC st'' body
  pure $ "({ MiVal " ++ cvar ++ " = " ++ xc ++ "; " ++ bodyC ++ "; })"

nExprToC st (App f x) = do
  fc <- nExprToC st f
  xc <- nExprToC st x
  pure $ "mi_apply(" ++ fc ++ ", " ++ xc ++ ")"

nExprToC st (Lam param body) = do
  cid <- freshId st
  let realName = lamParamName param
      fvs = Set.toList $ Set.delete realName $ Set.delete param (exprFreeVars body)
      -- A name is "effectively global" only if its scope value IS the global var name
      -- (i.e., it hasn't been shadowed by a local/pattern binding)
      isEffectiveGlobal fv = Set.member fv (ncgGlobals st) &&
        Map.lookup fv (ncgScope st) == Just (cVarName fv)
      -- Filter to only variables actually in scope AND not effectively-global AND not self-names
      capturedFvs = filter (\fv -> Map.member fv (ncgScope st) &&
                                   not (isEffectiveGlobal fv) &&
                                   not (Set.member fv (ncgSelfNames st))) fvs
      -- Self-referencing names that need reconstruction
      selfFvs = filter (\fv -> Set.member fv (ncgSelfNames st)) fvs
      fnName = "_nfn_" ++ show cid
      envTypeName = "_nenv_" ++ show cid

  if null capturedFvs
    then do
      -- No captures: just emit a function, return mi_native(fn)
      let selfBindings = concatMap (\sn ->
            "  MiVal " ++ cVarName sn ++ " = mi_native_env(" ++ fnName ++ ", _env_raw);\n")
            selfFvs
          innerScope = Map.fromList $
            (param, "_arg") : [(realName, "_arg") | realName /= param] ++
            [(sn, cVarName sn) | sn <- selfFvs]
          -- Clear self-names and forward-decls so inner lambdas capture normally
          bodyScope = (withScope st innerScope)
            { ncgSelfNames = Set.difference (ncgSelfNames st) (Set.fromList selfFvs)
            , ncgForwardDecls = Set.empty }
      bodyC <- nExprToC bodyScope body
      addTopDef st $ unlines
        [ "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env_raw) {"
        , if null selfFvs then "  (void)_env_raw;" else selfBindings
        , "  return " ++ bodyC ++ ";"
        , "}"
        , ""
        ]
      pure $ "mi_native(" ++ fnName ++ ")"
    else do
      -- Has captures: emit env struct + function, return mi_native_env(fn, env)
      addTopDef st $ unlines
        [ "typedef struct {"
        , unlines [ "  MiVal " ++ sanitizeName fv ++ ";" | fv <- capturedFvs ]
        , "} " ++ envTypeName ++ ";"
        , ""
        ]
      let selfBindings = concatMap (\sn ->
            "  MiVal " ++ cVarName sn ++ " = mi_native_env(" ++ fnName ++ ", _env_raw);\n")
            selfFvs
          innerScope = Map.fromList $
            (param, "_arg") :
            [(realName, "_arg") | realName /= param] ++
            [ (fv, "_env->" ++ sanitizeName fv) | fv <- capturedFvs ] ++
            [(sn, cVarName sn) | sn <- selfFvs]
          -- Clear self-names and forward-decls so inner lambdas capture normally
          bodyScope = (withScope st innerScope)
            { ncgSelfNames = Set.difference (ncgSelfNames st) (Set.fromList selfFvs)
            , ncgForwardDecls = Set.empty }
      bodyC <- nExprToC bodyScope body
      addTopDef st $ unlines
        [ "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env_raw) {"
        , "  " ++ envTypeName ++ " *_env = (" ++ envTypeName ++ " *)_env_raw;"
        , "  return " ++ bodyC ++ ";"
        , "}"
        , ""
        ]
      -- Emit code to allocate and fill env struct
      let deferredFvs = filter (\fv -> Set.member fv (ncgForwardDecls st)) capturedFvs
          immediateFvs = filter (\fv -> not (Set.member fv (ncgForwardDecls st))) capturedFvs
      if null deferredFvs
        then do
          -- No deferred captures: emit inline as before
          let envAlloc = "({ " ++ envTypeName ++ " *_e = mi_alloc(sizeof(" ++ envTypeName ++ ")); " ++
                concatMap (\fv -> "_e->" ++ sanitizeName fv ++ " = " ++
                           maybe (cVarName fv) id (Map.lookup fv (ncgScope st)) ++ "; ")
                  capturedFvs ++
                "mi_native_env(" ++ fnName ++ ", _e); })"
          pure envAlloc
        else do
          -- Some captures are forward-declared: skip them inline, record patches
          let envAlloc = "({ " ++ envTypeName ++ " *_e = mi_alloc(sizeof(" ++ envTypeName ++ ")); " ++
                concatMap (\fv -> "_e->" ++ sanitizeName fv ++ " = " ++
                           maybe (cVarName fv) id (Map.lookup fv (ncgScope st)) ++ "; ")
                  immediateFvs ++
                "mi_native_env(" ++ fnName ++ ", _e); })"
              -- Backpatch via MiVal's env pointer: ((envType *)cvar.as.native.env)->field = value;
              -- The cvar for this closure is looked up in ncgScope — it's the
              -- name that emitBindings assigned to the binding whose body is this Lam.
              -- We record patches using the captured fv's scope value and the env type,
              -- which emitBindings will resolve after all closures are assigned.
              patches = map (\fv ->
                let val = maybe (cVarName fv) id (Map.lookup fv (ncgScope st))
                in (envTypeName, sanitizeName fv, val))
                deferredFvs
          modifyIORef (ncgEnvPatches st) (patches ++)
          pure envAlloc

nExprToC st (Record tag bindings) = do
  let runtimeBs = filter (not . skipBinding) bindings
  tagVar <- internTag st (T.unpack tag)
  if null runtimeBs
    then pure $ "mi_make_rec(" ++ tagVar ++ ", 0, NULL, NULL)"
    else do
      let n = length runtimeBs
      fieldExprs <- forM runtimeBs $ \b -> do
        val <- nExprToC st (bindBody b)
        pure (T.unpack (bindName b), val)
      let namesArr = "(const char*[]){" ++ intercalate ", " [cStringLit nm | (nm, _) <- fieldExprs] ++ "}"
          fieldsArr = "(MiVal[]){" ++ intercalate ", " [val | (_, val) <- fieldExprs] ++ "}"
      pure $ "mi_make_rec(" ++ tagVar ++ ", " ++ show n ++
             ", " ++ namesArr ++ ", " ++ fieldsArr ++ ")"

nExprToC st (FieldAccess e field) = do
  ec <- nExprToC st e
  pure $ "mi_struct_field(" ++ ec ++ ", " ++ cStringLit (T.unpack field) ++ ")"

nExprToC st (With body bindings)
  -- Tail-recursive loop: generated by Optimize.hs wrapTailRecBindings.
  -- Pattern: With (App...(Name "_f_loop") args...) [Binding "_f_loop" (Lam ...)]
  | Just (loopName, params, initArgs, loopBody) <- matchNativeTailLoop (With body bindings) = do
    cid <- freshId st
    let label = "_tco_" ++ show cid
        paramVars = [label ++ "_" ++ show i | i <- [0::Int .. length params - 1]]
        resultVar = "_tco_res_" ++ show cid
        loopScope = Map.fromList (zip params paramVars)
        bodyScope = withScope st loopScope
        -- Check if this loop can be fully unboxed (int64_t variables)
        paramSet = Set.fromList params
        bodyIsInt = isIntBody paramSet loopName (length params) loopBody
        bodyIsFloat = not bodyIsInt &&
                      isFloatBody paramSet loopName (length params) loopBody
    if bodyIsInt
      then do
        -- Fully unboxed int loop: int64_t variables, direct arithmetic
        initCs <- mapM (nExprToC st) initArgs
        let intTypes = [(p, NInt) | p <- params]
            intScope = withKnownTypes bodyScope intTypes
            initDecls = concat [ "int64_t " ++ pv ++ " = (" ++ ic ++ ").as.i; "
                               | (pv, ic) <- zip paramVars initCs ]
        bodyC <- nTailBodyToIntC intScope loopName (length params) paramVars label loopBody
        pure $ "({ " ++ initDecls ++ "MiVal " ++ resultVar ++ "; " ++
               label ++ ": " ++ resultVar ++ " = " ++ bodyC ++ "; " ++
               resultVar ++ "; })"
      else if bodyIsFloat
      then do
        -- Fully unboxed float loop: double variables, direct arithmetic
        initCs <- mapM (nExprToC st) initArgs
        let floatTypes = [(p, NFloat) | p <- params]
            floatScope = withKnownTypes bodyScope floatTypes
            initDecl pv ic arg = case inferType st arg of
              NInt   -> "double " ++ pv ++ " = (double)(" ++ ic ++ ").as.i; "
              _      -> "double " ++ pv ++ " = (" ++ ic ++ ").as.f; "
            initDecls = concat [ initDecl pv ic arg
                               | (pv, ic, arg) <- zip3 paramVars initCs initArgs ]
        bodyC <- nTailBodyToFloatC floatScope loopName (length params) paramVars label loopBody
        pure $ "({ " ++ initDecls ++ "MiVal " ++ resultVar ++ "; " ++
               label ++ ": " ++ resultVar ++ " = " ++ bodyC ++ "; " ++
               resultVar ++ "; })"
      else do
        -- Standard MiVal loop — but propagate any known types for params
        let paramTypes = [(p, inferType st arg) | (p, arg) <- zip params initArgs]
            typedScope = withKnownTypes bodyScope paramTypes
        initCs <- mapM (nExprToC st) initArgs
        bodyC <- nTailBodyToC typedScope loopName (length params) paramVars label loopBody
        let initDecls = concat [ "MiVal " ++ pv ++ " = " ++ ic ++ "; "
                               | (pv, ic) <- zip paramVars initCs ]
        pure $ "({ " ++ initDecls ++ "MiVal " ++ resultVar ++ "; " ++
               label ++ ": " ++ resultVar ++ " = " ++ bodyC ++ "; " ++
               resultVar ++ "; })"

nExprToC st (With body bindings) = do
  let runtimeBs = filter (not . skipBinding) bindings
  if null runtimeBs
    then nExprToC st body
    else do
      -- Use GCC statement expression: ({ MiVal v1 = ...; ...; body; })
      (bindDecls, newScope, typedSt) <- emitBindings st runtimeBs
      let bodyScope = withScope typedSt newScope
      bodyC <- nExprToC bodyScope body
      pure $ "({ " ++ concat bindDecls ++ bodyC ++ "; })"

nExprToC st (Case scrut alts) = do
  -- Detect the truthiness check pattern and emit efficient code
  case matchTruthiness scrut alts of
    Just (cond, thenBr, elseBr) -> do
      condC <- emitCondCheck st cond
      thenC <- nExprToC st thenBr
      elseC <- nExprToC st elseBr
      pure $ "(" ++ condC ++ " ? " ++ thenC ++ " : " ++ elseC ++ ")"
    Nothing -> do
      sc <- nExprToC st scrut
      cid <- freshId st
      let scrutVar = "_cs_" ++ show cid
      altCodes <- mapM (nAltToC st scrutVar) alts
      pure $ "({ MiVal " ++ scrutVar ++ " = " ++ sc ++ "; " ++
             buildTernaryChain altCodes ++ "; })"

nExprToC st (Thunk body) = do
  -- A thunk wraps its body in a MI_CLOSURE with param="_thunk_".
  -- When mi_force encounters this, it evaluates the body expression,
  -- which calls into a native C function that computes the actual value.
  cid <- freshId st
  let fvs = Set.toList (exprFreeVars body)
      isEffGlobal fv = Set.member fv (ncgGlobals st) &&
        Map.lookup fv (ncgScope st) == Just (cVarName fv)
      capturedFvs = filter (\fv -> Map.member fv (ncgScope st) &&
                                   not (isEffGlobal fv)) fvs
      fnName = "_nthunk_" ++ show cid
      envTypeName = "_ntenv_" ++ show cid

  if null capturedFvs
    then do
      bodyC <- nExprToC st body
      addTopDef st $ unlines
        [ "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env_raw) {"
        , "  (void)_arg; (void)_env_raw;"
        , "  return " ++ bodyC ++ ";"
        , "}"
        , ""
        ]
      pure $ "((MiVal){.type = MI_CLOSURE, .as.closure = {" ++
             "mi_expr_app(mi_expr_val(mi_native(" ++ fnName ++ ")), mi_expr_int(0)), " ++
             "\"_thunk_\", NULL}})"
    else do
      let innerScope = Map.fromList [(fv, "_env->" ++ sanitizeName fv) | fv <- capturedFvs]
          bodyScope = st { ncgScope = Map.union innerScope (ncgScope st) }
      bodyC <- nExprToC bodyScope body
      addTopDef st $ unlines $
        [ "typedef struct {"
        ] ++ ["  MiVal " ++ sanitizeName fv ++ ";" | fv <- capturedFvs] ++
        [ "} " ++ envTypeName ++ ";"
        , ""
        , "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env_raw) {"
        , "  (void)_arg;"
        , "  " ++ envTypeName ++ " *_env = (" ++ envTypeName ++ "*)_env_raw;"
        , "  return " ++ bodyC ++ ";"
        , "}"
        , ""
        ]
      -- Allocate env, populate captures, create MI_CLOSURE thunk
      pure $ "({ " ++ envTypeName ++ " *_te = mi_alloc(sizeof(" ++ envTypeName ++ ")); " ++
             concatMap (\fv ->
               "_te->" ++ sanitizeName fv ++ " = " ++
               maybe (cVarName fv) id (Map.lookup fv (ncgScope st)) ++ "; ") capturedFvs ++
             "(MiVal){.type = MI_CLOSURE, .as.closure = {" ++
             "mi_expr_app(mi_expr_val(mi_native_env(" ++ fnName ++ ", _te)), mi_expr_int(0)), " ++
             "\"_thunk_\", NULL}}; })"

nExprToC st (ListLit es) = nExprToC st (listLitToCons es)
  where
    listLitToCons [] = Record "Nil" []
    listLitToCons (x:xs) = Record "Cons" [mkBind "head" x, mkBind "tail" (listLitToCons xs)]
    mkBind n e = Binding { bindName = n, bindDomain = Value, bindParams = []
                         , bindBody = e, bindPos = Nothing }

nExprToC _ (Error msg) =
  pure $ "(mi_error(NULL, " ++ cStringLit (T.unpack msg) ++ "), mi_int(0))"

nExprToC _ (Import path) =
  pure $ "mi_int(0) /* unresolved import: " ++ T.unpack path ++ " */"

nExprToC st (Quote e) = nExprToC st e
nExprToC st (Splice e) = nExprToC st e

nExprToC st (CFunction hdr cname retTy paramTys stdImport) = do
  -- Reuse the standard codegen's FFI machinery (cfunctionToC).
  -- It generates mi_native(fn) closures with curried wrappers — exactly what we need.
  let hdrStr = T.unpack hdr
      inc = if stdImport then "<" ++ hdrStr ++ ">" else show hdrStr
  when (not (T.null hdr)) $
    addInclude st ("#include " ++ inc)
  let usesWchar t = case t of { CPtr name -> name == "wchar_t"; _ -> False }
  when (usesWchar retTy || any usesWchar paramTys) $
    addInclude st "#include <wchar.h>"
  -- Handle struct accessor pattern: __acc:StructType:field.path
  actualCName <- case T.stripPrefix "__acc:" cname of
    Just spec -> do
      let (structType, rest) = T.breakOn ":" spec
          fieldPath = T.drop 1 rest
      cid <- freshId st
      let fnName = "mi_acc_" ++ show cid
          accExpr = "((" ++ T.unpack structType ++ "*)_p)->" ++ T.unpack fieldPath
          fnDef = "static inline " ++ cRetTypeName retTy ++ " " ++ fnName ++
                  "(void *_p) {\n  return (" ++ cRetTypeName retTy ++ ")(" ++ accExpr ++ ");\n}\n\n"
      addTopDef st fnDef
      pure fnName
    Nothing -> pure (T.unpack cname)
  -- Create a CGState adapter sharing our IORefs
  intCtx <- newIORef Set.empty
  let cgSt = CGState (ncgNextId st) (ncgTopDefs st) (ncgIncludes st) intCtx
  cfunctionToC cgSt actualCName retTy paramTys

nExprToC st (Namespace bindings) = do
  -- Nested namespace: like With but without a distinct body
  let runtimeBs = filter (not . skipBinding) bindings
      -- The "body" is the last binding's name, or a record of all bindings
  (bindDecls, newScope, _) <- emitBindings st runtimeBs
  -- Return a record containing all the namespace's bindings
  let n = length runtimeBs
      namesArr = "(const char*[]){" ++ intercalate ", " [cStringLit (T.unpack (bindName b)) | b <- runtimeBs] ++ "}"
      fieldsArr = "(MiVal[]){" ++ intercalate ", " [maybe "mi_int(0)" id (Map.lookup (bindName b) newScope) | b <- runtimeBs] ++ "}"
  modTag <- internTag st "_module_"
  pure $ "({ " ++ concat bindDecls ++
         "mi_make_rec(" ++ modTag ++ ", " ++ show n ++ ", " ++ namesArr ++ ", " ++ fieldsArr ++ "); })"

-- ── Pattern Matching ───────────────────────────────────────────────

-- | Generate a condition + body pair for one case alternative
nAltToC :: NCGState -> String -> Alt -> IO (String, String)
nAltToC st scrutVar (Alt pat guard body) = do
  (cond, bindings) <- patternToC st scrutVar pat
  let newScope = Map.fromList [(T.pack k, v) | (k, v) <- bindings]
      bodyScope = withScope st newScope
  bodyC <- nExprToC bodyScope body
  -- Handle guard if present
  case guard of
    Nothing -> pure (cond, bodyC)
    Just g -> do
      guardC <- nExprToC bodyScope g
      pure ("(" ++ cond ++ " && mi_truthy(" ++ guardC ++ "))", bodyC)

-- | Generate a tag match condition. Uses pointer comparison first (fast path
-- for records created by native codegen), with strcmp fallback for records
-- created by runtime helpers like mi_cons.
tagMatchCond :: String -> String -> String
tagMatchCond scrutVar tagVar =
  scrutVar ++ ".type == MI_RECORD && (" ++ scrutVar ++ ".as.rec.tag == " ++ tagVar ++
  " || strcmp(" ++ scrutVar ++ ".as.rec.tag, " ++ tagVar ++ ") == 0)"

-- | Convert a pattern to a C condition expression + list of variable bindings.
-- Uses interned tags for pointer comparison instead of strcmp.
patternToC :: NCGState -> String -> Pat -> IO (String, [(String, String)])
patternToC _ _ PWild = pure ("1", [])
patternToC _ scrutVar (PVar v) = pure ("1", [(T.unpack v, scrutVar)])
patternToC _ scrutVar (PLit (IntLit n)) =
  pure ("(" ++ scrutVar ++ ".type == MI_INT && " ++ scrutVar ++ ".as.i == " ++ show n ++ ")", [])
patternToC _ scrutVar (PLit (StringLit s)) =
  pure ("(" ++ scrutVar ++ ".type == MI_STRING && strcmp(" ++ scrutVar ++ ".as.str.data, " ++ cStringLit (T.unpack s) ++ ") == 0)", [])
patternToC _ scrutVar (PLit _) = pure ("0 /* unsupported literal pattern */", [])
patternToC st scrutVar (PRec tag fieldPats) = do
  tagVar <- internTag st (T.unpack tag)
  let tagCond = tagMatchCond scrutVar tagVar
  results <- mapM (\((_name, subPat), idx) ->
      let fieldExpr = scrutVar ++ ".as.rec.fields[" ++ show idx ++ "]"
      in patternToC st fieldExpr subPat
    ) (zip fieldPats [0::Int ..])
  let (fieldConds, fieldBinds) = unzip results
      allCond = if null fieldConds
        then tagCond
        else "(" ++ tagCond ++ " && " ++ intercalate " && " fieldConds ++ ")"
  pure (allCond, concat fieldBinds)

patternToC st scrutVar (PList pats mRest) = do
  consTag <- internTag st "Cons"
  nilTag <- internTag st "Nil"
  let go sv [] Nothing = pure ("(" ++ tagMatchCond sv nilTag ++ ")", [])
      go sv [] (Just rest) = pure ("1", [(T.unpack rest, sv)])
      go sv (p:ps) mR = do
        let headExpr = sv ++ ".as.rec.fields[0]"
            tailExpr = sv ++ ".as.rec.fields[1]"
            consCond = tagMatchCond sv consTag
        (headCond, headBinds) <- patternToC st headExpr p
        (restCond, restBinds) <- go tailExpr ps mR
        pure ("(" ++ consCond ++ " && " ++ headCond ++ " && " ++ restCond ++ ")",
            headBinds ++ restBinds)
  go scrutVar pats mRest

-- | Build a ternary chain from condition/body pairs
buildTernaryChain :: [(String, String)] -> String
buildTernaryChain [] = "mi_int(0) /* no matching case */"
buildTernaryChain [(_, body)] = body  -- last alt (should be wildcard)
buildTernaryChain ((cond, body) : rest) =
  "(" ++ cond ++ ") ? " ++ body ++ " : " ++ buildTernaryChain rest

-- ── Truthiness Pattern Detection ───────────────────────────────────

-- | Detect the double-Case `if` pattern from the prelude:
--   Case (truthinessCheck cond) [Alt (PLit 0) Nothing elseBr, Alt PWild Nothing thenBr]
-- where truthinessCheck = Case cond [False→0, Nil→0, Nothing→0, 0→0, ""→0, _→1]
--
-- Returns (condition, thenBranch, elseBranch) for direct emission.
matchTruthiness :: Expr -> [Alt] -> Maybe (Expr, Expr, Expr)
matchTruthiness (Case innerScrut innerAlts) [Alt (PLit (IntLit 0)) Nothing elseBr, Alt PWild Nothing thenBr]
  | isTruthinessAlts innerAlts = Just (innerScrut, thenBr, elseBr)
matchTruthiness _ _ = Nothing

-- | Check if alts match the canonical truthiness pattern:
-- [False→0, Nil→0, Nothing→0, 0→0, ""→0, _→1]
isTruthinessAlts :: [Alt] -> Bool
isTruthinessAlts
  [ Alt (PRec "False" [])     Nothing (IntLit 0)
  , Alt (PRec "Nil" [])       Nothing (IntLit 0)
  , Alt (PRec "Nothing" [])   Nothing (IntLit 0)
  , Alt (PLit (IntLit 0))     Nothing (IntLit 0)
  , Alt (PLit (StringLit "")) Nothing (IntLit 0)
  , Alt PWild                 Nothing (IntLit 1)
  ] = True
isTruthinessAlts _ = False

-- | Emit a truthiness check for a condition expression.
-- Uses type inference to select the most efficient check:
-- - Known int: direct .as.i (skip mi_truthy)
-- - Comparison op: result is always int, use .as.i
-- - Known record: tag-based check (skip mi_truthy switch)
-- - Unknown: fall back to mi_truthy()
emitCondCheck :: NCGState -> Expr -> IO String
emitCondCheck st cond = case inferType st cond of
  NInt -> do
    c <- nExprToC st cond
    pure $ "(" ++ c ++ ").as.i"
  NRecord tag -> do
    -- Known record tag — truthiness is compile-time known
    pure $ if tag `elem` ["False", "Nil", "Nothing"] then "0" else "1"
  _ -> do
    c <- nExprToC st cond
    pure $ "mi_truthy(" ++ c ++ ")"

-- ── Native Binary Operations ───────────────────────────────────────

-- | Emit optimized inline C for binary operations.
-- Instead of calling mi_binop (string dispatch + type checks),
-- emit direct C with fast-path for int operands.
nativeBinop :: Text -> String -> String -> String
nativeBinop op lc rc = case op of
  "+"  -> intFastMath "+" lc rc
  "-"  -> intFastMath "-" lc rc
  "*"  -> intFastMath "*" lc rc
  "/"  -> intFastMath "/" lc rc
  "%"  -> intFastMath "%" lc rc
  "==" -> intFastCmp "==" lc rc
  "/=" -> intFastCmp "!=" lc rc
  "<"  -> intFastCmp "<" lc rc
  ">"  -> intFastCmp ">" lc rc
  "<=" -> intFastCmp "<=" lc rc
  ">=" -> intFastCmp ">=" lc rc
  -- Cons, logical ops, and others: fall through to mi_binop
  _    -> "mi_binop(" ++ cStringLit (T.unpack op) ++ ", " ++ lc ++ ", " ++ rc ++ ")"
  where
    -- Fast path: if both operands are MI_INT, do the op directly.
    -- Uses statement expression to avoid evaluating operands twice.
    intFastMath cop a b =
      "({ MiVal _a = " ++ a ++ ", _b = " ++ b ++ "; " ++
      "_a.type == MI_INT && _b.type == MI_INT ? " ++
      "mi_int(_a.as.i " ++ cop ++ " _b.as.i) : " ++
      "mi_binop(" ++ cStringLit (T.unpack op) ++ ", _a, _b); })"
    intFastCmp cop a b =
      "({ MiVal _a = " ++ a ++ ", _b = " ++ b ++ "; " ++
      "_a.type == MI_INT && _b.type == MI_INT ? " ++
      "mi_int(_a.as.i " ++ cop ++ " _b.as.i) : " ++
      "mi_binop(" ++ cStringLit (T.unpack op) ++ ", _a, _b); })"

-- | Check if an expression is already in WHNF (no thunk forcing needed).
-- Values produced by these expressions can never be MI_CLOSURE with _thunk_.
isNativeWHNF :: Expr -> Bool
isNativeWHNF (IntLit _) = True
isNativeWHNF (FloatLit _) = True
isNativeWHNF (StringLit _) = True
isNativeWHNF (Record _ _) = True
isNativeWHNF (Lam _ _) = True
isNativeWHNF (BinOp _ _ _) = True
isNativeWHNF _ = False

-- ── Unboxed Integer Loop Optimization ──────────────────────────────

-- | Check if an expression is guaranteed to produce an int value,
-- given a set of variable names known to be int-typed.
isKnownInt :: Set.Set Text -> Expr -> Bool
isKnownInt _    (IntLit _) = True
isKnownInt ints (Name n)   = Set.member n ints
isKnownInt ints (BinOp op l r)
  | op `elem` ["+","-","*","/","%","==","/=","<",">","<=",">="]
  = isKnownInt ints l && isKnownInt ints r
isKnownInt _ _ = False

-- | Check if a TCO loop body only uses int operations — all base-case
-- returns and tail-call arguments are int expressions on the given names.
isIntBody :: Set.Set Text -> Text -> Int -> Expr -> Bool
isIntBody ints funcName nparams = go
  where
    go (Case scrut alts)
      | Just (_, thenBr, elseBr) <- matchTruthiness scrut alts
      = go thenBr && go elseBr
    go (Case scrut alts)
      -- Non-truthiness case: scrutinee must be int, patterns must be int-compatible
      | isKnownInt ints scrut
      , all isIntAlt alts = all (\(Alt _ _ b) -> go b) alts
      | otherwise = False
    go (With b bs)
      -- Only allow With if all bindings are int-valued (safe to emit in unboxed context)
      = all (\bi -> isKnownInt ints (bindBody bi)) bs && go b
    go e
      | Just args <- matchCallChain funcName e
      , length args == nparams = all (isKnownInt ints) args
    go e = isKnownInt ints e

    isIntAlt (Alt (PLit (IntLit _)) _ _) = True
    isIntAlt (Alt PWild _ _) = True
    isIntAlt _ = False

-- | Emit an expression as an int64_t value (no MiVal boxing).
-- Only valid when the expression is known to produce MI_INT.
nExprToInt :: NCGState -> Expr -> IO String
nExprToInt _  (IntLit n) = pure (show n)
nExprToInt st (Name n) = case Map.lookup n (ncgScope st) of
  Just cname -> pure cname
  Nothing    -> pure $ "/* unbound " ++ T.unpack n ++ " */ 0"
nExprToInt st (BinOp op l r) = do
  lc <- nExprToInt st l
  rc <- nExprToInt st r
  pure $ "(" ++ lc ++ " " ++ intCOp op ++ " " ++ rc ++ ")"
nExprToInt st expr = do
  c <- nExprToC st expr
  pure $ "(" ++ c ++ ").as.i"

intCOp :: Text -> String
intCOp "==" = "=="; intCOp "/=" = "!="; intCOp "<" = "<"; intCOp ">" = ">"
intCOp "<=" = "<="; intCOp ">=" = ">="; intCOp "+" = "+"; intCOp "-" = "-"
intCOp "*" = "*"; intCOp "/" = "/"; intCOp "%" = "%"
intCOp op = error $ "intCOp: unsupported " ++ T.unpack op

-- ── Float Loop Optimization ───────────────────────────────────────

-- | Check if an expression is known to produce a float
isKnownFloat :: Set.Set Text -> Expr -> Bool
isKnownFloat _      (FloatLit _) = True
isKnownFloat _      (IntLit _)   = True  -- int promotes to float
isKnownFloat floats (Name n)     = Set.member n floats
isKnownFloat floats (BinOp op l r)
  | op `elem` ["+","-","*","/"]
  = isKnownFloat floats l && isKnownFloat floats r
isKnownFloat _ _ = False

-- | Check if a TCO loop body only uses float operations
isFloatBody :: Set.Set Text -> Text -> Int -> Expr -> Bool
isFloatBody floats funcName nparams = go
  where
    go (Case scrut alts)
      | Just (_, thenBr, elseBr) <- matchTruthiness scrut alts
      = go thenBr && go elseBr
    go (Case scrut alts)
      | isKnownFloat floats scrut || isKnownInt (Set.empty) scrut
      , all isNumAlt alts = all (\(Alt _ _ b) -> go b) alts
      | otherwise = False
    go (With b bs)
      = all (\bi -> isKnownFloat floats (bindBody bi) || isKnownInt Set.empty (bindBody bi)) bs && go b
    go e
      | Just args <- matchCallChain funcName e
      , length args == nparams = all (isKnownFloat floats) args
    go e = isKnownFloat floats e

    isNumAlt (Alt (PLit (IntLit _)) _ _) = True
    isNumAlt (Alt (PLit (FloatLit _)) _ _) = True
    isNumAlt (Alt PWild _ _) = True
    isNumAlt _ = False

-- | Emit an expression as a double value (no MiVal boxing)
nExprToFloat :: NCGState -> Expr -> IO String
nExprToFloat _ (FloatLit f) = pure (show f)
nExprToFloat _ (IntLit n)   = pure (show n ++ ".0")
nExprToFloat st (Name n) = case Map.lookup n (ncgScope st) of
  Just cname -> pure cname
  Nothing    -> pure $ "/* unbound " ++ T.unpack n ++ " */ 0.0"
nExprToFloat st (BinOp op l r) = do
  lc <- nExprToFloat st l
  rc <- nExprToFloat st r
  pure $ "(" ++ lc ++ " " ++ floatCOp op ++ " " ++ rc ++ ")"
nExprToFloat st expr = do
  c <- nExprToC st expr
  pure $ "(" ++ c ++ ").as.f"

floatCOp :: Text -> String
floatCOp "+" = "+"; floatCOp "-" = "-"; floatCOp "*" = "*"; floatCOp "/" = "/"
floatCOp "==" = "=="; floatCOp "/=" = "!="; floatCOp "<" = "<"; floatCOp ">" = ">"
floatCOp "<=" = "<="; floatCOp ">=" = ">="
floatCOp op = error $ "floatCOp: unsupported " ++ T.unpack op

-- | Compile TCO loop body with int64_t variables (fully unboxed).
-- Tail calls update int64_t vars and goto; base cases re-box with mi_int().
nTailBodyToIntC :: NCGState -> Text -> Int -> [String] -> String -> Expr -> IO String
nTailBodyToIntC st funcName nparams paramVars label expr
  -- Tail call: compute new int values and goto
  | Just args <- matchCallChain funcName expr
  , length args == nparams = do
    argCs <- mapM (nExprToInt st) args
    cid <- freshId st
    let tmpVars = ["_tc_" ++ show cid ++ "_" ++ show i | i <- [0::Int .. nparams - 1]]
        tmpDecls = concat ["int64_t " ++ tv ++ " = " ++ ac ++ "; " | (tv, ac) <- zip tmpVars argCs]
        assignments = concat [pv ++ " = " ++ tv ++ "; " | (pv, tv) <- zip paramVars tmpVars]
    pure $ "({ " ++ tmpDecls ++ assignments ++ "goto " ++ label ++ "; mi_int(0); })"

nTailBodyToIntC st funcName nparams paramVars label (Case scrut alts) =
  case matchTruthiness scrut alts of
    Just (cond, thenBr, elseBr) -> do
      condC <- nExprToInt st cond
      thenC <- nTailBodyToIntC st funcName nparams paramVars label thenBr
      elseC <- nTailBodyToIntC st funcName nparams paramVars label elseBr
      pure $ "(" ++ condC ++ " ? " ++ thenC ++ " : " ++ elseC ++ ")"
    Nothing
      -- Int scrutinee with int-compatible patterns: direct int comparison
      | isKnownInt (knownIntSet st) scrut -> do
        sc <- nExprToInt st scrut
        altPairs <- mapM (intTailAlt st funcName nparams paramVars label sc) alts
        pure $ buildTernaryChain altPairs
      | otherwise ->
        -- Fallback to MiVal for non-int cases
        nTailBodyToC st funcName nparams paramVars label (Case scrut alts)
  where
    intTailAlt s fn np pv lbl scC (Alt (PLit (IntLit n)) Nothing body) = do
      bodyC <- nTailBodyToIntC s fn np pv lbl body
      pure ("(" ++ scC ++ " == " ++ show n ++ ")", bodyC)
    intTailAlt s fn np pv lbl _ (Alt PWild Nothing body) = do
      bodyC <- nTailBodyToIntC s fn np pv lbl body
      pure ("1", bodyC)
    intTailAlt s fn np pv lbl _ (Alt _ _ body) = do
      bodyC <- nTailBodyToIntC s fn np pv lbl body
      pure ("0 /* unsupported pattern */", bodyC)

nTailBodyToIntC st funcName nparams paramVars label (With body bindings) = do
  let runtimeBs = filter (not . skipBinding) bindings
  if null runtimeBs
    then nTailBodyToIntC st funcName nparams paramVars label body
    else do
      (bindDecls, newScope, typedSt) <- emitBindings st runtimeBs
      let bodyScope = withScope typedSt newScope
      bodyC <- nTailBodyToIntC bodyScope funcName nparams paramVars label body
      pure $ "({ " ++ concat bindDecls ++ bodyC ++ "; })"

-- Base case: re-box int result as MiVal
nTailBodyToIntC st _ _ _ _ expr
  | isKnownInt (knownIntSet st) expr = do
    c <- nExprToInt st expr
    pure $ "mi_int(" ++ c ++ ")"
  | otherwise = nExprToC st expr

-- | Compile TCO loop body with double variables (fully unboxed float loop).
nTailBodyToFloatC :: NCGState -> Text -> Int -> [String] -> String -> Expr -> IO String
nTailBodyToFloatC st funcName nparams paramVars label expr
  | Just args <- matchCallChain funcName expr
  , length args == nparams = do
    argCs <- mapM (nExprToFloat st) args
    cid <- freshId st
    let tmpVars = ["_tc_" ++ show cid ++ "_" ++ show i | i <- [0::Int .. nparams - 1]]
        tmpDecls = concat ["double " ++ tv ++ " = " ++ ac ++ "; " | (tv, ac) <- zip tmpVars argCs]
        assignments = concat [pv ++ " = " ++ tv ++ "; " | (pv, tv) <- zip paramVars tmpVars]
    pure $ "({ " ++ tmpDecls ++ assignments ++ "goto " ++ label ++ "; mi_float(0); })"

nTailBodyToFloatC st funcName nparams paramVars label (Case scrut alts) =
  case matchTruthiness scrut alts of
    Just (cond, thenBr, elseBr) -> do
      condC <- emitCondCheck st cond
      thenC <- nTailBodyToFloatC st funcName nparams paramVars label thenBr
      elseC <- nTailBodyToFloatC st funcName nparams paramVars label elseBr
      pure $ "(" ++ condC ++ " ? " ++ thenC ++ " : " ++ elseC ++ ")"
    Nothing -> nTailBodyToC st funcName nparams paramVars label (Case scrut alts)

nTailBodyToFloatC st funcName nparams paramVars label (With body bindings) = do
  let runtimeBs = filter (not . skipBinding) bindings
  if null runtimeBs
    then nTailBodyToFloatC st funcName nparams paramVars label body
    else do
      (bindDecls, newScope, typedSt) <- emitBindings st runtimeBs
      let bodyScope = withScope typedSt newScope
      bodyC <- nTailBodyToFloatC bodyScope funcName nparams paramVars label body
      pure $ "({ " ++ concat bindDecls ++ bodyC ++ "; })"

nTailBodyToFloatC st _ _ _ _ expr
  | isKnownFloat (knownFloatSet st) expr = do
    c <- nExprToFloat st expr
    pure $ "mi_float(" ++ c ++ ")"
  | otherwise = nExprToC st expr

-- | Match a tail-recursive With pattern produced by wrapTailRecBindings.
-- Returns (loopName, paramNames, initArgExprs, loopBody)
matchNativeTailLoop :: Expr -> Maybe (Text, [Text], [Expr], Expr)
matchNativeTailLoop (With callExpr [b])
  | bindDomain b == Value
  , not (null params)
  , let funcName = bindName b
  , Just initArgs <- matchCallChain funcName callExpr
  , length initArgs == length params
  , allTailCalls funcName (length params) lamBody
  = Just (funcName, params, initArgs, lamBody)
  where
    (params, lamBody) = unwrapLams' (bindBody b)
    unwrapLams' (Lam p body) = let (ps, b') = unwrapLams' body in (lamParamName p : ps, b')
    unwrapLams' body = ([], body)
matchNativeTailLoop _ = Nothing

-- | Match a chain of App calls: App(App(Name f, a1), a2) → Just [a1, a2]
matchCallChain :: Text -> Expr -> Maybe [Expr]
matchCallChain funcName expr = go expr []
  where
    go (App f x) args = go f (x : args)
    go (Name n) args | n == funcName = Just args
    go _ _ = Nothing

-- | Check if all occurrences of funcName are in tail position
allTailCalls :: Text -> Int -> Expr -> Bool
allTailCalls funcName nparams body = tailCheck body
  where
    tailCheck (Case scrut alts) =
      noRef scrut && all (\(Alt _ g b) -> maybe True noRef g && tailCheck b) alts
    tailCheck e
      | Just args <- matchCallChain funcName e
      , length args == nparams = all noRef args
    tailCheck (Name n) = n /= funcName
    tailCheck (BinOp _ l r) = noRef l && noRef r
    tailCheck (App f x) = noRef f && noRef x
    tailCheck (Lam _ b) = noRef b
    tailCheck (With b bs) = tailCheck b && all (noRef . bindBody) bs
    tailCheck (Record _ bs) = all (noRef . bindBody) bs
    tailCheck (FieldAccess e _) = noRef e
    tailCheck (Thunk b) = noRef b
    tailCheck _ = True

    noRef (Name n) = n /= funcName
    noRef (BinOp _ l r) = noRef l && noRef r
    noRef (App f x) = noRef f && noRef x
    noRef (Lam _ b) = noRef b
    noRef (Case scrut alts) = noRef scrut && all (\(Alt _ g b) -> maybe True noRef g && noRef b) alts
    noRef (With b bs) = noRef b && all (noRef . bindBody) bs
    noRef (Record _ bs) = all (noRef . bindBody) bs
    noRef (FieldAccess e _) = noRef e
    noRef (Thunk b) = noRef b
    noRef (Namespace bs) = all (noRef . bindBody) bs
    noRef (ListLit es) = all noRef es
    noRef _ = True

-- | Compile a loop body with tail calls as gotos.
-- In tail position, self-calls update loop vars and goto the label.
-- In non-tail position, falls through to normal nExprToC.
nTailBodyToC :: NCGState -> Text -> Int -> [String] -> String -> Expr -> IO String
nTailBodyToC st funcName nparams paramVars label expr
  -- Tail call: update params and goto
  | Just args <- matchCallChain funcName expr
  , length args == nparams = do
    argCs <- mapM (nExprToC st) args
    cid <- freshId st
    let tmpVars = ["_tc_" ++ show cid ++ "_" ++ show i | i <- [0::Int .. nparams - 1]]
        tmpDecls = concat ["MiVal " ++ tv ++ " = " ++ ac ++ "; " | (tv, ac) <- zip tmpVars argCs]
        assignments = concat [pv ++ " = " ++ tv ++ "; " | (pv, tv) <- zip paramVars tmpVars]
    pure $ "({ " ++ tmpDecls ++ assignments ++ "goto " ++ label ++ "; mi_int(0); })"

nTailBodyToC st funcName nparams paramVars label (Case scrut alts) = do
  -- Detect truthiness pattern in tail position too
  case matchTruthiness scrut alts of
    Just (cond, thenBr, elseBr) -> do
      condC <- emitCondCheck st cond
      thenC <- nTailBodyToC st funcName nparams paramVars label thenBr
      elseC <- nTailBodyToC st funcName nparams paramVars label elseBr
      pure $ "(" ++ condC ++ " ? " ++ thenC ++ " : " ++ elseC ++ ")"
    Nothing -> do
      sc <- nExprToC st scrut
      cid <- freshId st
      let scrutVar = "_cs_" ++ show cid
      altCodes <- mapM (nTailAltToC st funcName nparams paramVars label scrutVar) alts
      pure $ "({ MiVal " ++ scrutVar ++ " = " ++ sc ++ "; " ++
             buildTernaryChain altCodes ++ "; })"

nTailBodyToC st funcName nparams paramVars label (With body bindings) = do
  let runtimeBs = filter (not . skipBinding) bindings
  if null runtimeBs
    then nTailBodyToC st funcName nparams paramVars label body
    else do
      (bindDecls, newScope, typedSt) <- emitBindings st runtimeBs
      let bodyScope = withScope typedSt newScope
      bodyC <- nTailBodyToC bodyScope funcName nparams paramVars label body
      pure $ "({ " ++ concat bindDecls ++ bodyC ++ "; })"

-- Non-tail position: use normal code generation
nTailBodyToC st _ _ _ _ expr = nExprToC st expr

-- | Like nAltToC but compiles bodies in tail-call mode
nTailAltToC :: NCGState -> Text -> Int -> [String] -> String -> String -> Alt -> IO (String, String)
nTailAltToC st funcName nparams paramVars label scrutVar (Alt pat guard body) = do
  (cond, bindings) <- patternToC st scrutVar pat
  let newScope = Map.fromList [(T.pack k, v) | (k, v) <- bindings]
      bodyScope = withScope st newScope
  bodyC <- nTailBodyToC bodyScope funcName nparams paramVars label body
  case guard of
    Nothing -> pure (cond, bodyC)
    Just g -> do
      guardC <- nExprToC bodyScope g
      pure ("(" ++ cond ++ " && mi_truthy(" ++ guardC ++ "))", bodyC)

-- ── Binding Emission ───────────────────────────────────────────────

-- | Emit C declarations for a list of bindings, returning the decl strings,
--   a scope map of the new variable names, and the updated type knowledge.
emitBindings :: NCGState -> [Binding] -> IO ([String], Map.Map Text String, NCGState)
emitBindings st bindings = do
  -- Pre-compute all binding names and cvars for self-recursive support
  -- Handle duplicate names by suffixing with unique counter
  cid <- freshId st
  let assignCVars :: [Binding] -> Set.Set Text -> [(Binding, String)] -> [(Binding, String)]
      assignCVars [] _ acc = reverse acc
      assignCVars (b:bs) seen acc =
        let name = bindName b
            baseCVar = cVarName name
            (cvar, seen') = if Set.member name seen
              then let idx = length (filter (\(b', _) -> bindName b' == name) acc) + 1
                   in (baseCVar ++ "_" ++ show cid ++ "_" ++ show idx, seen)
              else (baseCVar, Set.insert name seen)
        in assignCVars bs seen' ((b, cvar) : acc)
      bsWithCVars = assignCVars bindings Set.empty []
      bindingScope = Map.fromList [(bindName b, cv) | (b, cv) <- bsWithCVars]
      -- Detect self-recursive bindings
      selfRecNames = Set.fromList
        [ bindName b | b <- bindings
        , let bodyExpr = if null (bindParams b) then bindBody b
                         else foldr Lam (bindBody b) (bindParams b)
        , Set.member (bindName b) (exprFreeVars bodyExpr)
        ]
      -- Check for forward references: does any binding reference a later-defined name?
      hasForwardRef = any (\(i, b) ->
        let bodyFvs = exprFreeVars (bindBody b)
            laterNames = Set.fromList [bindName b' | (j, b') <- zip [0::Int ..] bindings, j > i]
        in not (Set.null (Set.intersection bodyFvs laterNames))
        ) (zip [0::Int ..] bindings)
      -- Also need forward decls when there are mutual references
      needForwardDecls = hasForwardRef || Set.size selfRecNames > 1
  if needForwardDecls && length bsWithCVars > 1
    then do
      -- Two-phase: forward-declare all vars, then assign
      let fwdDecls = [("MiVal " ++ cv ++ "; ") | (_, cv) <- bsWithCVars]
          -- Set forward-declared names so lambda captures can defer patches
          fwdNames = Set.fromList [bindName b | (b, _) <- bsWithCVars]
          stWithFwd = st { ncgForwardDecls = Set.union fwdNames (ncgForwardDecls st) }
          -- Separate closure bindings from non-closure bindings
          isLamBind (b, _) = case (if null (bindParams b) then bindBody b
                                   else foldr Lam (bindBody b) (bindParams b)) of
            Lam _ _ -> True
            _ -> False
          (closureBs, otherBs) = partition isLamBind bsWithCVars
      -- Clear any previous patches
      writeIORef (ncgEnvPatches st) []
      -- Process closures first
      (closureDecls, scope1, st1, patches) <- goAssign stWithFwd fwdDecls bindingScope [] selfRecNames closureBs
      -- Then process non-closures (patches are already resolved)
      (otherDecls, scope2, st2, _) <- goAssign st1 [] scope1 [] selfRecNames otherBs
      -- Order: fwdDecls + closure assignments + patches + other assignments
      let finalSt' = st2 { ncgForwardDecls = ncgForwardDecls st }
      pure (closureDecls ++ patches ++ otherDecls, scope2, finalSt')
    else
      go st [] bindingScope selfRecNames bsWithCVars
  where
    -- Standard single-phase: declare + assign in one statement
    go curSt accDecls accScope _ [] = pure (reverse accDecls, accScope, curSt)
    go curSt accDecls accScope selfRec ((b, cvar):bs) = do
      let name = bindName b
          curScope = withScope curSt accScope
          curScope' = if Set.member name selfRec
            then curScope { ncgSelfNames = Set.insert name (ncgSelfNames curScope) }
            else curScope
      bodyExpr <- if null (bindParams b)
        then pure (bindBody b)
        else pure $ foldr Lam (bindBody b) (bindParams b)
      let compiledExpr = if bindDomain b == Lazy then Thunk bodyExpr else bodyExpr
      code <- nExprToC curScope' compiledExpr
      let decl = if bindDomain b == Lazy
            then "MiVal " ++ cvar ++ " = " ++ code ++ "; "
            else if isNativeWHNF bodyExpr
              then "MiVal " ++ cvar ++ " = " ++ code ++ "; "
              else "MiVal " ++ cvar ++ " = mi_force(" ++ code ++ ", NULL); "
      let inferredType = if bindDomain b == Lazy then NUnknown
                         else inferType curScope' bodyExpr
          nextSt = if inferredType /= NUnknown
                   then withKnownType curSt name inferredType
                   else curSt
      go nextSt (decl : accDecls) (Map.insert name cvar accScope) selfRec bs

    -- Two-phase: vars already forward-declared, emit assignments only
    goAssign curSt accDecls accScope accPatches _ [] = pure (reverse accDecls, accScope, curSt, reverse accPatches)
    goAssign curSt accDecls accScope accPatches selfRec ((b, cvar):bs) = do
      let name = bindName b
          curScope = withScope curSt accScope
          curScope' = if Set.member name selfRec
            then curScope { ncgSelfNames = Set.insert name (ncgSelfNames curScope) }
            else curScope
      bodyExpr <- if null (bindParams b)
        then pure (bindBody b)
        else pure $ foldr Lam (bindBody b) (bindParams b)
      let compiledExpr = if bindDomain b == Lazy then Thunk bodyExpr else bodyExpr
      -- Snapshot patches before compilation
      patchesBefore <- readIORef (ncgEnvPatches curSt)
      code <- nExprToC curScope' compiledExpr
      -- Collect new patches and associate with this binding's cvar
      patchesAfter <- readIORef (ncgEnvPatches curSt)
      writeIORef (ncgEnvPatches curSt) patchesBefore
      let newPatches = take (length patchesAfter - length patchesBefore) patchesAfter
          resolvedPatches = map (\(envType, field, val) ->
            "((" ++ envType ++ " *)" ++ cvar ++ ".as.native.env)->" ++ field ++ " = " ++ val ++ "; ")
            newPatches
      let assign = if bindDomain b == Lazy
            then cvar ++ " = " ++ code ++ "; "
            else if isNativeWHNF bodyExpr
              then cvar ++ " = " ++ code ++ "; "
              else cvar ++ " = mi_force(" ++ code ++ ", NULL); "
      let inferredType = if bindDomain b == Lazy then NUnknown
                         else inferType curScope' bodyExpr
          nextSt = if inferredType /= NUnknown
                   then withKnownType curSt name inferredType
                   else curSt
      goAssign nextSt (assign : accDecls) (Map.insert name cvar accScope) (resolvedPatches ++ accPatches) selfRec bs

-- ── Helpers ────────────────────────────────────────────────────────

-- | Operators handled by the C runtime's mi_binop
isBuiltinOp :: Text -> Bool
isBuiltinOp op = op `elem`
  [ "+", "-", "*", "/", "%", "**", "<", ">", "<=", ">=", "==", "/="
  , ":" ]

-- | Skip type/trait/doc/parse annotations and module refs
skipBinding :: Binding -> Bool
skipBinding b = bindDomain b `elem` [Type, Trait, Doc, Parse] || isModuleRef (bindName b)
  where isModuleRef n = "__mod_" `T.isPrefixOf` n && "__" `T.isSuffixOf` n

-- | Dead code elimination: compute the set of names transitively reachable
--   from a set of root names. Only emit bindings for reachable names.
reachableNames :: [Binding] -> Set.Set Text -> Set.Set Text
reachableNames bindings roots =
  let nameToFvs = Map.fromList
        [ (bindName b, bodyFvs b)
        | b <- bindings ]
      bodyFvs b =
        let body = if null (bindParams b) then bindBody b
                   else foldr Lam (bindBody b) (bindParams b)
        in exprFreeVars body
      go visited frontier
        | Set.null frontier = visited
        | otherwise =
            let newFvs = Set.unions [Map.findWithDefault Set.empty n nameToFvs | n <- Set.toList frontier]
                newNames = Set.difference newFvs visited
            in go (Set.union visited newNames) newNames
  in go roots roots

-- ── Top-Level Code Generation ──────────────────────────────────────

-- | Detect a `main` binding that takes at least one parameter.
isMainBinding :: Binding -> Bool
isMainBinding b = bindName b == "main" &&
  (not (null (bindParams b)) || isLam (bindBody b))
  where
    isLam (Lam _ _) = True
    isLam _         = False

-- | Built-in functions registered in main()
builtinEntries :: [(String, String)]
builtinEntries =
  [ ("if",          "mi_native(mi_builtin_if)")
  , ("truthy",      "mi_native(mi_builtin_truthy)")
  , ("strlen",      "mi_native(mi_builtin_len)")
  , ("len",         "mi_native(mi_builtin_len)")
  , ("charAt",      "mi_native(mi_builtin_charAt)")
  , ("slice",       "mi_native(mi_builtin_slice)")
  , ("indexOf",     "mi_native(mi_builtin_indexOf)")
  , ("split",       "mi_native(mi_builtin_split)")
  , ("trim",        "mi_native(mi_builtin_trim)")
  , ("toUpper",     "mi_native(mi_builtin_toUpper)")
  , ("toLower",     "mi_native(mi_builtin_toLower)")
  , ("replace",     "mi_native(mi_builtin_replace)")
  , ("toString",    "mi_native(mi_builtin_toString)")
  , ("_toString",   "mi_native(mi_builtin_toString)")
  , ("toInt",       "mi_native(mi_builtin_toInt)")
  , ("toFloat",     "mi_native(mi_builtin_toFloat)")
  , ("float",       "mi_native(mi_builtin_float)")
  , ("round",       "mi_native(mi_builtin_round)")
  , ("floor",       "mi_native(mi_builtin_floor)")
  , ("ceil",        "mi_native(mi_builtin_ceil)")
  , ("fields",      "mi_native(mi_builtin_fields)")
  , ("fieldNames",  "mi_native(mi_builtin_fieldNames)")
  , ("tag",         "mi_native(mi_builtin_tag)")
  , ("getField",    "mi_native(mi_builtin_getField)")
  , ("setField",    "mi_native(mi_builtin_setField)")
  , ("gc_manage",   "mi_native(mi_builtin_gc_manage)")
  , ("__sized_int", "mi_native(mi_builtin_sized_int)")
  , ("__sized_uint","mi_native(mi_builtin_sized_uint)")
  ]

-- | Generate a complete C program using native code generation.
-- Instead of building expression trees for mi_eval, this emits direct
-- C function calls and local variables.
nativeCodegen :: Handle -> Set.Set Text -> Expr -> IO ()
nativeCodegen h hidden expr = do
  st <- newNCGState

  -- Register builtins in scope
  let builtinScope = Map.fromList
        [ (T.pack name, cVarName (T.pack name)) | (name, _) <- builtinEntries ]
  let st' = withScope st builtinScope

  mainCode <- case expr of
    Namespace bs -> nativeNamespaceToC st' hidden bs
    _ -> do
      code <- nExprToC st' expr
      pure $ unlines
        [ "int main(int argc, char **argv) {"
        , "  (void)argc; (void)argv;"
        , "  MiEnv *_env = mi_env_new(NULL);"  -- keep for builtins that need it
        , "  mi_println_val(" ++ code ++ ");"
        , "  return 0;"
        , "}"
        ]

  -- Emit the C file
  emitPreamble h
  incs <- readIORef (ncgIncludes st)
  mapM_ (\inc -> hPutStrLn h inc) (nub (reverse incs))
  -- Emit interned tag string declarations
  tagDecls <- emitTagDecls st
  unless (null tagDecls) $ do
    hPutStrLn h "\n// Interned record tag strings"
    mapM_ (hPutStrLn h) tagDecls
    hPutStrLn h ""
  defs <- readIORef (ncgTopDefs st)
  mapM_ (hPutStr h) (reverse defs)
  hPutStrLn h ""
  hPutStr h mainCode

-- | Generate main() for a top-level Namespace (list of bindings)
nativeNamespaceToC :: NCGState -> Set.Set Text -> [Binding] -> IO String
nativeNamespaceToC st hidden allBindings = do
  let runtimeBs = filter (not . skipBinding) allBindings
      hasMainWithArg = any isMainBinding runtimeBs
      -- Dead code elimination: only emit bindings reachable from roots
      roots = if hasMainWithArg
              then Set.singleton "main"
              else Set.fromList [bindName b | b <- runtimeBs,
                                 not (Set.member (bindName b) hidden)]
      reachable = reachableNames runtimeBs roots
      -- Only include builtins that are reachable (referenced by live code)
      liveBuiltinEntries = [(n, v) | (n, v) <- builtinEntries,
                            Set.member (T.pack n) reachable]
      liveBuiltinNames = [T.pack n | (n, _) <- liveBuiltinEntries]
      liveBs = filter (\b -> Set.member (bindName b) reachable) runtimeBs
  ref <- newIORef ""
  let emit s = modifyIORef ref (++ s)

  -- Pre-declare live namespace bindings + builtins as static globals
  let allNames = [bindName b | b <- liveBs]
      allGlobalNames = Set.fromList (allNames ++ liveBuiltinNames)
      globalScope = Map.fromList $
            [(name, cVarName name) | name <- allNames] ++
            [(T.pack n, cVarName (T.pack n)) | (n, _) <- liveBuiltinEntries] ++
            (if not hasMainWithArg then [("world", cVarName "world")] else [])
      st' = withGlobals (withScope st globalScope) allGlobalNames

  -- Emit static global declarations for all namespace bindings
  addTopDef st $ unlines $
    ["// Static globals for namespace bindings"] ++
    ["static MiVal " ++ cVarName name ++ ";" | name <- allNames,
     not (Set.member name (Set.fromList liveBuiltinNames))]

  -- Builtins are also static globals
  addTopDef st $ unlines $
    ["// Static globals for builtins"] ++
    ["static MiVal " ++ cVarName (T.pack n) ++ ";" | (n, _) <- liveBuiltinEntries]

  if not hasMainWithArg
    then addTopDef st "static MiVal _v_world;\n"
    else pure ()

  emit "int main(int argc, char **argv) {\n"
  emit "  (void)argc; (void)argv;\n"

  -- Initialize builtins
  emit "  // Built-in functions\n"
  mapM_ (\(name, val) -> do
    emit $ "  " ++ cVarName (T.pack name) ++ " = " ++ val ++ ";\n"
    ) liveBuiltinEntries
  emit "\n"

  if not hasMainWithArg
    then emit "  _v_world = mi_build_world(argc, argv);\n\n"
    else pure ()

  -- Emit user bindings (all assignments, no declarations)
  -- Track inferred types as we process each binding for type propagation
  emit "  // User bindings\n"
  let emitBinding curSt b = do
        let name = bindName b
            cvar = cVarName name
        bodyExpr <- if null (bindParams b)
          then pure (bindBody b)
          else pure $ foldr Lam (bindBody b) (bindParams b)
        -- Lazy bindings: wrap body in Thunk to defer evaluation
        let compiledExpr = if bindDomain b == Lazy then Thunk bodyExpr else bodyExpr
        code <- nExprToC curSt compiledExpr
        -- Value bindings: force (no-op for non-thunks, evaluates deferred thunks)
        let forceCode = if bindDomain b == Lazy then code
                        else "mi_force(" ++ code ++ ", NULL)"
        emit $ "  " ++ cvar ++ " = " ++ forceCode ++ ";\n"
        -- Propagate inferred type (skip lazy bindings — they're thunks at runtime)
        let inferredType = if bindDomain b == Lazy then NUnknown
                           else inferType curSt bodyExpr
        pure $ if inferredType /= NUnknown
               then withKnownType curSt name inferredType
               else curSt
  foldM_ (\curSt b -> emitBinding curSt b) st' liveBs

  -- Re-register len/strlen after prelude (overrides prelude's definition)
  when (Set.member "len" reachable || Set.member "strlen" reachable) $ do
    emit "\n  // Re-register native len after prelude\n"
    when (Set.member "len" reachable) $
      emit $ "  " ++ cVarName "len" ++ " = mi_native(mi_builtin_len);\n"
    when (Set.member "strlen" reachable) $
      emit $ "  " ++ cVarName "strlen" ++ " = mi_native(mi_builtin_len);\n"

  emit "\n"
  if hasMainWithArg
    then do
      emit "  mi_in_eval = 1;\n"
      emit $ "  MiVal _world = mi_build_world(argc, argv);\n"
      emit $ "  MiVal _result = mi_apply(" ++ cVarName "main" ++ ", _world);\n"
      emit "  return (_result.type == MI_INT) ? (int)_result.as.i : 0;\n"
      emit "}\n"
    else do
      mapM_ (\b -> do
        let name = bindName b
            cvar = cVarName name
        if Set.member name hidden
          then pure ()
          else emit $ "  printf(\"" ++ T.unpack name ++ " = \"); mi_print_val(" ++ cvar ++ "); printf(\"\\n\");\n"
        ) liveBs
      emit "  return 0;\n"
      emit "}\n"

  readIORef ref


