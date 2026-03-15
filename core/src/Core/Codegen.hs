{-# LANGUAGE OverloadedStrings #-}
module Core.Codegen (codegen, emitPreamble, CGState(..), cfunctionToC, cRetTypeName) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.IORef
import Data.List (intercalate, isInfixOf, foldl', nub)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Core.Syntax
import System.IO (Handle, hPutStr, hPutStrLn)
import System.FilePath (takeFileName)

-- | Operators handled by the C runtime's mi_binop
isBuiltinOp :: Text -> Bool
isBuiltinOp op = op `elem`
  [ "+", "-", "*", "/", "%", "**", "<", ">", "<=", ">=", "==", "/="
  , ":" ]

-- | Codegen state
data CGState = CGState
  { cgNextId   :: IORef Int
  , cgTopDefs  :: IORef [String]
  , cgIncludes :: IORef [String]
  , cgIntCtx   :: IORef (Set.Set Text)  -- known-integer variable names for native binop
  }

newCGState :: IO CGState
newCGState = CGState <$> newIORef 0 <*> newIORef [] <*> newIORef [] <*> newIORef Set.empty

freshId :: CGState -> IO Int
freshId st = do
  n <- readIORef (cgNextId st)
  writeIORef (cgNextId st) (n + 1)
  pure n

addTopDef :: CGState -> String -> IO ()
addTopDef st s = modifyIORef (cgTopDefs st) (s :)

-- | Execute an action with a set of known-integer variable names,
-- restoring the previous context afterward.
withIntCtx :: CGState -> Set.Set Text -> IO a -> IO a
withIntCtx st ctx action = do
  old <- readIORef (cgIntCtx st)
  writeIORef (cgIntCtx st) (Set.union old ctx)
  result <- action
  writeIORef (cgIntCtx st) old
  pure result

-- | Check if an expression is known to produce an integer value.
isKnownInt :: Set.Set Text -> Expr -> Bool
isKnownInt _ (IntLit _) = True
isKnownInt _ (SizedInt _ _ _) = True
isKnownInt ctx (Name n) = Set.member n ctx
isKnownInt ctx (BinOp op l r) = isBuiltinOp op && isKnownInt ctx l && isKnownInt ctx r
isKnownInt _ _ = False

-- | Check if an expression is a simple integer initializer.
isIntInit :: Expr -> Bool
isIntInit (IntLit _) = True
isIntInit (SizedInt _ _ _) = True
isIntInit (BinOp op l r) = isBuiltinOp op && isIntInit l && isIntInit r
isIntInit _ = False

-- | Should this binding domain be skipped during codegen?
skipDomain :: Domain -> Bool
skipDomain Type  = True
skipDomain Trait = True
skipDomain Doc   = True
skipDomain Parse = True
skipDomain _     = False

-- | Should this binding be skipped entirely during codegen?
skipBinding :: Binding -> Bool
skipBinding b = skipDomain (bindDomain b) || isModuleRef (bindName b)
  where isModuleRef n = "__mod_" `T.isPrefixOf` n && "__" `T.isSuffixOf` n

-- | Generate C code from a (partially reduced) Expr
codegen :: Handle -> Set.Set Text -> Expr -> IO ()
codegen h hidden expr = do
  st <- newCGState
  mainCode <- captureIO st hidden expr
  emitPreamble h
  incs <- readIORef (cgIncludes st)
  mapM_ (\inc -> hPutStrLn h inc) (nub (reverse incs))
  defs <- readIORef (cgTopDefs st)
  mapM_ (hPutStr h) (reverse defs)
  hPutStrLn h ""
  hPutStr h mainCode

-- | Build main()
captureIO :: CGState -> Set.Set Text -> Expr -> IO String
captureIO st hidden expr = do
  ref <- newIORef ""
  let emit s = modifyIORef ref (++ s)
  let emitBuiltins = do
        emit "  mi_env_set(_env, \"if\", mi_native(mi_builtin_if));\n"
        emit "  mi_env_set(_env, \"truthy\", mi_native(mi_builtin_truthy));\n"
        emit "  mi_env_set(_env, \"strlen\", mi_native(mi_builtin_len));\n"
        emit "  mi_env_set(_env, \"len\", mi_native(mi_builtin_len));\n"
        emit "  mi_env_set(_env, \"charAt\", mi_native(mi_builtin_charAt));\n"
        emit "  mi_env_set(_env, \"slice\", mi_native(mi_builtin_slice));\n"
        emit "  mi_env_set(_env, \"indexOf\", mi_native(mi_builtin_indexOf));\n"
        emit "  mi_env_set(_env, \"split\", mi_native(mi_builtin_split));\n"
        emit "  mi_env_set(_env, \"trim\", mi_native(mi_builtin_trim));\n"
        emit "  mi_env_set(_env, \"toUpper\", mi_native(mi_builtin_toUpper));\n"
        emit "  mi_env_set(_env, \"toLower\", mi_native(mi_builtin_toLower));\n"
        emit "  mi_env_set(_env, \"replace\", mi_native(mi_builtin_replace));\n"
        emit "  mi_env_set(_env, \"toString\", mi_native(mi_builtin_toString));\n"
        emit "  mi_env_set(_env, \"_toString\", mi_native(mi_builtin_toString));\n"
        emit "  mi_env_set(_env, \"toInt\", mi_native(mi_builtin_toInt));\n"
        emit "  mi_env_set(_env, \"toFloat\", mi_native(mi_builtin_toFloat));\n"
        emit "  mi_env_set(_env, \"float\", mi_native(mi_builtin_float));\n"
        emit "  mi_env_set(_env, \"round\", mi_native(mi_builtin_round));\n"
        emit "  mi_env_set(_env, \"floor\", mi_native(mi_builtin_floor));\n"
        emit "  mi_env_set(_env, \"ceil\", mi_native(mi_builtin_ceil));\n"
        emit "  mi_env_set(_env, \"fields\", mi_native(mi_builtin_fields));\n"
        emit "  mi_env_set(_env, \"fieldNames\", mi_native(mi_builtin_fieldNames));\n"
        emit "  mi_env_set(_env, \"tag\", mi_native(mi_builtin_tag));\n"
        emit "  mi_env_set(_env, \"getField\", mi_native(mi_builtin_getField));\n"
        emit "  mi_env_set(_env, \"setField\", mi_native(mi_builtin_setField));\n"
        emit "  mi_env_set(_env, \"gc_manage\", mi_native(mi_builtin_gc_manage));\n"
        emit "  mi_env_set(_env, \"__sized_int\", mi_native(mi_builtin_sized_int));\n"
        emit "  mi_env_set(_env, \"__sized_uint\", mi_native(mi_builtin_sized_uint));\n"
        emit "\n"
  case expr of
    Namespace bs -> do
      let runtimeBs = filter (not . skipBinding) bs
      let hasMainWithArg = any isMainBinding runtimeBs
      emit "int main(int argc, char **argv) {\n"
      emit "  MiEnv *_env = mi_env_new(NULL);\n"
      emitBuiltins
      if not hasMainWithArg
        then emit "  mi_env_set(_env, \"world\", mi_build_world(argc, argv));\n"
        else pure ()
      mapM_ (\b -> do
        code <- bindingEvalCode st b
        emit code
        ) runtimeBs
      -- Re-emit native len after prelude so it handles both strings and lists
      emit "  mi_env_set(_env, \"len\", mi_native(mi_builtin_len));\n"
      emit "  mi_env_set(_env, \"strlen\", mi_native(mi_builtin_len));\n"
      emit "\n"
      if hasMainWithArg
        then do
          emit "  mi_in_eval = 1;\n"
          emit "  MiVal _world = mi_build_world(argc, argv);\n"
          emit "  MiVal _result = mi_apply(mi_env_get(_env, \"main\"), _world);\n"
          emit "  return (_result.type == MI_INT) ? (int)_result.as.i : ((_result.type == MI_SIZED_INT) ? (int)(_result.as.sized.is_big ? mi_bn_to_i64(_result.as.sized.big) : _result.as.sized.i) : 0);\n}\n"
        else do
          mapM_ (\b -> do
            let name = T.unpack (bindName b)
            if Set.member (bindName b) hidden
              then pure ()
              else emit $ "  printf(\"" ++ name ++ " = \"); mi_print_val(mi_env_get(_env, \"" ++ name ++ "\")); printf(\"\\n\");\n"
            ) runtimeBs
          emit "  return 0;\n}\n"
    _ -> do
      emit "int main(int argc, char **argv) {\n"
      emit "  (void)argc; (void)argv;\n"
      emit "  MiEnv *_env = mi_env_new(NULL);\n"
      emitBuiltins
      code <- exprToC st expr
      emit "  mi_in_eval = 1;\n"
      emit $ "  mi_print_val(mi_eval(" ++ code ++ ", _env)); printf(\"\\n\");\n"
      emit "  return 0;\n}\n"
  readIORef ref

-- | Detect a `main` binding that takes at least one parameter.
isMainBinding :: Binding -> Bool
isMainBinding b = bindName b == "main" &&
  (not (null (bindParams b)) || isLam (bindBody b))
  where
    isLam (Lam _ _) = True
    isLam _         = False

-- | Generate code to evaluate a binding and add it to _env
bindingEvalCode :: CGState -> Binding -> IO String
bindingEvalCode st b = do
  let name = T.unpack (bindName b)
  bodyExpr <- if null (bindParams b)
    then pure (bindBody b)
    else pure $ foldr Lam (bindBody b) (bindParams b)
  code0 <- exprToC st bodyExpr
  let code = withLoc (bindPos b) code0
  if bindDomain b == Lazy
    then pure $ "  { MiExpr *_thunk_body = " ++ code ++ ";\n" ++
                "    MiVal _thunk; _thunk.type = MI_CLOSURE;\n" ++
                "    _thunk.as.closure.body = _thunk_body;\n" ++
                "    _thunk.as.closure.param = \"_thunk_\";\n" ++
                "    _thunk.as.closure.env = _env;\n" ++
                "    mi_env_set(_env, \"" ++ name ++ "\", _thunk); }\n"
    else pure $ "  mi_env_set(_env, \"" ++ name ++ "\", mi_force(mi_eval(" ++ code ++ ", _env), _env));\n"

-- ── Expression → MiExpr* construction ─────────────────────────────

-- | Map builtin names to their C runtime function identifiers.
-- Used by Builtin AST node codegen to emit direct references.
builtinCNames :: Map.Map Text String
builtinCNames = Map.fromList
  [ ("if", "mi_builtin_if"), ("truthy", "mi_builtin_truthy")
  , ("len", "mi_builtin_len"), ("strlen", "mi_builtin_len")
  , ("charAt", "mi_builtin_charAt"), ("slice", "mi_builtin_slice")
  , ("indexOf", "mi_builtin_indexOf"), ("split", "mi_builtin_split")
  , ("trim", "mi_builtin_trim"), ("toUpper", "mi_builtin_toUpper")
  , ("toLower", "mi_builtin_toLower"), ("replace", "mi_builtin_replace")
  , ("toString", "mi_builtin_toString"), ("_toString", "mi_builtin_toString")
  , ("toInt", "mi_builtin_toInt"), ("toFloat", "mi_builtin_toFloat")
  , ("float", "mi_builtin_float"), ("round", "mi_builtin_round")
  , ("floor", "mi_builtin_floor"), ("ceil", "mi_builtin_ceil")
  , ("fields", "mi_builtin_fields"), ("fieldNames", "mi_builtin_fieldNames")
  , ("tag", "mi_builtin_tag"), ("getField", "mi_builtin_getField")
  , ("setField", "mi_builtin_setField"), ("gc_manage", "mi_builtin_gc_manage")
  , ("__sized_int", "mi_builtin_sized_int"), ("__sized_uint", "mi_builtin_sized_uint")
  ]

-- | Wrap a C expression with source location
withLoc :: Maybe SrcPos -> String -> String
withLoc Nothing c = c
withLoc (Just pos) c = "mi_expr_loc(" ++ c ++ ", \"" ++ cEscLoc pos ++ "\")"

cEscLoc :: SrcPos -> String
cEscLoc (SrcPos f l c) = concatMap escChar f ++ ":" ++ show l ++ ":" ++ show c
  where
    escChar '\\' = "\\\\"
    escChar '"'  = "\\\""
    escChar ch   = [ch]

exprToC :: CGState -> Expr -> IO String
exprToC _ (IntLit n)
  | n > 9223372036854775807 || n < (-9223372036854775808) =
    pure $ "mi_expr_val(mi_sized_big(mi_bn_from_str(\"" ++ show n ++ "\"), 1))"
  | otherwise = pure $ "mi_expr_int(" ++ show n ++ ")"
exprToC _ (FloatLit d) = pure $ "mi_expr_float(" ++ show d ++ ")"
exprToC _ (SizedInt n w s)
  | w == 0 && (n > 9223372036854775807 || n < (-9223372036854775808)) =
    -- Value exceeds int64_t range, emit as string-parsed bignum
    pure $ "mi_expr_val(mi_sized_big(mi_bn_from_str(\"" ++ show n ++ "\"), " ++ (if s then "1" else "0") ++ "))"
  | otherwise = pure $ "mi_expr_sized_int(" ++ show n ++ ", " ++ show w ++ ", " ++ (if s then "1" else "0") ++ ")"
exprToC _ (SizedFloat d w)
  | w == 32 = pure $ "mi_expr_float32(" ++ show (realToFrac d :: Float) ++ "f)"
  | otherwise = pure $ "mi_expr_float(" ++ show d ++ ")"
exprToC _ (StringLit s) = pure $ "mi_expr_string(" ++ cStringLit (T.unpack s) ++ ")"
exprToC _ (Name n)
  | "__mod_" `T.isPrefixOf` n && "__" `T.isSuffixOf` n =
    pure "mi_expr_string(\"<closure>\")"  -- circular module reference placeholder
  | otherwise = pure $ "mi_expr_name(\"" ++ T.unpack n ++ "\")"

-- | Builtin: emit a direct reference to the C builtin function,
-- bypassing env lookup so module-level shadows don't intercept.
exprToC _ (Builtin n) =
  case Map.lookup n builtinCNames of
    Just cfn -> pure $ "mi_native(" ++ cfn ++ ")"
    Nothing  -> pure $ "mi_expr_name(\"" ++ T.unpack n ++ "\")"

exprToC st (BinOp op l r)
  | not (isBuiltinOp op) = do
    fc <- exprToC st (Name op)
    lc <- exprToC st l
    rc <- exprToC st r
    pure $ "mi_expr_app(mi_expr_app(" ++ fc ++ ", " ++ lc ++ "), " ++ rc ++ ")"
  | otherwise = do
    intCtx <- readIORef (cgIntCtx st)
    lc <- exprToC st l
    rc <- exprToC st r
    if not (Set.null intCtx)
      then pure $ "mi_expr_native_binop(\"" ++ T.unpack op ++ "\", " ++ lc ++ ", " ++ rc ++ ")"
      else pure $ "mi_expr_binop(\"" ++ T.unpack op ++ "\", " ++ lc ++ ", " ++ rc ++ ")"

-- | Detect fold/range → for-loop pattern:
--   App(App(App(Name "fold"), Lam acc (Lam iter body)), init,
--       App(App(App(Name "range_helper"), start), end), Record "Nil" []))
exprToC st expr
  | Just (accName, iterName, body, initE, startE, endE) <- matchFoldRange expr = do
    initC  <- exprToC st initE
    startC <- exprToC st startE
    endC   <- exprToC st endE
    -- both iter_var and acc_var assumed int; native_binop type-checks at runtime
    let intVars = Set.fromList [iterName, accName]
    bodyC  <- withIntCtx st intVars (exprToC st body)
    pure $ "mi_expr_for_range(\"" ++ T.unpack accName ++ "\", \"" ++
           T.unpack iterName ++ "\", " ++ initC ++ ", " ++ startC ++ ", " ++
           endC ++ ", " ++ bodyC ++ ")"

exprToC st (App f x) = do
  fc <- exprToC st f
  xc <- exprToC st x
  pure $ "mi_expr_app(" ++ fc ++ ", " ++ xc ++ ")"

exprToC st (Lam param body) = do
  bc <- exprToC st body
  pure $ "mi_expr_lam(\"" ++ T.unpack param ++ "\", " ++ bc ++ ")"

-- | Detect tail-recursive With binding → EXPR_TAIL_LOOP
exprToC st expr
  | Just (funcName, params, initExprs, bodyExpr) <- matchTailLoop expr = do
    let nparams = length params
    initCs <- mapM (exprToC st) initExprs
    -- Params whose init exprs are integer are known-int inside the loop body
    let intParams = Set.fromList params  -- all tail-loop params assumed int; native_binop type-checks at runtime
    bodyC  <- withIntCtx st intParams (tailBodyToC st funcName nparams bodyExpr)
    let paramStrs = map (\p -> "\"" ++ T.unpack p ++ "\"") params
    pure $ "mi_expr_tail_loop(" ++ show nparams ++ ", " ++
           "(char*[]){" ++ intercalate ", " paramStrs ++ "}, " ++
           "(MiExpr*[]){" ++ intercalate ", " initCs ++ "}, " ++
           bodyC ++ ")"

exprToC st (With body bindings) = do
  bc <- exprToC st body
  bindCodes <- mapM (bindingStructToC st) (filter (not . skipBinding) bindings)
  pure $ "mi_expr_with(" ++ bc ++ ", " ++ show (length bindCodes) ++ ", " ++
    intercalate ", " bindCodes ++ ")"

exprToC st (Record tag bindings) = do
  bindCodes <- mapM (bindingStructToC st) (filter (not . skipBinding) bindings)
  pure $ "mi_expr_record(\"" ++ T.unpack tag ++ "\", " ++ show (length bindCodes) ++
    (if null bindCodes then "" else ", " ++ intercalate ", " bindCodes) ++ ")"

exprToC st (FieldAccess e field) = do
  ec <- exprToC st e
  pure $ "mi_expr_field(" ++ ec ++ ", \"" ++ T.unpack field ++ "\")"

exprToC st (Namespace bindings) = do
  bindCodes <- mapM (bindingStructToC st) (filter (not . skipBinding) bindings)
  let args = if null bindCodes then "" else ", " ++ intercalate ", " bindCodes
  pure $ "mi_expr_namespace(" ++ show (length bindCodes) ++ args ++ ")"

-- | Detect the double-case truthiness pattern from `if cond then else`:
--   Case (Case cond [False→0, Nil→0, Nothing→0, 0→0, ""→0, _→1])
--        [0→else, _→then]
exprToC st (Case (Case cond innerAlts) [Alt (PLit (IntLit 0)) Nothing elseBr, Alt PWild Nothing thenBr])
  | isTruthinessCase innerAlts = do
    cc <- exprToC st cond
    tc <- exprToC st thenBr
    ec <- exprToC st elseBr
    pure $ "mi_expr_if(" ++ cc ++ ", " ++ tc ++ ", " ++ ec ++ ")"

exprToC st (Case scrut alts) = do
  sc <- exprToC st scrut
  altCodes <- mapM (altToC st) alts
  pure $ "mi_expr_case(" ++ sc ++ ", " ++ show (length alts) ++ ", " ++
    intercalate ", " altCodes ++ ")"

exprToC st (Thunk body) = do
  bc <- exprToC st body
  pure $ "mi_expr_thunk(" ++ bc ++ ")"

exprToC st (ListLit es) = exprToC st (listLitToCons es)
  where
    listLitToCons []     = Record "Nil" []
    listLitToCons (x:xs) = Record "Cons" [mkBind "head" x, mkBind "tail" (listLitToCons xs)]

exprToC _ (Error msg) = pure $ "mi_expr_string(" ++ cStringLit (T.unpack msg) ++ ")"
exprToC _ (Import path) = pure $ "mi_expr_string(" ++ cStringLit ("unresolved import: " ++ T.unpack path) ++ ")"
exprToC st (Quote e) = exprToC st e   -- should be reduced to records already
exprToC st (Splice e) = exprToC st e  -- should be reduced away already

exprToC st (CFunction hdr cname retTy paramTys standardImport) = do
  let hdrStr = T.unpack hdr
      inc = if standardImport then "<" ++ hdrStr ++ ">" else show hdrStr
  when (not (T.null hdr)) $
    modifyIORef (cgIncludes st) (("#include " ++ inc) :)
  -- Include <wchar.h> when any parameter/return type mentions wchar_t so
  -- wide-character functions (fgetws, etc.) are declared on platforms
  -- where they live in wchar.h.
  let usesWchar t = case t of
        CPtr name -> name == "wchar_t"
        _         -> False
      anyWchar = usesWchar retTy || any usesWchar paramTys
  when anyWchar $ modifyIORef (cgIncludes st) (("#include <wchar.h>") :)

  -- Detect accessor pattern: __acc:StructType:field.path
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

  nativeCode <- cfunctionToC st actualCName retTy paramTys
  pure $ "mi_expr_val(" ++ nativeCode ++ ")"

-- | Escape a string for C
cStringLit :: String -> String
cStringLit s = "\"" ++ concatMap esc s ++ "\""
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c    = [c]

-- ── C FFI codegen (native closures) ──────────────────────────────

cfunctionToC :: CGState -> String -> CType -> [CType] -> IO String
cfunctionToC st cname retTy allParamTys = do
  let indexed = zip [0::Int ..] allParamTys
      inputParams  = [(i, t) | (i, t) <- indexed, not (isOutputParam t)]
      outputParams = [(i, t) | (i, t) <- indexed, isOutputParam t]
      nInputs = length inputParams
  case nInputs of
    0 -> cffiLeaf st cname retTy allParamTys inputParams outputParams
    _ -> cffiCurried st cname retTy allParamTys inputParams outputParams

cffiLeaf :: CGState -> String -> CType -> [CType]
         -> [(Int, CType)] -> [(Int, CType)] -> IO String
cffiLeaf st cname retTy allParamTys inputParams outputParams = do
  cid <- freshId st
  let fnName = "mi_cffi_" ++ show cid
      nInputs = length inputParams
      hasOuts = not (null outputParams)
      -- Find callback parameters and generate trampolines
      cbParams = [(i, cbRet, cbPs) | (i, CCallback cbRet cbPs) <- inputParams]
      envFields
        | nInputs <= 1 = ""
        | otherwise = concatMap (\k ->
            "  MiVal _a" ++ show k ++ ";\n") [0..nInputs-2]
      envStruct
        | nInputs <= 1 = ""
        | otherwise = "struct " ++ fnName ++ "_env {\n" ++ envFields ++ "};\n\n"
      envCast
        | nInputs <= 1 = ""
        | otherwise = "  struct " ++ fnName ++ "_env *_e = (struct " ++ fnName ++ "_env *)_env;\n"
      envUnpack
        | nInputs <= 1 = ""
        | otherwise = concatMap (\k ->
            "  MiVal _a" ++ show k ++ " = _e->_a" ++ show k ++ ";\n") [0..nInputs-2]
      inputArgExpr k
        | nInputs == 0 = error "no inputs"
        | nInputs == 1 = "_arg"
        | k == nInputs - 1 = "_arg"
        | otherwise = "_a" ++ show k
      outDecls = concatMap (\(i, t) ->
        case t of
          COut (CPtr tname) ->
            let tn = T.unpack tname
            in "  " ++ tn ++ " *_out_" ++ show i ++ " = mi_alloc(sizeof(" ++ tn ++ "));\n" ++
               "  memset(_out_" ++ show i ++ ", 0, sizeof(" ++ tn ++ "));\n"
          COut ct ->
            "  " ++ cOutDeclType ct ++ " _out_" ++ show i ++ " = 0;\n"
          _ -> "") outputParams

  -- Generate trampolines for callback parameters
  cbTrampolines <- mapM (\(cbIdx, cbRet', cbPs') -> do
    let inputIdx = length [() | (j, _) <- inputParams, j < cbIdx]
        trampolineName = fnName ++ "_cb" ++ show inputIdx
        closureGlobal = "static MiVal *" ++ trampolineName ++ "_closure = NULL;\n"
        trampolineCode = genTrampoline trampolineName cbRet' cbPs'
        -- Code to set the closure global before the call
        setupCode = "  " ++ trampolineName ++ "_closure = mi_alloc(sizeof(MiVal));\n" ++
                    "  *" ++ trampolineName ++ "_closure = " ++ inputArgExpr inputIdx ++ ";\n" ++
                    "  mi_gc_pin(" ++ trampolineName ++ "_closure);\n"
    pure (cbIdx, inputIdx, trampolineName, closureGlobal ++ trampolineCode, setupCode)
    ) cbParams

  let cbTrampolineMap = Map.fromList [(idx, tn) | (_, idx, tn, _, _) <- cbTrampolines]
      cbTrampolineDefs = concatMap (\(_, _, _, code, _) -> code) cbTrampolines
      cbSetupCode = concatMap (\(_, _, _, _, setup) -> setup) cbTrampolines
      cArgList = intercalate ", " $ map (\(origIdx, t) ->
        if isOutputParam t
          then case t of
            COut (CPtr _) -> "_out_" ++ show origIdx  -- already a T*, pass directly
            _ -> "&_out_" ++ show origIdx
          else let inputIdx = length [() | (j, _) <- inputParams, j < origIdx]
               in case Map.lookup inputIdx cbTrampolineMap of
                    Just tn -> tn ++ "_trampoline"
                    Nothing -> miToCArg t (inputArgExpr inputIdx)
        ) (zip [0..] allParamTys)
      callExpr = cname ++ "(" ++ cArgList ++ ")"
      returnExpr
        | not hasOuts = cRetToMi retTy callExpr
        | retTy == CVoid =
            "(" ++ callExpr ++ ",\n" ++ buildOutRecord outputParams ++ ")"
        | otherwise =
            "({ " ++ cRetTypeName retTy ++ " _ret = " ++ callExpr ++ ";\n" ++
            buildOutRecordWithRet retTy outputParams ++ " })"
      fnDef = cbTrampolineDefs ++ envStruct ++
              "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env) {\n" ++
              (if nInputs == 0 then "  (void)_arg; " else "") ++
              (if nInputs <= 1 then "  (void)_env;\n" else "") ++
              envCast ++ envUnpack ++ outDecls ++ cbSetupCode ++
              "  return " ++ returnExpr ++ ";\n}\n\n"
  addTopDef st fnDef
  pure $ "mi_native(" ++ fnName ++ ")"

-- | Generate a C trampoline function for a callback parameter
genTrampoline :: String -> CType -> [CType] -> String
genTrampoline name retTy paramTys =
  let cRetStr = cRetTypeName retTy
      paramDecls = intercalate ", " $
        zipWith (\i t -> cRetTypeName t ++ " _a" ++ show i) [0::Int ..] paramTys
      -- Build chained mi_apply calls: mi_apply(mi_apply(closure, a0), a1)
      applyChain = foldl' (\acc (i, t) ->
        let argConv = cRetToMi t ("_a" ++ show i)
        in "mi_apply(" ++ acc ++ ", " ++ argConv ++ ")"
        ) ("*" ++ name ++ "_closure") (zip [0::Int ..] paramTys)
      bodyStr = if retTy == CVoid
        then "  " ++ applyChain ++ ";\n"
        else "  MiVal _result = " ++ applyChain ++ ";\n" ++
             "  return " ++ miValToC retTy "_result" ++ ";\n"
  in "static " ++ cRetStr ++ " " ++ name ++ "_trampoline(" ++ paramDecls ++ ") {\n" ++
     bodyStr ++ "}\n\n"

-- | Map CInt bit width to C type name
cIntTypeName :: Int -> String
cIntTypeName 8  = "int8_t"
cIntTypeName 16 = "int16_t"
cIntTypeName 32 = "int"
cIntTypeName 64 = "int64_t"
cIntTypeName _  = "int64_t"

-- | Map CUInt bit width to C type name
cUIntTypeName :: Int -> String
cUIntTypeName 8  = "uint8_t"
cUIntTypeName 16 = "uint16_t"
cUIntTypeName 32 = "unsigned int"
cUIntTypeName 64 = "uint64_t"
cUIntTypeName _  = "uint64_t"

-- | C type name for out-parameter declarations
cOutDeclType :: CType -> String
cOutDeclType (CInt w)  = cIntTypeName w
cOutDeclType (CUInt w) = cUIntTypeName w
cOutDeclType CLong     = "long"
cOutDeclType CULong    = "unsigned long"
cOutDeclType CFloat    = "double"
cOutDeclType CFloat32  = "float"
cOutDeclType (CPtr t)  = T.unpack t ++ " *"
cOutDeclType _         = "int"

-- | Extract a C value from a MiVal (reverse of cRetToMi)
miValToC :: CType -> String -> String
miValToC (CInt w) name  = "(" ++ cIntTypeName w ++ ")((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i)"
miValToC (CUInt w) name = "(" ++ cUIntTypeName w ++ ")((uint64_t)((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i))"
miValToC CLong name     = "(long)((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i)"
miValToC CULong name    = "(unsigned long)((uint64_t)((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i))"
miValToC CFloat name    = "(double)(" ++ name ++ ".as.f)"
miValToC CFloat32 name  = "(float)(" ++ name ++ ".as.f32)"
miValToC CString name   = name ++ ".as.str.data"
miValToC (CPtr _) name  = name ++ ".as.ptr"
miValToC CVoid _        = ""
miValToC _ name         = name ++ ".as.i"

buildOutRecord :: [(Int, CType)] -> String
buildOutRecord outs =
  let n = length outs
      fields = concatMap (\(idx, (i, t)) ->
        let val = outToMi t ("_out_" ++ show i)
        in "    _fields[" ++ show idx ++ "] = " ++ val ++ ";\n") (zip [0..] outs)
      names = concatMap (\(i, _) -> "\"out" ++ show i ++ "\", ") outs
  in "({\n    MiVal *_fields = mi_alloc(" ++ show n ++ " * sizeof(MiVal));\n" ++
     "    static const char *_names[] = {" ++ names ++ "};\n" ++ fields ++
     "    MiVal _r; _r.type = MI_RECORD; _r.as.rec.tag = \"Result\";" ++
     " _r.as.rec.names = _names; _r.as.rec.fields = _fields;" ++
     " _r.as.rec.nfields = " ++ show n ++ "; _r;\n  })"

buildOutRecordWithRet :: CType -> [(Int, CType)] -> String
buildOutRecordWithRet retTy outs =
  let n = 1 + length outs
      retField = "    _fields[0] = " ++ cRetToMi retTy "_ret" ++ ";\n"
      outFields = concatMap (\(idx, (i, t)) ->
        let val = outToMi t ("_out_" ++ show (i :: Int))
        in "    _fields[" ++ show ((idx :: Int) + 1) ++ "] = " ++ val ++ ";\n")
        (zip [0..] outs)
      names = "\"value\", " ++ concatMap (\(i, _) -> "\"out" ++ show i ++ "\", ") outs
  in "MiVal *_fields = mi_alloc(" ++ show n ++ " * sizeof(MiVal));\n" ++
     "  static const char *_names[] = {" ++ names ++ "};\n" ++
     retField ++ outFields ++
     "  MiVal _r; _r.type = MI_RECORD; _r.as.rec.tag = \"Result\";" ++
     " _r.as.rec.names = _names; _r.as.rec.fields = _fields;" ++
     " _r.as.rec.nfields = " ++ show n ++ "; _r;"

outToMi :: CType -> String -> String
outToMi (COut ct) name = cRetToMi ct name
outToMi t name         = cRetToMi t name

cRetTypeName :: CType -> String
cRetTypeName (CInt w) = cIntTypeName w
cRetTypeName (CUInt w) = cUIntTypeName w
cRetTypeName CLong   = "long"
cRetTypeName CULong  = "unsigned long"
cRetTypeName CFloat  = "double"
cRetTypeName CFloat32 = "float"
cRetTypeName CString = "char *"
cRetTypeName (CPtr _) = "void *"
cRetTypeName CVoid   = "void"
cRetTypeName (COut ct) = cOutDeclType ct
cRetTypeName (CStruct name _) = T.unpack name
cRetTypeName (CStructPtr name _) = T.unpack name ++ " *"
cRetTypeName (CCallback _ _) = "void*"  -- function pointers as opaque

cffiCurried :: CGState -> String -> CType -> [CType]
            -> [(Int, CType)] -> [(Int, CType)] -> IO String
cffiCurried st cname retTy allParamTys inputParams outputParams = do
  let nInputs = length inputParams
  leafCode <- cffiLeaf st cname retTy allParamTys inputParams outputParams
  if nInputs == 1
    then pure leafCode
    else do
      leafFnName <- do
        n <- readIORef (cgNextId st)
        pure $ "mi_cffi_" ++ show (n - 1)
      ids <- mapM (\_ -> freshId st) [0..nInputs-2]
      let wrapperNames = map (\i -> "mi_cffi_" ++ show i) ids
      mapM_ (\k -> do
        let fnName = wrapperNames !! k
            envName = fnName ++ "_env"
            nextName = if k == nInputs - 2 then leafFnName else wrapperNames !! (k + 1)
            nextEnvName = nextName ++ "_env"
        if k == 0
          then do
            let fnDef = "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env) {\n" ++
                        "  (void)_env;\n" ++
                        "  struct " ++ nextEnvName ++ " *_ne = mi_alloc(sizeof(struct " ++ nextEnvName ++ "));\n" ++
                        "  _ne->_a0 = _arg;\n" ++
                        "  return mi_native_env(" ++ nextName ++ ", _ne);\n}\n\n"
            addTopDef st fnDef
          else do
            let envFields = concatMap (\i -> "  MiVal _a" ++ show i ++ ";\n") [0..k-1]
                envStruct = "struct " ++ envName ++ " {\n" ++ envFields ++ "};\n\n"
                envCast = "  struct " ++ envName ++ " *_e = (struct " ++ envName ++ " *)_env;\n"
                allocNext = "  struct " ++ nextEnvName ++ " *_ne = mi_alloc(sizeof(struct " ++ nextEnvName ++ "));\n" ++
                            concatMap (\i -> "  _ne->_a" ++ show i ++ " = _e->_a" ++ show i ++ ";\n") [0..k-1] ++
                            "  _ne->_a" ++ show k ++ " = _arg;\n"
                fnDef = envStruct ++
                        "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env) {\n" ++
                        envCast ++ allocNext ++
                        "  return mi_native_env(" ++ nextName ++ ", _ne);\n}\n\n"
            addTopDef st fnDef
        ) (reverse [0..nInputs-2])
      pure $ "mi_native(" ++ wrapperNames !! 0 ++ ")"

cRetToMi :: CType -> String -> String
cRetToMi (CInt w) expr  = "mi_sized_int((int64_t)(" ++ expr ++ "), " ++ show w ++ ", 1)"
cRetToMi (CUInt w) expr = "mi_sized_int((int64_t)(" ++ expr ++ "), " ++ show w ++ ", 0)"
cRetToMi CLong expr      = "mi_sized_int((int64_t)(" ++ expr ++ "), 64, 1)"
cRetToMi CULong expr     = "mi_sized_int((int64_t)(" ++ expr ++ "), 64, 0)"
cRetToMi CFloat expr     = "mi_float((double)(" ++ expr ++ "))"
cRetToMi CFloat32 expr   = "mi_float32((float)(" ++ expr ++ "))"
cRetToMi CString expr    = "mi_nullable_str(" ++ expr ++ ")"
cRetToMi CVoid expr      = "(" ++ expr ++ ", mi_int(0))"
cRetToMi (CPtr _) expr   = "mi_nullable_ptr((void*)(" ++ expr ++ "))"
cRetToMi (COut _) _      = "mi_int(0)"
cRetToMi (CCallback _ _) expr = "mi_pointer((void*)(" ++ expr ++ "))"
cRetToMi (CStruct name fields) expr =
  "({ " ++ T.unpack name ++ " _s = " ++ expr ++ "; " ++
  "mi_struct_to_record(" ++ show (length fields) ++ ", " ++
  "(const char*[]){" ++ intercalate ", " [show (T.unpack fn) | (fn, _) <- fields] ++ "}, " ++
  "(MiVal[]){" ++ intercalate ", " [cRetToMiRaw ft ("_s." ++ T.unpack fn) | (fn, ft) <- fields] ++ "}); })"
cRetToMi (CStructPtr name fields) expr =
  "({ " ++ T.unpack name ++ " *_sp = " ++ expr ++ "; " ++
  "(_sp == NULL) ? mi_record(\"Nothing\", 0, NULL, NULL) : " ++
  "mi_struct_to_record(" ++ show (length fields) ++ ", " ++
  "(const char*[]){" ++ intercalate ", " [show (T.unpack fn) | (fn, _) <- fields] ++ "}, " ++
  "(MiVal[]){" ++ intercalate ", " [cRetToMiRaw ft ("_sp->" ++ T.unpack fn) | (fn, ft) <- fields] ++ "}); })"

-- | Like cRetToMi but keeps pointers as raw MI_POINTER (no Maybe wrapping).
-- Used for struct field extraction where pointers are part of a known layout.
cRetToMiRaw :: CType -> String -> String
cRetToMiRaw (CPtr _) expr = "mi_pointer((void*)(" ++ expr ++ "))"
cRetToMiRaw CString expr = "mi_string(" ++ expr ++ ")"
cRetToMiRaw t expr = cRetToMi t expr

miToCArg :: CType -> String -> String
miToCArg (CInt w) name  = "(" ++ cIntTypeName w ++ ")((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i)"
miToCArg (CUInt w) name = "(" ++ cUIntTypeName w ++ ")((uint64_t)((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i))"
miToCArg CLong name     = "(long)((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i)"
miToCArg CULong name    = "(unsigned long)((uint64_t)((" ++ name ++ ".type == MI_SIZED_INT) ? (" ++ name ++ ".as.sized.is_big ? mi_bn_to_i64(" ++ name ++ ".as.sized.big) : " ++ name ++ ".as.sized.i) : " ++ name ++ ".as.i))"
miToCArg CFloat name    = "mi_to_float(" ++ name ++ ")"
miToCArg CFloat32 name  = "mi_to_float32(" ++ name ++ ")"
miToCArg CString name  = "mi_maybe_str(" ++ name ++ ")"
miToCArg CVoid _       = "/* void */"
miToCArg (CPtr base) name = let b = T.unpack base in if "FILE" `isInfixOf` b then "(FILE*)mi_maybe_ptr(" ++ name ++ ")" else "mi_maybe_ptr(" ++ name ++ ")"
miToCArg (COut _) name = "&_out_" ++ name
miToCArg (CStruct sname fields) name =
  "((" ++ T.unpack sname ++ "){ " ++
  intercalate ", " ["." ++ T.unpack fn ++ " = " ++ miToCArg ft ("mi_struct_field(" ++ name ++ ", \"" ++ T.unpack fn ++ "\")") | (fn, ft) <- fields] ++
  " })"
miToCArg (CStructPtr sname fields) name =
  "((" ++ name ++ ".type == MI_RECORD && strcmp(" ++ name ++ ".as.rec.tag, \"Nothing\") == 0) ? NULL : " ++
  "(" ++ name ++ ".type == MI_RECORD && " ++ name ++ ".as.rec.nfields > 0 && " ++ name ++ ".as.rec.tag[0] == '\\0') ? " ++
  "&((" ++ T.unpack sname ++ "){ " ++
  intercalate ", " ["." ++ T.unpack fn ++ " = " ++ miToCArg ft ("mi_struct_field(" ++ name ++ ", \"" ++ T.unpack fn ++ "\")") | (fn, ft) <- fields] ++
  " }) : " ++
  "(" ++ T.unpack sname ++ " *)mi_maybe_ptr(" ++ name ++ "))"
miToCArg (CCallback _ _) _ = "/* callback handled by trampoline */"

isOutputParam :: CType -> Bool
isOutputParam (COut _)  = True
isOutputParam _         = False

-- | Convert a Binding to C MiBinding struct
bindingStructToC :: CGState -> Binding -> IO String
bindingStructToC st b = do
  bc <- exprToC st (bindBody b)
  let bc' = withLoc (bindPos b) bc
  let lazyFlag = if bindDomain b == Lazy then "1" else "0"
  pure $ "mi_binding(\"" ++ T.unpack (bindName b) ++ "\", " ++ lazyFlag ++ ", " ++
    show (length (bindParams b)) ++
    concatMap (\p -> ", \"" ++ T.unpack p ++ "\"") (bindParams b) ++
    ", " ++ bc' ++ ")"

-- | Convert an Alt to C MiAlt struct
altToC :: CGState -> Alt -> IO String
altToC st (Alt pat guard body) = do
  pc <- patToC pat
  bc <- exprToC st body
  gc <- case guard of
    Nothing -> pure "NULL"
    Just g  -> exprToC st g
  pure $ "mi_alt(" ++ pc ++ ", " ++ gc ++ ", " ++ bc ++ ")"

-- | Convert a Pat to C MiPat
patToC :: Pat -> IO String
patToC (PVar v) = pure $ "mi_pat_var(\"" ++ T.unpack v ++ "\")"
patToC PWild = pure "mi_pat_wild()"
patToC (PLit (IntLit n)) = pure $ "mi_pat_int(" ++ show n ++ ")"
patToC (PLit (StringLit s)) = pure $ "mi_pat_string(" ++ cStringLit (T.unpack s) ++ ")"
patToC (PLit _) = pure "mi_pat_wild()"
patToC (PRec tag fields_) = do
  fieldCodes <- mapM (\(f, p) -> do
    pc <- patToC p
    pure $ "\"" ++ T.unpack f ++ "\", " ++ pc
    ) fields_
  pure $ "mi_pat_rec(\"" ++ T.unpack tag ++ "\", " ++ show (length fields_) ++
    (if null fields_ then "" else ", " ++ intercalate ", " fieldCodes) ++ ")"
patToC (PList pats mrest) = patToC (plistToConsPat pats mrest)
  where
    plistToConsPat [] Nothing      = PRec "Nil" []
    plistToConsPat [] (Just name)  = PVar name
    plistToConsPat (p:ps) mr       = PRec "Cons" [("head", p), ("tail", plistToConsPat ps mr)]

-- | Check whether a list of case alternatives is the truthiness-check pattern
-- generated by `if`: 6 alts where all falsy patterns map to IntLit 0
-- and the wildcard maps to IntLit 1.
isTruthinessCase :: [Alt] -> Bool
isTruthinessCase alts =
  length alts == 6 &&
  all isFalsyAlt (init alts) &&
  case last alts of
    Alt PWild Nothing (IntLit 1) -> True
    _                            -> False
  where
    isFalsyAlt (Alt _ Nothing (IntLit 0)) = True
    isFalsyAlt _                          = False

-- | Match the fold/range pattern in the reduced AST.
-- fold f acc (range start end) appears after reduction as:
--   App (App (App (Name "fold") (Lam acc (Lam iter body))) init)
--       (App (App (App (Name "range_helper") start) end) (Record "Nil" []))
matchFoldRange :: Expr -> Maybe (Text, Text, Expr, Expr, Expr, Expr)
matchFoldRange
  (App (App (App (Name "fold") (Lam accName (Lam iterName body))) initE)
       (App (App (App (Name "range_helper") startE) endE) (Record "Nil" [])))
  = Just (accName, iterName, body, initE, startE, endE)
matchFoldRange _ = Nothing

-- | Match tail-recursive With binding pattern:
--   With (App ... (App (Name funcName) arg1) ... argN) [Binding funcName [] (Lam p1 (Lam p2 ... body))]
-- where all self-calls in body are in tail position.
-- Returns: (param names, initial arg exprs, original body)
matchTailLoop :: Expr -> Maybe (Text, [Text], [Expr], Expr)
matchTailLoop (With callExpr [b])
  | bindDomain b == Value
  , not (null params)
  , let funcName = bindName b
  , Just initArgs <- matchCallChain funcName callExpr
  , length initArgs == length params
  , allTailCalls funcName (length params) lamBody
  = Just (funcName, params, initArgs, lamBody)
  where
    (params, lamBody) = unwrapLams (bindBody b)
matchTailLoop _ = Nothing

-- | Unwrap a chain of Lam constructors into (param names, body)
unwrapLams :: Expr -> ([Text], Expr)
unwrapLams (Lam p body) = let (ps, b) = unwrapLams body in (p:ps, b)
unwrapLams body = ([], body)

-- | Match a chain of App calls: App(App(App(Name funcName, a1), a2), a3) → Just [a1, a2, a3]
matchCallChain :: Text -> Expr -> Maybe [Expr]
matchCallChain funcName expr = go expr []
  where
    go (App f x) args = go f (x : args)
    go (Name n) args | n == funcName = Just args
    go _ _ = Nothing

-- | Check if all occurrences of funcName are in tail position.
allTailCalls :: Text -> Int -> Expr -> Bool
allTailCalls funcName nparams body = tailCheck body
  where
    -- An expression is valid if all self-references are in tail position
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
    tailCheck (Namespace bs) = all (noRef . bindBody) bs
    tailCheck (ListLit es) = all noRef es
    tailCheck (Quote e) = noRef e
    tailCheck (Splice e) = noRef e
    tailCheck _ = True

    -- No reference to funcName at all
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
    noRef (Quote e) = noRef e
    noRef (Splice e) = noRef e
    noRef _ = True

-- | Like exprToC but emits mi_expr_tail_call for tail calls to funcName.
-- In tail position, self-calls become tail_call; in non-tail positions,
-- falls back to normal exprToC (which won't encounter funcName since
-- allTailCalls verified it only appears in tail position).
tailBodyToC :: CGState -> Text -> Int -> Expr -> IO String
tailBodyToC st funcName nparams (Case (Case cond innerAlts) [Alt (PLit (IntLit 0)) Nothing elseBr, Alt PWild Nothing thenBr])
  | isTruthinessCase innerAlts = do
    cc <- exprToC st cond
    tc <- tailBodyToC st funcName nparams thenBr
    ec <- tailBodyToC st funcName nparams elseBr
    pure $ "mi_expr_if(" ++ cc ++ ", " ++ tc ++ ", " ++ ec ++ ")"
tailBodyToC st funcName nparams (Case scrut alts) = do
  sc <- exprToC st scrut
  altCodes <- mapM (tailAltToC st funcName nparams) alts
  pure $ "mi_expr_case(" ++ sc ++ ", " ++ show (length alts) ++ ", " ++
    intercalate ", " altCodes ++ ")"
tailBodyToC st funcName nparams expr
  | Just args <- matchCallChain funcName expr
  , length args == nparams = do
    argCs <- mapM (exprToC st) args
    pure $ "mi_expr_tail_call(" ++ show nparams ++ ", " ++
           intercalate ", " argCs ++ ")"
tailBodyToC st funcName nparams (With body bindings) = do
  bc <- tailBodyToC st funcName nparams body
  bindCodes <- mapM (bindingStructToC st) (filter (not . skipBinding) bindings)
  pure $ "mi_expr_with(" ++ bc ++ ", " ++ show (length bindCodes) ++ ", " ++
    intercalate ", " bindCodes ++ ")"
tailBodyToC st _ _ expr = exprToC st expr

tailAltToC :: CGState -> Text -> Int -> Alt -> IO String
tailAltToC st funcName nparams (Alt pat guard body) = do
  pc <- patToC pat
  bc <- tailBodyToC st funcName nparams body
  gc <- case guard of
    Nothing -> pure "NULL"
    Just g  -> exprToC st g
  pure $ "mi_alt(" ++ pc ++ ", " ++ gc ++ ", " ++ bc ++ ")"

-- ── C runtime preamble ────────────────────────────────────────────

emitPreamble :: Handle -> IO ()
emitPreamble h = hPutStr h $ unlines
  [ "#define _GNU_SOURCE"
  , "#include <stdio.h>"
  , "#include <stdlib.h>"
  , "#include <string.h>"
  , "#include <stdint.h>"
  , "#include <inttypes.h>"
  , "#include <stdarg.h>"
  , "#include <ctype.h>"
  , "#include <math.h>"
  , ""
  , "// ── Arena allocator ──"
  , "#define MI_ARENA_BLOCK_SIZE (1024 * 1024)"
  , "typedef struct MiArenaBlock { char *data; size_t used; size_t cap; struct MiArenaBlock *next; } MiArenaBlock;"
  , "typedef struct { MiArenaBlock *head; } MiArena;"
  , "static MiArena mi_arena = {0};"
  , ""
  , "static void *mi_alloc(size_t size) {"
  , "  size = (size + 7) & ~7;"
  , "  MiArenaBlock *b = mi_arena.head;"
  , "  if (!b || b->used + size > b->cap) {"
  , "    size_t cap = MI_ARENA_BLOCK_SIZE;"
  , "    if (size > cap) cap = size;"
  , "    b = (MiArenaBlock*)malloc(sizeof(MiArenaBlock) + cap);"
  , "    b->data = (char*)(b + 1); b->used = 0; b->cap = cap;"
  , "    b->next = mi_arena.head; mi_arena.head = b;"
  , "  }"
  , "  void *p = b->data + b->used; b->used += size;"
  , "  memset(p, 0, size);"
  , "  return p;"
  , "}"
  , ""
  , "static char *mi_strdup(const char *s) {"
  , "  size_t n = strlen(s) + 1;"
  , "  char *p = (char*)mi_alloc(n);"
  , "  memcpy(p, s, n);"
  , "  return p;"
  , "}"
  , ""
  , "// ── Forward declarations ──"
  , "typedef struct MiVal MiVal;"
  , "typedef struct MiExpr MiExpr;"
  , "typedef struct MiEnv MiEnv;"
  , "typedef struct MiBinding MiBinding;"
  , "typedef struct MiAlt MiAlt;"
  , "typedef struct MiPat MiPat;"
  , ""
  , "// ── MiVal: runtime values ──"
  , "typedef enum { MI_INT, MI_FLOAT, MI_FLOAT32, MI_STRING, MI_RECORD, MI_CLOSURE, MI_NATIVE, MI_POINTER, MI_MANAGED, MI_SIZED_INT, MI_TAIL_CALL } MiType;"
  , ""
  , "struct MiVal {"
  , "  MiType type;"
  , "  union {"
  , "    int64_t i;"
  , "    double f;"
  , "    float f32;"
  , "    struct { char *data; int len; } str;"
  , "    struct { const char *tag; const char **names; MiVal *fields; int nfields; } rec;"
  , "    struct { MiExpr *body; const char *param; MiEnv *env; } closure;"
  , "    struct { MiVal (*fn)(MiVal, void*); void *env; } native;"
  , "    void *ptr;"
  , "    struct { union { int64_t i; struct MiBignum *big; }; int width; int is_signed; int is_big; } sized;"
  , "    struct { MiVal *args; int nargs; } tail_call;"
  , "  } as;"
  , "};"
  , ""
  , "// ── MiExpr: expression tree ──"
  , "typedef enum {"
  , "  EXPR_INT, EXPR_FLOAT, EXPR_FLOAT32, EXPR_STRING, EXPR_NAME, EXPR_BINOP, EXPR_APP,"
  , "  EXPR_LAM, EXPR_WITH, EXPR_RECORD, EXPR_FIELD, EXPR_NAMESPACE,"
  , "  EXPR_CASE, EXPR_THUNK, EXPR_VAL, EXPR_SIZED_INT,"
  , "  EXPR_IF, EXPR_FOR_RANGE, EXPR_TAIL_LOOP, EXPR_TAIL_CALL,"
  , "  EXPR_NATIVE_BINOP"
  , "} ExprType;"
  , ""
  , "struct MiExpr {"
  , "  ExprType type;"
  , "  const char *loc;"
  , "  union {"
  , "    int64_t i;"
  , "    double f;"
  , "    char *s;"
  , "    char *name;"
  , "    struct { char *op; MiExpr *left; MiExpr *right; } binop;"
  , "    struct { MiExpr *fn; MiExpr *arg; } app;"
  , "    struct { char *param; MiExpr *body; } lam;"
  , "    struct { MiExpr *body; int nbindings; MiBinding *bindings; } with;"
  , "    struct { char *tag; int nbindings; MiBinding *bindings; } record;"
  , "    struct { MiExpr *expr; char *field; } field;"
  , "    struct { int nbindings; MiBinding *bindings; } ns;"
  , "    struct { MiExpr *scrut; int nalts; MiAlt *alts; } cas;"
  , "    struct { MiExpr *body; } thunk;"
  , "    struct { int64_t i; int width; int is_signed; } sized;"
  , "    struct { MiExpr *cond; MiExpr *then_br; MiExpr *else_br; } if_expr;"
  , "    struct { char *acc_var; char *iter_var; MiExpr *init; MiExpr *start; MiExpr *end; MiExpr *body; } for_range;"
  , "    struct { int nparams; char **params; MiExpr **inits; MiExpr *body; } tail_loop;"
  , "    struct { int nargs; MiExpr **args; } tail_call;"
  , "    struct { char *op; MiExpr *left; MiExpr *right; } native_binop;"
  , "    MiVal val;"
  , "  } as;"
  , "};"
  , ""
  , "struct MiBinding {"
  , "  char *name;"
  , "  int lazy;"
  , "  int nparams;"
  , "  char **params;"
  , "  MiExpr *body;"
  , "};"
  , ""
  , "// ── MiPat ──"
  , "typedef enum { PAT_VAR, PAT_WILD, PAT_INT, PAT_STRING, PAT_REC } PatType;"
  , ""
  , "struct MiPat {"
  , "  PatType type;"
  , "  union {"
  , "    char *var;"
  , "    int64_t i;"
  , "    char *s;"
  , "    struct { char *tag; int nfields; char **field_names; MiPat **field_pats; } rec;"
  , "  } as;"
  , "};"
  , ""
  , "struct MiAlt {"
  , "  MiPat *pat;"
  , "  MiExpr *guard;"
  , "  MiExpr *body;"
  , "};"
  , ""
  , "// ── MiEnv ──"
  , "struct MiEnv {"
  , "  char *name;"
  , "  MiVal val;"
  , "  MiEnv *next;"
  , "  MiEnv *parent;"
  , "  int gc_mark;"
  , "  MiEnv *gc_all;"
  , "};"
  , ""
  , "// ── MiGcManaged: GC-tracked pointer with finalizer ──"
  , "typedef struct MiGcManaged {"
  , "  void *ptr;"
  , "  void (*c_finalizer)(void*);"
  , "  MiVal native_fn;"
  , "  int gc_mark;"
  , "  struct MiGcManaged *gc_next;"
  , "} MiGcManaged;"
  , "static MiGcManaged *mi_gc_managed_list = NULL;"
  , ""
  , "// ── MiBignum: arbitrary-precision integer ──"
  , "typedef struct MiBignum {"
  , "  uint32_t *digits;  // base-2^32 digits, little-endian"
  , "  int len;           // number of digits"
  , "  int cap;           // allocated capacity"
  , "  int sign;          // 0 = non-negative, 1 = negative"
  , "} MiBignum;"
  , ""
  , "static MiBignum *mi_bn_alloc(int cap) {"
  , "  MiBignum *b = (MiBignum*)mi_alloc(sizeof(MiBignum));"
  , "  b->digits = (uint32_t*)mi_alloc(cap * sizeof(uint32_t));"
  , "  memset(b->digits, 0, cap * sizeof(uint32_t));"
  , "  b->len = 0; b->cap = cap; b->sign = 0; return b;"
  , "}"
  , ""
  , "static void mi_bn_ensure(MiBignum *b, int cap) {"
  , "  if (b->cap >= cap) return;"
  , "  int nc = b->cap * 2; if (nc < cap) nc = cap;"
  , "  uint32_t *nd = (uint32_t*)mi_alloc(nc * sizeof(uint32_t));"
  , "  memcpy(nd, b->digits, b->len * sizeof(uint32_t));"
  , "  memset(nd + b->len, 0, (nc - b->len) * sizeof(uint32_t));"
  , "  b->digits = nd; b->cap = nc;"
  , "}"
  , ""
  , "static void mi_bn_trim(MiBignum *b) {"
  , "  while (b->len > 0 && b->digits[b->len - 1] == 0) b->len--;"
  , "  if (b->len == 0) b->sign = 0;"
  , "}"
  , ""
  , "static MiBignum *mi_bn_from_i64(int64_t v) {"
  , "  MiBignum *b = mi_bn_alloc(2);"
  , "  uint64_t uv;"
  , "  if (v < 0) { b->sign = 1; uv = (v == INT64_MIN) ? ((uint64_t)INT64_MAX + 1) : (uint64_t)(-v); }"
  , "  else { b->sign = 0; uv = (uint64_t)v; }"
  , "  b->digits[0] = (uint32_t)(uv & 0xFFFFFFFFu);"
  , "  b->digits[1] = (uint32_t)(uv >> 32);"
  , "  b->len = 2; mi_bn_trim(b); return b;"
  , "}"
  , ""
  , "static int mi_bn_is_zero(MiBignum *b) { return b->len == 0; }"
  , ""
  , "static int mi_bn_fits_i64(MiBignum *b) {"
  , "  if (b->len == 0) return 1;"
  , "  if (b->len == 1) return 1;"
  , "  if (b->len == 2) {"
  , "    uint64_t uv = ((uint64_t)b->digits[1] << 32) | b->digits[0];"
  , "    if (b->sign) return uv <= ((uint64_t)INT64_MAX + 1);"
  , "    else return uv <= (uint64_t)INT64_MAX;"
  , "  }"
  , "  return 0;"
  , "}"
  , ""
  , "static int64_t mi_bn_to_i64(MiBignum *b) {"
  , "  if (b->len == 0) return 0;"
  , "  uint64_t uv = b->digits[0];"
  , "  if (b->len >= 2) uv |= ((uint64_t)b->digits[1] << 32);"
  , "  if (b->sign) return -(int64_t)uv;"
  , "  return (int64_t)uv;"
  , "}"
  , ""
  , "// compare magnitudes: -1, 0, 1"
  , "static int mi_bn_cmp_mag(MiBignum *a, MiBignum *b) {"
  , "  if (a->len != b->len) return (a->len > b->len) ? 1 : -1;"
  , "  for (int i = a->len - 1; i >= 0; i--) {"
  , "    if (a->digits[i] != b->digits[i]) return (a->digits[i] > b->digits[i]) ? 1 : -1;"
  , "  }"
  , "  return 0;"
  , "}"
  , ""
  , "// signed compare"
  , "static int mi_bn_cmp(MiBignum *a, MiBignum *b) {"
  , "  if (mi_bn_is_zero(a) && mi_bn_is_zero(b)) return 0;"
  , "  if (a->sign != b->sign) return a->sign ? -1 : 1;"
  , "  int mc = mi_bn_cmp_mag(a, b);"
  , "  return a->sign ? -mc : mc;"
  , "}"
  , ""
  , "// add magnitudes into result"
  , "static MiBignum *mi_bn_add_mag(MiBignum *a, MiBignum *b) {"
  , "  int n = (a->len > b->len ? a->len : b->len) + 1;"
  , "  MiBignum *r = mi_bn_alloc(n);"
  , "  uint64_t carry = 0;"
  , "  for (int i = 0; i < n; i++) {"
  , "    uint64_t s = carry;"
  , "    if (i < a->len) s += a->digits[i];"
  , "    if (i < b->len) s += b->digits[i];"
  , "    r->digits[i] = (uint32_t)(s & 0xFFFFFFFFu);"
  , "    carry = s >> 32;"
  , "  }"
  , "  r->len = n; mi_bn_trim(r); return r;"
  , "}"
  , ""
  , "// subtract magnitudes (assumes |a| >= |b|)"
  , "static MiBignum *mi_bn_sub_mag(MiBignum *a, MiBignum *b) {"
  , "  MiBignum *r = mi_bn_alloc(a->len);"
  , "  int64_t borrow = 0;"
  , "  for (int i = 0; i < a->len; i++) {"
  , "    int64_t s = (int64_t)a->digits[i] - borrow;"
  , "    if (i < b->len) s -= (int64_t)b->digits[i];"
  , "    if (s < 0) { s += (int64_t)1 << 32; borrow = 1; } else { borrow = 0; }"
  , "    r->digits[i] = (uint32_t)s;"
  , "  }"
  , "  r->len = a->len; mi_bn_trim(r); return r;"
  , "}"
  , ""
  , "static MiBignum *mi_bn_add(MiBignum *a, MiBignum *b) {"
  , "  if (a->sign == b->sign) {"
  , "    MiBignum *r = mi_bn_add_mag(a, b); r->sign = a->sign; return r;"
  , "  }"
  , "  int c = mi_bn_cmp_mag(a, b);"
  , "  if (c == 0) return mi_bn_alloc(1);"
  , "  if (c > 0) { MiBignum *r = mi_bn_sub_mag(a, b); r->sign = a->sign; return r; }"
  , "  MiBignum *r = mi_bn_sub_mag(b, a); r->sign = b->sign; return r;"
  , "}"
  , ""
  , "static MiBignum *mi_bn_sub(MiBignum *a, MiBignum *b) {"
  , "  MiBignum tmp = *b; tmp.sign = !b->sign;"
  , "  return mi_bn_add(a, &tmp);"
  , "}"
  , ""
  , "static MiBignum *mi_bn_mul(MiBignum *a, MiBignum *b) {"
  , "  if (mi_bn_is_zero(a) || mi_bn_is_zero(b)) return mi_bn_alloc(1);"
  , "  int n = a->len + b->len;"
  , "  MiBignum *r = mi_bn_alloc(n);"
  , "  for (int i = 0; i < a->len; i++) {"
  , "    uint64_t carry = 0;"
  , "    for (int j = 0; j < b->len; j++) {"
  , "      uint64_t prod = (uint64_t)a->digits[i] * b->digits[j] + r->digits[i+j] + carry;"
  , "      r->digits[i+j] = (uint32_t)(prod & 0xFFFFFFFFu);"
  , "      carry = prod >> 32;"
  , "    }"
  , "    r->digits[i + b->len] += (uint32_t)carry;"
  , "  }"
  , "  r->len = n; r->sign = (a->sign != b->sign) ? 1 : 0;"
  , "  mi_bn_trim(r); return r;"
  , "}"
  , ""
  , "// divmod: returns quotient, stores remainder in *rem"
  , "static MiBignum *mi_bn_divmod(MiBignum *a, MiBignum *b, MiBignum **rem) {"
  , "  if (mi_bn_is_zero(b)) { fprintf(stderr, \"Division by zero\\n\"); exit(1); }"
  , "  if (mi_bn_cmp_mag(a, b) < 0) {"
  , "    if (rem) { MiBignum *r = mi_bn_alloc(a->len); memcpy(r->digits, a->digits, a->len*sizeof(uint32_t)); r->len = a->len; r->sign = a->sign; *rem = r; }"
  , "    return mi_bn_alloc(1);"
  , "  }"
  , "  // single-digit divisor fast path"
  , "  if (b->len == 1) {"
  , "    uint32_t d = b->digits[0];"
  , "    MiBignum *q = mi_bn_alloc(a->len);"
  , "    uint64_t carry = 0;"
  , "    for (int i = a->len - 1; i >= 0; i--) {"
  , "      uint64_t cur = (carry << 32) | a->digits[i];"
  , "      q->digits[i] = (uint32_t)(cur / d);"
  , "      carry = cur % d;"
  , "    }"
  , "    q->len = a->len; q->sign = (a->sign != b->sign) ? 1 : 0; mi_bn_trim(q);"
  , "    if (rem) { MiBignum *r = mi_bn_from_i64((int64_t)carry); r->sign = a->sign; mi_bn_trim(r); *rem = r; }"
  , "    return q;"
  , "  }"
  , "  // multi-digit: binary long division"
  , "  int total_bits = a->len * 32;"
  , "  MiBignum *q = mi_bn_alloc(a->len);"
  , "  MiBignum *r = mi_bn_alloc(a->len + 1);"
  , "  r->len = 0;"
  , "  for (int i = total_bits - 1; i >= 0; i--) {"
  , "    // r = r << 1"
  , "    mi_bn_ensure(r, r->len + 1);"
  , "    uint32_t carry = 0;"
  , "    for (int j = 0; j < r->len; j++) {"
  , "      uint32_t nc = r->digits[j] >> 31;"
  , "      r->digits[j] = (r->digits[j] << 1) | carry; carry = nc;"
  , "    }"
  , "    if (carry) { r->digits[r->len] = carry; r->len++; }"
  , "    // set bit 0 of r from bit i of a"
  , "    int wi = i / 32, bi = i % 32;"
  , "    if (wi < a->len && (a->digits[wi] >> bi) & 1) {"
  , "      if (r->len == 0) { r->digits[0] = 1; r->len = 1; }"
  , "      else r->digits[0] |= 1;"
  , "    }"
  , "    mi_bn_trim(r);"
  , "    if (mi_bn_cmp_mag(r, b) >= 0) {"
  , "      MiBignum *nr = mi_bn_sub_mag(r, b);"
  , "      *r = *nr;"
  , "      int qw = i / 32, qb = i % 32;"
  , "      mi_bn_ensure(q, qw + 1);"
  , "      q->digits[qw] |= ((uint32_t)1 << qb);"
  , "      if (qw + 1 > q->len) q->len = qw + 1;"
  , "    }"
  , "  }"
  , "  q->sign = (a->sign != b->sign) ? 1 : 0; mi_bn_trim(q);"
  , "  if (rem) { r->sign = a->sign; mi_bn_trim(r); *rem = r; }"
  , "  return q;"
  , "}"
  , ""
  , "static MiBignum *mi_bn_div(MiBignum *a, MiBignum *b) { return mi_bn_divmod(a, b, NULL); }"
  , ""
  , "static MiBignum *mi_bn_mod(MiBignum *a, MiBignum *b) {"
  , "  MiBignum *rem; mi_bn_divmod(a, b, &rem); return rem;"
  , "}"
  , ""
  , "static MiBignum *mi_bn_neg(MiBignum *a) {"
  , "  MiBignum *r = mi_bn_alloc(a->len);"
  , "  memcpy(r->digits, a->digits, a->len * sizeof(uint32_t));"
  , "  r->len = a->len; r->sign = mi_bn_is_zero(a) ? 0 : !a->sign; return r;"
  , "}"
  , ""
  , "static double mi_bn_to_double(MiBignum *b) {"
  , "  double r = 0.0, base = 1.0;"
  , "  for (int i = 0; i < b->len; i++) {"
  , "    r += b->digits[i] * base;"
  , "    base *= 4294967296.0;"
  , "  }"
  , "  return b->sign ? -r : r;"
  , "}"
  , ""
  , "static int mi_bn_tostr(MiBignum *b, char *buf, int buflen) {"
  , "  if (mi_bn_is_zero(b)) { buf[0] = '0'; buf[1] = '\\0'; return 1; }"
  , "  MiBignum *tmp = mi_bn_alloc(b->len);"
  , "  memcpy(tmp->digits, b->digits, b->len * sizeof(uint32_t));"
  , "  tmp->len = b->len; tmp->sign = 0;"
  , "  int rbufcap = b->len * 10 + 16;"
  , "  char *rbuf = (char*)mi_alloc(rbufcap); int rlen = 0;"
  , "  MiBignum *ten = mi_bn_from_i64(10);"
  , "  while (!mi_bn_is_zero(tmp)) {"
  , "    MiBignum *rem; MiBignum *q = mi_bn_divmod(tmp, ten, &rem);"
  , "    rbuf[rlen++] = '0' + (mi_bn_is_zero(rem) ? 0 : (int)rem->digits[0]);"
  , "    tmp = q;"
  , "  }"
  , "  int pos = 0;"
  , "  if (b->sign) buf[pos++] = '-';"
  , "  for (int i = rlen - 1; i >= 0 && pos < buflen - 1; i--) buf[pos++] = rbuf[i];"
  , "  buf[pos] = '\\0'; return pos;"
  , "}"
  , ""
  , "static MiBignum *mi_bn_from_str(const char *s) {"
  , "  int neg = 0, start = 0;"
  , "  if (s[0] == '-') { neg = 1; start = 1; }"
  , "  MiBignum *r = mi_bn_alloc(1);"
  , "  MiBignum *ten = mi_bn_from_i64(10);"
  , "  for (int i = start; s[i]; i++) {"
  , "    if (s[i] < '0' || s[i] > '9') break;"
  , "    r = mi_bn_mul(r, ten);"
  , "    MiBignum *d = mi_bn_from_i64(s[i] - '0');"
  , "    r = mi_bn_add(r, d);"
  , "  }"
  , "  r->sign = neg && !mi_bn_is_zero(r); return r;"
  , "}"
  , ""
  , "// ── Sized integer clamping ──"
  , "static int64_t mi_clamp_signed(int w, int64_t v) {"
  , "  if (w >= 64) return v;"
  , "  int64_t half = (int64_t)1 << (w - 1);"
  , "  int64_t mod = (int64_t)1 << w;"
  , "  return ((v % mod + mod + half) % mod) - half;"
  , "}"
  , "static uint64_t mi_clamp_unsigned(int w, int64_t v) {"
  , "  if (w >= 64) return (uint64_t)v;"
  , "  uint64_t mod = (uint64_t)1 << w;"
  , "  return ((uint64_t)v) % mod;"
  , "}"
  , ""
  , "// ── Value constructors ──"
  , "static MiVal mi_int(int64_t v) { MiVal r; r.type = MI_INT; r.as.i = v; return r; }"
  , "static MiVal mi_sized_int(int64_t v, int w, int s) {"
  , "  MiVal r; r.type = MI_SIZED_INT; r.as.sized.is_big = 0;"
  , "  if (w == 0) { r.as.sized.i = v; }"
  , "  else { r.as.sized.i = s ? mi_clamp_signed(w, v) : (int64_t)mi_clamp_unsigned(w, v); }"
  , "  r.as.sized.width = w; r.as.sized.is_signed = s; return r;"
  , "}"
  , "static MiVal mi_sized_big(MiBignum *b, int s) {"
  , "  if (mi_bn_fits_i64(b)) return mi_sized_int(mi_bn_to_i64(b), 0, s);"
  , "  MiVal r; r.type = MI_SIZED_INT; r.as.sized.is_big = 1;"
  , "  r.as.sized.big = b; r.as.sized.width = 0; r.as.sized.is_signed = s; return r;"
  , "}"
  , "static MiVal mi_float(double v) { MiVal r; r.type = MI_FLOAT; r.as.f = v; return r; }"
  , "static MiVal mi_float32(float v) { MiVal r; r.type = MI_FLOAT32; r.as.f32 = v; return r; }"
  , "static MiVal mi_string(const char *s) { MiVal r; r.type = MI_STRING; int n = strlen(s); r.as.str.data = mi_alloc(n+1); memcpy(r.as.str.data, s, n+1); r.as.str.len = n; return r; }"
  , "static MiVal mi_stringn(const char *s, int n) { MiVal r; r.type = MI_STRING; r.as.str.data = mi_alloc(n+1); memcpy(r.as.str.data, s, n); r.as.str.data[n] = '\\0'; r.as.str.len = n; return r; }"
  , "static MiVal mi_pointer(void *p) { MiVal r; r.type = MI_POINTER; r.as.ptr = p; return r; }"
  , "static MiVal mi_managed(void *p, void (*fin)(void*)) {"
  , "  MiGcManaged *m = (MiGcManaged*)malloc(sizeof(MiGcManaged));"
  , "  m->ptr = p; m->c_finalizer = fin; m->native_fn.type = MI_INT; m->native_fn.as.i = 0;"
  , "  m->gc_mark = 0; m->gc_next = mi_gc_managed_list; mi_gc_managed_list = m;"
  , "  MiVal r; r.type = MI_MANAGED; r.as.ptr = m; return r;"
  , "}"
  , "static void *mi_raw_ptr(MiVal v) {"
  , "  if (v.type == MI_MANAGED) return ((MiGcManaged*)v.as.ptr)->ptr;"
  , "  return v.as.ptr;"
  , "}"
  , "static void *mi_maybe_ptr(MiVal v) {"
  , "  if (v.type == MI_RECORD && strcmp(v.as.rec.tag, \"Nothing\") == 0) return NULL;"
  , "  if (v.type == MI_RECORD && strcmp(v.as.rec.tag, \"Just\") == 0 && v.as.rec.nfields > 0)"
  , "    return mi_raw_ptr(v.as.rec.fields[0]);"
  , "  return mi_raw_ptr(v);"
  , "}"
  , "static const char *mi_maybe_str(MiVal v) {"
  , "  if (v.type == MI_RECORD && strcmp(v.as.rec.tag, \"Nothing\") == 0) return NULL;"
  , "  if (v.type == MI_RECORD && strcmp(v.as.rec.tag, \"Just\") == 0 && v.as.rec.nfields > 0)"
  , "    return v.as.rec.fields[0].as.str.data;"
  , "  return v.as.str.data;"
  , "}"
  , "// Struct-by-value helpers"
  , "static MiVal mi_struct_to_record(int n, const char *names[], MiVal fields[]) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"\";"
  , "  r.as.rec.nfields = n;"
  , "  r.as.rec.names = mi_alloc(n * sizeof(const char*));"
  , "  r.as.rec.fields = mi_alloc(n * sizeof(MiVal));"
  , "  for (int i = 0; i < n; i++) { r.as.rec.names[i] = names[i]; r.as.rec.fields[i] = fields[i]; }"
  , "  return r;"
  , "}"
  , "// General-purpose record constructor with tag (used by native codegen)"
  , "static MiVal mi_make_rec(const char *tag, int n, const char *names[], MiVal fields[]) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = tag;"
  , "  r.as.rec.nfields = n;"
  , "  if (n == 0) { r.as.rec.names = NULL; r.as.rec.fields = NULL; return r; }"
  , "  r.as.rec.names = mi_alloc(n * sizeof(const char*));"
  , "  r.as.rec.fields = mi_alloc(n * sizeof(MiVal));"
  , "  for (int i = 0; i < n; i++) { r.as.rec.names[i] = names[i]; r.as.rec.fields[i] = fields[i]; }"
  , "  return r;"
  , "}"
  , "// Auto-constructor for capitalized names (e.g. Just, Nil, Cons)"
  , "static MiVal mi_auto_ctor(const char *name) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = name;"
  , "  r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL;"
  , "  return r;"
  , "}"
  , "static MiVal mi_struct_field(MiVal rec, const char *name) {"
  , "  for (int i = 0; i < rec.as.rec.nfields; i++)"
  , "    if (strcmp(rec.as.rec.names[i], name) == 0) return rec.as.rec.fields[i];"
  , "  fprintf(stderr, \"struct field not found: %s\\n\", name); exit(1);"
  , "}"
  , "static MiVal mi_nil(void) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"Nil\"; r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL; return r;"
  , "}"
  , "static MiVal mi_cons(MiVal head, MiVal tail) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"Cons\"; r.as.rec.nfields = 2;"
  , "  r.as.rec.names = mi_alloc(2 * sizeof(const char*)); r.as.rec.names[0] = \"head\"; r.as.rec.names[1] = \"tail\";"
  , "  r.as.rec.fields = mi_alloc(2 * sizeof(MiVal)); r.as.rec.fields[0] = head; r.as.rec.fields[1] = tail;"
  , "  return r;"
  , "}"
  , "static MiVal mi_nothing(void) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"Nothing\"; r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL; return r;"
  , "}"
  , "static MiVal mi_just(MiVal val) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"Just\"; r.as.rec.nfields = 1;"
  , "  r.as.rec.names = mi_alloc(sizeof(const char*)); r.as.rec.names[0] = \"val\";"
  , "  r.as.rec.fields = mi_alloc(sizeof(MiVal)); r.as.rec.fields[0] = val;"
  , "  return r;"
  , "}"
  , "static MiVal mi_nullable_ptr(void *p) { if (p == NULL) return mi_nothing(); return mi_just(mi_pointer(p)); }"
  , "static MiVal mi_nullable_str(const char *s) { if (s == NULL) return mi_nothing(); return mi_just(mi_string(s)); }"
  , "static MiVal mi_native(MiVal (*fn)(MiVal, void*)) {"
  , "  MiVal r; r.type = MI_NATIVE; r.as.native.fn = fn; r.as.native.env = NULL; return r;"
  , "}"
  , "static MiVal mi_native_env(MiVal (*fn)(MiVal, void*), void *env) {"
  , "  MiVal r; r.type = MI_NATIVE; r.as.native.fn = fn; r.as.native.env = env; return r;"
  , "}"
  , ""
  , "// ── Error helpers ──"
  , "#if defined(__GNUC__) || defined(__clang__)"
  , "#define MI_NORETURN __attribute__((noreturn))"
  , "#elif defined(_MSC_VER)"
  , "#define MI_NORETURN __declspec(noreturn)"
  , "#else"
  , "#define MI_NORETURN _Noreturn"
  , "#endif"
  , "static MI_NORETURN void mi_error(const char *loc, const char *msg) {"
  , "  if (loc) fprintf(stderr, \"%s: %s\\n\", loc, msg);"
  , "  else fprintf(stderr, \"%s\\n\", msg);"
  , "  exit(1);"
  , "}"
  , "static MI_NORETURN void mi_errorf(const char *loc, const char *fmt, ...) {"
  , "  if (loc) fprintf(stderr, \"%s: \", loc);"
  , "  va_list args; va_start(args, fmt);"
  , "  vfprintf(stderr, fmt, args); va_end(args);"
  , "  fprintf(stderr, \"\\n\"); exit(1);"
  , "}"
  , ""
  , "// ── Env pool & GC ──"
  , "static int mi_in_eval = 0;"
  , "static MiEnv *mi_env_pool = NULL;"
  , "static MiEnv *mi_gc_all = NULL;"
  , "static int mi_gc_alloc_count = 0;"
  , "#define MI_GC_INTERVAL 100000"
  , ""
  , "// Shadow stack: GC roots for MiVal locals on the C stack"
  , "#define MI_MAX_ROOTS 256"
  , "static MiVal *mi_gc_roots[MI_MAX_ROOTS];"
  , "static int mi_gc_root_count = 0;"
  , ""
  , "// Shadow stack: GC roots for MiEnv* locals on the C stack"
  , "static MiEnv *mi_gc_env_roots[MI_MAX_ROOTS];"
  , "static int mi_gc_env_root_count = 0;"
  , ""
  , "// Pin list: closures pinned for async callbacks"
  , "static MiVal **mi_gc_pinned = NULL;"
  , "static int mi_gc_pin_count = 0;"
  , "static int mi_gc_pin_cap = 0;"
  , "static void mi_gc_pin(MiVal *v) {"
  , "  if (mi_gc_pin_count >= mi_gc_pin_cap) {"
  , "    mi_gc_pin_cap = mi_gc_pin_cap ? mi_gc_pin_cap * 2 : 16;"
  , "    mi_gc_pinned = realloc(mi_gc_pinned, mi_gc_pin_cap * sizeof(MiVal*));"
  , "  }"
  , "  mi_gc_pinned[mi_gc_pin_count++] = v;"
  , "}"
  , ""
  , "static MiEnv *mi_env_pool_get(void) {"
  , "  mi_gc_alloc_count++;"
  , "  MiEnv *e;"
  , "  if (mi_env_pool) { e = mi_env_pool; mi_env_pool = e->next; }"
  , "  else { e = (MiEnv*)malloc(sizeof(MiEnv)); }"
  , "  memset(e, 0, sizeof(MiEnv));"
  , "  e->gc_all = mi_gc_all; mi_gc_all = e;"
  , "  return e;"
  , "}"
  , ""
  , "// ── Env operations ──"
  , "static MiEnv *mi_env_new(MiEnv *parent) {"
  , "  MiEnv *e;"
  , "  if (mi_in_eval) { e = mi_env_pool_get(); }"
  , "  else { e = mi_alloc(sizeof(MiEnv)); memset(e, 0, sizeof(MiEnv)); }"
  , "  e->parent = parent; return e;"
  , "}"
  , ""
  , "static void mi_env_set(MiEnv *env, const char *name, MiVal val) {"
  , "  for (MiEnv *e = env->next; e; e = e->next) {"
  , "    if (e->name && strcmp(e->name, name) == 0) { e->val = val; return; }"
  , "  }"
  , "  MiEnv *entry;"
  , "  if (mi_in_eval) { entry = mi_env_pool_get(); entry->name = (char*)name; }"
  , "  else { entry = mi_alloc(sizeof(MiEnv)); memset(entry, 0, sizeof(MiEnv)); entry->name = mi_strdup(name); }"
  , "  entry->val = val;"
  , "  entry->next = env->next; entry->parent = NULL;"
  , "  env->next = entry;"
  , "}"
  , ""
  , "static MiVal mi_env_get_loc(MiEnv *env, const char *name, const char *loc) {"
  , "  MiEnv *frame = env;"
  , "  while (frame) {"
  , "    for (MiEnv *e = frame; e; e = e->next)"
  , "      if (e->name && strcmp(e->name, name) == 0) return e->val;"
  , "    frame = frame->parent;"
  , "  }"
  , "  if (name[0] >= 'A' && name[0] <= 'Z') {"
  , "    MiVal r; r.type = MI_RECORD; r.as.rec.tag = name;"
  , "    r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL;"
  , "    return r;"
  , "  }"
  , "  mi_errorf(loc, \"unbound variable: %s\", name);"
  , "  exit(1);"
  , "}"
  , "static MiVal mi_env_get(MiEnv *env, const char *name) { return mi_env_get_loc(env, name, NULL); }"
  , ""
  , "// ── Env GC (mark-sweep for pool-allocated envs) ──"
  , "static void mi_gc_scan_val(MiVal v);"
  , "static void mi_gc_mark_env(MiEnv *env) {"
  , "  while (env && !env->gc_mark) {"
  , "    env->gc_mark = 1;"
  , "    for (MiEnv *e = env->next; e && !e->gc_mark; e = e->next) {"
  , "      e->gc_mark = 1;"
  , "      mi_gc_scan_val(e->val);"
  , "    }"
  , "    env = env->parent;"
  , "  }"
  , "}"
  , "static void mi_gc_scan_val(MiVal v) {"
  , "  restart:;"
  , "  if (v.type == MI_CLOSURE && v.as.closure.env) {"
  , "    mi_gc_mark_env(v.as.closure.env);"
  , "  } else if (v.type == MI_MANAGED && v.as.ptr) {"
  , "    MiGcManaged *m = (MiGcManaged*)v.as.ptr; m->gc_mark = 1;"
  , "    mi_gc_scan_val(m->native_fn);"
  , "  } else if (v.type == MI_RECORD && v.as.rec.fields) {"
  , "    for (int i = 0; i < v.as.rec.nfields - 1; i++)"
  , "      mi_gc_scan_val(v.as.rec.fields[i]);"
  , "    if (v.as.rec.nfields > 0) { v = v.as.rec.fields[v.as.rec.nfields - 1]; goto restart; }"
  , "  }"
  , "}"
  , "static void mi_gc_collect(MiEnv *root) {"
  , "  mi_gc_mark_env(root);"
  , "  for (int i = 0; i < mi_gc_root_count; i++) mi_gc_scan_val(*mi_gc_roots[i]);"
  , "  for (int i = 0; i < mi_gc_env_root_count; i++) mi_gc_mark_env(mi_gc_env_roots[i]);"
  , "  for (int i = 0; i < mi_gc_pin_count; i++) mi_gc_scan_val(*mi_gc_pinned[i]);"
  , "  MiEnv *live = NULL;"
  , "  MiEnv *e = mi_gc_all;"
  , "  while (e) {"
  , "    MiEnv *next = e->gc_all;"
  , "    if (!e->gc_mark) {"
  , "      e->next = mi_env_pool; mi_env_pool = e;"
  , "    } else {"
  , "      e->gc_mark = 0;"
  , "      e->gc_all = live; live = e;"
  , "    }"
  , "    e = next;"
  , "  }"
  , "  mi_gc_all = live;"
  , "  // Sweep managed pointers"
  , "  MiGcManaged *live_m = NULL;"
  , "  MiGcManaged *m = mi_gc_managed_list;"
  , "  while (m) {"
  , "    MiGcManaged *next_m = m->gc_next;"
  , "    if (!m->gc_mark) {"
  , "      if (m->c_finalizer) m->c_finalizer(m->ptr);"
  , "      else if (m->native_fn.type == MI_NATIVE) m->native_fn.as.native.fn(mi_pointer(m->ptr), m->native_fn.as.native.env);"
  , "      free(m);"
  , "    } else {"
  , "      m->gc_mark = 0; m->gc_next = live_m; live_m = m;"
  , "    }"
  , "    m = next_m;"
  , "  }"
  , "  mi_gc_managed_list = live_m;"
  , "  mi_gc_alloc_count = 0;"
  , "}"
  , ""
  , "// ── Expr constructors ──"
  , "static MiExpr *mi_expr_int(int64_t v) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_INT; e->as.i = v; return e;"
  , "}"
  , "static MiExpr *mi_expr_sized_int(int64_t v, int width, int is_signed) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_SIZED_INT;"
  , "  e->as.sized.i = v; e->as.sized.width = width; e->as.sized.is_signed = is_signed; return e;"
  , "}"
  , "static MiExpr *mi_expr_float(double v) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_FLOAT; e->as.f = v; return e;"
  , "}"
  , "static MiExpr *mi_expr_float32(float v) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_FLOAT32; e->as.f = (double)v; return e;"
  , "}"
  , "static MiExpr *mi_expr_string(const char *s) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_STRING; e->as.s = mi_strdup(s); return e;"
  , "}"
  , "static MiExpr *mi_expr_name(const char *n) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_NAME; e->as.name = mi_strdup(n); return e;"
  , "}"
  , "static MiExpr *mi_expr_binop(const char *op, MiExpr *l, MiExpr *r) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_BINOP;"
  , "  e->as.binop.op = mi_strdup(op); e->as.binop.left = l; e->as.binop.right = r; return e;"
  , "}"
  , "static MiExpr *mi_expr_app(MiExpr *fn, MiExpr *arg) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_APP;"
  , "  e->as.app.fn = fn; e->as.app.arg = arg; return e;"
  , "}"
  , "static MiExpr *mi_expr_lam(const char *param, MiExpr *body) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_LAM;"
  , "  e->as.lam.param = mi_strdup(param); e->as.lam.body = body; return e;"
  , "}"
  , "static MiExpr *mi_expr_val(MiVal v) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_VAL; e->as.val = v; return e;"
  , "}"
  , "static MiExpr *mi_expr_field(MiExpr *expr, const char *field) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_FIELD;"
  , "  e->as.field.expr = expr; e->as.field.field = mi_strdup(field); return e;"
  , "}"
  , "static MiExpr *mi_expr_thunk(MiExpr *body) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_THUNK; e->as.thunk.body = body; return e;"
  , "}"
  , ""
  , "static MiBinding mi_binding(const char *name, int lazy, int nparams, ...) {"
  , "  MiBinding b; b.name = mi_strdup(name); b.lazy = lazy; b.nparams = nparams;"
  , "  va_list args; va_start(args, nparams);"
  , "  if (nparams > 0) {"
  , "    b.params = mi_alloc(nparams * sizeof(char*));"
  , "    for (int i = 0; i < nparams; i++) b.params[i] = mi_strdup(va_arg(args, char*));"
  , "  } else { b.params = NULL; }"
  , "  b.body = va_arg(args, MiExpr*); va_end(args); return b;"
  , "}"
  , ""
  , "static MiExpr *mi_expr_with(MiExpr *body, int n, ...) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_WITH;"
  , "  e->as.with.body = body; e->as.with.nbindings = n;"
  , "  e->as.with.bindings = mi_alloc(n * sizeof(MiBinding));"
  , "  va_list args; va_start(args, n);"
  , "  for (int i = 0; i < n; i++) e->as.with.bindings[i] = va_arg(args, MiBinding);"
  , "  va_end(args); return e;"
  , "}"
  , ""
  , "static MiExpr *mi_expr_record(const char *tag, int n, ...) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_RECORD;"
  , "  e->as.record.tag = mi_strdup(tag); e->as.record.nbindings = n;"
  , "  e->as.record.bindings = mi_alloc(n * sizeof(MiBinding));"
  , "  va_list args; va_start(args, n);"
  , "  for (int i = 0; i < n; i++) e->as.record.bindings[i] = va_arg(args, MiBinding);"
  , "  va_end(args); return e;"
  , "}"
  , ""
  , "static MiExpr *mi_expr_namespace(int n, ...) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_NAMESPACE;"
  , "  e->as.ns.nbindings = n; e->as.ns.bindings = mi_alloc(n * sizeof(MiBinding));"
  , "  va_list args; va_start(args, n);"
  , "  for (int i = 0; i < n; i++) e->as.ns.bindings[i] = va_arg(args, MiBinding);"
  , "  va_end(args); return e;"
  , "}"
  , ""
  , "static MiExpr *mi_expr_loc(MiExpr *e, const char *loc) { e->loc = loc; return e; }"
  , ""
  , "// ── Pat constructors ──"
  , "static MiPat *mi_pat_var(const char *v) {"
  , "  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_VAR; p->as.var = mi_strdup(v); return p;"
  , "}"
  , "static MiPat *mi_pat_wild(void) { MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_WILD; return p; }"
  , "static MiPat *mi_pat_int(int64_t v) {"
  , "  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_INT; p->as.i = v; return p;"
  , "}"
  , "static MiPat *mi_pat_string(const char *s) {"
  , "  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_STRING; p->as.s = mi_strdup(s); return p;"
  , "}"
  , "static MiPat *mi_pat_rec(const char *tag, int n, ...) {"
  , "  MiPat *p = mi_alloc(sizeof(MiPat)); p->type = PAT_REC;"
  , "  p->as.rec.tag = mi_strdup(tag); p->as.rec.nfields = n;"
  , "  p->as.rec.field_names = mi_alloc(n * sizeof(char*));"
  , "  p->as.rec.field_pats = mi_alloc(n * sizeof(MiPat*));"
  , "  va_list args; va_start(args, n);"
  , "  for (int i = 0; i < n; i++) {"
  , "    p->as.rec.field_names[i] = mi_strdup(va_arg(args, char*));"
  , "    p->as.rec.field_pats[i] = va_arg(args, MiPat*);"
  , "  }"
  , "  va_end(args); return p;"
  , "}"
  , ""
  , "static MiAlt mi_alt(MiPat *pat, MiExpr *guard, MiExpr *body) { MiAlt a; a.pat = pat; a.guard = guard; a.body = body; return a; }"
  , ""
  , "static MiExpr *mi_expr_case(MiExpr *scrut, int n, ...) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_CASE;"
  , "  e->as.cas.scrut = scrut; e->as.cas.nalts = n;"
  , "  e->as.cas.alts = mi_alloc(n * sizeof(MiAlt));"
  , "  va_list args; va_start(args, n);"
  , "  for (int i = 0; i < n; i++) e->as.cas.alts[i] = va_arg(args, MiAlt);"
  , "  va_end(args); return e;"
  , "}"
  , ""
  , "// ── Optimized expression constructors ──"
  , "static MiExpr *mi_expr_if(MiExpr *cond, MiExpr *then_br, MiExpr *else_br) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_IF;"
  , "  e->as.if_expr.cond = cond; e->as.if_expr.then_br = then_br;"
  , "  e->as.if_expr.else_br = else_br; return e;"
  , "}"
  , "static MiExpr *mi_expr_for_range(const char *acc_var, const char *iter_var,"
  , "    MiExpr *init, MiExpr *start, MiExpr *end, MiExpr *body) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_FOR_RANGE;"
  , "  e->as.for_range.acc_var = mi_strdup(acc_var);"
  , "  e->as.for_range.iter_var = mi_strdup(iter_var);"
  , "  e->as.for_range.init = init; e->as.for_range.start = start;"
  , "  e->as.for_range.end = end; e->as.for_range.body = body; return e;"
  , "}"
  , "static MiExpr *mi_expr_tail_loop(int nparams, char **params, MiExpr **inits, MiExpr *body) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_TAIL_LOOP;"
  , "  e->as.tail_loop.nparams = nparams; e->as.tail_loop.params = params;"
  , "  e->as.tail_loop.inits = inits; e->as.tail_loop.body = body; return e;"
  , "}"
  , "static MiExpr *mi_expr_tail_call(int nargs, ...) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_TAIL_CALL;"
  , "  e->as.tail_call.nargs = nargs;"
  , "  e->as.tail_call.args = mi_alloc(nargs * sizeof(MiExpr*));"
  , "  va_list args; va_start(args, nargs);"
  , "  for (int i = 0; i < nargs; i++) e->as.tail_call.args[i] = va_arg(args, MiExpr*);"
  , "  va_end(args); return e;"
  , "}"
  , ""
  , "static MiExpr *mi_expr_native_binop(const char *op, MiExpr *left, MiExpr *right) {"
  , "  MiExpr *e = mi_alloc(sizeof(MiExpr)); e->type = EXPR_NATIVE_BINOP;"
  , "  e->as.native_binop.op = mi_strdup(op);"
  , "  e->as.native_binop.left = left; e->as.native_binop.right = right; return e;"
  , "}"
  , ""
  , "// ── Helpers ──"
  , "static double mi_to_float(MiVal v) {"
  , "  if (v.type == MI_FLOAT) return v.as.f;"
  , "  if (v.type == MI_FLOAT32) return (double)v.as.f32;"
  , "  if (v.type == MI_SIZED_INT) return v.as.sized.is_big ? mi_bn_to_double(v.as.sized.big) : (double)v.as.sized.i;"
  , "  return (double)v.as.i;"
  , "}"
  , "static float mi_to_float32(MiVal v) {"
  , "  if (v.type == MI_FLOAT32) return v.as.f32;"
  , "  if (v.type == MI_FLOAT) return (float)v.as.f;"
  , "  if (v.type == MI_SIZED_INT) return v.as.sized.is_big ? (float)mi_bn_to_double(v.as.sized.big) : (float)v.as.sized.i;"
  , "  return (float)v.as.i;"
  , "}"
  , ""
  , "static int mi_positional_idx(const char *name) {"
  , "  if (name[0] != '_' || name[1] == '\\0') return -1;"
  , "  for (int i = 1; name[i]; i++) if (name[i] < '0' || name[i] > '9') return -1;"
  , "  return atoi(name + 1);"
  , "}"
  , ""
  , "static MiVal mi_field_loc(MiVal rec, const char *name, const char *loc) {"
  , "  if (rec.type != MI_RECORD) { mi_error(loc, \"field access on non-record\"); }"
  , "  int idx = mi_positional_idx(name);"
  , "  if (idx >= 0 && idx < rec.as.rec.nfields) return rec.as.rec.fields[idx];"
  , "  for (int i = 0; i < rec.as.rec.nfields; i++)"
  , "    if (rec.as.rec.names && strcmp(rec.as.rec.names[i], name) == 0)"
  , "      return rec.as.rec.fields[i];"
  , "  mi_errorf(loc, \"field '%s' not found in record '%s'\", name, rec.as.rec.tag);"
  , "  exit(1);"
  , "}"
  , "static MiVal mi_field(MiVal rec, const char *name) { return mi_field_loc(rec, name, NULL); }"
  , ""
  , "static MiVal mi_apply(MiVal f, MiVal arg);"
  , "static MiVal mi_eval(MiExpr *expr, MiEnv *env);"
  , "static MiVal mi_force(MiVal v, MiEnv *env);"
  , "static int64_t mi_truthy(MiVal v);"
  , ""
  , "// ── Printing ──"
  , "static int mi_is_cons_list(MiVal v);"
  , "static void mi_print_cons_items(MiVal v, int first);"
  , ""
  , "static void mi_print_val(MiVal v) {"
  , "  switch (v.type) {"
  , "    case MI_INT:    printf(\"%\" PRId64, v.as.i); break;"
  , "    case MI_SIZED_INT: if (v.as.sized.is_big) { int bsz = v.as.sized.big->len*10+16; char *t = (char*)mi_alloc(bsz); mi_bn_tostr(v.as.sized.big, t, bsz); printf(\"%s\", t); } else printf(\"%\" PRId64, v.as.sized.i); break;"
  , "    case MI_FLOAT:  printf(\"%g\", v.as.f); break;"
  , "    case MI_FLOAT32: printf(\"%g\", (double)v.as.f32); break;"
  , "    case MI_STRING: printf(\"%.*s\", v.as.str.len, v.as.str.data); break;"
  , "    case MI_RECORD:"
  , "      if (mi_is_cons_list(v)) {"
  , "        printf(\"[\"); mi_print_cons_items(v, 1); printf(\"]\");"
  , "      } else {"
  , "        printf(\"%s {\", v.as.rec.tag);"
  , "        for (int i = 0; i < v.as.rec.nfields; i++) {"
  , "          if (i > 0) printf(\", \");"
  , "          if (v.as.rec.names) printf(\"%s = \", v.as.rec.names[i]);"
  , "          mi_print_val(v.as.rec.fields[i]);"
  , "        }"
  , "        printf(\"}\");"
  , "      }"
  , "      break;"
  , "    case MI_CLOSURE: printf(\"<closure>\"); break;"
  , "    case MI_NATIVE:  printf(\"<closure>\"); break;"
  , "    case MI_POINTER: printf(\"<ptr:%p>\", v.as.ptr); break;"
  , "    case MI_MANAGED: printf(\"<managed:%p>\", mi_raw_ptr(v)); break;"
  , "  }"
  , "}"
  , "static int mi_is_cons_list(MiVal v) {"
  , "  if (v.type != MI_RECORD) return 0;"
  , "  if (strcmp(v.as.rec.tag, \"Nil\") == 0 && v.as.rec.nfields == 0) return 1;"
  , "  if (strcmp(v.as.rec.tag, \"Cons\") == 0 && v.as.rec.nfields == 2) return mi_is_cons_list(v.as.rec.fields[1]);"
  , "  return 0;"
  , "}"
  , "static void mi_print_cons_items(MiVal v, int first) {"
  , "  if (strcmp(v.as.rec.tag, \"Nil\") == 0) return;"
  , "  if (!first) printf(\", \");"
  , "  mi_print_val(v.as.rec.fields[0]);"
  , "  mi_print_cons_items(v.as.rec.fields[1], 0);"
  , "}"
  , "static void mi_println_val(MiVal v) { mi_print_val(v); printf(\"\\n\"); }"
  , ""
  , "// ── Application ──"
  , "static MiVal mi_apply(MiVal f, MiVal arg) {"
  , "  f = mi_force(f, NULL);"
  , "  if (f.type == MI_NATIVE) return f.as.native.fn(arg, f.as.native.env);"
  , "  if (f.type == MI_CLOSURE) {"
  , "    MiEnv *call_env = mi_env_new(f.as.closure.env);"
  , "    mi_env_set(call_env, f.as.closure.param, arg);"
  , "    return mi_eval(f.as.closure.body, call_env);"
  , "  }"
  , "  mi_error(NULL, \"apply on non-function\");"
  , "}"
  , ""
  , "// ── Force thunk ──"
  , "static MiVal mi_force(MiVal v, MiEnv *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_CLOSURE && v.as.closure.param && strcmp(v.as.closure.param, \"_thunk_\") == 0)"
  , "    return mi_eval(v.as.closure.body, v.as.closure.env);"
  , "  return v;"
  , "}"
  , ""
  , "// ── Pattern matching ──"
  , "static int mi_match(MiPat *pat, MiVal val, MiEnv *env) {"
  , "  switch (pat->type) {"
  , "    case PAT_VAR:    mi_env_set(env, pat->as.var, val); return 1;"
  , "    case PAT_WILD:   return 1;"
  , "    case PAT_INT:    return (val.type == MI_INT && val.as.i == pat->as.i) || (val.type == MI_SIZED_INT && !val.as.sized.is_big && val.as.sized.i == pat->as.i);"
  , "    case PAT_STRING: return val.type == MI_STRING && strcmp(val.as.str.data, pat->as.s) == 0;"
  , "    case PAT_REC:"
  , "      if (val.type != MI_RECORD || strcmp(val.as.rec.tag, pat->as.rec.tag) != 0) return 0;"
  , "      for (int i = 0; i < pat->as.rec.nfields; i++) {"
  , "        int idx = mi_positional_idx(pat->as.rec.field_names[i]);"
  , "        if (idx >= 0 && idx < val.as.rec.nfields) {"
  , "          if (!mi_match(pat->as.rec.field_pats[i], val.as.rec.fields[idx], env)) return 0;"
  , "          continue;"
  , "        }"
  , "        int found = 0;"
  , "        for (int j = 0; j < val.as.rec.nfields; j++) {"
  , "          if (strcmp(val.as.rec.names[j], pat->as.rec.field_names[i]) == 0) {"
  , "            if (!mi_match(pat->as.rec.field_pats[i], val.as.rec.fields[j], env)) return 0;"
  , "            found = 1; break;"
  , "          }"
  , "        }"
  , "        if (!found) return 0;"
  , "      }"
  , "      return 1;"
  , "  }"
  , "  return 0;"
  , "}"
  , ""
  , "// ── Structural equality ──"
  , "static int mi_vals_equal(MiVal a, MiVal b);"
  , "static int mi_vals_equal(MiVal a, MiVal b) {"
  , "  if ((a.type == MI_INT || a.type == MI_SIZED_INT) && (b.type == MI_INT || b.type == MI_SIZED_INT)) {"
  , "    int a_big = (a.type == MI_SIZED_INT && a.as.sized.is_big);"
  , "    int b_big = (b.type == MI_SIZED_INT && b.as.sized.is_big);"
  , "    if (a_big || b_big) {"
  , "      MiBignum *ba = a_big ? a.as.sized.big : mi_bn_from_i64((a.type == MI_SIZED_INT) ? a.as.sized.i : a.as.i);"
  , "      MiBignum *bb = b_big ? b.as.sized.big : mi_bn_from_i64((b.type == MI_SIZED_INT) ? b.as.sized.i : b.as.i);"
  , "      return mi_bn_cmp(ba, bb) == 0;"
  , "    }"
  , "    int64_t va = (a.type == MI_SIZED_INT) ? a.as.sized.i : a.as.i;"
  , "    int64_t vb = (b.type == MI_SIZED_INT) ? b.as.sized.i : b.as.i;"
  , "    return va == vb;"
  , "  }"
  , "  if (a.type != b.type) return 0;"
  , "  switch (a.type) {"
  , "    case MI_INT: return a.as.i == b.as.i;"
  , "    case MI_FLOAT: return a.as.f == b.as.f;"
  , "    case MI_FLOAT32: return a.as.f32 == b.as.f32;"
  , "    case MI_STRING: return a.as.str.len == b.as.str.len && memcmp(a.as.str.data, b.as.str.data, a.as.str.len) == 0;"
  , "    case MI_RECORD:"
  , "      if (strcmp(a.as.rec.tag, b.as.rec.tag) != 0 || a.as.rec.nfields != b.as.rec.nfields) return 0;"
  , "      for (int i = 0; i < a.as.rec.nfields; i++) {"
  , "        if (strcmp(a.as.rec.names[i], b.as.rec.names[i]) != 0) return 0;"
  , "        if (!mi_vals_equal(a.as.rec.fields[i], b.as.rec.fields[i])) return 0;"
  , "      }"
  , "      return 1;"
  , "    default: return 0;"
  , "  }"
  , "}"
  , ""
  , "// ── Arithmetic ──"
  , "static MiVal mi_binop(const char *op, MiVal a, MiVal b) {"
  , "  a = mi_force(a, NULL); b = mi_force(b, NULL);"
  , "  if (strcmp(op, \":\") == 0) return mi_cons(a, b);"
  , "  if (strcmp(op, \"==\") == 0) return mi_int(mi_vals_equal(a, b));"
  , "  if (strcmp(op, \"/=\") == 0) return mi_int(!mi_vals_equal(a, b));"
  , "  if (strcmp(op, \"+\") == 0 && a.type == MI_STRING && b.type == MI_STRING) {"
  , "    int la = a.as.str.len, lb = b.as.str.len;"
  , "    char *r = mi_alloc(la + lb + 1); memcpy(r, a.as.str.data, la); memcpy(r+la, b.as.str.data, lb); r[la+lb] = '\\0';"
  , "    MiVal v; v.type = MI_STRING; v.as.str.data = r; v.as.str.len = la + lb; return v;"
  , "  }"
  , "  if (a.type == MI_STRING && b.type == MI_STRING) {"
  , "    int cmp = strcmp(a.as.str.data, b.as.str.data);"
  , "    if (strcmp(op, \"<\") == 0) return mi_int(cmp < 0);"
  , "    if (strcmp(op, \">\") == 0) return mi_int(cmp > 0);"
  , "    if (strcmp(op, \"<=\") == 0) return mi_int(cmp <= 0);"
  , "    if (strcmp(op, \">=\") == 0) return mi_int(cmp >= 0);"
  , "  }"
  , "  if (a.type == MI_FLOAT || b.type == MI_FLOAT) {"
  , "    double fa = mi_to_float(a), fb = mi_to_float(b);"
  , "    if (strcmp(op, \"+\") == 0) return mi_float(fa + fb);"
  , "    if (strcmp(op, \"-\") == 0) return mi_float(fa - fb);"
  , "    if (strcmp(op, \"*\") == 0) return mi_float(fa * fb);"
  , "    if (strcmp(op, \"/\") == 0) return mi_float(fa / fb);"
  , "    if (strcmp(op, \"**\") == 0 && (b.type == MI_INT || b.type == MI_SIZED_INT)) { int64_t bi = (b.type == MI_SIZED_INT) ? (b.as.sized.is_big ? mi_bn_to_i64(b.as.sized.big) : b.as.sized.i) : b.as.i; if (bi >= 0) { double r=1.0; for(int64_t e=bi;e>0;e--) r*=fa; return mi_float(r); } }"
  , "    if (strcmp(op, \"<\") == 0) return mi_int(fa < fb);"
  , "    if (strcmp(op, \">\") == 0) return mi_int(fa > fb);"
  , "    if (strcmp(op, \"<=\") == 0) return mi_int(fa <= fb);"
  , "    if (strcmp(op, \">=\") == 0) return mi_int(fa >= fb);"
  , "    fprintf(stderr, \"unsupported operator %s for float operands\\n\", op); exit(1);"
  , "  }"
  , "  if (a.type == MI_FLOAT32 || b.type == MI_FLOAT32) {"
  , "    float fa = mi_to_float32(a), fb = mi_to_float32(b);"
  , "    if (strcmp(op, \"+\") == 0) return mi_float32(fa + fb);"
  , "    if (strcmp(op, \"-\") == 0) return mi_float32(fa - fb);"
  , "    if (strcmp(op, \"*\") == 0) return mi_float32(fa * fb);"
  , "    if (strcmp(op, \"/\") == 0) return mi_float32(fa / fb);"
  , "    if (strcmp(op, \"**\") == 0 && (b.type == MI_INT || b.type == MI_SIZED_INT)) { int64_t bi = (b.type == MI_SIZED_INT) ? (b.as.sized.is_big ? mi_bn_to_i64(b.as.sized.big) : b.as.sized.i) : b.as.i; if (bi >= 0) { float r=1.0f; for(int64_t e=bi;e>0;e--) r*=fa; return mi_float32(r); } }"
  , "    if (strcmp(op, \"<\") == 0) return mi_int(fa < fb);"
  , "    if (strcmp(op, \">\") == 0) return mi_int(fa > fb);"
  , "    if (strcmp(op, \"<=\") == 0) return mi_int(fa <= fb);"
  , "    if (strcmp(op, \">=\") == 0) return mi_int(fa >= fb);"
  , "    fprintf(stderr, \"unsupported operator %s for float32 operands\\n\", op); exit(1);"
  , "  }"
  , "  if (a.type == MI_SIZED_INT || b.type == MI_SIZED_INT) {"
  , "    int w = (a.type == MI_SIZED_INT) ? a.as.sized.width : ((b.type == MI_SIZED_INT) ? b.as.sized.width : 0);"
  , "    int s = (a.type == MI_SIZED_INT) ? a.as.sized.is_signed : ((b.type == MI_SIZED_INT) ? b.as.sized.is_signed : 1);"
  , "    if (a.type == MI_SIZED_INT && b.type == MI_SIZED_INT) {"
  , "      w = (a.as.sized.width > b.as.sized.width) ? a.as.sized.width : b.as.sized.width;"
  , "      s = a.as.sized.is_signed || b.as.sized.is_signed;"
  , "    }"
  , "    int a_big = (a.type == MI_SIZED_INT && a.as.sized.is_big);"
  , "    int b_big = (b.type == MI_SIZED_INT && b.as.sized.is_big);"
  , "    // If either is bignum or width==0 (arbitrary precision), use bignum path"
  , "    if (a_big || b_big || w == 0) {"
  , "      MiBignum *ba = a_big ? a.as.sized.big : mi_bn_from_i64((a.type == MI_SIZED_INT) ? a.as.sized.i : a.as.i);"
  , "      MiBignum *bb = b_big ? b.as.sized.big : mi_bn_from_i64((b.type == MI_SIZED_INT) ? b.as.sized.i : b.as.i);"
  , "      if (strcmp(op, \"+\") == 0) return mi_sized_big(mi_bn_add(ba, bb), s);"
  , "      if (strcmp(op, \"-\") == 0) return mi_sized_big(mi_bn_sub(ba, bb), s);"
  , "      if (strcmp(op, \"*\") == 0) return mi_sized_big(mi_bn_mul(ba, bb), s);"
  , "      if (strcmp(op, \"/\") == 0) return mi_sized_big(mi_bn_div(ba, bb), s);"
  , "      if (strcmp(op, \"%\") == 0) return mi_sized_big(mi_bn_mod(ba, bb), s);"
  , "      if (strcmp(op, \"**\") == 0) {"
  , "        int64_t exp = b_big ? mi_bn_to_i64(bb) : ((b.type == MI_SIZED_INT) ? b.as.sized.i : b.as.i);"
  , "        MiBignum *r = mi_bn_from_i64(1);"
  , "        for (int64_t e = exp; e > 0; e--) r = mi_bn_mul(r, ba);"
  , "        return mi_sized_big(r, s);"
  , "      }"
  , "      if (strcmp(op, \"<\") == 0) return mi_int(mi_bn_cmp(ba, bb) < 0);"
  , "      if (strcmp(op, \">\") == 0) return mi_int(mi_bn_cmp(ba, bb) > 0);"
  , "      if (strcmp(op, \"<=\") == 0) return mi_int(mi_bn_cmp(ba, bb) <= 0);"
  , "      if (strcmp(op, \">=\") == 0) return mi_int(mi_bn_cmp(ba, bb) >= 0);"
  , "      fprintf(stderr, \"unsupported operator %s for bignum\\n\", op); exit(1);"
  , "    }"
  , "    // Fixed-width path"
  , "    int64_t ia = (a.type == MI_SIZED_INT) ? a.as.sized.i : a.as.i;"
  , "    int64_t ib = (b.type == MI_SIZED_INT) ? b.as.sized.i : b.as.i;"
  , "    int64_t r;"
  , "    if (strcmp(op, \"+\") == 0) r = ia + ib;"
  , "    else if (strcmp(op, \"-\") == 0) r = ia - ib;"
  , "    else if (strcmp(op, \"*\") == 0) r = ia * ib;"
  , "    else if (strcmp(op, \"/\") == 0) { r = (ib == 0) ? 0 : ia / ib; }"
  , "    else if (strcmp(op, \"%\") == 0) { r = (ib == 0) ? 0 : ia % ib; }"
  , "    else if (strcmp(op, \"**\") == 0) { r = 1; for(int64_t e=ib;e>0;e--) r*=ia; }"
  , "    else if (strcmp(op, \"<\") == 0) return mi_int(ia < ib);"
  , "    else if (strcmp(op, \">\") == 0) return mi_int(ia > ib);"
  , "    else if (strcmp(op, \"<=\") == 0) return mi_int(ia <= ib);"
  , "    else if (strcmp(op, \">=\") == 0) return mi_int(ia >= ib);"
  , "    else { fprintf(stderr, \"unsupported operator %s for sized int\\n\", op); exit(1); }"
  , "    return mi_sized_int(r, w, s);"
  , "  }"
  , "  int64_t ia = a.as.i, ib = b.as.i;"
  , "  // Overflow-detecting arithmetic for plain ints (auto-promote to bignum)"
  , "  if (strcmp(op, \"+\") == 0) { int64_t r; if (__builtin_add_overflow(ia, ib, &r)) { return mi_sized_big(mi_bn_add(mi_bn_from_i64(ia), mi_bn_from_i64(ib)), 1); } return mi_int(r); }"
  , "  if (strcmp(op, \"-\") == 0) { int64_t r; if (__builtin_sub_overflow(ia, ib, &r)) { return mi_sized_big(mi_bn_sub(mi_bn_from_i64(ia), mi_bn_from_i64(ib)), 1); } return mi_int(r); }"
  , "  if (strcmp(op, \"*\") == 0) { int64_t r; if (__builtin_mul_overflow(ia, ib, &r)) { return mi_sized_big(mi_bn_mul(mi_bn_from_i64(ia), mi_bn_from_i64(ib)), 1); } return mi_int(r); }"
  , "  if (strcmp(op, \"/\") == 0) { if (ib==0) return mi_int(0); return mi_int(ia / ib); }"
  , "  if (strcmp(op, \"%\") == 0) { if (ib==0) return mi_int(0); return mi_int(ia % ib); }"
  , "  if (strcmp(op, \"**\") == 0) {"
  , "    MiBignum *base = mi_bn_from_i64(ia);"
  , "    MiBignum *r = mi_bn_from_i64(1);"
  , "    for (int64_t e = ib; e > 0; e--) r = mi_bn_mul(r, base);"
  , "    return mi_sized_big(r, 1);"
  , "  }"
  , "  if (strcmp(op, \"<\") == 0) return mi_int(ia < ib);"
  , "  if (strcmp(op, \">\") == 0) return mi_int(ia > ib);"
  , "  if (strcmp(op, \"<=\") == 0) return mi_int(ia <= ib);"
  , "  if (strcmp(op, \">=\") == 0) return mi_int(ia >= ib);"
  , "  fprintf(stderr, \"unknown operator: %s\\n\", op); exit(1);"
  , "}"
  , ""
  , "// ── Wrap params ──"
  , "static MiExpr *mi_wrap_lambda(int nparams, char **params, MiExpr *body) {"
  , "  MiExpr *e = body;"
  , "  for (int i = nparams - 1; i >= 0; i--) e = mi_expr_lam(params[i], e);"
  , "  return e;"
  , "}"
  , ""
  , "// ── Evaluator (with TCO trampoline) ──"
  , "static MiVal mi_eval(MiExpr *expr, MiEnv *env) {"
  , "  eval_top:;"
  , "  if (mi_in_eval && mi_gc_alloc_count > MI_GC_INTERVAL) mi_gc_collect(env);"
  , "  switch (expr->type) {"
  , "    case EXPR_INT:    return mi_int(expr->as.i);"
  , "    case EXPR_SIZED_INT: return mi_sized_int(expr->as.sized.i, expr->as.sized.width, expr->as.sized.is_signed);"
  , "    case EXPR_FLOAT:  return mi_float(expr->as.f);"
  , "    case EXPR_FLOAT32: return mi_float32((float)expr->as.f);"
  , "    case EXPR_STRING: return mi_string(expr->as.s);"
  , "    case EXPR_VAL:    return expr->as.val;"
  , "    case EXPR_NAME:   return mi_env_get_loc(env, expr->as.name, expr->loc);"
  , ""
  , "    case EXPR_BINOP: {"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = env;"
  , "      MiVal l = mi_force(mi_eval(expr->as.binop.left, env), env);"
  , "      if (mi_gc_root_count < MI_MAX_ROOTS) mi_gc_roots[mi_gc_root_count++] = &l;"
  , "      MiVal r = mi_force(mi_eval(expr->as.binop.right, env), env);"
  , "      if (mi_gc_root_count > 0) mi_gc_root_count--;"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      return mi_binop(expr->as.binop.op, l, r);"
  , "    }"
  , ""
  , "    case EXPR_APP: {"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = env;"
  , "      MiVal fn = mi_force(mi_eval(expr->as.app.fn, env), env);"
  , "      if (mi_gc_root_count < MI_MAX_ROOTS) mi_gc_roots[mi_gc_root_count++] = &fn;"
  , "      MiVal a = mi_eval(expr->as.app.arg, env);"
  , "      if (mi_gc_root_count > 0) mi_gc_root_count--;"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      if (fn.type == MI_CLOSURE) {"
  , "        MiEnv *call_env = mi_env_new(fn.as.closure.env);"
  , "        mi_env_set(call_env, fn.as.closure.param, a);"
  , "        expr = fn.as.closure.body; env = call_env; goto eval_top;"
  , "      }"
  , "      if (fn.type == MI_NATIVE) {"
  , "        MiVal result = fn.as.native.fn(a, fn.as.native.env);"
  , "        if (result.type == MI_CLOSURE && result.as.closure.param"
  , "            && strcmp(result.as.closure.param, \"_thunk_\") == 0) {"
  , "          expr = result.as.closure.body; env = result.as.closure.env; goto eval_top;"
  , "        }"
  , "        return result;"
  , "      }"
  , "      mi_error(expr->loc, \"apply on non-function\");"
  , "    }"
  , ""
  , "    case EXPR_LAM: {"
  , "      MiVal r; r.type = MI_CLOSURE;"
  , "      r.as.closure.body = expr->as.lam.body;"
  , "      r.as.closure.param = expr->as.lam.param;"
  , "      r.as.closure.env = env;"
  , "      return r;"
  , "    }"
  , ""
  , "    case EXPR_WITH: {"
  , "      MiEnv *inner = mi_env_new(env);"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = inner;"
  , "      for (int i = 0; i < expr->as.with.nbindings; i++) {"
  , "        MiBinding *b = &expr->as.with.bindings[i];"
  , "        MiExpr *body = b->nparams > 0 ? mi_wrap_lambda(b->nparams, b->params, b->body) : b->body;"
  , "        if (b->lazy) {"
  , "          MiVal thunk; thunk.type = MI_CLOSURE;"
  , "          thunk.as.closure.body = body; thunk.as.closure.param = \"_thunk_\";"
  , "          thunk.as.closure.env = inner;"
  , "          mi_env_set(inner, b->name, thunk);"
  , "        } else {"
  , "          mi_env_set(inner, b->name, mi_force(mi_eval(body, inner), inner));"
  , "        }"
  , "      }"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      expr = expr->as.with.body; env = inner; goto eval_top;"
  , "    }"
  , ""
  , "    case EXPR_RECORD: {"
  , "      int n = expr->as.record.nbindings;"
  , "      MiVal *fields = mi_alloc(n * sizeof(MiVal));"
  , "      const char **names = mi_alloc(n * sizeof(char*));"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = env;"
  , "      for (int i = 0; i < n; i++) {"
  , "        MiBinding *b = &expr->as.record.bindings[i];"
  , "        names[i] = b->name;"
  , "        MiExpr *body = b->nparams > 0 ? mi_wrap_lambda(b->nparams, b->params, b->body) : b->body;"
  , "        fields[i] = mi_eval(body, env);"
  , "      }"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      MiVal r; r.type = MI_RECORD;"
  , "      r.as.rec.tag = expr->as.record.tag; r.as.rec.names = names;"
  , "      r.as.rec.fields = fields; r.as.rec.nfields = n;"
  , "      return r;"
  , "    }"
  , ""
  , "    case EXPR_FIELD: {"
  , "      MiVal v = mi_eval(expr->as.field.expr, env);"
  , "      v = mi_force(v, env);"
  , "      return mi_field_loc(v, expr->as.field.field, expr->loc);"
  , "    }"
  , ""
  , "    case EXPR_NAMESPACE: {"
  , "      MiEnv *inner = mi_env_new(env);"
  , "      int n = expr->as.ns.nbindings;"
  , "      MiVal *vals = mi_alloc(n * sizeof(MiVal));"
  , "      const char **names = mi_alloc(n * sizeof(const char *));"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = inner;"
  , "      for (int i = 0; i < n; i++) {"
  , "        MiBinding *b = &expr->as.ns.bindings[i];"
  , "        MiExpr *body = b->nparams > 0 ? mi_wrap_lambda(b->nparams, b->params, b->body) : b->body;"
  , "        names[i] = b->name;"
  , "        if (b->lazy) {"
  , "          MiVal thunk; thunk.type = MI_CLOSURE;"
  , "          thunk.as.closure.body = body; thunk.as.closure.param = \"_thunk_\";"
  , "          thunk.as.closure.env = inner;"
  , "          mi_env_set(inner, b->name, thunk); vals[i] = thunk;"
  , "        } else {"
  , "          vals[i] = mi_force(mi_eval(body, inner), inner);"
  , "          mi_env_set(inner, b->name, vals[i]);"
  , "        }"
  , "      }"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      MiVal rec; rec.type = MI_RECORD;"
  , "      rec.as.rec.tag = \"_module_\";"
  , "      rec.as.rec.nfields = n;"
  , "      rec.as.rec.names = names;"
  , "      rec.as.rec.fields = vals;"
  , "      return rec;"
  , "    }"
  , ""
  , "    case EXPR_CASE: {"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = env;"
  , "      MiVal scrut = mi_eval(expr->as.cas.scrut, env);"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      scrut = mi_force(scrut, env);"
  , "      for (int i = 0; i < expr->as.cas.nalts; i++) {"
  , "        MiEnv *match_env = mi_env_new(env);"
  , "        if (mi_match(expr->as.cas.alts[i].pat, scrut, match_env)) {"
  , "          if (expr->as.cas.alts[i].guard) {"
  , "            MiVal gv = mi_eval(expr->as.cas.alts[i].guard, match_env);"
  , "            if (!mi_truthy(gv)) continue;"
  , "          }"
  , "          expr = expr->as.cas.alts[i].body; env = match_env; goto eval_top;"
  , "        }"
  , "      }"
  , "      mi_error(expr->loc, \"match: no matching pattern\");"
  , "    }"
  , ""
  , "    case EXPR_IF: {"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = env;"
  , "      MiVal cond = mi_force(mi_eval(expr->as.if_expr.cond, env), env);"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      if (mi_truthy(cond)) {"
  , "        expr = expr->as.if_expr.then_br; goto eval_top;"
  , "      } else {"
  , "        expr = expr->as.if_expr.else_br; goto eval_top;"
  , "      }"
  , "    }"
  , ""
  , "    case EXPR_FOR_RANGE: {"
  , "      MiEnv *loop_env = mi_env_new(env);"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = loop_env;"
  , "      MiVal acc = mi_force(mi_eval(expr->as.for_range.init, env), env);"
  , "      int64_t start = mi_force(mi_eval(expr->as.for_range.start, env), env).as.i;"
  , "      int64_t end = mi_force(mi_eval(expr->as.for_range.end, env), env).as.i;"
  , "      for (int64_t _i = start; _i < end; _i++) {"
  , "        mi_env_set(loop_env, expr->as.for_range.iter_var, mi_int(_i));"
  , "        mi_env_set(loop_env, expr->as.for_range.acc_var, acc);"
  , "        acc = mi_force(mi_eval(expr->as.for_range.body, loop_env), loop_env);"
  , "      }"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      return acc;"
  , "    }"
  , ""
  , "    case EXPR_TAIL_LOOP: {"
  , "      MiEnv *loop_env = mi_env_new(env);"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = loop_env;"
  , "      for (int _ti = 0; _ti < expr->as.tail_loop.nparams; _ti++) {"
  , "        MiVal init_val = mi_force(mi_eval(expr->as.tail_loop.inits[_ti], env), env);"
  , "        mi_env_set(loop_env, expr->as.tail_loop.params[_ti], init_val);"
  , "      }"
  , "      for (;;) {"
  , "        MiVal result = mi_eval(expr->as.tail_loop.body, loop_env);"
  , "        result = mi_force(result, loop_env);"
  , "        if (result.type != MI_TAIL_CALL) {"
  , "          if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "          return result;"
  , "        }"
  , "        for (int _ti = 0; _ti < result.as.tail_call.nargs; _ti++) {"
  , "          mi_env_set(loop_env, expr->as.tail_loop.params[_ti], result.as.tail_call.args[_ti]);"
  , "        }"
  , "        free(result.as.tail_call.args);"
  , "      }"
  , "    }"
  , ""
  , "    case EXPR_TAIL_CALL: {"
  , "      int _tn = expr->as.tail_call.nargs;"
  , "      MiVal *_targs = (MiVal*)malloc(_tn * sizeof(MiVal));"
  , "      if (mi_gc_env_root_count < MI_MAX_ROOTS) mi_gc_env_roots[mi_gc_env_root_count++] = env;"
  , "      for (int _ti = 0; _ti < _tn; _ti++) {"
  , "        _targs[_ti] = mi_force(mi_eval(expr->as.tail_call.args[_ti], env), env);"
  , "      }"
  , "      if (mi_gc_env_root_count > 0) mi_gc_env_root_count--;"
  , "      MiVal r; r.type = MI_TAIL_CALL;"
  , "      r.as.tail_call.args = _targs; r.as.tail_call.nargs = _tn;"
  , "      return r;"
  , "    }"
  , ""
  , "    case EXPR_NATIVE_BINOP: {"
  , "      MiVal _vl = mi_eval(expr->as.native_binop.left, env);"
  , "      MiVal _vr = mi_eval(expr->as.native_binop.right, env);"
  , "      const char *_nop = expr->as.native_binop.op;"
  , "      if (_vl.type == MI_INT && _vr.type == MI_INT) {"
  , "        int64_t _nl = _vl.as.i, _nr = _vr.as.i;"
  , "        if (_nop[0] == '+' && !_nop[1]) { int64_t _res; if (__builtin_add_overflow(_nl, _nr, &_res)) return mi_sized_big(mi_bn_add(mi_bn_from_i64(_nl), mi_bn_from_i64(_nr)), 1); return mi_int(_res); }"
  , "        if (_nop[0] == '-' && !_nop[1]) { int64_t _res; if (__builtin_sub_overflow(_nl, _nr, &_res)) return mi_sized_big(mi_bn_sub(mi_bn_from_i64(_nl), mi_bn_from_i64(_nr)), 1); return mi_int(_res); }"
  , "        if (_nop[0] == '*' && !_nop[1]) { int64_t _res; if (__builtin_mul_overflow(_nl, _nr, &_res)) return mi_sized_big(mi_bn_mul(mi_bn_from_i64(_nl), mi_bn_from_i64(_nr)), 1); return mi_int(_res); }"
  , "        if (_nop[0] == '/' && !_nop[1]) return mi_int(_nr == 0 ? 0 : _nl / _nr);"
  , "        if (_nop[0] == '%' && !_nop[1]) return mi_int(_nr == 0 ? 0 : _nl % _nr);"
  , "        if (_nop[0] == '<' && !_nop[1]) return mi_int(_nl < _nr);"
  , "        if (_nop[0] == '<' && _nop[1] == '=') return mi_int(_nl <= _nr);"
  , "        if (_nop[0] == '>' && !_nop[1]) return mi_int(_nl > _nr);"
  , "        if (_nop[0] == '>' && _nop[1] == '=') return mi_int(_nl >= _nr);"
  , "        if (_nop[0] == '=' && _nop[1] == '=') return mi_int(_nl == _nr);"
  , "        if (_nop[0] == '/' && _nop[1] == '=') return mi_int(_nl != _nr);"
  , "        if (_nop[0] == '*' && _nop[1] == '*') {"
  , "          MiBignum *_base = mi_bn_from_i64(_nl);"
  , "          MiBignum *_pres = mi_bn_from_i64(1);"
  , "          for (int64_t _pi = 0; _pi < _nr; _pi++) _pres = mi_bn_mul(_pres, _base);"
  , "          return mi_sized_big(_pres, 1);"
  , "        }"
  , "        return mi_binop(_nop, mi_int(_nl), mi_int(_nr));"
  , "      }"
  , "      if (_vl.type == MI_FLOAT && _vr.type == MI_FLOAT) {"
  , "        double _fl = _vl.as.f, _fr = _vr.as.f;"
  , "        if (_nop[0] == '+' && !_nop[1]) return mi_float(_fl + _fr);"
  , "        if (_nop[0] == '-' && !_nop[1]) return mi_float(_fl - _fr);"
  , "        if (_nop[0] == '*' && !_nop[1]) return mi_float(_fl * _fr);"
  , "        if (_nop[0] == '/' && !_nop[1]) return mi_float(_fr == 0.0 ? 0.0 : _fl / _fr);"
  , "        if (_nop[0] == '%' && !_nop[1]) return mi_float(_fr == 0.0 ? 0.0 : fmod(_fl, _fr));"
  , "        if (_nop[0] == '<' && !_nop[1]) return mi_int(_fl < _fr);"
  , "        if (_nop[0] == '<' && _nop[1] == '=') return mi_int(_fl <= _fr);"
  , "        if (_nop[0] == '>' && !_nop[1]) return mi_int(_fl > _fr);"
  , "        if (_nop[0] == '>' && _nop[1] == '=') return mi_int(_fl >= _fr);"
  , "        if (_nop[0] == '=' && _nop[1] == '=') return mi_int(_fl == _fr);"
  , "        if (_nop[0] == '/' && _nop[1] == '=') return mi_int(_fl != _fr);"
  , "        if (_nop[0] == '*' && _nop[1] == '*') return mi_float(pow(_fl, _fr));"
  , "      }"
  , "      if (_vl.type == MI_FLOAT32 && _vr.type == MI_FLOAT32) {"
  , "        float _fl = _vl.as.f32, _fr = _vr.as.f32;"
  , "        if (_nop[0] == '+' && !_nop[1]) return mi_float32(_fl + _fr);"
  , "        if (_nop[0] == '-' && !_nop[1]) return mi_float32(_fl - _fr);"
  , "        if (_nop[0] == '*' && !_nop[1]) return mi_float32(_fl * _fr);"
  , "        if (_nop[0] == '/' && !_nop[1]) return mi_float32(_fr == 0.0f ? 0.0f : _fl / _fr);"
  , "        if (_nop[0] == '%' && !_nop[1]) return mi_float32(_fr == 0.0f ? 0.0f : fmodf(_fl, _fr));"
  , "        if (_nop[0] == '<' && !_nop[1]) return mi_int(_fl < _fr);"
  , "        if (_nop[0] == '<' && _nop[1] == '=') return mi_int(_fl <= _fr);"
  , "        if (_nop[0] == '>' && !_nop[1]) return mi_int(_fl > _fr);"
  , "        if (_nop[0] == '>' && _nop[1] == '=') return mi_int(_fl >= _fr);"
  , "        if (_nop[0] == '=' && _nop[1] == '=') return mi_int(_fl == _fr);"
  , "        if (_nop[0] == '/' && _nop[1] == '=') return mi_int(_fl != _fr);"
  , "        if (_nop[0] == '*' && _nop[1] == '*') return mi_float32(powf(_fl, _fr));"
  , "      }"
  , "      return mi_binop(_nop, _vl, _vr);"
  , "    }"
  , ""
  , "    case EXPR_THUNK: {"
  , "      MiVal r; r.type = MI_CLOSURE;"
  , "      r.as.closure.body = expr->as.thunk.body;"
  , "      r.as.closure.param = \"_thunk_\";"
  , "      r.as.closure.env = env;"
  , "      return r;"
  , "    }"
  , "  }"
  , "  mi_errorf(expr->loc, \"eval: unknown expr type %d\", expr->type);"
  , "}"
  , ""
  , "// ── Built-ins ──"
  , "static int64_t mi_truthy(MiVal v) {"
  , "  switch (v.type) {"
  , "    case MI_INT:    return v.as.i != 0;"
  , "    case MI_SIZED_INT: return v.as.sized.is_big ? !mi_bn_is_zero(v.as.sized.big) : v.as.sized.i != 0;"
  , "    case MI_FLOAT:  return v.as.f != 0.0;"
  , "    case MI_FLOAT32: return v.as.f32 != 0.0f;"
  , "    case MI_STRING: return v.as.str.len != 0;"
  , "    case MI_RECORD:"
  , "      if (strcmp(v.as.rec.tag, \"False\") == 0 && v.as.rec.nfields == 0) return 0;"
  , "      if (strcmp(v.as.rec.tag, \"Nil\") == 0 && v.as.rec.nfields == 0) return 0;"
  , "      return 1;"
  , "    default: return 1;"
  , "  }"
  , "}"
  , "static MiVal mi_builtin_truthy(MiVal v, void *e) { (void)e; return mi_int(mi_truthy(v)); }"
  , "static MiVal mi_builtin_print(MiVal v, void *e) { (void)e; mi_print_val(v); return mi_int(0); }"
  , "static MiVal mi_builtin_println(MiVal v, void *e) { (void)e; mi_println_val(v); return mi_int(0); }"
  , ""
  , "// if: 3-arg curried"
  , "struct mi_if_env2 { MiVal cond; MiVal then_val; };"
  , "static MiVal mi_builtin_if3(MiVal else_val, void *env) {"
  , "  struct mi_if_env2 *e = (struct mi_if_env2 *)env;"
  , "  if (mi_truthy(e->cond)) return e->then_val;"
  , "  return else_val;"
  , "}"
  , "struct mi_if_env1 { MiVal cond; };"
  , "static MiVal mi_builtin_if2(MiVal then_val, void *env) {"
  , "  struct mi_if_env1 *e = (struct mi_if_env1 *)env;"
  , "  struct mi_if_env2 *e2 = mi_alloc(sizeof(struct mi_if_env2));"
  , "  e2->cond = e->cond; e2->then_val = then_val;"
  , "  return mi_native_env(mi_builtin_if3, e2);"
  , "}"
  , "static MiVal mi_builtin_if(MiVal cond, void *env) {"
  , "  (void)env; struct mi_if_env1 *e = mi_alloc(sizeof(struct mi_if_env1));"
  , "  e->cond = cond; return mi_native_env(mi_builtin_if2, e);"
  , "}"
  , ""
  , "// len"
  , "static MiVal mi_builtin_len(MiVal v, void *e) {"
  , "  (void)e;"
  , "  if (v.type == MI_STRING) return mi_int(v.as.str.len);"
  , "  if (v.type == MI_RECORD) {"
  , "    int64_t n = 0; MiVal cur = v;"
  , "    while (cur.type == MI_RECORD && strcmp(cur.as.rec.tag, \"Cons\") == 0) { n++; cur = cur.as.rec.fields[1]; }"
  , "    return mi_int(n);"
  , "  }"
  , "  return mi_int(0);"
  , "}"
  , ""
  , "// charAt"
  , "struct mi_charAt_env { MiVal str; };"
  , "static MiVal mi_builtin_charAt2(MiVal idx, void *env) {"
  , "  struct mi_charAt_env *e = (struct mi_charAt_env *)env;"
  , "  int i = (int)idx.as.i;"
  , "  if (i < 0 || i >= e->str.as.str.len) return mi_nothing();"
  , "  return mi_just(mi_stringn(e->str.as.str.data + i, 1));"
  , "}"
  , "static MiVal mi_builtin_charAt(MiVal str, void *env) {"
  , "  (void)env; struct mi_charAt_env *e = mi_alloc(sizeof(struct mi_charAt_env));"
  , "  e->str = str; return mi_native_env(mi_builtin_charAt2, e);"
  , "}"
  , ""
  , "// slice"
  , "struct mi_slice_env1 { MiVal val; };"
  , "struct mi_slice_env2 { MiVal val; int start; };"
  , "static MiVal mi_builtin_slice3(MiVal end_val, void *env) {"
  , "  struct mi_slice_env2 *e = (struct mi_slice_env2 *)env;"
  , "  int s = e->start;"
  , "  int end = (int)end_val.as.i;"
  , "  if (e->val.type == MI_STRING) {"
  , "    int n = e->val.as.str.len;"
  , "    if (s < 0) s = 0; if (end > n) end = n; if (s > end) s = end;"
  , "    return mi_stringn(e->val.as.str.data + s, end - s);"
  , "  }"
  , "  MiVal cur = e->val;"
  , "  for (int i = 0; i < s && cur.type == MI_RECORD && strcmp(cur.as.rec.tag, \"Cons\") == 0; i++) cur = cur.as.rec.fields[1];"
  , "  int count = end - s; if (count < 0) count = 0;"
  , "  MiVal result = mi_nil();"
  , "  for (int i = 0; i < count && cur.type == MI_RECORD && strcmp(cur.as.rec.tag, \"Cons\") == 0; i++) {"
  , "    result = mi_cons(cur.as.rec.fields[0], result); cur = cur.as.rec.fields[1];"
  , "  }"
  , "  MiVal rev = mi_nil();"
  , "  while (result.type == MI_RECORD && strcmp(result.as.rec.tag, \"Cons\") == 0) {"
  , "    rev = mi_cons(result.as.rec.fields[0], rev); result = result.as.rec.fields[1];"
  , "  }"
  , "  return rev;"
  , "}"
  , "static MiVal mi_builtin_slice2(MiVal start, void *env) {"
  , "  struct mi_slice_env1 *e1 = (struct mi_slice_env1 *)env;"
  , "  struct mi_slice_env2 *e2 = mi_alloc(sizeof(struct mi_slice_env2));"
  , "  e2->val = e1->val; e2->start = (int)start.as.i;"
  , "  return mi_native_env(mi_builtin_slice3, e2);"
  , "}"
  , "static MiVal mi_builtin_slice(MiVal val, void *env) {"
  , "  (void)env; struct mi_slice_env1 *e = mi_alloc(sizeof(struct mi_slice_env1));"
  , "  e->val = val; return mi_native_env(mi_builtin_slice2, e);"
  , "}"
  , ""
  , "// indexOf"
  , "struct mi_indexOf_env { MiVal haystack; };"
  , "static MiVal mi_builtin_indexOf2(MiVal needle, void *env) {"
  , "  struct mi_indexOf_env *e = (struct mi_indexOf_env *)env;"
  , "  char *p = strstr(e->haystack.as.str.data, needle.as.str.data);"
  , "  return mi_int(p ? (int64_t)(p - e->haystack.as.str.data) : -1);"
  , "}"
  , "static MiVal mi_builtin_indexOf(MiVal haystack, void *env) {"
  , "  (void)env; struct mi_indexOf_env *e = mi_alloc(sizeof(struct mi_indexOf_env));"
  , "  e->haystack = haystack; return mi_native_env(mi_builtin_indexOf2, e);"
  , "}"
  , ""
  , "// split"
  , "struct mi_split_env { MiVal str; };"
  , "static MiVal mi_builtin_split2(MiVal delim, void *env) {"
  , "  struct mi_split_env *e = (struct mi_split_env *)env;"
  , "  int dlen = delim.as.str.len;"
  , "  if (dlen == 0) {"
  , "    int n = e->str.as.str.len;"
  , "    MiVal lst = mi_nil();"
  , "    for (int i = n - 1; i >= 0; i--) lst = mi_cons(mi_stringn(e->str.as.str.data + i, 1), lst);"
  , "    return lst;"
  , "  }"
  , "  int cap = 16; int count = 0;"
  , "  MiVal *items = (MiVal*)malloc(cap * sizeof(MiVal));"
  , "  const char *p = e->str.as.str.data;"
  , "  const char *end = p + e->str.as.str.len;"
  , "  while (p <= end) {"
  , "    const char *found = strstr(p, delim.as.str.data);"
  , "    if (!found || found >= end) { found = end; }"
  , "    if (count >= cap) { cap *= 2; items = (MiVal*)realloc(items, cap * sizeof(MiVal)); }"
  , "    items[count++] = mi_stringn(p, (int)(found - p));"
  , "    if (found == end) break;"
  , "    p = found + dlen;"
  , "  }"
  , "  MiVal lst = mi_nil();"
  , "  for (int i = count - 1; i >= 0; i--) lst = mi_cons(items[i], lst);"
  , "  free(items);"
  , "  return lst;"
  , "}"
  , "static MiVal mi_builtin_split(MiVal str, void *env) {"
  , "  (void)env; struct mi_split_env *e = mi_alloc(sizeof(struct mi_split_env));"
  , "  e->str = str; return mi_native_env(mi_builtin_split2, e);"
  , "}"
  , ""
  , "// trim"
  , "static MiVal mi_builtin_trim(MiVal str, void *env) {"
  , "  (void)env;"
  , "  const char *s = str.as.str.data; int n = str.as.str.len;"
  , "  int start = 0; while (start < n && (s[start]==' '||s[start]=='\\t'||s[start]=='\\n'||s[start]=='\\r')) start++;"
  , "  int end = n; while (end > start && (s[end-1]==' '||s[end-1]=='\\t'||s[end-1]=='\\n'||s[end-1]=='\\r')) end--;"
  , "  return mi_stringn(s + start, end - start);"
  , "}"
  , ""
  , "// toUpper / toLower"
  , "static MiVal mi_builtin_toUpper(MiVal str, void *env) {"
  , "  (void)env; int n = str.as.str.len; char *buf = mi_alloc(n+1);"
  , "  for (int i = 0; i < n; i++) buf[i] = toupper((unsigned char)str.as.str.data[i]);"
  , "  buf[n] = '\\0';"
  , "  MiVal r; r.type = MI_STRING; r.as.str.data = buf; r.as.str.len = n; return r;"
  , "}"
  , "static MiVal mi_builtin_toLower(MiVal str, void *env) {"
  , "  (void)env; int n = str.as.str.len; char *buf = mi_alloc(n+1);"
  , "  for (int i = 0; i < n; i++) buf[i] = tolower((unsigned char)str.as.str.data[i]);"
  , "  buf[n] = '\\0';"
  , "  MiVal r; r.type = MI_STRING; r.as.str.data = buf; r.as.str.len = n; return r;"
  , "}"
  , ""
  , "// replace"
  , "struct mi_replace_env1 { MiVal str; };"
  , "struct mi_replace_env2 { MiVal str; MiVal old; };"
  , "static MiVal mi_builtin_replace3(MiVal new_str, void *env) {"
  , "  struct mi_replace_env2 *e = (struct mi_replace_env2 *)env;"
  , "  if (e->old.as.str.len == 0) return e->str;"
  , "  int count = 0;"
  , "  const char *p = e->str.as.str.data;"
  , "  while ((p = strstr(p, e->old.as.str.data)) != NULL) { count++; p += e->old.as.str.len; }"
  , "  if (count == 0) return e->str;"
  , "  int new_len = e->str.as.str.len + count * (new_str.as.str.len - e->old.as.str.len);"
  , "  char *buf = mi_alloc(new_len + 1); int pos = 0;"
  , "  p = e->str.as.str.data;"
  , "  while (*p) {"
  , "    const char *found = strstr(p, e->old.as.str.data);"
  , "    if (!found) { int rem = e->str.as.str.len - (int)(p - e->str.as.str.data); memcpy(buf+pos, p, rem); pos += rem; break; }"
  , "    int seg = (int)(found - p); memcpy(buf+pos, p, seg); pos += seg;"
  , "    memcpy(buf+pos, new_str.as.str.data, new_str.as.str.len); pos += new_str.as.str.len;"
  , "    p = found + e->old.as.str.len;"
  , "  }"
  , "  buf[new_len] = '\\0';"
  , "  MiVal r; r.type = MI_STRING; r.as.str.data = buf; r.as.str.len = new_len; return r;"
  , "}"
  , "static MiVal mi_builtin_replace2(MiVal old, void *env) {"
  , "  struct mi_replace_env1 *e1 = (struct mi_replace_env1 *)env;"
  , "  struct mi_replace_env2 *e2 = mi_alloc(sizeof(struct mi_replace_env2));"
  , "  e2->str = e1->str; e2->old = old;"
  , "  return mi_native_env(mi_builtin_replace3, e2);"
  , "}"
  , "static MiVal mi_builtin_replace(MiVal str, void *env) {"
  , "  (void)env; struct mi_replace_env1 *e = mi_alloc(sizeof(struct mi_replace_env1));"
  , "  e->str = str; return mi_native_env(mi_builtin_replace2, e);"
  , "}"
  , ""
  , "// toString"
  , "static void mi_format_val_buf(MiVal v, char **buf, int *len, int *cap);"
  , "static void mi_buf_append(char **buf, int *len, int *cap, const char *s) {"
  , "  int slen = strlen(s);"
  , "  while (*len + slen >= *cap) { *cap *= 2; *buf = realloc(*buf, *cap); }"
  , "  memcpy(*buf + *len, s, slen); *len += slen; (*buf)[*len] = 0;"
  , "}"
  , "static void mi_format_val_buf(MiVal v, char **buf, int *len, int *cap) {"
  , "  char tmp[64];"
  , "  v = mi_force(v, NULL);"
  , "  switch (v.type) {"
  , "    case MI_INT: snprintf(tmp, sizeof(tmp), \"%\" PRId64, v.as.i); mi_buf_append(buf,len,cap,tmp); break;"
  , "    case MI_SIZED_INT: if (v.as.sized.is_big) { int bsz = v.as.sized.big->len*10+16; char *t2 = (char*)mi_alloc(bsz); mi_bn_tostr(v.as.sized.big, t2, bsz); mi_buf_append(buf,len,cap,t2); } else { snprintf(tmp, sizeof(tmp), \"%\" PRId64, v.as.sized.i); mi_buf_append(buf,len,cap,tmp); } break;"
  , "    case MI_FLOAT: snprintf(tmp, sizeof(tmp), \"%g\", v.as.f); mi_buf_append(buf,len,cap,tmp); break;"
  , "    case MI_FLOAT32: snprintf(tmp, sizeof(tmp), \"%g\", (double)v.as.f32); mi_buf_append(buf,len,cap,tmp); break;"
  , "    case MI_STRING: {"
  , "      while (*len + v.as.str.len >= *cap) { *cap *= 2; *buf = realloc(*buf, *cap); }"
  , "      memcpy(*buf + *len, v.as.str.data, v.as.str.len); *len += v.as.str.len; (*buf)[*len] = 0;"
  , "      break; }"
  , "    case MI_RECORD:"
  , "      if (mi_is_cons_list(v)) {"
  , "        mi_buf_append(buf,len,cap,\"[\");"
  , "        int first = 1; MiVal cur = v;"
  , "        while (cur.type == MI_RECORD && strcmp(cur.as.rec.tag, \"Cons\") == 0) {"
  , "          if (!first) mi_buf_append(buf,len,cap,\", \");"
  , "          mi_format_val_buf(cur.as.rec.fields[0], buf, len, cap);"
  , "          cur = mi_force(cur.as.rec.fields[1], NULL); first = 0;"
  , "        }"
  , "        mi_buf_append(buf,len,cap,\"]\");"
  , "      } else {"
  , "        if (v.as.rec.tag[0]) { mi_buf_append(buf,len,cap,v.as.rec.tag); mi_buf_append(buf,len,cap,\" {\"); }"
  , "        else mi_buf_append(buf,len,cap,\"{\");"
  , "        for (int i = 0; i < v.as.rec.nfields; i++) {"
  , "          if (i > 0) mi_buf_append(buf,len,cap,\", \");"
  , "          if (v.as.rec.names) { mi_buf_append(buf,len,cap,v.as.rec.names[i]); mi_buf_append(buf,len,cap,\" = \"); }"
  , "          mi_format_val_buf(v.as.rec.fields[i], buf, len, cap);"
  , "        }"
  , "        mi_buf_append(buf,len,cap,\"}\");"
  , "      }"
  , "      break;"
  , "    case MI_CLOSURE: mi_buf_append(buf,len,cap,\"<closure>\"); break;"
  , "    case MI_NATIVE:  mi_buf_append(buf,len,cap,\"<closure>\"); break;"
  , "    case MI_POINTER: { char tmp[64]; snprintf(tmp, sizeof(tmp), \"<ptr:%p>\", v.as.ptr); mi_buf_append(buf,len,cap,tmp); break; }"
  , "    case MI_MANAGED: { char tmp[64]; snprintf(tmp, sizeof(tmp), \"<managed:%p>\", mi_raw_ptr(v)); mi_buf_append(buf,len,cap,tmp); break; }"
  , "  }"
  , "}"
  , "static MiVal mi_builtin_toString(MiVal v, void *env) {"
  , "  (void)env;"
  , "  int len = 0, cap = 256;"
  , "  char *buf = malloc(cap);"
  , "  buf[0] = 0;"
  , "  mi_format_val_buf(v, &buf, &len, &cap);"
  , "  MiVal result = mi_string(buf);"
  , "  free(buf);"
  , "  return result;"
  , "}"
  , ""
  , "// toInt"
  , "static MiVal mi_builtin_toInt(MiVal v, void *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_INT) return mi_just(v);"
  , "  if (v.type == MI_SIZED_INT) { if (v.as.sized.is_big) return mi_just(mi_int(mi_bn_to_i64(v.as.sized.big))); return mi_just(mi_int(v.as.sized.i)); }"
  , "  if (v.type == MI_FLOAT) return mi_just(mi_int((int64_t)v.as.f));"
  , "  if (v.type == MI_FLOAT32) return mi_just(mi_int((int64_t)v.as.f32));"
  , "  if (v.type == MI_STRING) {"
  , "    char *end; long long n = strtoll(v.as.str.data, &end, 10);"
  , "    if (end != v.as.str.data && *end == '\\0') return mi_just(mi_int(n));"
  , "    return mi_nothing();"
  , "  }"
  , "  return mi_nothing();"
  , "}"
  , ""
  , "// toFloat"
  , "static MiVal mi_builtin_toFloat(MiVal v, void *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_FLOAT) return mi_just(v);"
  , "  if (v.type == MI_FLOAT32) return mi_just(mi_float((double)v.as.f32));"
  , "  if (v.type == MI_INT) return mi_just(mi_float((double)v.as.i));"
  , "  if (v.type == MI_STRING) {"
  , "    char *end; double n = strtod(v.as.str.data, &end);"
  , "    if (end != v.as.str.data && *end == '\\0') return mi_just(mi_float(n));"
  , "    return mi_nothing();"
  , "  }"
  , "  return mi_nothing();"
  , "}"
  , ""
  , "// float / round / floor / ceil"
  , "static MiVal mi_builtin_float(MiVal v, void *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_FLOAT) return v;"
  , "  if (v.type == MI_FLOAT32) return mi_float((double)v.as.f32);"
  , "  if (v.type == MI_SIZED_INT) return mi_float(v.as.sized.is_big ? mi_bn_to_double(v.as.sized.big) : (double)v.as.sized.i);"
  , "  return mi_float((double)v.as.i);"
  , "}"
  , "static MiVal mi_builtin_round(MiVal v, void *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_INT || v.type == MI_SIZED_INT) return v;"
  , "  double d = v.as.f; return mi_int((int64_t)(d < 0 ? d - 0.5 : d + 0.5));"
  , "}"
  , "static MiVal mi_builtin_floor(MiVal v, void *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_INT || v.type == MI_SIZED_INT) return v;"
  , "  double d = v.as.f; return mi_int((int64_t)(d >= 0 ? d : d - 1));"
  , "}"
  , "static MiVal mi_builtin_ceil(MiVal v, void *env) {"
  , "  (void)env;"
  , "  if (v.type == MI_INT || v.type == MI_SIZED_INT) return v;"
  , "  double d = v.as.f; int64_t i = (int64_t)d; return mi_int(d > (double)i ? i + 1 : i);"
  , "}"
  , ""
  , "// ── Sized integer runtime builtins ──"
  , "struct mi_sized_env { int width; };"
  , "static MiVal mi_builtin_sized_int2(MiVal val, void *env) {"
  , "  struct mi_sized_env *e = (struct mi_sized_env *)env;"
  , "  int64_t v = (val.type == MI_SIZED_INT) ? (val.as.sized.is_big ? mi_bn_to_i64(val.as.sized.big) : val.as.sized.i) : val.as.i;"
  , "  return mi_sized_int(v, e->width, 1);"
  , "}"
  , "static MiVal mi_builtin_sized_int(MiVal width, void *env) {"
  , "  (void)env; struct mi_sized_env *e = mi_alloc(sizeof(struct mi_sized_env));"
  , "  e->width = (int)width.as.i; return mi_native_env(mi_builtin_sized_int2, e);"
  , "}"
  , "static MiVal mi_builtin_sized_uint2(MiVal val, void *env) {"
  , "  struct mi_sized_env *e = (struct mi_sized_env *)env;"
  , "  int64_t v = (val.type == MI_SIZED_INT) ? (val.as.sized.is_big ? mi_bn_to_i64(val.as.sized.big) : val.as.sized.i) : val.as.i;"
  , "  return mi_sized_int(v, e->width, 0);"
  , "}"
  , "static MiVal mi_builtin_sized_uint(MiVal width, void *env) {"
  , "  (void)env; struct mi_sized_env *e = mi_alloc(sizeof(struct mi_sized_env));"
  , "  e->width = (int)width.as.i; return mi_native_env(mi_builtin_sized_uint2, e);"
  , "}"
  , ""
  , "// ── Record introspection ──"
  , "static MiVal mi_builtin_fields(MiVal v, void *env) {"
  , "  (void)env; v = mi_force(v, NULL);"
  , "  if (v.type != MI_RECORD) return mi_nil();"
  , "  MiVal lst = mi_nil();"
  , "  for (int i = v.as.rec.nfields - 1; i >= 0; i--) lst = mi_cons(v.as.rec.fields[i], lst);"
  , "  return lst;"
  , "}"
  , "static MiVal mi_builtin_fieldNames(MiVal v, void *env) {"
  , "  (void)env; v = mi_force(v, NULL);"
  , "  if (v.type != MI_RECORD) return mi_nil();"
  , "  MiVal lst = mi_nil();"
  , "  for (int i = v.as.rec.nfields - 1; i >= 0; i--) lst = mi_cons(mi_string(v.as.rec.names[i]), lst);"
  , "  return lst;"
  , "}"
  , "static MiVal mi_builtin_tag(MiVal v, void *env) {"
  , "  (void)env; v = mi_force(v, NULL);"
  , "  if (v.type != MI_RECORD) return mi_string(\"\");"
  , "  return mi_string(v.as.rec.tag);"
  , "}"
  , "struct mi_getField_env { MiVal rec; };"
  , "static MiVal mi_builtin_getField2(MiVal name, void *env) {"
  , "  struct mi_getField_env *e = (struct mi_getField_env *)env;"
  , "  if (name.type != MI_STRING) return mi_nothing();"
  , "  for (int i = 0; i < e->rec.as.rec.nfields; i++) {"
  , "    if (strcmp(e->rec.as.rec.names[i], name.as.str.data) == 0) return mi_just(e->rec.as.rec.fields[i]);"
  , "  }"
  , "  return mi_nothing();"
  , "}"
  , "static MiVal mi_builtin_getField2_notfound(MiVal name, void *env) {"
  , "  (void)name; (void)env; return mi_nothing();"
  , "}"
  , "static MiVal mi_builtin_getField(MiVal rec, void *env) {"
  , "  (void)env; rec = mi_force(rec, NULL);"
  , "  if (rec.type != MI_RECORD) return mi_native_env(mi_builtin_getField2_notfound, NULL);"
  , "  struct mi_getField_env *e = mi_alloc(sizeof(struct mi_getField_env));"
  , "  e->rec = rec; return mi_native_env(mi_builtin_getField2, e);"
  , "}"
  , "struct mi_setField_env { MiVal rec; };"
  , "struct mi_setField_env2 { MiVal rec; const char *name; };"
  , "static MiVal mi_builtin_setField3(MiVal val, void *env) {"
  , "  struct mi_setField_env2 *e = (struct mi_setField_env2 *)env;"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = e->rec.as.rec.tag;"
  , "  int n = e->rec.as.rec.nfields;"
  , "  int found = 0;"
  , "  for (int i = 0; i < n; i++) if (strcmp(e->rec.as.rec.names[i], e->name) == 0) found = 1;"
  , "  int nn = found ? n : n + 1;"
  , "  r.as.rec.nfields = nn;"
  , "  r.as.rec.names = mi_alloc(sizeof(const char*) * nn);"
  , "  r.as.rec.fields = mi_alloc(sizeof(MiVal) * nn);"
  , "  for (int i = 0; i < n; i++) {"
  , "    r.as.rec.names[i] = e->rec.as.rec.names[i];"
  , "    r.as.rec.fields[i] = (strcmp(e->rec.as.rec.names[i], e->name) == 0) ? val : e->rec.as.rec.fields[i];"
  , "  }"
  , "  if (!found) { r.as.rec.names[n] = e->name; r.as.rec.fields[n] = val; }"
  , "  return r;"
  , "}"
  , "static MiVal mi_builtin_setField2(MiVal name, void *env) {"
  , "  struct mi_setField_env *e = (struct mi_setField_env *)env;"
  , "  if (name.type != MI_STRING) return e->rec;"
  , "  struct mi_setField_env2 *e2 = mi_alloc(sizeof(struct mi_setField_env2));"
  , "  e2->rec = e->rec; e2->name = name.as.str.data;"
  , "  return mi_native_env(mi_builtin_setField3, e2);"
  , "}"
  , "static MiVal mi_builtin_setField(MiVal rec, void *env) {"
  , "  (void)env; rec = mi_force(rec, NULL);"
  , "  if (rec.type != MI_RECORD) return rec;"
  , "  struct mi_setField_env *e = mi_alloc(sizeof(struct mi_setField_env));"
  , "  e->rec = rec; return mi_native_env(mi_builtin_setField2, e);"
  , "}"
  , ""
  , "// ── gc_manage builtin: wraps pointer + native free → MI_MANAGED ──"
  , "static MiVal mi_builtin_gc_manage2(MiVal free_fn, void *env) {"
  , "  MiVal ptr_val = *(MiVal*)env;"
  , "  MiGcManaged *m = (MiGcManaged*)malloc(sizeof(MiGcManaged));"
  , "  m->ptr = mi_maybe_ptr(ptr_val); m->c_finalizer = NULL; m->native_fn = free_fn;"
  , "  m->gc_mark = 0; m->gc_next = mi_gc_managed_list; mi_gc_managed_list = m;"
  , "  MiVal r; r.type = MI_MANAGED; r.as.ptr = m; return r;"
  , "}"
  , "static MiVal mi_builtin_gc_manage(MiVal ptr, void *env) {"
  , "  (void)env; MiVal *stored = mi_alloc(sizeof(MiVal)); *stored = ptr;"
  , "  return mi_native_env(mi_builtin_gc_manage2, stored);"
  , "}"
  , ""
  , "// ── World / IO ──"
  , "static MiVal mi_ok(MiVal v) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"Ok\";"
  , "  r.as.rec.nfields = 1;"
  , "  r.as.rec.names = mi_alloc(sizeof(const char*)); r.as.rec.names[0] = \"value\";"
  , "  r.as.rec.fields = mi_alloc(sizeof(MiVal)); r.as.rec.fields[0] = v;"
  , "  return r;"
  , "}"
  , "static MiVal mi_err(const char *msg) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"Err\";"
  , "  r.as.rec.nfields = 1;"
  , "  r.as.rec.names = mi_alloc(sizeof(const char*)); r.as.rec.names[0] = \"msg\";"
  , "  r.as.rec.fields = mi_alloc(sizeof(MiVal)); r.as.rec.fields[0] = mi_string(msg);"
  , "  return r;"
  , "}"
  , "static MiVal mi_none(void) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = \"None\";"
  , "  r.as.rec.nfields = 0; r.as.rec.names = NULL; r.as.rec.fields = NULL;"
  , "  return r;"
  , "}"
  , ""
  , "static MiVal mi_world_readFile(MiVal path, void *env) {"
  , "  (void)env;"
  , "  FILE *f = fopen(path.as.str.data, \"r\");"
  , "  if (!f) {"
  , "    char buf[512]; snprintf(buf, sizeof(buf), \"Cannot open file: %s\", path.as.str.data);"
  , "    return mi_err(buf);"
  , "  }"
  , "  fseek(f, 0, SEEK_END); long len = ftell(f); fseek(f, 0, SEEK_SET);"
  , "  char *contents = mi_alloc(len + 1);"
  , "  long nread = fread(contents, 1, len, f); contents[nread] = '\\0'; fclose(f);"
  , "  return mi_ok(mi_stringn(contents, (int)nread));"
  , "}"
  , ""
  , "struct mi_wf_env { MiVal path; };"
  , "static MiVal mi_world_writeFile2(MiVal contents, void *env) {"
  , "  struct mi_wf_env *e = (struct mi_wf_env *)env;"
  , "  FILE *f = fopen(e->path.as.str.data, \"w\");"
  , "  if (!f) {"
  , "    char buf[512]; snprintf(buf, sizeof(buf), \"Cannot write file: %s\", e->path.as.str.data);"
  , "    return mi_err(buf);"
  , "  }"
  , "  fwrite(contents.as.str.data, 1, contents.as.str.len, f); fclose(f);"
  , "  return mi_ok(mi_int(0));"
  , "}"
  , "static MiVal mi_world_writeFile(MiVal path, void *env) {"
  , "  (void)env; struct mi_wf_env *e = mi_alloc(sizeof(struct mi_wf_env));"
  , "  e->path = path; return mi_native_env(mi_world_writeFile2, e);"
  , "}"
  , ""
  , "static MiVal mi_world_appendFile2(MiVal contents, void *env) {"
  , "  struct mi_wf_env *e = (struct mi_wf_env *)env;"
  , "  FILE *f = fopen(e->path.as.str.data, \"a\");"
  , "  if (!f) {"
  , "    char buf[512]; snprintf(buf, sizeof(buf), \"Cannot append file: %s\", e->path.as.str.data);"
  , "    return mi_err(buf);"
  , "  }"
  , "  fwrite(contents.as.str.data, 1, contents.as.str.len, f); fclose(f);"
  , "  return mi_ok(mi_int(0));"
  , "}"
  , "static MiVal mi_world_appendFile(MiVal path, void *env) {"
  , "  (void)env; struct mi_wf_env *e = mi_alloc(sizeof(struct mi_wf_env));"
  , "  e->path = path; return mi_native_env(mi_world_appendFile2, e);"
  , "}"
  , ""
  , "static MiVal mi_world_exists(MiVal path, void *env) {"
  , "  (void)env; FILE *f = fopen(path.as.str.data, \"r\");"
  , "  if (f) { fclose(f); return mi_int(1); }"
  , "  return mi_int(0);"
  , "}"
  , ""
  , "static MiVal mi_world_remove(MiVal path, void *env) {"
  , "  (void)env;"
  , "  if (remove(path.as.str.data) == 0) return mi_ok(mi_int(0));"
  , "  char buf[512]; snprintf(buf, sizeof(buf), \"Cannot remove: %s\", path.as.str.data);"
  , "  return mi_err(buf);"
  , "}"
  , ""
  , "static MiVal mi_world_readLine(MiVal prompt, void *env) {"
  , "  (void)env;"
  , "  if (prompt.type == MI_STRING && prompt.as.str.len > 0) {"
  , "    printf(\"%.*s\", prompt.as.str.len, prompt.as.str.data); fflush(stdout);"
  , "  }"
  , "  char buf[4096];"
  , "  if (fgets(buf, sizeof(buf), stdin)) {"
  , "    size_t len = strlen(buf);"
  , "    if (len > 0 && buf[len-1] == '\\n') buf[len-1] = '\\0';"
  , "    return mi_string(buf);"
  , "  }"
  , "  return mi_string(\"\");"
  , "}"
  , ""
  , "static MiVal mi_world_getEnv(MiVal name, void *env) {"
  , "  (void)env;"
  , "  const char *val = getenv(name.as.str.data);"
  , "  if (val) return mi_ok(mi_string(val));"
  , "  return mi_none();"
  , "}"
  , ""
  , "struct mi_exec_env { MiVal cmd; };"
  , "static MiVal mi_world_exec2(MiVal args, void *env) {"
  , "  struct mi_exec_env *e = (struct mi_exec_env *)env;"
  , "  char cmdline[8192];"
  , "  int pos = snprintf(cmdline, sizeof(cmdline), \"%s\", e->cmd.as.str.data);"
  , "  MiVal cur = args;"
  , "  while (cur.type == MI_RECORD && strcmp(cur.as.rec.tag, \"Cons\") == 0 && pos < (int)sizeof(cmdline)-1) {"
  , "    MiVal a = cur.as.rec.fields[0];"
  , "    pos += snprintf(cmdline + pos, sizeof(cmdline) - pos, \" %s\", a.as.str.data);"
  , "    cur = cur.as.rec.fields[1];"
  , "  }"
  , "  FILE *p = popen(cmdline, \"r\");"
  , "  if (!p) return mi_err(\"Failed to execute command\");"
  , "  char *output = mi_alloc(65536); size_t total = 0; size_t n;"
  , "  while ((n = fread(output + total, 1, 65536 - total - 1, p)) > 0) total += n;"
  , "  output[total] = '\\0';"
  , "  int status = pclose(p);"
  , "  if (status != 0) {"
  , "    char buf[512]; snprintf(buf, sizeof(buf), \"Command exited with status %d\", status);"
  , "    return mi_err(buf);"
  , "  }"
  , "  return mi_ok(mi_stringn(output, (int)total));"
  , "}"
  , "static MiVal mi_world_exec(MiVal cmd, void *env) {"
  , "  (void)env; struct mi_exec_env *e = mi_alloc(sizeof(struct mi_exec_env));"
  , "  e->cmd = cmd; return mi_native_env(mi_world_exec2, e);"
  , "}"
  , ""
  , "static MiVal mi_world_exit(MiVal code, void *env) {"
  , "  (void)env; exit((int)code.as.i);"
  , "  return mi_int(0);"
  , "}"
  , ""
  , "static MiVal mi_make_record(const char *tag, int n, const char **names, MiVal *fields) {"
  , "  MiVal r; r.type = MI_RECORD; r.as.rec.tag = tag;"
  , "  r.as.rec.nfields = n; r.as.rec.names = names; r.as.rec.fields = fields;"
  , "  return r;"
  , "}"
  , ""
  , "static MiVal mi_build_world(int argc, char **argv) {"
  , "  MiVal argvList = mi_nil();"
  , "  for (int i = argc - 1; i >= 0; i--) argvList = mi_cons(mi_string(argv[i]), argvList);"
  , ""
  , "  const int io_n = 3;"
  , "  const char **io_names = mi_alloc(io_n * sizeof(const char*));"
  , "  MiVal *io_fields = mi_alloc(io_n * sizeof(MiVal));"
  , "  io_names[0] = \"println\";  io_fields[0] = mi_native(mi_builtin_println);"
  , "  io_names[1] = \"print\";    io_fields[1] = mi_native(mi_builtin_print);"
  , "  io_names[2] = \"readLine\"; io_fields[2] = mi_native(mi_world_readLine);"
  , "  MiVal io_rec = mi_make_record(\"_io_\", io_n, io_names, io_fields);"
  , ""
  , "  const int fsr_n = 2;"
  , "  const char **fsr_names = mi_alloc(fsr_n * sizeof(const char*));"
  , "  MiVal *fsr_fields = mi_alloc(fsr_n * sizeof(MiVal));"
  , "  fsr_names[0] = \"file\";   fsr_fields[0] = mi_native(mi_world_readFile);"
  , "  fsr_names[1] = \"exists\"; fsr_fields[1] = mi_native(mi_world_exists);"
  , "  MiVal fsr_rec = mi_make_record(\"_fs_read_\", fsr_n, fsr_names, fsr_fields);"
  , ""
  , "  const int fsw_n = 3;"
  , "  const char **fsw_names = mi_alloc(fsw_n * sizeof(const char*));"
  , "  MiVal *fsw_fields = mi_alloc(fsw_n * sizeof(MiVal));"
  , "  fsw_names[0] = \"file\";   fsw_fields[0] = mi_native(mi_world_writeFile);"
  , "  fsw_names[1] = \"append\"; fsw_fields[1] = mi_native(mi_world_appendFile);"
  , "  fsw_names[2] = \"remove\"; fsw_fields[2] = mi_native(mi_world_remove);"
  , "  MiVal fsw_rec = mi_make_record(\"_fs_write_\", fsw_n, fsw_names, fsw_fields);"
  , ""
  , "  const int fs_n = 2;"
  , "  const char **fs_names = mi_alloc(fs_n * sizeof(const char*));"
  , "  MiVal *fs_fields = mi_alloc(fs_n * sizeof(MiVal));"
  , "  fs_names[0] = \"read\";  fs_fields[0] = fsr_rec;"
  , "  fs_names[1] = \"write\"; fs_fields[1] = fsw_rec;"
  , "  MiVal fs_rec = mi_make_record(\"_fs_\", fs_n, fs_names, fs_fields);"
  , ""
  , "  const int proc_n = 2;"
  , "  const char **proc_names = mi_alloc(proc_n * sizeof(const char*));"
  , "  MiVal *proc_fields = mi_alloc(proc_n * sizeof(MiVal));"
  , "  proc_names[0] = \"exec\"; proc_fields[0] = mi_native(mi_world_exec);"
  , "  proc_names[1] = \"exit\"; proc_fields[1] = mi_native(mi_world_exit);"
  , "  MiVal proc_rec = mi_make_record(\"_process_\", proc_n, proc_names, proc_fields);"
  , ""
  , "  const int world_n = 5;"
  , "  const char **world_names = mi_alloc(world_n * sizeof(const char*));"
  , "  MiVal *world_fields = mi_alloc(world_n * sizeof(MiVal));"
  , "  world_names[0] = \"argv\";    world_fields[0] = argvList;"
  , "  world_names[1] = \"getEnv\";  world_fields[1] = mi_native(mi_world_getEnv);"
  , "  world_names[2] = \"io\";      world_fields[2] = io_rec;"
  , "  world_names[3] = \"fs\";      world_fields[3] = fs_rec;"
  , "  world_names[4] = \"process\"; world_fields[4] = proc_rec;"
  , "  return mi_make_record(\"_world_\", world_n, world_names, world_fields);"
  , "}"
  , ""
  ]
