{-# LANGUAGE OverloadedStrings #-}
module Milang.Codegen (codegen) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.List (nub, intercalate)
import Milang.Syntax
import System.IO (Handle, hPutStr, hPutStrLn)

-- | Codegen state: counter for unique closure IDs + accumulated top-level defs + includes
data CGState = CGState
  { cgNextId   :: IORef Int
  , cgTopDefs  :: IORef [String]  -- closure function defs (reversed)
  , cgIncludes :: IORef [String]  -- extra #include lines
  }

newCGState :: IO CGState
newCGState = CGState <$> newIORef 0 <*> newIORef [] <*> newIORef []

freshId :: CGState -> IO Int
freshId st = do
  n <- readIORef (cgNextId st)
  writeIORef (cgNextId st) (n + 1)
  pure n

-- Append a top-level C definition (closure function + optional env struct)
addTopDef :: CGState -> String -> IO ()
addTopDef st s = modifyIORef (cgTopDefs st) (s :)

-- | Generate C code from a (partially reduced) Expr and write to handle
codegen :: Handle -> Expr -> IO ()
codegen h expr = do
  st <- newCGState
  mainCode <- captureIO st h expr
  emitPreamble h
  -- Emit extra #includes for C headers
  incs <- readIORef (cgIncludes st)
  mapM_ (hPutStrLn h) (nub incs)
  hPutStrLn h ""
  defs <- readIORef (cgTopDefs st)
  mapM_ (hPutStr h) (reverse defs)
  hPutStrLn h ""
  hPutStr h mainCode

-- | Capture the main() code as a string while accumulating closure defs
captureIO :: CGState -> Handle -> Expr -> IO String
captureIO st _h expr = do
  ref <- newIORef ""
  let emit s = modifyIORef ref (++ s)
  case expr of
    Namespace bs -> do
      emit "int main(void) {\n"
      emit "  MiVal mi_print = mi_closure(mi_builtin_print, NULL);\n"
      emit "  MiVal mi_println = mi_closure(mi_builtin_println, NULL);\n"
      emit "  MiVal mi_if = mi_closure(mi_builtin_if1, NULL);\n"
      emit "  MiVal mi_len = mi_closure(mi_builtin_len, NULL);\n"
      emit "  MiVal mi_get = mi_closure(mi_builtin_get1, NULL);\n"
      emit "  MiVal mi_push = mi_closure(mi_builtin_push1, NULL);\n"
      emit "  MiVal mi_concat = mi_closure(mi_builtin_concat1, NULL);\n"
      emit "  MiVal mi_map = mi_closure(mi_builtin_map1, NULL);\n"
      emit "  MiVal mi_fold = mi_closure(mi_builtin_fold1, NULL);\n"
      emit "  MiVal mi_filter = mi_closure(mi_builtin_filter1, NULL);\n\n"
      mapM_ (\b -> do
        let cname = sanitize (bindName b)
        code <- exprToC st (bindBody b)
        emit $ "  MiVal " ++ cname ++ " = " ++ code ++ ";\n"
        ) bs
      emit "\n"
      mapM_ (\b -> do
        let cname = sanitize (bindName b)
            name  = T.unpack (bindName b)
        emit $ "  printf(\"" ++ name ++ " = \"); mi_print_val(" ++ cname ++ "); printf(\"\\n\");\n"
        ) bs
      emit "  return 0;\n}\n"
    _ -> do
      emit "int main(void) {\n"
      emit "  MiVal mi_print = mi_closure(mi_builtin_print, NULL);\n"
      emit "  MiVal mi_println = mi_closure(mi_builtin_println, NULL);\n"
      emit "  MiVal mi_if = mi_closure(mi_builtin_if1, NULL);\n"
      emit "  MiVal mi_len = mi_closure(mi_builtin_len, NULL);\n"
      emit "  MiVal mi_get = mi_closure(mi_builtin_get1, NULL);\n"
      emit "  MiVal mi_push = mi_closure(mi_builtin_push1, NULL);\n"
      emit "  MiVal mi_concat = mi_closure(mi_builtin_concat1, NULL);\n"
      emit "  MiVal mi_map = mi_closure(mi_builtin_map1, NULL);\n"
      emit "  MiVal mi_fold = mi_closure(mi_builtin_fold1, NULL);\n"
      emit "  MiVal mi_filter = mi_closure(mi_builtin_filter1, NULL);\n\n"
      code <- exprToC st expr
      emit $ "  mi_print_val(" ++ code ++ "); printf(\"\\n\");\n"
      emit "  return 0;\n}\n"
  readIORef ref

-- | Convert an Expr to a C expression string, accumulating closure defs in state
exprToC :: CGState -> Expr -> IO String
exprToC _ (IntLit n) = pure $ "mi_int(" ++ show n ++ ")"
exprToC _ (FloatLit d) = pure $ "mi_float(" ++ show d ++ ")"
exprToC _ (StringLit s) = pure $ "mi_string(" ++ show (T.unpack s) ++ ")"
exprToC _ (Name n) = pure $ sanitize n

exprToC st (BinOp op l r) = do
  lc <- exprToC st l
  rc <- exprToC st r
  pure $ opFunc op ++ "(" ++ lc ++ ", " ++ rc ++ ")"

exprToC st (App f x) = do
  fc <- exprToC st f
  xc <- exprToC st x
  pure $ "mi_apply(" ++ fc ++ ", " ++ xc ++ ")"

exprToC st (Lam param body) = do
  cid <- freshId st
  let fnName = "mi_fn_" ++ show cid
      fvs = nub $ freeVars body [param]
  -- Recursively generate body expression (may create nested closures)
  bodyCode <- exprToC st body
  -- Generate env struct + function
  let envStructName = fnName ++ "_env"
  let envDecl = if null fvs then ""
        else "struct " ++ envStructName ++ " {\n" ++
             concatMap (\v -> "  MiVal " ++ sanitize v ++ ";\n") fvs ++
             "};\n\n"
  let envCast = if null fvs then ""
        else "  struct " ++ envStructName ++ " *_env = (struct " ++ envStructName ++ " *)__env;\n"
  let envUnpack = if null fvs then ""
        else concatMap (\v -> "  MiVal " ++ sanitize v ++ " = _env->" ++ sanitize v ++ ";\n") fvs
  let fnDef = envDecl ++
        "static MiVal " ++ fnName ++ "(MiVal " ++ sanitize param ++ ", void *__env) {\n" ++
        envCast ++ envUnpack ++
        "  return " ++ bodyCode ++ ";\n" ++
        "}\n\n"
  addTopDef st fnDef
  -- Emit closure construction
  if null fvs
    then pure $ "mi_closure(" ++ fnName ++ ", NULL)"
    else do
      let allocEnv = "({ struct " ++ envStructName ++ " *_e = malloc(sizeof(struct " ++ envStructName ++ "));\n" ++
            concatMap (\v -> "    _e->" ++ sanitize v ++ " = " ++ sanitize v ++ ";\n") fvs ++
            "    mi_closure(" ++ fnName ++ ", _e); })"
      pure allocEnv

exprToC st (With body bs) = do
  bindCode <- mapM (\b -> do
    code <- exprToC st (bindBody b)
    pure $ "    MiVal " ++ sanitize (bindName b) ++ " = " ++ code ++ ";\n"
    ) bs
  bodyCode <- exprToC st body
  pure $ "({\n" ++ concat bindCode ++ "    " ++ bodyCode ++ ";\n  })"

exprToC st (Record tag bs) = do
  let n = length bs
      names = map (T.unpack . bindName) bs
  fieldCodes <- mapM (\(i, b) -> do
    code <- exprToC st (bindBody b)
    pure $ "    _fields[" ++ show i ++ "] = " ++ code ++ ";\n"
    ) (zip [0::Int ..] bs)
  pure $ "({\n    MiVal *_fields = malloc(" ++ show n ++ " * sizeof(MiVal));\n" ++
    "    static const char *_names[] = {" ++
    concatMap (\nm -> "\"" ++ nm ++ "\", ") names ++ "};\n" ++
    concat fieldCodes ++
    "    MiVal _r; _r.type = MI_RECORD; _r.as.rec.tag = \"" ++ T.unpack tag ++
    "\"; _r.as.rec.names = _names; _r.as.rec.fields = _fields; _r.as.rec.nfields = " ++
    show n ++ "; _r;\n  })"

exprToC st (FieldAccess e field) = do
  ec <- exprToC st e
  pure $ "mi_field(" ++ ec ++ ", \"" ++ T.unpack field ++ "\")"

exprToC st (Namespace bs) = do
  bindCode <- mapM (\b -> do
    code <- exprToC st (bindBody b)
    pure $ "    MiVal " ++ sanitize (bindName b) ++ " = " ++ code ++ ";\n"
    ) bs
  let lastRef = case bs of
        [] -> "    mi_int(0);\n"
        _  -> "    " ++ sanitize (bindName (last bs)) ++ ";\n"
  pure $ "({\n" ++ concat bindCode ++ lastRef ++ "  })"

exprToC st (Case scrut alts) = do
  scrutCode <- exprToC st scrut
  altCode <- altsToC st alts
  pure $ "({\n    MiVal _scrut = " ++ scrutCode ++ ";\n" ++
    "    MiVal _result;\n" ++ altCode ++ "    _result;\n  })"

exprToC st (CFunction hdr cname retTy paramTys) = do
  -- Register the #include
  modifyIORef (cgIncludes st) (("#include " ++ show (T.unpack hdr)) :)
  -- Generate curried wrapper closures
  cfunctionToC st (T.unpack cname) retTy paramTys

-- Thunk: deferred expression, compiled as a zero-arg closure
exprToC st (Thunk body) = exprToC st (Lam "_thunk_" body)

-- List literal: [a, b, c]
exprToC st (ListLit es) = do
  let n = length es
  elemCodes <- mapM (\(i, e) -> do
    code <- exprToC st e
    pure $ "    _items[" ++ show i ++ "] = " ++ code ++ ";\n"
    ) (zip [0::Int ..] es)
  pure $ "({\n    MiVal *_items = malloc(" ++ show n ++ " * sizeof(MiVal));\n" ++
    concat elemCodes ++
    "    mi_list(_items, " ++ show n ++ ");\n  })"

-- | Generate curried closure wrappers for a C function.
-- Input params (CInt, CFloat, CString, CPtr) become curried closure args.
-- Output params (COutInt, COutFloat) are auto-allocated in the innermost wrapper
-- and returned as part of a result record.
cfunctionToC :: CGState -> String -> CType -> [CType] -> IO String
cfunctionToC st cname retTy allParamTys = do
  let -- Split into input indices and output indices
      indexed = zip [0::Int ..] allParamTys
      inputParams  = [(i, t) | (i, t) <- indexed, not (isOutputParam t)]
      outputParams = [(i, t) | (i, t) <- indexed, isOutputParam t]
      nInputs = length inputParams
  case nInputs of
    0 -> cffiLeaf st cname retTy allParamTys inputParams outputParams
    _ -> cffiCurried st cname retTy allParamTys inputParams outputParams

-- | Generate the leaf (innermost) function that actually calls the C function
cffiLeaf :: CGState -> String -> CType -> [CType]
         -> [(Int, CType)]   -- input params with original indices
         -> [(Int, CType)]   -- output params with original indices
         -> IO String
cffiLeaf st cname retTy allParamTys inputParams outputParams = do
  cid <- freshId st
  let fnName = "mi_cffi_" ++ show cid
      nInputs = length inputParams
      nAll = length allParamTys
      hasOuts = not (null outputParams)

      -- Env: for 0 inputs, no env. For 1 input, no env (arg is direct).
      -- For N>1 inputs, env has inputs 0..N-2, last input is direct _arg.
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

      -- Map from original C param index → the MiVal expression for that arg
      inputArgExpr k
        | nInputs == 0 = error "no inputs"
        | nInputs == 1 = "_arg"  -- single input, received directly
        | k == nInputs - 1 = "_arg"  -- last input
        | otherwise = "_a" ++ show k

      -- Build the argument list for the C call in original order
      outDecls = concatMap (\(i, t) ->
        let cty = case t of COutInt -> "int"; COutFloat -> "double"; _ -> "int"
        in "  " ++ cty ++ " _out_" ++ show i ++ " = 0;\n") outputParams
      cArgList = intercalate ", " $ map (\(origIdx, t) ->
        if isOutputParam t
          then "&_out_" ++ show origIdx
          else
            -- Find which input index this original index corresponds to
            let inputIdx = length [() | (j, _) <- inputParams, j < origIdx]
            in miToCArg t (inputArgExpr inputIdx)
        ) (zip [0..] allParamTys)

      callExpr = cname ++ "(" ++ cArgList ++ ")"

      -- Build return value
      returnExpr
        | not hasOuts = cRetToMi retTy callExpr
        | retTy == CVoid =
            -- void return with output params: return record of outputs
            "(" ++ callExpr ++ ",\n" ++ buildOutRecord outputParams ++ ")"
        | otherwise =
            -- non-void return with output params: return record with value + outputs
            "({ " ++ cRetTypeName retTy ++ " _ret = " ++ callExpr ++ ";\n" ++
            buildOutRecordWithRet retTy outputParams ++ " })"

      fnDef = envStruct ++
              "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env) {\n" ++
              (if nInputs == 0 then "  (void)_arg; " else "") ++
              (if nInputs <= 1 then "  (void)_env;\n" else "") ++
              envCast ++ envUnpack ++ outDecls ++
              "  return " ++ returnExpr ++ ";\n}\n\n"

  addTopDef st fnDef
  pure $ "mi_closure(" ++ fnName ++ ", NULL)"

-- | Build a record from output params only (for void-returning functions)
buildOutRecord :: [(Int, CType)] -> String
buildOutRecord outs =
  let n = length outs
      fields = concatMap (\(idx, (i, t)) ->
        let val = outToMi t ("_out_" ++ show i)
        in "    _fields[" ++ show idx ++ "] = " ++ val ++ ";\n") (zip [0..] outs)
      names = concatMap (\(i, _) ->
        "\"out" ++ show i ++ "\", ") outs
  in "({\n    MiVal *_fields = malloc(" ++ show n ++ " * sizeof(MiVal));\n" ++
     "    static const char *_names[] = {" ++ names ++ "};\n" ++
     fields ++
     "    MiVal _r; _r.type = MI_RECORD; _r.as.rec.tag = \"Result\";" ++
     " _r.as.rec.names = _names; _r.as.rec.fields = _fields;" ++
     " _r.as.rec.nfields = " ++ show n ++ "; _r;\n  })"

-- | Build a record from return value + output params
buildOutRecordWithRet :: CType -> [(Int, CType)] -> String
buildOutRecordWithRet retTy outs =
  let n = 1 + length outs
      retField = "    _fields[0] = " ++ cRetToMi retTy "_ret" ++ ";\n"
      outFields = concatMap (\(idx, (i, t)) ->
        let val = outToMi t ("_out_" ++ show (i :: Int))
        in "    _fields[" ++ show ((idx :: Int) + 1) ++ "] = " ++ val ++ ";\n")
        (zip [0..] outs)
      names = "\"value\", " ++ concatMap (\(i, _) ->
        "\"out" ++ show i ++ "\", ") outs
  in "MiVal *_fields = malloc(" ++ show n ++ " * sizeof(MiVal));\n" ++
     "  static const char *_names[] = {" ++ names ++ "};\n" ++
     retField ++ outFields ++
     "  MiVal _r; _r.type = MI_RECORD; _r.as.rec.tag = \"Result\";" ++
     " _r.as.rec.names = _names; _r.as.rec.fields = _fields;" ++
     " _r.as.rec.nfields = " ++ show n ++ "; _r;"

-- | Convert an output param value to MiVal
outToMi :: CType -> String -> String
outToMi COutInt name   = "mi_int((int64_t)" ++ name ++ ")"
outToMi COutFloat name = "mi_float((double)" ++ name ++ ")"
outToMi t name         = cRetToMi t name

-- | C type name for declaring a temporary
cRetTypeName :: CType -> String
cRetTypeName CInt    = "int64_t"
cRetTypeName CFloat  = "double"
cRetTypeName CString = "char *"
cRetTypeName CPtr    = "void *"
cRetTypeName CVoid   = "void"
cRetTypeName COutInt = "int"
cRetTypeName COutFloat = "double"

-- | Generate the curried closure chain for N-input functions
cffiCurried :: CGState -> String -> CType -> [CType]
            -> [(Int, CType)] -> [(Int, CType)] -> IO String
cffiCurried st cname retTy allParamTys inputParams outputParams = do
  let nInputs = length inputParams
  -- Generate the leaf first
  leafCode <- cffiLeaf st cname retTy allParamTys inputParams outputParams
  if nInputs == 1
    then pure leafCode  -- cffiLeaf already made a single closure
    else do
      -- Need currying wrappers for inputs 0..nInputs-2
      -- The leaf already handles the last input + all outputs
      -- We need to wrap it: level 0 captures arg 0, returns closure for level 1, etc.
      -- Level nInputs-2 captures arg nInputs-2, returns the leaf closure with full env
      leafFnName <- do
        -- The leaf's ID was the last freshId call; read current counter to get it
        n <- readIORef (cgNextId st)
        pure $ "mi_cffi_" ++ show (n - 1)

      -- Generate currying levels from nInputs-2 down to 0
      ids <- mapM (\_ -> freshId st) [0..nInputs-2]
      let wrapperNames = map (\i -> "mi_cffi_" ++ show i) ids

      -- Generate wrappers innermost-first (so forward decls work when reversed)
      mapM_ (\k -> do
        let fnName = wrapperNames !! k
            envName = fnName ++ "_env"
            nextName = if k == nInputs - 2
                       then leafFnName
                       else wrapperNames !! (k + 1)
            nextEnvName = nextName ++ "_env"
        if k == 0
          then do
            -- Outermost: no env, capture first arg
            let fnDef = "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env) {\n" ++
                        "  (void)_env;\n" ++
                        "  struct " ++ nextEnvName ++ " *_ne = malloc(sizeof(struct " ++ nextEnvName ++ "));\n" ++
                        "  _ne->_a0 = _arg;\n" ++
                        "  return mi_closure(" ++ nextName ++ ", _ne);\n}\n\n"
            addTopDef st fnDef
          else do
            -- Middle: has args 0..k-1 in env, receives arg k, builds next env
            let envFields = concatMap (\i ->
                  "  MiVal _a" ++ show i ++ ";\n") [0..k-1]
                envStruct = "struct " ++ envName ++ " {\n" ++ envFields ++ "};\n\n"
                envCast = "  struct " ++ envName ++ " *_e = (struct " ++ envName ++ " *)_env;\n"
                allocNext = "  struct " ++ nextEnvName ++ " *_ne = malloc(sizeof(struct " ++ nextEnvName ++ "));\n" ++
                            concatMap (\i ->
                              "  _ne->_a" ++ show i ++ " = _e->_a" ++ show i ++ ";\n") [0..k-1] ++
                            "  _ne->_a" ++ show k ++ " = _arg;\n"
                fnDef = envStruct ++
                        "static MiVal " ++ fnName ++ "(MiVal _arg, void *_env) {\n" ++
                        envCast ++ allocNext ++
                        "  return mi_closure(" ++ nextName ++ ", _ne);\n}\n\n"
            addTopDef st fnDef
        ) (reverse [0..nInputs-2])

      pure $ "mi_closure(" ++ wrapperNames !! 0 ++ ", NULL)"

-- | Convert a C return value to MiVal
cRetToMi :: CType -> String -> String
cRetToMi CInt expr     = "mi_int((int64_t)(" ++ expr ++ "))"
cRetToMi CFloat expr   = "mi_float((double)(" ++ expr ++ "))"
cRetToMi CString expr  = "mi_string(" ++ expr ++ ")"
cRetToMi CVoid expr    = "(" ++ expr ++ ", mi_int(0))"
cRetToMi CPtr expr     = "mi_pointer((void*)(" ++ expr ++ "))"
cRetToMi COutInt _     = "mi_int(0) /* unexpected output param as return */"
cRetToMi COutFloat _   = "mi_float(0) /* unexpected output param as return */"

-- | Convert a MiVal argument to C type for calling
miToCArg :: CType -> String -> String
miToCArg CInt name     = "(int)(" ++ name ++ ".as.i)"
miToCArg CFloat name   = "mi_to_float(" ++ name ++ ")"
miToCArg CString name  = name ++ ".as.s"
miToCArg CVoid _       = "/* void */"
miToCArg CPtr name     = name ++ ".as.ptr"
miToCArg COutInt name  = "&_out_" ++ name  -- placeholder, handled specially
miToCArg COutFloat name = "&_out_" ++ name  -- placeholder, handled specially

-- | Check if any parameter is an output param
hasOutputParams :: [CType] -> Bool
hasOutputParams = any (\t -> t == COutInt || t == COutFloat)

-- | Is this type an output parameter?
isOutputParam :: CType -> Bool
isOutputParam COutInt   = True
isOutputParam COutFloat = True
isOutputParam _         = False

altsToC :: CGState -> [Alt] -> IO String
altsToC _ [] = pure "    { fprintf(stderr, \"match: no matching pattern\\n\"); exit(1); }\n"
altsToC st (Alt pat body : rest) = do
  bodyCode <- exprToC st body
  let check = patCheck "_scrut" pat
      binds = patBinds "_scrut" pat
  restCode <- case rest of
    [] -> pure ""
    _  -> do
      rc <- altsToC st rest
      pure $ " else " ++ rc
  pure $ "    " ++ check ++ " {\n" ++ binds ++
    "      _result = " ++ bodyCode ++ ";\n    }" ++ restCode

patCheck :: String -> Pat -> String
patCheck _ (PVar _) = "if (1)"
patCheck _ PWild    = "if (1)"
patCheck s (PLit (IntLit n)) = "if (" ++ s ++ ".as.i == " ++ show n ++ ")"
patCheck s (PLit (StringLit t)) = "if (" ++ s ++ ".type == MI_STRING && strcmp(" ++ s ++ ".as.s, " ++ show (T.unpack t) ++ ") == 0)"
patCheck s (PLit _) = "if (0) /* unsupported literal pattern */"
patCheck s (PRec tag _) = "if (" ++ s ++ ".type == MI_RECORD && strcmp(" ++ s ++ ".as.rec.tag, \"" ++ T.unpack tag ++ "\") == 0)"
patCheck s (PList pats Nothing) = "if (" ++ s ++ ".type == MI_LIST && " ++ s ++ ".as.list.len == " ++ show (length pats) ++ ")"
patCheck s (PList pats (Just _)) = "if (" ++ s ++ ".type == MI_LIST && " ++ s ++ ".as.list.len >= " ++ show (length pats) ++ ")"

patBinds :: String -> Pat -> String
patBinds s (PVar v) = "      MiVal " ++ sanitize v ++ " = " ++ s ++ ";\n"
patBinds _ PWild = ""
patBinds _ (PLit _) = ""
patBinds s (PRec _ fields) = concatMap (fieldBind s) fields
patBinds s (PList pats mrest) =
  concatMap (\(i, p) -> listElemBind s i p) (zip [0::Int ..] pats) ++
  case mrest of
    Nothing -> ""
    Just name ->
      let n = length pats
      in "      MiVal *_rest_items = malloc((" ++ s ++ ".as.list.len - " ++ show n ++ ") * sizeof(MiVal));\n" ++
         "      for (int _ri = " ++ show n ++ "; _ri < " ++ s ++ ".as.list.len; _ri++)\n" ++
         "        _rest_items[_ri - " ++ show n ++ "] = " ++ s ++ ".as.list.items[_ri];\n" ++
         "      MiVal " ++ sanitize name ++ " = mi_list(_rest_items, " ++ s ++ ".as.list.len - " ++ show n ++ ");\n"

-- Bind a single list element by index in a pattern match
listElemBind :: String -> Int -> Pat -> String
listElemBind s i (PVar v) = "      MiVal " ++ sanitize v ++ " = " ++ s ++ ".as.list.items[" ++ show i ++ "];\n"
listElemBind _ _ PWild = ""
listElemBind s i _ = "      MiVal _elem_" ++ show i ++ " = " ++ s ++ ".as.list.items[" ++ show i ++ "];\n"

fieldBind :: String -> (Text, Pat) -> String
fieldBind s (field, PVar v) =
  "      MiVal " ++ sanitize v ++ " = mi_field(" ++ s ++ ", \"" ++ T.unpack field ++ "\");\n"
fieldBind _ (_, PWild) = ""
fieldBind s (field, _) =
  "      MiVal _f_" ++ T.unpack field ++ " = mi_field(" ++ s ++ ", \"" ++ T.unpack field ++ "\");\n"

-- | Find free variables in an expression, excluding the given bound names
freeVars :: Expr -> [Text] -> [Text]
freeVars (IntLit _)       _     = []
freeVars (FloatLit _)     _     = []
freeVars (StringLit _)    _     = []
freeVars (Name n)         bound = [n | n `notElem` bound]
freeVars (BinOp _ l r)    bound = freeVars l bound ++ freeVars r bound
freeVars (App f x)        bound = freeVars f bound ++ freeVars x bound
freeVars (Lam p b)        bound = freeVars b (p : bound)
freeVars (With body bs)   bound =
  let bnames = map bindName bs
      bound' = bnames ++ bound
  in concatMap (\b -> freeVars (bindBody b) bound') bs ++ freeVars body bound'
freeVars (Record _ bs)    bound = concatMap (\b -> freeVars (bindBody b) bound) bs
freeVars (FieldAccess e _) bound = freeVars e bound
freeVars (Namespace bs)   bound =
  let bnames = map bindName bs
      bound' = bnames ++ bound
  in concatMap (\b -> freeVars (bindBody b) bound') bs
freeVars (Case s alts)    bound =
  freeVars s bound ++ concatMap (\a -> freeVars (altBody a) bound) alts
freeVars (CFunction {})   _     = []
freeVars (Thunk body)     bound = freeVars body bound
freeVars (ListLit es)     bound = concatMap (\e -> freeVars e bound) es

-- ── C runtime preamble ────────────────────────────────────────────

emitPreamble :: Handle -> IO ()
emitPreamble h = do
  hPutStr h $ unlines
    [ "#define _GNU_SOURCE"
    , "#include <stdio.h>"
    , "#include <stdlib.h>"
    , "#include <string.h>"
    , "#include <stdint.h>"
    , "#include <math.h>"
    , ""
    , "typedef enum { MI_INT, MI_FLOAT, MI_STRING, MI_RECORD, MI_CLOSURE, MI_POINTER, MI_LIST } MiType;"
    , ""
    , "typedef struct MiVal {"
    , "  MiType type;"
    , "  union {"
    , "    int64_t i;"
    , "    double f;"
    , "    char *s;"
    , "    struct { const char *tag; const char **names; struct MiVal *fields; int nfields; } rec;"
    , "    struct { struct MiVal (*fn)(struct MiVal, void*); void *env; } closure;"
    , "    void *ptr;"
    , "    struct { struct MiVal *items; int len; int cap; } list;"
    , "  } as;"
    , "} MiVal;"
    , ""
    , "static MiVal mi_int(int64_t v) { MiVal r; r.type = MI_INT; r.as.i = v; return r; }"
    , "static MiVal mi_float(double v) { MiVal r; r.type = MI_FLOAT; r.as.f = v; return r; }"
    , "static MiVal mi_string(const char *s) { MiVal r; r.type = MI_STRING; r.as.s = strdup(s); return r; }"
    , "static MiVal mi_pointer(void *p) { MiVal r; r.type = MI_POINTER; r.as.ptr = p; return r; }"
    , ""
    , "static MiVal mi_list(MiVal *items, int len) {"
    , "  MiVal r; r.type = MI_LIST; r.as.list.items = items; r.as.list.len = len; r.as.list.cap = len; return r;"
    , "}"
    , ""
    , "static MiVal mi_closure(MiVal (*fn)(MiVal, void*), void *env) {"
    , "  MiVal r; r.type = MI_CLOSURE; r.as.closure.fn = fn; r.as.closure.env = env; return r;"
    , "}"
    , ""
    , "static MiVal mi_apply(MiVal f, MiVal arg) {"
    , "  if (f.type != MI_CLOSURE) { fprintf(stderr, \"apply on non-closure\\n\"); exit(1); }"
    , "  return f.as.closure.fn(arg, f.as.closure.env);"
    , "}"
    , ""
    , "static double mi_to_float(MiVal v) {"
    , "  return v.type == MI_FLOAT ? v.as.f : (double)v.as.i;"
    , "}"
    , ""
    , "static MiVal mi_add(MiVal a, MiVal b) {"
    , "  if (a.type == MI_STRING && b.type == MI_STRING) {"
    , "    size_t la = strlen(a.as.s), lb = strlen(b.as.s);"
    , "    char *r = malloc(la + lb + 1);"
    , "    memcpy(r, a.as.s, la); memcpy(r+la, b.as.s, lb+1);"
    , "    MiVal v; v.type = MI_STRING; v.as.s = r; return v;"
    , "  }"
    , "  if (a.type == MI_FLOAT || b.type == MI_FLOAT)"
    , "    return mi_float(mi_to_float(a) + mi_to_float(b));"
    , "  return mi_int(a.as.i + b.as.i);"
    , "}"
    , "static MiVal mi_sub(MiVal a, MiVal b) {"
    , "  if (a.type == MI_FLOAT || b.type == MI_FLOAT)"
    , "    return mi_float(mi_to_float(a) - mi_to_float(b));"
    , "  return mi_int(a.as.i - b.as.i);"
    , "}"
    , "static MiVal mi_mul(MiVal a, MiVal b) {"
    , "  if (a.type == MI_FLOAT || b.type == MI_FLOAT)"
    , "    return mi_float(mi_to_float(a) * mi_to_float(b));"
    , "  return mi_int(a.as.i * b.as.i);"
    , "}"
    , "static MiVal mi_div(MiVal a, MiVal b) {"
    , "  if (a.type == MI_FLOAT || b.type == MI_FLOAT)"
    , "    return mi_float(mi_to_float(a) / mi_to_float(b));"
    , "  if (b.as.i == 0) { fprintf(stderr, \"division by zero\\n\"); exit(1); }"
    , "  return mi_int(a.as.i / b.as.i);"
    , "}"
    , "static MiVal mi_pow(MiVal a, MiVal b) {"
    , "  if (a.type == MI_FLOAT || b.type == MI_FLOAT)"
    , "    return mi_float(pow(mi_to_float(a), mi_to_float(b)));"
    , "  int64_t result = 1, base = a.as.i, exp = b.as.i;"
    , "  for (; exp > 0; exp--) result *= base;"
    , "  return mi_int(result);"
    , "}"
    , ""
    , "static MiVal mi_eq(MiVal a, MiVal b) { return mi_int(a.as.i == b.as.i); }"
    , "static MiVal mi_neq(MiVal a, MiVal b) { return mi_int(a.as.i != b.as.i); }"
    , "static MiVal mi_lt(MiVal a, MiVal b) { return mi_int(a.as.i < b.as.i); }"
    , "static MiVal mi_gt(MiVal a, MiVal b) { return mi_int(a.as.i > b.as.i); }"
    , "static MiVal mi_le(MiVal a, MiVal b) { return mi_int(a.as.i <= b.as.i); }"
    , "static MiVal mi_ge(MiVal a, MiVal b) { return mi_int(a.as.i >= b.as.i); }"
    , ""
    , "static void mi_print_val(MiVal v) {"
    , "  switch (v.type) {"
    , "    case MI_INT:    printf(\"%ld\", v.as.i); break;"
    , "    case MI_FLOAT:  printf(\"%g\", v.as.f); break;"
    , "    case MI_STRING: printf(\"%s\", v.as.s); break;"
    , "    case MI_RECORD:"
    , "      printf(\"%s {\", v.as.rec.tag);"
    , "      for (int i = 0; i < v.as.rec.nfields; i++) {"
    , "        if (i > 0) printf(\", \");"
    , "        if (v.as.rec.names) printf(\"%s = \", v.as.rec.names[i]);"
    , "        mi_print_val(v.as.rec.fields[i]);"
    , "      }"
    , "      printf(\"}\");"
    , "      break;"
    , "    case MI_CLOSURE: printf(\"<closure>\"); break;"
    , "    case MI_POINTER: printf(\"<ptr:%p>\", v.as.ptr); break;"
    , "    case MI_LIST:"
    , "      printf(\"[\");"
    , "      for (int i = 0; i < v.as.list.len; i++) {"
    , "        if (i > 0) printf(\", \");"
    , "        mi_print_val(v.as.list.items[i]);"
    , "      }"
    , "      printf(\"]\");"
    , "      break;"
    , "  }"
    , "}"
    , ""
    , "static void mi_println_val(MiVal v) {"
    , "  mi_print_val(v); printf(\"\\n\");"
    , "}"
    , ""
    , "static MiVal mi_field(MiVal rec, const char *name) {"
    , "  if (rec.type != MI_RECORD) { fprintf(stderr, \"field access on non-record\\n\"); exit(1); }"
    , "  for (int i = 0; i < rec.as.rec.nfields; i++) {"
    , "    if (rec.as.rec.names && strcmp(rec.as.rec.names[i], name) == 0)"
    , "      return rec.as.rec.fields[i];"
    , "  }"
    , "  fprintf(stderr, \"field '%s' not found in record '%s'\\n\", name, rec.as.rec.tag);"
    , "  exit(1);"
    , "}"
    , ""
    -- Built-in IO: print and println as closures
    , "static MiVal mi_builtin_print(MiVal v, void *env) {"
    , "  (void)env; mi_print_val(v); return mi_int(0);"
    , "}"
    , "static MiVal mi_builtin_println(MiVal v, void *env) {"
    , "  (void)env; mi_println_val(v); return mi_int(0);"
    , "}"
    , ""
    -- Force a thunk (closure with dummy arg)
    , "static MiVal mi_force(MiVal v) {"
    , "  if (v.type == MI_CLOSURE) return mi_apply(v, mi_int(0));"
    , "  return v;"
    , "}"
    , ""
    -- Built-in if: 3-arg curried function (cond, then-thunk, else-thunk)
    , "struct mi_if_env2 { MiVal cond; MiVal then_val; };"
    , "static MiVal mi_builtin_if3(MiVal else_val, void *env) {"
    , "  struct mi_if_env2 *e = (struct mi_if_env2 *)env;"
    , "  if (e->cond.as.i != 0) return mi_force(e->then_val);"
    , "  return mi_force(else_val);"
    , "}"
    , "struct mi_if_env1 { MiVal cond; };"
    , "static MiVal mi_builtin_if2(MiVal then_val, void *env) {"
    , "  struct mi_if_env1 *e = (struct mi_if_env1 *)env;"
    , "  struct mi_if_env2 *e2 = malloc(sizeof(struct mi_if_env2));"
    , "  e2->cond = e->cond; e2->then_val = then_val;"
    , "  return mi_closure(mi_builtin_if3, e2);"
    , "}"
    , "static MiVal mi_builtin_if1(MiVal cond, void *env) {"
    , "  (void)env;"
    , "  struct mi_if_env1 *e = malloc(sizeof(struct mi_if_env1));"
    , "  e->cond = cond;"
    , "  return mi_closure(mi_builtin_if2, e);"
    , "}"
    , ""
    -- List built-ins: len, get, push, concat, slice, map, fold, filter
    , "static MiVal mi_builtin_len(MiVal lst, void *env) {"
    , "  (void)env; return mi_int(lst.as.list.len);"
    , "}"
    , "struct mi_get_env { MiVal lst; };"
    , "static MiVal mi_builtin_get2(MiVal idx, void *env) {"
    , "  struct mi_get_env *e = (struct mi_get_env *)env;"
    , "  int i = (int)idx.as.i;"
    , "  if (i < 0 || i >= e->lst.as.list.len) { fprintf(stderr, \"list index out of range\\n\"); exit(1); }"
    , "  return e->lst.as.list.items[i];"
    , "}"
    , "static MiVal mi_builtin_get1(MiVal lst, void *env) {"
    , "  (void)env;"
    , "  struct mi_get_env *e = malloc(sizeof(struct mi_get_env));"
    , "  e->lst = lst;"
    , "  return mi_closure(mi_builtin_get2, e);"
    , "}"
    , "struct mi_push_env { MiVal lst; };"
    , "static MiVal mi_builtin_push2(MiVal val, void *env) {"
    , "  struct mi_push_env *e = (struct mi_push_env *)env;"
    , "  int n = e->lst.as.list.len;"
    , "  MiVal *items = malloc((n + 1) * sizeof(MiVal));"
    , "  for (int i = 0; i < n; i++) items[i] = e->lst.as.list.items[i];"
    , "  items[n] = val;"
    , "  return mi_list(items, n + 1);"
    , "}"
    , "static MiVal mi_builtin_push1(MiVal lst, void *env) {"
    , "  (void)env;"
    , "  struct mi_push_env *e = malloc(sizeof(struct mi_push_env));"
    , "  e->lst = lst;"
    , "  return mi_closure(mi_builtin_push2, e);"
    , "}"
    , "struct mi_concat_env { MiVal a; };"
    , "static MiVal mi_builtin_concat2(MiVal b, void *env) {"
    , "  struct mi_concat_env *e = (struct mi_concat_env *)env;"
    , "  int na = e->a.as.list.len, nb = b.as.list.len;"
    , "  MiVal *items = malloc((na + nb) * sizeof(MiVal));"
    , "  for (int i = 0; i < na; i++) items[i] = e->a.as.list.items[i];"
    , "  for (int i = 0; i < nb; i++) items[na + i] = b.as.list.items[i];"
    , "  return mi_list(items, na + nb);"
    , "}"
    , "static MiVal mi_builtin_concat1(MiVal a, void *env) {"
    , "  (void)env;"
    , "  struct mi_concat_env *e = malloc(sizeof(struct mi_concat_env));"
    , "  e->a = a;"
    , "  return mi_closure(mi_builtin_concat2, e);"
    , "}"
    , "struct mi_map_env { MiVal fn; };"
    , "static MiVal mi_builtin_map2(MiVal lst, void *env) {"
    , "  struct mi_map_env *e = (struct mi_map_env *)env;"
    , "  int n = lst.as.list.len;"
    , "  MiVal *items = malloc(n * sizeof(MiVal));"
    , "  for (int i = 0; i < n; i++) items[i] = mi_apply(e->fn, lst.as.list.items[i]);"
    , "  return mi_list(items, n);"
    , "}"
    , "static MiVal mi_builtin_map1(MiVal fn, void *env) {"
    , "  (void)env;"
    , "  struct mi_map_env *e = malloc(sizeof(struct mi_map_env));"
    , "  e->fn = fn;"
    , "  return mi_closure(mi_builtin_map2, e);"
    , "}"
    , "struct mi_fold_env1 { MiVal fn; };"
    , "struct mi_fold_env2 { MiVal fn; MiVal acc; };"
    , "static MiVal mi_builtin_fold3(MiVal lst, void *env) {"
    , "  struct mi_fold_env2 *e = (struct mi_fold_env2 *)env;"
    , "  MiVal acc = e->acc;"
    , "  for (int i = 0; i < lst.as.list.len; i++)"
    , "    acc = mi_apply(mi_apply(e->fn, acc), lst.as.list.items[i]);"
    , "  return acc;"
    , "}"
    , "static MiVal mi_builtin_fold2(MiVal acc, void *env) {"
    , "  struct mi_fold_env1 *e1 = (struct mi_fold_env1 *)env;"
    , "  struct mi_fold_env2 *e2 = malloc(sizeof(struct mi_fold_env2));"
    , "  e2->fn = e1->fn; e2->acc = acc;"
    , "  return mi_closure(mi_builtin_fold3, e2);"
    , "}"
    , "static MiVal mi_builtin_fold1(MiVal fn, void *env) {"
    , "  (void)env;"
    , "  struct mi_fold_env1 *e = malloc(sizeof(struct mi_fold_env1));"
    , "  e->fn = fn;"
    , "  return mi_closure(mi_builtin_fold2, e);"
    , "}"
    , "struct mi_filter_env { MiVal fn; };"
    , "static MiVal mi_builtin_filter2(MiVal lst, void *env) {"
    , "  struct mi_filter_env *e = (struct mi_filter_env *)env;"
    , "  int n = lst.as.list.len;"
    , "  MiVal *items = malloc(n * sizeof(MiVal));"
    , "  int count = 0;"
    , "  for (int i = 0; i < n; i++) {"
    , "    MiVal r = mi_apply(e->fn, lst.as.list.items[i]);"
    , "    if (r.as.i != 0) items[count++] = lst.as.list.items[i];"
    , "  }"
    , "  return mi_list(items, count);"
    , "}"
    , "static MiVal mi_builtin_filter1(MiVal fn, void *env) {"
    , "  (void)env;"
    , "  struct mi_filter_env *e = malloc(sizeof(struct mi_filter_env));"
    , "  e->fn = fn;"
    , "  return mi_closure(mi_builtin_filter2, e);"
    , "}"
    , ""
    ]

-- ── Helpers ───────────────────────────────────────────────────────

opFunc :: Text -> String
opFunc "+"  = "mi_add"
opFunc "-"  = "mi_sub"
opFunc "*"  = "mi_mul"
opFunc "/"  = "mi_div"
opFunc "**" = "mi_pow"
opFunc "==" = "mi_eq"
opFunc "/=" = "mi_neq"
opFunc "<"  = "mi_lt"
opFunc ">"  = "mi_gt"
opFunc "<=" = "mi_le"
opFunc ">=" = "mi_ge"
opFunc op   = "/* unknown op " ++ T.unpack op ++ " */ mi_add"

sanitize :: Text -> String
sanitize t
  | s `elem` cKeywords = "mi_" ++ s
  | otherwise = concatMap esc s
  where
    s = T.unpack t
    esc '\'' = "_prime"
    esc c    = [c]

cKeywords :: [String]
cKeywords =
  [ "auto", "break", "case", "char", "const", "continue", "default", "do"
  , "double", "else", "enum", "extern", "float", "for", "goto", "if"
  , "int", "long", "register", "return", "short", "signed", "sizeof"
  , "static", "struct", "switch", "typedef", "union", "unsigned", "void"
  , "volatile", "while", "inline", "restrict"
  , "print", "println"  -- reserved for builtins
  , "len", "get", "push", "concat", "map", "fold", "filter"  -- list builtins
  ]
