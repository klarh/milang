{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Core.Reduce
  ( reduce, reduceWithEnv, Env, emptyEnv
  , Warning(..), warnings
  , exprFreeVars
  , envInsert, envMap, envLookup
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isUpper)
import Data.Graph (stronglyConnComp, SCC(..))
import Data.List (isPrefixOf)
import Text.Read (readMaybe)

import Core.Syntax

-- ── Environment ───────────────────────────────────────────────────

data Env = Env
  { envMap    :: !(Map.Map Text Expr)
  , envRec    :: !(Set.Set Text)      -- recursive (cyclic) names
  , envImpure :: !(Set.Set Text)      -- world-tainted names
  , envTypes  :: !(Map.Map Text Expr) -- type annotations (:: domain)
  , envTraits :: !(Map.Map Text Expr) -- trait annotations (:~ domain)
  , envWarns  :: ![Warning]           -- accumulated warnings/errors
  } deriving (Show)

data Warning
  = TypeWarning (Maybe SrcPos) Text Text  -- pos, name, message
  | TraitWarning (Maybe SrcPos) Text Text
  | GeneralWarning (Maybe SrcPos) Text
  deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty Set.empty Set.empty Map.empty Map.empty []

warnings :: Env -> [Warning]
warnings = envWarns

envLookup :: Text -> Env -> Maybe Expr
envLookup n = Map.lookup n . envMap

envInsert :: Text -> Expr -> Env -> Env
envInsert n v env = env { envMap = Map.insert n v (envMap env) }

envDelete :: Text -> Env -> Env
envDelete n env = env { envMap = Map.delete n (envMap env) }

envIsRec :: Text -> Env -> Bool
envIsRec n = Set.member n . envRec

envMarkImpure :: Text -> Env -> Env
envMarkImpure n env = env { envImpure = Set.insert n (envImpure env) }

envIsImpure :: Text -> Env -> Bool
envIsImpure n = Set.member n . envImpure

envAddWarning :: Warning -> Env -> Env
envAddWarning w env = env { envWarns = w : envWarns env }

envInsertType :: Text -> Expr -> Env -> Env
envInsertType n t env = env { envTypes = Map.insert n t (envTypes env) }

envInsertTrait :: Text -> Expr -> Env -> Env
envInsertTrait n t env = env { envTraits = Map.insert n t (envTraits env) }

-- ── Core properties ───────────────────────────────────────────────

maxDepth :: Int
maxDepth = 128

minRecDepth :: Int
minRecDepth = 8

isResidual :: Expr -> Bool
isResidual (IntLit _)      = False
isResidual (FloatLit _)    = False
isResidual (SizedInt {})   = False
isResidual (SizedFloat {}) = False
isResidual (StringLit _)   = False
isResidual (Record _ _)    = False
isResidual _               = True

isConcrete :: Expr -> Bool
isConcrete (IntLit _)      = True
isConcrete (FloatLit _)    = True
isConcrete (SizedInt {})   = True
isConcrete (SizedFloat {}) = True
isConcrete (StringLit _)   = True
isConcrete (Lam _ _)       = True
isConcrete (Record _ bs)   = all (isConcrete . bindBody) bs
isConcrete (Namespace bs)  = all (isConcrete . bindBody) bs
isConcrete (CFunction {})  = True
isConcrete _               = False

isWorldTainted :: Env -> Expr -> Bool
isWorldTainted env expr =
  let fvs = exprFreeVars expr
  in "world" `Set.member` fvs || any (`envIsImpure` env) (Set.toList fvs)

collectApp :: Expr -> (Expr, [Expr])
collectApp (App f x) = let (h, args) = collectApp f in (h, args ++ [x])
collectApp e         = (e, [])

isOperatorName :: Text -> Bool
isOperatorName t = not (T.null t) && T.all (`elem` ("+-*/^<>=!&|@%?:" :: String)) t

-- ── Main reduce ───────────────────────────────────────────────────

-- | Top-level reduce: returns (reduced expr, warnings).
-- Type/trait checking is integrated into the unified reduction pass.
reduce :: Env -> Expr -> (Expr, [Warning])
reduce env e = case e of
  Namespace _ ->
    let env' = evalBindings maxDepth env (nsBindings e)
        expanded = concatMap expandUnion (nsBindings e)
        (valueBs, annotBs) = partitionBindings expanded
        mergedValues = mergeOpenDefs valueBs
        valBs' = map (reduceBind maxDepth env') mergedValues
        annBs' = map (reduceBind maxDepth env') annotBs
    in (Namespace (valBs' ++ annBs'), reverse (envWarns env'))
  _ -> (reduceD maxDepth env e, [])
  where
    nsBindings (Namespace bs) = bs
    nsBindings _ = []

-- | Reduce an expression within an env, returning the updated env, result, and warnings.
-- Used by the REPL to accumulate state across inputs.
reduceWithEnv :: Env -> Expr -> (Env, Expr, [Warning])
reduceWithEnv env e = case e of
  Namespace _ ->
    let env' = evalBindings maxDepth env (nsBindings e)
        expanded = concatMap expandUnion (nsBindings e)
        (valueBs, annotBs) = partitionBindings expanded
        mergedValues = mergeOpenDefs valueBs
        valBs' = map (reduceBind maxDepth env') mergedValues
        annBs' = map (reduceBind maxDepth env') annotBs
    in (env', Namespace (valBs' ++ annBs'), reverse (envWarns env'))
  _ -> (env, reduceD maxDepth env e, [])
  where
    nsBindings (Namespace bs) = bs
    nsBindings _ = []

reduceD :: Int -> Env -> Expr -> Expr
reduceD _ _ e@(IntLit _)      = e
reduceD _ _ e@(FloatLit _)    = e
reduceD _ _ e@(SizedInt {})   = e
reduceD _ _ e@(SizedFloat {}) = e
reduceD _ _ e@(StringLit _)   = e
reduceD _ _ e@(Error _)       = e

reduceD d env (Name n)
  | d <= 0    = Name n
  | otherwise =
    case envLookup n env of
      Just (Name m) | m == n -> Name n  -- self-reference; don't recurse
      Just val@(Lam _ _) -> val
      Just val@(Namespace _) -> val  -- Namespace is a value, don't re-reduce
      Just val -> reduceD (d - 1) env val
      Nothing
        | isOperatorName n -> Lam "_a" (Lam "_b" (BinOp n (Name "_a") (Name "_b")))
        | not (T.null n) && isUpper (T.head n) ->
            -- Auto-constructor: bare uppercase name becomes zero-arg record
            Record n []
        | otherwise -> Name n

reduceD d env (BinOp "<-" l r) =
  let l' = forceThunk d env (reduceD d env l)
      r' = reduceD d env r
  in reduceRecordUpdate l' r'

reduceD d env (BinOp op l r) =
  let l' = forceThunk d env (reduceD d env l)
      r' = forceThunk d env (reduceD d env r)
      result = reduceBinOp op l' r'
  in case result of
    BinOp op' _ _ | op' == op ->
      case envLookup op env of
        Just fn -> reduceD d env (App (App fn l') r)
        Nothing -> result
    _ -> result

reduceD d env (App f x) =
  -- Check for single-arg builtins by name before env lookup (e.g. len on strings)
  case f of
    Name n | Just r <- tryBuiltin1 d env n x -> r
    _ -> case collectApp (App f x) of
      (Name n, args) | envIsRec n env ->
        let args' = map (reduceD d env) args
        in if all isConcrete args' && d >= minRecDepth
           then case envLookup n env of
                  Just fn -> foldl (reduceApp d env) fn args'
                  Nothing -> foldl App (Name n) args'
           else foldl App (Name n) args'
      _ ->
        let f' = reduceD d env f
        in case f' of
          -- Call-by-name for lambda application: don't evaluate arg until needed.
          -- This prevents divergence in recursive branches (e.g., if cond t e).
          Lam _ _ -> reduceApp d env f' x
          _       -> let x' = reduceD d env x
                     in reduceApp d env f' x'

reduceD d env (Lam p b)
  -- Quoted params: alpha-rename the real param name to prevent env capture.
  -- The body may contain $param (splice) which references the real name.
  | isQuotedParam p =
    let pn = lamParamName p
        allNames = Set.unions [ exprFreeVars b, Map.keysSet (envMap env)
                              , Set.unions (map exprFreeVars (Map.elems (envMap env))) ]
        fresh = freshName pn allNames
        b' = substExpr pn (Name fresh) b
        env' = envDelete fresh env
    in Lam ("#" <> fresh) (reduceD d env' b')
  -- Regular params: alpha-rename if the param could be captured
  | otherwise =
    let bodyFVs = Set.delete p (exprFreeVars b)
        envCapturesP = any (\fv -> case envLookup fv env of
          Just v  -> p `Set.member` exprFreeVars v
          Nothing -> False) (Set.toList bodyFVs)
        needsRename = p `Map.member` envMap env || envCapturesP
    in if needsRename
       then let allNames = Set.union (Map.keysSet (envMap env)) (exprFreeVars b)
                fresh = freshName p allNames
                b' = substExpr p (Name fresh) b
                env' = envDelete fresh env
            in Lam fresh (reduceD d env' b')
       else let env' = envDelete p env
            in Lam p (reduceD d env' b)

reduceD d env (With body bindings) =
  -- First, evaluate bindings normally (handles SCC ordering, concrete values)
  let env' = evalBindings d env bindings
  -- Then force-insert any bindings that evalBindings skipped (non-concrete).
  -- We check whether evalBindings actually inserted the name by comparing
  -- the new env against the old env.
      env'' = foldl (\e b ->
        let name = bindName b
        in case (envLookup name env, envLookup name e) of
             (old, new) | old == new ->
               -- evalBindings didn't change this binding — force insert
               let val = wrapLambda (bindParams b) (bindBody b)
               in envInsert name (reduceD d e val) e
             _ -> e  -- evalBindings already updated it
        ) env' bindings
      body' = reduceD d env'' body
      bs' = map (reduceBind d env'') bindings
      keepBinding b = isResidual (bindBody b) || envIsImpure (bindName b) env''
      residualBs = filter keepBinding bs'
  in if null residualBs then body' else With body' residualBs

reduceD d env (Record tag bindings) =
  Record tag (map (reduceBind d env) bindings)

reduceD d env (FieldAccess e field) =
  let e' = forceThunk d env (reduceD d env e)
  in reduceFieldAccess e' field

reduceD d env (Namespace bindings) =
  let expanded = concatMap expandUnion bindings
      (valueBs, annotBs) = partitionBindings expanded
      mergedValues = mergeOpenDefs valueBs
      env' = evalBindings d env expanded
      valBs' = map (reduceBind d env') mergedValues
      annBs' = map (reduceBind d env') annotBs
  in Namespace (valBs' ++ annBs')

reduceD d env (Case scrut alts) =
  let scrut' = forceThunk d env (reduceD d env scrut)
  in if isResidual scrut'
     then case scrut of
       -- When the scrutinee came from a name in env and a wildcard alt body
       -- references it, convert PWild to PVar so the C runtime binds the
       -- evaluated scrutinee once, preventing double evaluation of effectful
       -- expressions (e.g. toString (println x)).
       Name n
         | Map.member n (envMap env)
         , any (\(Alt p _ b) -> case p of
             PWild -> n `Set.member` exprFreeVars b
             _     -> False) alts ->
           let allNames = Set.unions
                 [ Map.keysSet (envMap env), exprFreeVars scrut'
                 , Set.unions (concatMap (\(Alt _ g b) ->
                     exprFreeVars b : maybe [] ((:[]) . exprFreeVars) g) alts) ]
               fresh = freshName n allNames
               envShadow = envInsert n (Name fresh) (envDelete fresh env)
               alts' = map (\alt@(Alt p g b) -> case p of
                 PWild | n `Set.member` exprFreeVars b
                         || maybe False ((n `Set.member`) . exprFreeVars) g ->
                   let envA = Set.foldl' (\e v -> envDelete v e) envShadow (patVars (PVar fresh))
                   in Alt (PVar fresh) (fmap (reduceD d envA) g) (reduceD d envA b)
                 _ ->
                   let envA = Set.foldl' (\e v -> envDelete v e) env (patVars p)
                   in Alt p (fmap (reduceD d envA) g) (reduceD d envA b)) alts
           in Case scrut' alts'
       _ ->
         Case scrut' (map (\(Alt p g b) ->
            let env' = Set.foldl' (\e v -> envDelete v e) env (patVars p)
            in Alt p (fmap (reduceD d env') g) (reduceD d env' b)) alts)
     else reduceCase d env scrut' alts

reduceD d env (Thunk body) =
  let body' = reduceD d env body
  in if isConcrete body' then body' else Thunk body'

reduceD d env (ListLit es) = listToCons (map (reduceD d env) es)

-- Import should have been resolved before reduction; if not, leave as-is
reduceD _ _ e@(Import _) = e

-- Quote: reify syntax as data (AST records), don't evaluate the body
reduceD _ _ (Quote e) = quoteExpr e

-- Splice: reduce the body; if it's an AST record, interpret as code and reduce.
-- If the body is still an unresolved Name, keep as residual Splice.
reduceD d env (Splice e) =
  let val = reduceD d env e
  in case val of
       Name _ -> Splice val  -- residual: name not yet resolved
       _ -> case spliceExprM val of
              Just expr -> reduceD d env expr
              Nothing   -> Splice val  -- can't splice non-AST value

reduceD _ _ (Error msg) = Error msg

-- CFunction is already fully concrete
reduceD _ _ e@(CFunction {}) = e

-- ── Binding evaluation (unified) ──────────────────────────────────

-- | Evaluate a sequence of bindings, extending the environment.
-- This is the unified dispatch: domain tag determines behavior.
evalBindings :: Int -> Env -> [Binding] -> Env
evalBindings d env bindings =
  let expanded = concatMap expandUnion bindings
      -- Separate by domain: value/lazy bindings go through SCC analysis,
      -- annotation bindings (type, trait, doc, parse) are processed immediately
      (valueBs, annotBs) = partitionBindings expanded
      -- Process annotations first (they inform value reduction)
      env1 = foldl (evalAnnotation d) env annotBs
      -- Merge open definitions for value bindings
      merged = mergeOpenDefs valueBs
      -- SCC analysis for value bindings
      bindNames = Set.fromList [bindName b | b <- merged]
      nodes = [ (b, bindName b,
                 Set.toList $ Set.intersection bindNames
                   (exprFreeVars (wrapLambda (bindParams b) (bindBody b))))
               | b <- merged ]
      sccs = stronglyConnComp nodes
      env2 = foldl (evalSCC d) env1 sccs
      -- Type and trait checking integrated into the unified reducer
      -- Use original bindings (not expanded) for type checking to avoid
      -- expandUnion constructor bindings overriding detectUnion arity-0 types
      typeWarns  = typeCheckBindings env2 bindings
      traitWarns = traitCheckBindings env2 expanded
      env3 = foldl (\e w -> envAddWarning w e) env2 (typeWarns ++ traitWarns)
  in env3

-- | Partition bindings into value/lazy vs annotation domains
partitionBindings :: [Binding] -> ([Binding], [Binding])
partitionBindings = foldr go ([], [])
  where
    go b (vs, as) = case bindDomain b of
      Value -> (b:vs, as)
      Lazy  -> (b:vs, as)
      _     -> (vs, b:as)

-- | Process an annotation binding within the unified reducer.
-- Type/Trait annotations get reduced and stored in their respective env maps.
-- NOTE: Type annotations are NOT reduced through the normal reducer because
-- the `:` operator means function arrow in the type domain, not cons.
evalAnnotation :: Int -> Env -> Binding -> Env
evalAnnotation d env b = case bindDomain b of
  Type  -> envInsertType (bindName b) (bindBody b) env  -- store raw
  Trait -> let tr = reduceD d env (bindBody b)
           in envInsertTrait (bindName b) tr env
  Doc   -> env  -- docs are informational; no effect on reduction
  Parse -> env  -- parse config handled pre-parse; no effect on reduction
  _     -> env  -- shouldn't happen

-- | Evaluate one SCC group of value bindings
isNamespaceVal :: Expr -> Bool
isNamespaceVal (Namespace _) = True
isNamespaceVal _ = False

evalSCC :: Int -> Env -> SCC Binding -> Env
evalSCC d env (AcyclicSCC b) =
  let name = bindName b
      b' = case Map.lookup name (envMap env) of
             Just oldExpr ->
               let newBody = wrapLambda (bindParams b) (bindBody b)
               in case chainBodies newBody oldExpr of
                    Just merged -> b { bindBody = merged, bindParams = [] }
                    Nothing     -> b
             Nothing -> b
      body = wrapLambda (bindParams b') (bindBody b')
      val = if bindDomain b' == Lazy
            then bindBody b'
            else reduceD d env body
      env1 = if isWorldTainted env body then envMarkImpure name env else env
      -- Emit warnings for Error nodes in reduced value
      env2 = case val of
        Error msg -> envAddWarning (GeneralWarning (bindPos b) (name <> ": " <> msg)) env1
        _         -> env1
  in if isConcrete val || isNamespaceVal val
     then envInsert name val env2
     else env2

evalSCC _ env (CyclicSCC bs) =
  let recNames = Set.fromList (map bindName bs)
      env1 = env { envRec = Set.union recNames (envRec env) }
  in foldl (\e b ->
       let body = wrapLambda (bindParams b) (bindBody b)
       in envInsert (bindName b) body e
     ) env1 bs

-- ── Reduce a binding's body ──────────────────────────────────────

reduceBind :: Int -> Env -> Binding -> Binding
reduceBind d env b = case bindDomain b of
  Value -> b { bindBody = reduceD d env (wrapLambda (bindParams b) (bindBody b)), bindParams = [] }
  Lazy  -> b { bindBody = reduceD d env (bindBody b) }
  Type  -> b  -- don't reduce type expressions (`:` means fn arrow, not cons)
  Trait -> b { bindBody = reduceD d env (bindBody b) }
  Doc   -> b
  Parse -> b

-- ── Application reduction ─────────────────────────────────────────

reduceApp :: Int -> Env -> Expr -> Expr -> Expr
-- Quoted parameter: auto-quote the argument (don't evaluate it)
reduceApp d env (Lam p body) arg
  | isQuotedParam p =
    let realName = lamParamName p
        d' = d - 1
        argFV = exprFreeVars arg
    in if realName `Set.member` argFV
       then -- Alpha-rename to avoid capture
         let allNames = Set.unions [argFV, exprFreeVars body, Map.keysSet (envMap env)]
             fresh = freshName realName allNames
             body' = substExpr realName (Name fresh) body
             quoted = quoteExpr arg
         in reduceD d' (envInsert fresh quoted env) body'
       else let quoted = quoteExpr arg
            in reduceD d' (envInsert realName quoted env) body
reduceApp d env (Lam p body) arg =
  let d' = d - 1
      argFVs = exprFreeVars arg
  in if p `Set.member` argFVs
     -- Param name appears free in arg — alpha-rename to avoid capture
     then let allNames = Set.unions [argFVs, exprFreeVars body, Map.keysSet (envMap env)]
              fresh = freshName p allNames
              body' = substExpr p (Name fresh) body
              env' = envInsert fresh arg env
          in reduceD d' env' body'
     else let env' = envInsert p arg env
          in reduceD d' env' body
reduceApp _ _ (Record tag fields) arg =
  -- Auto-constructor application: extend record with positional field
  let idx = length fields
      fieldName = "_" <> T.pack (show idx)
      newField = Binding { bindName = fieldName, bindParams = []
                         , bindBody = arg, bindDomain = Value, bindPos = Nothing }
  in Record tag (fields ++ [newField])
-- Builtin: tag extracts the tag name from a record
reduceApp _ _ (Name "tag") (Record t _) = StringLit t
-- Builtin: fields returns list of field values
reduceApp _ _ (Name "fields") (Record _ bs) =
  listToCons [bindBody b | b <- bs]
-- Builtin: fieldNames returns list of field name strings
reduceApp _ _ (Name "fieldNames") (Record _ bs) =
  listToCons [StringLit (bindName b) | b <- bs]
-- Builtin: getField rec name — curried: (getField rec) returns a closure, applied to name does lookup
reduceApp d env (App (Name "getField") rec) (StringLit name) =
  case reduceD d env rec of
    Record _ bs -> case [bindBody b | b <- bs, bindName b == name] of
      (v:_) -> Record "Just" [Binding { bindName = "val", bindParams = []
                                      , bindBody = v, bindDomain = Value, bindPos = Nothing }]
      []    -> Record "Nothing" []
    r -> App (App (Name "getField") r) (StringLit name)
-- Builtin: setField rec name val — curried
reduceApp d env (App (App (Name "setField") rec) (StringLit name)) val =
  case reduceD d env rec of
    Record tag bs ->
      let updated = [if bindName b == name then b { bindBody = val } else b | b <- bs]
          found = any (\b -> bindName b == name) bs
          bs' = if found then updated
                else bs ++ [Binding { bindName = name, bindParams = []
                                    , bindBody = val, bindDomain = Value, bindPos = Nothing }]
      in Record tag bs'
    r -> App (App (App (Name "setField") r) (StringLit name)) val
-- Builtin: replace str old new
reduceApp _ _ (App (App (Name "replace") (StringLit s)) (StringLit old)) (StringLit new) =
  StringLit (T.replace old new s)
-- Builtin: slice str start end
reduceApp _ _ (App (App (Name "slice") (StringLit s)) (IntLit i)) (IntLit j) =
  StringLit (T.take (fromIntegral (j - i)) (T.drop (fromIntegral i) s))
-- Builtin: indexOf str substr
reduceApp _ _ (App (Name "indexOf") (StringLit s)) (StringLit sub) =
  case T.breakOn sub s of
    (before, match) | T.null match -> IntLit (-1)
                    | otherwise    -> IntLit (fromIntegral (T.length before))
-- Builtin: charAt str index
reduceApp _ _ (App (Name "charAt") (StringLit s)) (IntLit i)
  | i >= 0 && fromIntegral i < T.length s =
    Record "Just" [Binding { bindName = "val", bindParams = []
                           , bindBody = StringLit (T.singleton (T.index s (fromIntegral i)))
                           , bindDomain = Value, bindPos = Nothing }]
  | otherwise = Record "Nothing" []
-- Builtin: split str delim
reduceApp _ _ (App (Name "split") (StringLit s)) (StringLit delim) =
  let parts = if T.null delim
              then map (StringLit . T.singleton) (T.unpack s)
              else map StringLit (T.splitOn delim s)
  in listToCons parts
-- Builtin: join delim list
reduceApp _ _ (App (Name "join") (StringLit delim)) lst
  | Just strs <- consToStrings lst = StringLit (T.intercalate delim strs)
  where
    consToStrings (Record "Nil" _) = Just []
    consToStrings (Record "Cons" [hd, tl])
      | StringLit s <- bindBody hd = (s :) <$> consToStrings (bindBody tl)
    consToStrings _ = Nothing
reduceApp _ _ f x = App f x

-- Builtins that should fire before env lookup (by original name)
tryBuiltin1 :: Int -> Env -> Text -> Expr -> Maybe Expr
tryBuiltin1 d env "len" x = case reduceD d env x of
  StringLit s -> Just $ IntLit (fromIntegral (T.length s))
  _           -> Nothing  -- fall through to prelude len for lists
tryBuiltin1 d env "trim" x = case reduceD d env x of
  StringLit s -> Just $ StringLit (T.strip s)
  _           -> Nothing
tryBuiltin1 d env "toUpper" x = case reduceD d env x of
  StringLit s -> Just $ StringLit (T.toUpper s)
  _           -> Nothing
tryBuiltin1 d env "toLower" x = case reduceD d env x of
  StringLit s -> Just $ StringLit (T.toLower s)
  _           -> Nothing
tryBuiltin1 d env "toString" x = case reduceD d env x of
  IntLit n      -> Just $ StringLit (T.pack (show n))
  FloatLit f    -> Just $ StringLit (T.pack (show f))
  SizedInt n _ _ -> Just $ StringLit (T.pack (show n))
  SizedFloat f _ -> Just $ StringLit (T.pack (show f))
  StringLit s   -> Just $ StringLit s
  _             -> Nothing
tryBuiltin1 d env "toInt" x = case reduceD d env x of
  StringLit s -> case reads (T.unpack s) of
    ((n,""):_) -> Just $ Record "Just" [Binding { bindName = "val", bindParams = []
                   , bindBody = IntLit n, bindDomain = Value, bindPos = Nothing }]
    _          -> Just $ Record "Nothing" []
  FloatLit f  -> Just $ Record "Just" [Binding { bindName = "val", bindParams = []
                   , bindBody = IntLit (truncate f), bindDomain = Value, bindPos = Nothing }]
  SizedFloat f _ -> Just $ Record "Just" [Binding { bindName = "val", bindParams = []
                   , bindBody = IntLit (truncate f), bindDomain = Value, bindPos = Nothing }]
  _           -> Nothing
tryBuiltin1 d env "toFloat" x = case reduceD d env x of
  StringLit s -> case reads (T.unpack s) of
    ((f,""):_) -> Just $ Record "Just" [Binding { bindName = "val", bindParams = []
                   , bindBody = FloatLit f, bindDomain = Value, bindPos = Nothing }]
    _          -> Just $ Record "Nothing" []
  _           -> Nothing
tryBuiltin1 d env "abs" x = case reduceD d env x of
  IntLit n      -> Just $ IntLit (Prelude.abs n)
  FloatLit f    -> Just $ FloatLit (Prelude.abs f)
  SizedInt n w s -> Just $ SizedInt (Prelude.abs n) w s
  SizedFloat f w -> Just $ SizedFloat (Prelude.abs f) w
  _             -> Nothing
tryBuiltin1 d env "float" x = case reduceD d env x of
  IntLit n       -> Just $ FloatLit (fromIntegral n)
  SizedInt n _ _ -> Just $ FloatLit (fromInteger n)
  _              -> Nothing
tryBuiltin1 d env "round" x = case reduceD d env x of
  FloatLit f -> Just $ IntLit (Prelude.round f)
  _          -> Nothing
tryBuiltin1 d env "floor" x = case reduceD d env x of
  FloatLit f -> Just $ IntLit (Prelude.floor f)
  _          -> Nothing
tryBuiltin1 d env "ceil" x = case reduceD d env x of
  FloatLit f -> Just $ IntLit (Prelude.ceiling f)
  _          -> Nothing
tryBuiltin1 _ _ _ _ = Nothing

-- ── Binary operator reduction ─────────────────────────────────────

reduceBinOp :: Text -> Expr -> Expr -> Expr
-- Int × Int (handles both IntLit and SizedInt)
reduceBinOp "+"  l r | Just a <- intVal l, Just b <- intVal r = sizedIntResult (a + b) l r
reduceBinOp "-"  l r | Just a <- intVal l, Just b <- intVal r = sizedIntResult (a - b) l r
reduceBinOp "*"  l r | Just a <- intVal l, Just b <- intVal r = sizedIntResult (a * b) l r
reduceBinOp "/"  l r | Just a <- intVal l, Just b <- intVal r, b /= 0 = sizedIntResult (div a b) l r
reduceBinOp "%"  l r | Just a <- intVal l, Just b <- intVal r, b /= 0 = sizedIntResult (mod a b) l r
reduceBinOp "**" l r | Just a <- intVal l, Just b <- intVal r, b >= 0  = sizedIntResult (a ^ b) l r
-- Float × Float (handles both FloatLit and SizedFloat)
reduceBinOp "+"  l r | Just a <- floatVal l, Just b <- floatVal r = sizedFloatResult (a + b) l r
reduceBinOp "-"  l r | Just a <- floatVal l, Just b <- floatVal r = sizedFloatResult (a - b) l r
reduceBinOp "*"  l r | Just a <- floatVal l, Just b <- floatVal r = sizedFloatResult (a * b) l r
reduceBinOp "/"  l r | Just a <- floatVal l, Just b <- floatVal r, b /= 0 = sizedFloatResult (a / b) l r
-- String concat
reduceBinOp "+" (StringLit a) (StringLit b) = StringLit (a <> b)
-- Int comparisons
reduceBinOp "==" l r | Just a <- intVal l, Just b <- intVal r = IntLit (if a == b then 1 else 0)
reduceBinOp "/=" l r | Just a <- intVal l, Just b <- intVal r = IntLit (if a /= b then 1 else 0)
reduceBinOp "<"  l r | Just a <- intVal l, Just b <- intVal r = IntLit (if a < b then 1 else 0)
reduceBinOp ">"  l r | Just a <- intVal l, Just b <- intVal r = IntLit (if a > b then 1 else 0)
reduceBinOp "<=" l r | Just a <- intVal l, Just b <- intVal r = IntLit (if a <= b then 1 else 0)
reduceBinOp ">=" l r | Just a <- intVal l, Just b <- intVal r = IntLit (if a >= b then 1 else 0)
-- Float comparisons
reduceBinOp "==" l r | Just a <- floatVal l, Just b <- floatVal r = IntLit (if a == b then 1 else 0)
reduceBinOp "/=" l r | Just a <- floatVal l, Just b <- floatVal r = IntLit (if a /= b then 1 else 0)
reduceBinOp "<"  l r | Just a <- floatVal l, Just b <- floatVal r = IntLit (if a < b then 1 else 0)
reduceBinOp ">"  l r | Just a <- floatVal l, Just b <- floatVal r = IntLit (if a > b then 1 else 0)
reduceBinOp "<=" l r | Just a <- floatVal l, Just b <- floatVal r = IntLit (if a <= b then 1 else 0)
reduceBinOp ">=" l r | Just a <- floatVal l, Just b <- floatVal r = IntLit (if a >= b then 1 else 0)
-- String comparison
reduceBinOp "==" (StringLit a) (StringLit b) = IntLit (if a == b then 1 else 0)
reduceBinOp "/=" (StringLit a) (StringLit b) = IntLit (if a /= b then 1 else 0)
reduceBinOp "<"  (StringLit a) (StringLit b) = IntLit (if a < b then 1 else 0)
reduceBinOp ">"  (StringLit a) (StringLit b) = IntLit (if a > b then 1 else 0)
reduceBinOp "<=" (StringLit a) (StringLit b) = IntLit (if a <= b then 1 else 0)
reduceBinOp ">=" (StringLit a) (StringLit b) = IntLit (if a >= b then 1 else 0)
-- Cons
reduceBinOp ":" hd tl = Record "Cons" [mkBind "head" hd, mkBind "tail" tl]
-- Division by zero
reduceBinOp "/" l r | Just _ <- intVal l, Just 0 <- intVal r = Error "division by zero"
reduceBinOp "%" l r | Just _ <- intVal l, Just 0 <- intVal r = Error "division by zero"
reduceBinOp "/" l r | Just _ <- floatVal l, Just 0 <- floatVal r = Error "division by zero"
-- Type mismatches: string with arithmetic operators
reduceBinOp op (StringLit _) r | isArith op, Just _ <- intVal r   = Error (T.concat ["cannot ", opVerb op, " string"])
reduceBinOp op (StringLit _) r | isArith op, Just _ <- floatVal r = Error (T.concat ["cannot ", opVerb op, " string"])
reduceBinOp op l (StringLit _) | isArith op, Just _ <- intVal l   = Error (T.concat ["cannot ", opVerb op, " string"])
reduceBinOp op l (StringLit _) | isArith op, Just _ <- floatVal l = Error (T.concat ["cannot ", opVerb op, " string"])
-- Residual
reduceBinOp op l r = BinOp op l r

isArith :: Text -> Bool
isArith op = op `elem` ["+", "-", "*", "/", "%", "**"]

opVerb :: Text -> Text
opVerb "+" = "add"
opVerb "-" = "subtract from"
opVerb "*" = "multiply"
opVerb "/" = "divide"
opVerb "%" = "modulo"
opVerb "**" = "exponentiate"
opVerb _ = "operate on"

-- | Extract integer value from IntLit or SizedInt
intVal :: Expr -> Maybe Integer
intVal (IntLit n)       = Just n
intVal (SizedInt n _ _) = Just n
intVal _                = Nothing

-- | Extract float value from FloatLit or SizedFloat
floatVal :: Expr -> Maybe Double
floatVal (FloatLit d)    = Just d
floatVal (SizedFloat d _) = Just d
floatVal _               = Nothing

-- | Produce a sized int result preserving width/signedness
sizedIntResult :: Integer -> Expr -> Expr -> Expr
sizedIntResult v (SizedInt _ w1 s1) (SizedInt _ w2 s2) =
  let w = max w1 w2; s = s1 || s2
  in SizedInt (if s then clampSigned w v else clampUnsigned w v) w s
sizedIntResult v (SizedInt _ w s) _ = SizedInt (if s then clampSigned w v else clampUnsigned w v) w s
sizedIntResult v _ (SizedInt _ w s) = SizedInt (if s then clampSigned w v else clampUnsigned w v) w s
sizedIntResult v _ _ = IntLit v

-- | Produce a sized float result preserving width
sizedFloatResult :: Double -> Expr -> Expr -> Expr
sizedFloatResult v (SizedFloat _ w1) (SizedFloat _ w2) = SizedFloat v (max w1 w2)
sizedFloatResult v (SizedFloat _ w) _ = SizedFloat v w
sizedFloatResult v _ (SizedFloat _ w) = SizedFloat v w
sizedFloatResult v _ _ = FloatLit v

-- | Clamp to signed range
clampSigned :: Integer -> Integer -> Integer
clampSigned w v = let half = 2 ^ (w - 1) in ((v + half) `mod` (2 * half)) - half

-- | Clamp to unsigned range
clampUnsigned :: Integer -> Integer -> Integer
clampUnsigned w v = v `mod` (2 ^ w)

-- ── Field access ──────────────────────────────────────────────────

reduceFieldAccess :: Expr -> Text -> Expr
reduceFieldAccess (Record _ bs) field =
  case [bindBody b | b <- bs, bindName b == field] of
    (v:_) -> v
    []    -> -- Try positional: _0, _1, ...
      case T.stripPrefix "_" field >>= readMaybeInt of
        Just i | i < length bs -> bindBody (bs !! i)
        _ -> FieldAccess (Record "" bs) field
  where
    readMaybeInt t = case reads (T.unpack t) of
      [(n, "")] -> Just n
      _ -> Nothing
reduceFieldAccess (Namespace bs) field =
  case [bindBody b | b <- bs, bindName b == field] of
    (v:_) -> v
    []    -> FieldAccess (Namespace bs) field
reduceFieldAccess e field = FieldAccess e field

-- | Record update: base <- {overrides}
reduceRecordUpdate :: Expr -> Expr -> Expr
reduceRecordUpdate (Record tag bs) updates =
  let overrides = case updates of
        Record _ obs   -> obs
        Namespace obs  -> obs
        _              -> []
      overrideMap = Map.fromList [(bindName o, o) | o <- overrides]
      -- Replace existing fields in-place, then append new ones
      updated = map (\b -> case Map.lookup (bindName b) overrideMap of
                       Just o  -> o
                       Nothing -> b) bs
      existingNames = Set.fromList [bindName b | b <- bs]
      newFields = filter (\o -> not (bindName o `Set.member` existingNames)) overrides
  in Record tag (updated ++ newFields)
reduceRecordUpdate l r = BinOp "<-" l r

-- ── Case/pattern matching ─────────────────────────────────────────

reduceCase :: Int -> Env -> Expr -> [Alt] -> Expr
reduceCase _ _ scrut [] = Error $ "non-exhaustive pattern match on: " <> T.pack (prettyExpr 0 scrut)
reduceCase d env scrut (Alt pat mGuard body : rest) =
  case matchPat pat scrut of
    Nothing -> reduceCase d env scrut rest
    Just binds ->
      let env' = foldl (\e (n,v) -> envInsert n v e) env binds
      in case mGuard of
        Nothing -> reduceD d env' body
        Just g  ->
          let g' = forceThunk d env' (reduceD d env' g)
          in case g' of
            IntLit 0 -> reduceCase d env scrut rest
            IntLit _ -> reduceD d env' body
            _        -> reduceCase d env scrut rest

matchPat :: Pat -> Expr -> Maybe [(Text, Expr)]
matchPat PWild _ = Just []
matchPat (PVar v) e = Just [(v, e)]
matchPat (PLit (IntLit a)) (IntLit b)       | a == b = Just []
matchPat (PLit (IntLit a)) (SizedInt b _ _) | a == b = Just []
matchPat (PLit (FloatLit a)) (FloatLit b)   | a == b = Just []
matchPat (PLit (FloatLit a)) (SizedFloat b _) | a == b = Just []
matchPat (PLit (StringLit a)) (StringLit b) | a == b = Just []
matchPat (PRec tag fields) (Record rtag bs)
  | tag == rtag || T.null tag =
    let matchField (fname, fpat) =
          case [bindBody b | b <- bs, bindName b == fname] of
            (v:_) -> matchPat fpat v
            []    ->
              -- Positional fallback: _0, _1, ... map to Nth non-tag field
              case T.stripPrefix "_" fname >>= readMaybe . T.unpack of
                Just idx
                  | (idx :: Int) >= 0, idx < length bs -> matchPat fpat (bindBody (bs !! idx))
                _ -> Nothing
    in fmap concat (mapM matchField fields)
matchPat (PList [] Nothing) (Record "Nil" _) = Just []
matchPat (PList (p:ps) mrest) (Record "Cons" bs) = do
  hd <- case [bindBody b | b <- bs, bindName b == "head"] of
          (v:_) -> Just v
          []    -> Nothing
  tl <- case [bindBody b | b <- bs, bindName b == "tail"] of
          (v:_) -> Just v
          []    -> Nothing
  hbinds <- matchPat p hd
  tbinds <- matchPat (PList ps mrest) tl
  pure (hbinds ++ tbinds)
matchPat (PList [] (Just restVar)) e = Just [(restVar, e)]
matchPat _ _ = Nothing

-- ── Helpers ───────────────────────────────────────────────────────

-- | Wrap parameters as lambdas
wrapLambda :: [Text] -> Expr -> Expr
wrapLambda ps body = foldr Lam body ps

-- | Force a thunk (unwrap Thunk wrapper and reduce)
forceThunk :: Int -> Env -> Expr -> Expr
forceThunk d env (Thunk e) = forceThunk d env (reduceD d env e)
forceThunk _ _   e         = e

-- | Apply environment bindings to an expression without reducing it
-- (for capturing bindings in deferred thunks)
applyEnv :: Int -> Env -> Expr -> Expr
applyEnv _ _ e = e  -- simplified: thunks capture by closure in codegen

-- | List literal to Cons/Nil encoding
listToCons :: [Expr] -> Expr
listToCons []     = Record "Nil" []
listToCons (x:xs) = Record "Cons" [mkBind "head" x, mkBind "tail" (listToCons xs)]

-- | Convert a Cons/Nil list back to a Haskell list
consToList :: Expr -> Maybe [Expr]
consToList (Record "Nil" _) = Just []
consToList (Record "Cons" bs) = do
  h <- fieldLookup "head" bs
  t <- fieldLookup "tail" bs
  rest <- consToList t
  Just (h : rest)
consToList _ = Nothing

-- | Expand union declarations
expandUnion :: Binding -> [Binding]
expandUnion b = case bindBody b of
  Namespace ctors
    | all (\c -> let n = bindName c in not (T.null n) && isUpper (T.head n)) ctors ->
      b : ctors
  _ -> [b]

-- | Merge open function definitions (later defs extend earlier match clauses)
mergeOpenDefs :: [Binding] -> [Binding]
mergeOpenDefs [] = []
mergeOpenDefs bindings =
  let (_, result) = foldl merge (Map.empty, []) bindings
  in reverse result
  where
    merge (seen, acc) b =
      let name = bindName b
      in case Map.lookup name seen of
        Nothing -> (Map.insert name b seen, b : acc)
        Just old ->
          let oldBody = wrapLambda (bindParams old) (bindBody old)
              newBody = wrapLambda (bindParams b) (bindBody b)
              merged = case chainBodies newBody oldBody of
                Just body' -> b { bindBody = body', bindParams = [] }
                Nothing    -> b
          in (Map.insert name merged seen,
              map (\x -> if bindName x == name then merged else x) acc)

chainBodies :: Expr -> Expr -> Maybe Expr
chainBodies new old = case (new, old) of
  (Lam p1 inner1, Lam p2 inner2)
    | p1 == p2  -> Lam p1 <$> chainBodies inner1 inner2
    | otherwise -> Lam p1 <$> chainBodies inner1 (substExpr p2 (Name p1) inner2)
  (Case scrut newAlts, Case _ oldAlts)
    | not (hasWildcard newAlts) ->
      Just $ Case scrut (newAlts ++ oldAlts)
  (Case scrut newAlts, _)
    | not (hasWildcard newAlts) ->
      Just $ Case scrut (newAlts ++ [Alt PWild Nothing (App old scrut)])
  _ -> Nothing
  where
    hasWildcard [] = False
    hasWildcard as = case altPat (last as) of
      PWild  -> True
      PVar _ -> True
      _      -> False

-- ── Free variables ────────────────────────────────────────────────

exprFreeVars :: Expr -> Set.Set Text
exprFreeVars (IntLit _)      = Set.empty
exprFreeVars (FloatLit _)    = Set.empty
exprFreeVars (SizedInt {})   = Set.empty
exprFreeVars (SizedFloat {}) = Set.empty
exprFreeVars (StringLit _)   = Set.empty
exprFreeVars (Name n)        = Set.singleton n
exprFreeVars (BinOp _ l r)   = Set.union (exprFreeVars l) (exprFreeVars r)
exprFreeVars (App f x)       = Set.union (exprFreeVars f) (exprFreeVars x)
exprFreeVars (Lam p b)       = Set.delete p (exprFreeVars b)
exprFreeVars (Record _ bs)   = Set.unions (map bindingFreeVars bs)
exprFreeVars (FieldAccess e _) = exprFreeVars e
exprFreeVars (Namespace bs)  = bindingsFreeVars bs
exprFreeVars (Case s alts)   = Set.union (exprFreeVars s) (Set.unions (map altFreeVars alts))
exprFreeVars (Thunk e)       = exprFreeVars e
exprFreeVars (ListLit es)    = Set.unions (map exprFreeVars es)
exprFreeVars (With e bs)     = Set.union (bindingsFreeVars bs) (exprFreeVars e)
exprFreeVars (Import _)      = Set.empty
exprFreeVars (Quote e)       = exprFreeVars e
exprFreeVars (Splice e)      = exprFreeVars e
exprFreeVars (CFunction {})  = Set.empty
exprFreeVars (Error _)       = Set.empty

bindingFreeVars :: Binding -> Set.Set Text
bindingFreeVars b =
  let bodyFVs = exprFreeVars (bindBody b)
      paramSet = Set.fromList (bindParams b)
  in Set.difference bodyFVs paramSet

bindingsFreeVars :: [Binding] -> Set.Set Text
bindingsFreeVars bs =
  let defined = Set.fromList (map bindName bs)
      allFvs = Set.unions (map bindingFreeVars bs)
  in Set.difference allFvs defined

altFreeVars :: Alt -> Set.Set Text
altFreeVars (Alt pat mGuard body) =
  let pvs = patVars pat
      bodyFvs = exprFreeVars body
      guardFvs = maybe Set.empty exprFreeVars mGuard
  in Set.difference (Set.union bodyFvs guardFvs) pvs

patVars :: Pat -> Set.Set Text
patVars PWild        = Set.empty
patVars (PVar v)     = Set.singleton v
patVars (PLit _)     = Set.empty
patVars (PRec _ fs)  = Set.unions [patVars p | (_, p) <- fs]
patVars (PList ps mr) = Set.unions (map patVars ps) `Set.union` maybe Set.empty Set.singleton mr

-- ── Substitution ──────────────────────────────────────────────────

substExpr :: Text -> Expr -> Expr -> Expr
substExpr var replacement = go
  where
    go (Name n) | n == var  = replacement
                | otherwise = Name n
    go (IntLit n)    = IntLit n
    go (FloatLit d)  = FloatLit d
    go e@(SizedInt {})  = e
    go e@(SizedFloat {}) = e
    go (StringLit s) = StringLit s
    go (BinOp op l r) = BinOp op (go l) (go r)
    go (App f x)     = App (go f) (go x)
    go (Lam p b)
      | p == var  = Lam p b  -- shadowed
      | otherwise = Lam p (go b)
    go (Record t bs)   = Record t (map goBind bs)
    go (FieldAccess e f) = FieldAccess (go e) f
    go (Namespace bs)  = Namespace (map goBind bs)
    go (Case s alts)   = Case (go s) (map goAlt alts)
    go (Thunk e)       = Thunk (go e)
    go (ListLit es)    = ListLit (map go es)
    go (With e bs)     = With (go e) (map goBind bs)
    go e@(Import _)    = e
    go (Quote e)       = Quote (go e)
    go (Splice e)      = Splice (go e)
    go e@(CFunction {}) = e
    go e@(Error _)     = e

    goBind b = b { bindBody = go (bindBody b) }

    goAlt (Alt p g body)
      | var `Set.member` patVars p = Alt p g body  -- shadowed by pattern
      | otherwise = Alt p (fmap go g) (go body)

-- | Generate a fresh name not in the given set
freshName :: Text -> Set.Set Text -> Text
freshName base used = head [n | i <- [0::Int ..],
                            let n = base <> T.pack (show i),
                            not (Set.member n used)]

-- ── Quote / Splice ──────────────────────────────────────────────

-- | Convert an expression to its AST record representation (without evaluating)
quoteExpr :: Expr -> Expr
quoteExpr (IntLit n)     = Record "Int" [mkBind "val" (IntLit n)]
quoteExpr (FloatLit f)   = Record "Float" [mkBind "val" (FloatLit f)]
quoteExpr (SizedInt n w s) = Record "SizedInt" [mkBind "val" (IntLit n),
                                                 mkBind "width" (IntLit w),
                                                 mkBind "signed" (IntLit (if s then 1 else 0))]
quoteExpr (SizedFloat d w) = Record "SizedFloat" [mkBind "val" (FloatLit d),
                                                    mkBind "width" (IntLit w)]
quoteExpr (StringLit s)  = Record "String" [mkBind "val" (StringLit s)]
quoteExpr (Name n)       = Record "Var" [mkBind "name" (StringLit n)]
quoteExpr (BinOp op l r) = Record "Op"
  [ mkBind "op" (StringLit op)
  , mkBind "left" (quoteExpr l)
  , mkBind "right" (quoteExpr r) ]
quoteExpr (App f x)      = Record "App"
  [ mkBind "fn" (quoteExpr f)
  , mkBind "arg" (quoteExpr x) ]
quoteExpr (Lam p b)      = Record "Lam"
  [ mkBind "param" (StringLit p)
  , mkBind "body" (quoteExpr b) ]
quoteExpr (Record t bs)  = Record "Rec"
  [ mkBind "tag" (StringLit t)
  , mkBind "fields" (listToCons [Record "" [mkBind "name" (StringLit (bindName b)),
                                            mkBind "val" (quoteExpr (bindBody b))] | b <- bs]) ]
quoteExpr (Case s alts)  = Record "Case"
  [ mkBind "scrutinee" (quoteExpr s)
  , mkBind "alts" (listToCons [quoteExpr (altBody a) | a <- alts]) ]
quoteExpr (ListLit es)   = Record "List"
  [ mkBind "items" (listToCons (map quoteExpr es)) ]
quoteExpr (Thunk e)      = Record "Thunk" [mkBind "body" (quoteExpr e)]
quoteExpr (FieldAccess e f) = Record "Access"
  [ mkBind "expr" (quoteExpr e)
  , mkBind "field" (StringLit f) ]
quoteExpr (With body bs) = Record "With"
  [ mkBind "body" (quoteExpr body)
  , mkBind "bindings" (listToCons [Record "" [mkBind "name" (StringLit (bindName b)),
                                              mkBind "val" (quoteExpr (bindBody b))] | b <- bs]) ]
quoteExpr (Namespace bs) = Record "Let"
  [ mkBind "bindings" (listToCons [Record "" [mkBind "name" (StringLit (bindName b)),
                                              mkBind "val" (quoteExpr (bindBody b))] | b <- bs]) ]
quoteExpr (Quote e)      = Record "Quote" [mkBind "body" (quoteExpr e)]
quoteExpr (Splice e)     = Record "Splice" [mkBind "body" (quoteExpr e)]
quoteExpr (CFunction {}) = Record "CFunc" []
quoteExpr (Error msg)    = Record "Error" [mkBind "msg" (StringLit msg)]
quoteExpr e              = Record "Unknown" [mkBind "val" e]

-- | Convert an AST record representation back to an expression
-- | Try to convert an AST record back to an expression (Maybe version)
spliceExprM :: Expr -> Maybe Expr
spliceExprM (Record "Int" bs)    = fieldLookup "val" bs
spliceExprM (Record "Float" bs)  = fieldLookup "val" bs
spliceExprM (Record "SizedInt" bs) = do
  IntLit val    <- fieldLookup "val" bs
  IntLit width  <- fieldLookup "width" bs
  IntLit signed <- fieldLookup "signed" bs
  Just (SizedInt val width (signed /= 0))
spliceExprM (Record "SizedFloat" bs) = do
  FloatLit val <- fieldLookup "val" bs
  IntLit width <- fieldLookup "width" bs
  Just (SizedFloat val width)
spliceExprM (Record "String" bs) = fieldLookup "val" bs
spliceExprM (Record "Var" bs)    = case fieldLookup "name" bs of
  Just (StringLit n) -> Just (Name n)
  _                  -> Nothing
spliceExprM (Record "Op" bs)     =
  case (fieldLookup "op" bs, fieldLookup "left" bs, fieldLookup "right" bs) of
    (Just (StringLit op), Just l, Just r) -> Just $ BinOp op (spliceExpr l) (spliceExpr r)
    _ -> Nothing
spliceExprM (Record "App" bs)    =
  case (fieldLookup "fn" bs, fieldLookup "arg" bs) of
    (Just f, Just x) -> Just $ App (spliceExpr f) (spliceExpr x)
    _ -> Nothing
spliceExprM (Record "Lam" bs)    =
  case (fieldLookup "param" bs, fieldLookup "body" bs) of
    (Just (StringLit p), Just b) -> Just $ Lam p (spliceExpr b)
    _ -> Nothing
spliceExprM (Record "Splice" bs) =
  case fieldLookup "body" bs of
    Just b -> Just $ Splice (spliceExpr b)
    _      -> Nothing
spliceExprM (Record "Thunk" bs) =
  case fieldLookup "body" bs of
    Just b -> Just $ Thunk (spliceExpr b)
    _      -> Nothing
spliceExprM (Record "Access" bs) =
  case (fieldLookup "expr" bs, fieldLookup "field" bs) of
    (Just e, Just (StringLit f)) -> Just $ FieldAccess (spliceExpr e) f
    _ -> Nothing
spliceExprM (Record "With" bs)   = do
  body   <- fieldLookup "body" bs
  fields <- fieldLookup "bindings" bs >>= spliceBindings
  Just (With (spliceExpr body) fields)
spliceExprM (Record "Let" bs)    = do
  fields <- fieldLookup "bindings" bs >>= spliceBindings
  Just (Namespace fields)
spliceExprM (Record "Quote" bs) =
  case fieldLookup "body" bs of
    Just b -> Just $ Quote (spliceExpr b)
    _      -> Nothing
spliceExprM (Record "List" bs) =
  case fieldLookup "items" bs of
    Just items -> case consToList items of
      Just es -> Just $ listToCons (map spliceExpr es)
      Nothing -> Just items
    _          -> Nothing
spliceExprM (Record "Rec" bs)  = do
  StringLit tag <- fieldLookup "tag" bs
  fields <- fieldLookup "fields" bs >>= spliceBindings
  Just (Record tag fields)
spliceExprM (Record "Case" bs) = do
  scrut <- fieldLookup "scrutinee" bs
  altsE <- fieldLookup "alts" bs
  case consToList altsE of
    Just es -> Just $ Case (spliceExpr scrut) [Alt PWild Nothing (spliceExpr e) | e <- es]
    Nothing -> Nothing
spliceExprM (Record "Error" bs) =
  case fieldLookup "msg" bs of
    Just (StringLit msg) -> Just $ Error msg
    _                    -> Nothing
-- Raw values pass through
spliceExprM e@(IntLit _)    = Just e
spliceExprM e@(FloatLit _)  = Just e
spliceExprM e@(StringLit _) = Just e
spliceExprM e@(Name _)      = Just e
spliceExprM e@(ListLit _)   = Just e
spliceExprM _               = Nothing

-- | Convert binding records back to Binding list
spliceBindings :: Expr -> Maybe [Binding]
spliceBindings expr = case consToList expr of
  Just es -> mapM spliceField es
  Nothing -> Nothing
  where
    spliceField (Record _ bs) = do
      StringLit name <- fieldLookup "name" bs
      val <- fieldLookup "val" bs
      Just (mkBind name (spliceExpr val))
    spliceField _ = Nothing

-- | Convert an AST record back to an expression (infallible, for recursive use)
spliceExpr :: Expr -> Expr
spliceExpr e = case spliceExprM e of
  Just r  -> r
  Nothing -> e

-- | Look up a field by name in a record's bindings
fieldLookup :: Text -> [Binding] -> Maybe Expr
fieldLookup name bs = case [bindBody b | b <- bs, bindName b == name] of
  (v:_) -> Just v
  []    -> Nothing

-- ══════════════════════════════════════════════════════════════════
-- ── Type domain: checking :: annotations within the unified reducer
-- ══════════════════════════════════════════════════════════════════

-- | Internal type representation for checking
data MiType
  = TInt | TFloat | TStr
  | TFun MiType MiType
  | TRecord Text [(Text, MiType)]
  | TUnion Text [Text]
  | TVar Text
  | TAny
  deriving (Show, Eq)

type TypeEnv = Map.Map Text MiType

-- | Pretty-print a type for error messages
prettyType :: MiType -> Text
prettyType TInt     = "Int"
prettyType TFloat   = "Float"
prettyType TStr     = "Str"
prettyType (TVar v) = v
prettyType TAny     = "?"
prettyType (TFun a b) = prettyType a <> " : " <> prettyType b
prettyType (TUnion name _) = name
prettyType (TRecord tag fields) =
  let t = if T.null tag then "" else tag <> " "
      fs = T.intercalate "; " [n <> " = " <> prettyType ty | (n, ty) <- fields]
  in t <> "{" <> fs <> "}"

-- | Check if a type variable occurs in a type (for occurs check)
occursIn :: Text -> MiType -> Bool
occursIn v (TVar v')       = False  -- TVar-to-TVar is just aliasing, not recursive
occursIn v (TFun a b)      = occursInAny v a || occursInAny v b
occursIn v (TRecord _ fs)  = any (occursInAny v . snd) fs
occursIn _ _               = False

-- Helper that includes TVar matches (for sub-expression checking)
occursInAny :: Text -> MiType -> Bool
occursInAny v (TVar v')       = v == v'
occursInAny v (TFun a b)      = occursInAny v a || occursInAny v b
occursInAny v (TRecord _ fs)  = any (occursInAny v . snd) fs
occursInAny _ _               = False

-- | Built-in type environment for native functions
builtinTypeEnv :: TypeEnv
builtinTypeEnv = Map.fromList
  [ ("len",        TFun TAny TInt)
  , ("strlen",     TFun TStr TInt)
  , ("slice",      TFun TAny (TFun TInt (TFun TInt TAny)))
  , ("fields",     TFun TAny (TUnion "List" ["Nil", "Cons"]))
  , ("fieldNames", TFun TAny (TUnion "List" ["Nil", "Cons"]))
  , ("tag",        TFun TAny TStr)
  , ("getField",   TFun TAny (TFun TStr TAny))
  , ("setField",   TFun TAny (TFun TStr (TFun TAny TAny)))
  ]

-- | Convert a type annotation expression to MiType
exprToType :: TypeEnv -> Expr -> MiType
exprToType _ (Name "Num")   = TInt
exprToType _ (Name "Int")   = TInt
exprToType _ (Name "UInt")  = TInt
exprToType _ (Name "Str")   = TStr
exprToType _ (Name "Float") = TFloat
exprToType _ (Name "String") = TStr
exprToType env (Name n)
  | not (T.null n) && isLower' (T.head n) = TVar n
  | otherwise = Map.findWithDefault TAny n env
  where isLower' c = c >= 'a' && c <= 'z'
exprToType env (BinOp ":" l r) = TFun (exprToType env l) (exprToType env r)
exprToType env (Record tag fields) =
  TRecord tag [(bindName b, exprToType env (bindBody b)) | b <- fields]
exprToType env (Namespace bindings) =
  TRecord "" [(bindName b, exprToType env (bindBody b)) | b <- bindings]
exprToType env (With (Name tag) fields)
  | not (T.null tag) && isUpper (T.head tag) =
    TRecord tag [(bindName b, exprToType env (bindBody b)) | b <- fields]
exprToType env (With body bs) =
  let env' = foldl (\acc b -> Map.insert (bindName b) (exprToType acc (bindBody b)) acc) env bs
  in exprToType env' body
exprToType env (App f x) =
  case exprToType env f of
    TFun _ ret -> ret
    _          -> TAny
exprToType _ (Lam _ _) = TAny
exprToType _ _ = TAny

-- | Detect if a binding is a union declaration
detectUnion :: Binding -> Maybe (Text, [(Text, Int)])
detectUnion b = case bindBody b of
  Namespace ctors
    | not (null ctors)
    , all (\c -> let n = bindName c in not (T.null n) && isUpper (T.head n)) ctors ->
      Just (bindName b, [(bindName c, length (bindParams c)) | c <- ctors])
  _ -> Nothing

-- | Build a TypeEnv from annotations and bindings.
-- Prelude operator annotations are included for operand checking.
-- But prelude annotations for names redefined by user code are skipped.
collectTypeEnv :: Env -> [Binding] -> TypeEnv
collectTypeEnv _env bindings =
  let -- User value bindings: names redefined in non-prelude source
      userValues = Set.fromList [bindName b
                                | b <- bindings
                                , bindDomain b == Value || bindDomain b == Lazy
                                , not (isPreludePos (bindPos b))]
      -- All annotations, tagged with whether they're from prelude
      allAnnots = [(bindName b, bindBody b, isPreludePos (bindPos b))
                  | b <- bindings, bindDomain b == Type]
      annotMap = Map.fromList [(n, (e, fromPrelude)) | (n, e, fromPrelude) <- allAnnots]
  in foldl (go userValues annotMap) builtinTypeEnv bindings
  where
    go userValues annotMap tenv b =
      case Map.lookup (bindName b) annotMap of
        -- Skip prelude annotation when user has redefined the name
        Just (_, True) | Set.member (bindName b) userValues -> inferOrDetect tenv b
        -- Use the annotation otherwise
        Just (tyExpr, _) ->
          Map.insert (bindName b) (exprToType tenv tyExpr) tenv
        Nothing -> inferOrDetect tenv b
    inferOrDetect tenv b =
      case detectUnion b of
        Just (uname, ctors) ->
          let tags = map fst ctors
              utype = TUnion uname tags
              tenv' = Map.insert uname utype tenv
              ctorType (tag, 0) = TRecord tag []
              ctorType (tag, n) = foldr (\_ t -> TFun TAny t) (TRecord tag []) [1..n]
          in foldl (\e ct -> Map.insert (fst ct) (ctorType ct) e) tenv' ctors
        Nothing ->
          let bodyTy = inferExpr tenv b
          in case bodyTy of
               TAny -> tenv
               _    -> Map.insert (bindName b) bodyTy tenv

-- | Infer the type of a binding's body (with params wrapped as function types)
inferExpr :: TypeEnv -> Binding -> MiType
inferExpr tenv b =
  let params = bindParams b
      -- Infer parameter types from how they're used in the body
      paramTys = map (inferParamFromBody tenv (bindBody b)) params
      tenv' = foldl (\e (p, ty) -> Map.insert p ty e) tenv (zip params paramTys)
      bodyTy = inferExprE tenv' (bindBody b)
  in foldr (\ty acc -> TFun ty acc) bodyTy paramTys

-- | Infer a parameter's type from its usage in the function body.
-- Looks at BinOp operand positions and App argument positions to find
-- constraints. For example, in `2 * x`, x is constrained to Int.
inferParamFromBody :: TypeEnv -> Expr -> Text -> MiType
inferParamFromBody tenv body param = findConstraint body
  where
    findConstraint (BinOp op l r)
      | isArithOp op || isCmpOp op =
        case (isParam l, isParam r) of
          (True, False) -> constrainType (inferExprE tenv r)
          (False, True) -> constrainType (inferExprE tenv l)
          _             -> merge (findConstraint l) (findConstraint r)
      | otherwise = merge (findConstraint l) (findConstraint r)
    findConstraint (App f x)
      | isParam x =
        case inferExprE tenv f of
          TFun argTy _ | argTy /= TAny -> argTy
          _ -> findConstraint f
      | otherwise = merge (findConstraint f) (findConstraint x)
    findConstraint (Lam _ e) = findConstraint e
    findConstraint (Case _ alts) =
      foldl (\acc (Alt _ _ b) -> merge acc (findConstraint b)) TAny alts
    findConstraint (With e bs) =
      merge (findConstraint e)
            (foldl (\acc b -> merge acc (findConstraint (bindBody b))) TAny bs)
    findConstraint _ = TAny

    isParam (Name n) = n == param
    isParam _        = False

    isArithOp op = op `elem` ["+", "-", "*", "/", "%", "**"]
    isCmpOp op   = op `elem` ["==", "/=", "<", ">", "<=", ">="]

    constrainType TInt   = TInt
    constrainType TFloat = TFloat
    constrainType TStr   = TStr
    constrainType _      = TAny

    merge TAny t = t
    merge t    _ = t

-- | Infer the type of an expression
inferExprE :: TypeEnv -> Expr -> MiType
inferExprE _ (IntLit _)    = TInt
inferExprE _ (FloatLit _)  = TFloat
inferExprE _ (StringLit _) = TStr
inferExprE tenv (Name n) = Map.findWithDefault TAny n tenv
inferExprE tenv (BinOp op l r)
  | op `elem` ["-", "*", "/", "^", "%", "**"] =
    case (inferExprE tenv l, inferExprE tenv r) of
      (TFloat, _) -> TFloat
      (_, TFloat) -> TFloat
      _           -> TInt
  | op == "+" =
    case (inferExprE tenv l, inferExprE tenv r) of
      (TStr, _)   -> TStr
      (_, TStr)   -> TStr
      (TFloat, _) -> TFloat
      (_, TFloat) -> TFloat
      _           -> TInt
  | op `elem` ["==", "/=", "<", ">", "<=", ">="] = TInt
  | op == ":" =
    let ht = inferExprE tenv l
    in TRecord "Cons" [("head", ht), ("tail", TAny)]
  | op == ">>" =
    -- f >> g: input of f → output of g
    let lt = inferExprE tenv l
        rt = inferExprE tenv r
    in case (lt, rt) of
      (TFun a _, TFun _ d) -> TFun a d
      (TAny,     TFun _ d) -> TFun TAny d
      (TFun a _, TAny)     -> TFun a TAny
      _                    -> TAny
  | op == "<<" =
    -- f << g: input of g → output of f
    let lt = inferExprE tenv l
        rt = inferExprE tenv r
    in case (lt, rt) of
      (TFun _ b, TFun c _) -> TFun c b
      (TAny,     TFun c _) -> TFun c TAny
      (TFun _ b, TAny)     -> TFun TAny b
      _                    -> TAny
  | op == "|>" =
    -- x |> f: return type of f
    let rt = inferExprE tenv r
    in case rt of
      TFun _ ret -> ret
      _          -> TAny
  | otherwise = TAny
inferExprE tenv (App f x) =
  case inferExprE tenv f of
    TFun _ ret -> ret
    -- If f is a Name, check the type env for its return type
    _ -> case f of
      Name n -> case Map.lookup n tenv of
                  Just (TFun _ ret) -> ret
                  _ -> TAny
      _ -> TAny
inferExprE tenv (Lam p body) =
  -- Infer param type from body usage (e.g., 2*x → x :: Int)
  let inferred = inferParamFromBody tenv body p
      paramTy = if inferred /= TAny then inferred
                else Map.findWithDefault TAny p tenv
      tenv' = Map.insert p paramTy tenv
  in TFun paramTy (inferExprE tenv' body)
inferExprE tenv (Record tag fields) =
  TRecord tag [(bindName b, inferExprE tenv (bindBody b)) | b <- fields]
inferExprE tenv (FieldAccess e field) =
  case inferExprE tenv e of
    TRecord _ fields ->
      case lookup field fields of
        Just t  -> t
        Nothing -> TAny
    _ -> TAny
inferExprE tenv (With e bs) =
  case e of
    Name tag | not (T.null tag) && isUpper (T.head tag) ->
      TRecord tag [(bindName b, inferExprE tenv (bindBody b)) | b <- bs]
    _ -> let tenv' = foldl (\acc b -> Map.insert (bindName b) (inferExprE acc (bindBody b)) acc) tenv bs
         in inferExprE tenv' e
inferExprE tenv (ListLit es) =
  case es of
    (e:_) -> TRecord "Cons" [("head", inferExprE tenv e), ("tail", TAny)]
    []    -> TRecord "Nil" []
inferExprE tenv (Case _ alts) =
  let altTypes = [inferExprE tenv (altBody a) | a <- alts]
      concreteTypes = filter (/= TAny) altTypes
  in case concreteTypes of
       (t:_) -> t
       []    -> TAny
inferExprE _ (Thunk e) = inferExprE Map.empty e
inferExprE _ _ = TAny

-- | Check type compatibility with type variable threading
checkCompatWith :: Map.Map Text MiType -> Maybe SrcPos -> Text -> MiType -> MiType
               -> (Map.Map Text MiType, [Warning])
checkCompatWith subst0 pos name expected actual = go subst0 expected actual
  where
    resolve subst (TVar v) =
      case Map.lookup v subst of
        Just t@(TVar v') | v' /= v -> resolve subst t
        Just t@(TVar _) -> t
        Just t           -> t
        Nothing          -> TVar v
    resolve _ t = t

    go subst TAny _ = (subst, [])
    go subst _ TAny = (subst, [])
    go subst (TVar v) act =
      let resolved = resolve subst (TVar v)
      in case resolved of
           TVar v' | v' == v ->
             if occursIn v act
             then (subst, [mkErr ("infinite type: " <> v <> " occurs in " <> prettyType act)])
             else (Map.insert v act subst, [])
           _ -> go subst resolved act
    go subst _ (TVar _) = (subst, [])
    go subst TInt TInt     = (subst, [])
    go subst TFloat TFloat = (subst, [])
    go subst TStr TStr     = (subst, [])
    go subst TInt TFloat   = (subst, [])   -- numeric compat
    go subst TFloat TInt   = (subst, [])
    go subst (TFun ea er) (TFun aa ar) =
      let (subst', errs1) = go subst ea aa
          (subst'', errs2) = go subst' er ar
      in (subst'', errs1 ++ errs2)
    go subst (TUnion _ tags) (TRecord atag _)
      | atag `elem` tags = (subst, [])
      | otherwise = (subst, [mkErr ("expected one of " <> T.intercalate "/" tags <> ", got " <> showTag atag)])
    go subst (TRecord etag _) (TUnion _ tags)
      | etag `elem` tags = (subst, [])  -- a Cons value is a valid List
    go subst (TUnion n1 _) (TUnion n2 _)
      | n1 == n2 = (subst, [])
    go subst (TRecord etag efields) (TRecord atag afields)
      | etag /= "" && atag /= "" && etag /= atag =
        (subst, [mkErr ("tag mismatch: expected " <> etag <> ", got " <> atag)])
      | otherwise =
        foldl (\(s, errs) (ename, etype) ->
          case lookup ename afields of
            Just atype -> let (s', e') = go s etype atype in (s', errs ++ e')
            Nothing    -> (s, errs ++ [mkErr ("missing field: " <> ename)])
          ) (subst, []) efields
    go subst e a =
      if e == a then (subst, [])
      else (subst, [mkErr ("expected " <> prettyType e <> ", got " <> prettyType a)])

    showTag t = if T.null t then "(untagged)" else t
    mkErr msg = TypeWarning pos name msg

-- | Check a binding's body against its LOCAL type annotation
checkTypeBinding :: TypeEnv -> Map.Map Text Expr -> Binding -> [Warning]
checkTypeBinding tenv localAnnots b =
  let (annotErrs, tenv') = case Map.lookup (bindName b) localAnnots of
        Nothing -> ([], tenv)
        Just tyExpr ->
          case bindBody b of
            IntLit 0 | null (bindParams b) -> ([], tenv)
            _ ->
              let expectedType = exprToType tenv tyExpr
                  (peeledEnv, bodyType) = peelParams tenv (bindParams b) expectedType
              in (checkExprAgainst peeledEnv (bindPos b) (bindName b) bodyType (bindBody b), peeledEnv)
      operandErrs = checkOperands tenv' (bindPos b) (bindName b) (bindBody b)
  in annotErrs ++ operandErrs

-- | Peel function type layers for parameters
peelParams :: TypeEnv -> [Text] -> MiType -> (TypeEnv, MiType)
peelParams tenv [] ty = (tenv, ty)
peelParams tenv (p:ps) (TFun argTy retTy) =
  peelParams (Map.insert p argTy tenv) ps retTy
peelParams tenv (_:ps) ty = peelParams tenv ps ty

-- | Bidirectional check: push expected type into expression
checkExprAgainst :: TypeEnv -> Maybe SrcPos -> Text -> MiType -> Expr -> [Warning]
checkExprAgainst tenv pos name (TFun argTy retTy) (Lam p body) =
  let tenv' = Map.insert p argTy tenv
  in checkExprAgainst tenv' pos name retTy body
checkExprAgainst tenv pos name expected expr =
  let actual = inferExprE tenv expr
  in snd (checkCompatWith Map.empty pos name expected actual)

-- | Walk an expression and report operand type mismatches
checkOperands :: TypeEnv -> Maybe SrcPos -> Text -> Expr -> [Warning]
checkOperands tenv pos name expr0 = snd (go 0 expr0)
  where
    go c (BinOp op l r) =
      let lt = inferExprE tenv l
          rt = inferExprE tenv r
          (opErrs, c1) = case Map.lookup op tenv of
            Just opTy ->
              let (freshTy, c') = freshenType c opTy
              in case freshTy of
                TFun argL (TFun argR _) ->
                  let (subst1, errs1) = checkCompatWith Map.empty pos name argL lt
                      (_,      errs2) = checkCompatWith subst1    pos name argR rt
                  in (errs1 ++ errs2, c')
                _ -> ([], c')
            _ -> ([], c)
          (c2, errs1) = go c1 l
          (c3, errs2) = go c2 r
      in (c3, opErrs ++ errs1 ++ errs2)
    go c (App f x) =
      let fty0 = inferExprE tenv f
          xty = inferExprE tenv x
          (fty, c1) = freshenType c fty0
          argErrs = case fty of
            TFun argTy _ -> snd (checkCompatWith Map.empty pos name argTy xty)
            _ -> []
          (c2, errs1) = go c1 f
          (c3, errs2) = go c2 x
      in (c3, argErrs ++ errs1 ++ errs2)
    go c (Lam _ body)   = go c body
    go c (With body bs)  =
      let (c1, e1) = go c body
          (c2, e2) = foldl (\(ci, ei) b -> let (ci', ei') = go ci (bindBody b) in (ci', ei ++ ei')) (c1, []) bs
      in (c2, e1 ++ e2)
    go c (Case scrut alts) =
      let (c1, bodyErrs) = foldl (\(ci, ei) (Alt _ _ body) -> let (ci', ei') = go ci body in (ci', ei ++ ei')) (c, []) alts
          exhaustErrs = checkExhaustiveness tenv pos name scrut alts
      in (c1, bodyErrs ++ exhaustErrs)
    go c (Record _ bs) =
      foldl (\(ci, ei) b -> let (ci', ei') = go ci (bindBody b) in (ci', ei ++ ei')) (c, []) bs
    go c (Thunk e)       = go c e
    go c _               = (c, [])

-- | Freshen type variables with a counter to give each call site unique type vars
freshenType :: Int -> MiType -> (MiType, Int)
freshenType c ty =
  let tvars = collectTVars ty
  in if Set.null tvars then (ty, c)
     else let pairs = zip (Set.toList tvars) [c..]
              subst = Map.fromList [(v, TVar (v <> "$" <> T.pack (show i))) | (v, i) <- pairs]
              c' = c + length pairs
          in (applySubst subst ty, c')

collectTVars :: MiType -> Set.Set Text
collectTVars (TVar v)       = Set.singleton v
collectTVars (TFun a b)     = Set.union (collectTVars a) (collectTVars b)
collectTVars (TRecord _ fs) = Set.unions [collectTVars t | (_, t) <- fs]
collectTVars _              = Set.empty

applySubst :: Map.Map Text MiType -> MiType -> MiType
applySubst s (TVar v)       = Map.findWithDefault (TVar v) v s
applySubst s (TFun a b)     = TFun (applySubst s a) (applySubst s b)
applySubst s (TRecord t fs) = TRecord t [(n, applySubst s ty) | (n, ty) <- fs]
applySubst _ t              = t

-- | Check pattern exhaustiveness for case expressions over known union types
checkExhaustiveness :: TypeEnv -> Maybe SrcPos -> Text -> Expr -> [Alt] -> [Warning]
checkExhaustiveness tenv pos name scrut alts =
  -- If any alt has a wildcard/variable pattern, it's exhaustive
  if any isWildcardAlt alts then []
  else case inferExprE tenv scrut of
    TUnion uname tags ->
      let coveredTags = Set.fromList (concatMap altTags alts)
          missingTags = filter (`Set.notMember` coveredTags) tags
      in if null missingTags then []
         else [GeneralWarning pos (name <> ": non-exhaustive pattern match on " <> uname
               <> ", missing: " <> T.intercalate ", " missingTags)]
    _ -> []
  where
    isWildcardAlt (Alt PWild _ _) = True
    isWildcardAlt (Alt (PVar _) _ _) = True
    isWildcardAlt _ = False
    altTags (Alt (PRec tag _) _ _) = [tag]
    altTags (Alt (PLit (StringLit tag)) _ _) = [tag]
    altTags _ = []

-- | Run type checking on all value bindings.
-- Called at the end of evalBindings to integrate type checking into the unified reducer.
typeCheckBindings :: Env -> [Binding] -> [Warning]
typeCheckBindings env bindings =
  let -- Local type annotations: only from user code, not prelude
      localAnnots = Map.fromList [(bindName b, bindBody b)
                                 | b <- bindings, bindDomain b == Type
                                 , not (isPreludePos (bindPos b))]
      tenv = collectTypeEnv env bindings
      -- Only check value/lazy bindings, not annotation bindings
      valueBinds = filter (\b -> bindDomain b == Value || bindDomain b == Lazy) bindings
      origWarns = concatMap (checkTypeBinding tenv localAnnots) valueBinds
      -- Also check reduced bodies for operand errors: after inlining,
      -- composition type mismatches (e.g. 2 * toString(...)) become visible.
      reducedWarns = concatMap (\b ->
        case envLookup (bindName b) env of
          Just val | not (isPreludePos (bindPos b)) ->
            checkOperands tenv (bindPos b) (bindName b) val
          _ -> []) valueBinds
  in origWarns ++ reducedWarns

isPreludePos :: Maybe SrcPos -> Bool
isPreludePos (Just pos) = "<prelude>" `isPrefixOf` srcFile pos
isPreludePos Nothing    = False

-- ══════════════════════════════════════════════════════════════════
-- ── Trait domain: checking :~ annotations within the unified reducer
-- ══════════════════════════════════════════════════════════════════

type Effects = Set.Set Text
type TraitEnv = Map.Map Text Effects

-- | World capability map (leaf effects)
worldCapabilities :: [([Text], Effects)]
worldCapabilities =
  [ (["io", "println"],         Set.singleton "console")
  , (["io", "print"],           Set.singleton "console")
  , (["io", "readLine"],        Set.singleton "console")
  , (["fs", "read", "file"],    Set.singleton "fs.read")
  , (["fs", "read", "exists"],  Set.singleton "fs.read")
  , (["fs", "write", "file"],   Set.singleton "fs.write")
  , (["fs", "write", "append"], Set.singleton "fs.write")
  , (["fs", "write", "remove"], Set.singleton "fs.write")
  , (["process", "exec"],       Set.singleton "process")
  , (["process", "exit"],       Set.singleton "process")
  , (["getEnv"],                Set.singleton "env")
  , (["argv"],                  Set.empty)
  ]

-- | Derived alias map: prefix → union of matching effects
worldAliases :: Map.Map Text Effects
worldAliases =
  Map.fromListWith Set.union
    [ (T.intercalate "." prefix, effs)
    | (path, effs) <- worldCapabilities
    , prefix <- tail (inits path)
    ]
  where
    inits :: [a] -> [[a]]
    inits []     = [[]]
    inits (x:xs) = [] : map (x:) (inits xs)

-- | Collect trait declarations into effect sets
collectTraitEnv :: Env -> [Binding] -> TraitEnv
collectTraitEnv env bindings =
  let raw = [(bindName b, ty) | b <- bindings, Just ty <- [Map.lookup (bindName b) (envTraits env)]]
  in foldl (\tenv (name, expr) -> Map.insert name (resolveEffects tenv expr) tenv) Map.empty raw

-- | Resolve a trait expression to a set of effect names
resolveEffects :: TraitEnv -> Expr -> Effects
resolveEffects tenv (ListLit es)     = Set.unions (map (resolveEffects tenv) es)
resolveEffects tenv (Record "Nil" _) = Set.empty
resolveEffects tenv (Record "Cons" bs) =
  let hd = case [bindBody b | b <- bs, bindName b == "head"] of
              [e] -> resolveEffects tenv e
              _   -> Set.empty
      tl = case [bindBody b | b <- bs, bindName b == "tail"] of
              [e] -> resolveEffects tenv e
              _   -> Set.empty
  in Set.union hd tl
resolveEffects tenv (Name n) = resolveEffectName tenv n
resolveEffects tenv (FieldAccess e f) = resolveEffectName tenv (dotName e f)
resolveEffects _ _ = Set.empty

resolveEffectName :: TraitEnv -> Text -> Effects
resolveEffectName tenv n =
  case Map.lookup n tenv of
    Just effects -> effects
    Nothing -> case Map.lookup n worldAliases of
      Just effects -> effects
      Nothing      -> Set.singleton n

dotName :: Expr -> Text -> Text
dotName (Name n) f        = n <> "." <> f
dotName (FieldAccess e f1) f2 = dotName e f1 <> "." <> f2
dotName _ f               = f

-- | Check trait annotations on bindings
traitCheckBindings :: Env -> [Binding] -> [Warning]
traitCheckBindings env bindings =
  let traitEnv = collectTraitEnv env bindings
      bindingMap = Map.fromList [(bindName b, b) | b <- bindings]
      -- Only check value/lazy bindings
      valueBinds = filter (\b -> bindDomain b == Value || bindDomain b == Lazy) bindings
  in concatMap (checkTrait traitEnv bindingMap valueBinds) valueBinds

-- | Check a single binding against its trait annotation.
-- Unannotated bindings default to pure (:~ []), except "main" and "_main"
-- which are implicitly granted all capabilities.
checkTrait :: TraitEnv -> Map.Map Text Binding -> [Binding] -> Binding -> [Warning]
checkTrait traitEnv bindingMap allBindings b =
  let name = bindName b
      -- main and _main (auto-wrapper for main-less files) are implicitly unconstrained
      isMain = name == "main" || name == "_main"
      declared = case Map.lookup name traitEnv of
                   Just d  -> d
                   Nothing | isMain    -> Set.empty  -- placeholder; skip check below
                           | otherwise -> Set.empty  -- pure by default
      skip = isMain && not (Map.member name traitEnv)
  in if skip
     then []
     else let inferred = inferBindingEffects traitEnv bindingMap allBindings b
              excess = Set.difference inferred declared
          in if Set.null excess
             then []
             else [TraitWarning (bindPos b) (bindName b)
                    ("effect violation: " <> name
                     <> (if Map.member name traitEnv
                         then " declared :~ " <> formatEffects declared
                         else " has no :~ annotation (assumed pure)")
                     <> " but uses " <> formatEffects excess)]

-- | Infer effects of a binding body
inferBindingEffects :: TraitEnv -> Map.Map Text Binding -> [Binding] -> Binding -> Effects
inferBindingEffects traitEnv bindingMap _allBindings b =
  let body = wrapLambda (bindParams b) (bindBody b)
      fvs = exprFreeVars body
      directEffects = Set.unions
        [ case Map.lookup fv traitEnv of
            Just effects -> effects
            Nothing      -> Set.empty
        | fv <- Set.toList fvs ]
      -- Build a set of world-tainted names from bindings that reference world
      taintedNames = inferTaintedNames bindingMap
      worldEffects = inferWorldEffects taintedNames (bindBody b)
      transitiveEffects = inferTransitive traitEnv bindingMap taintedNames (Set.singleton (bindName b)) fvs
      -- Detect effects from passing world/tainted args to functions
      argPassEffects = inferArgPassEffects taintedNames bindingMap (bindBody b)
  in Set.unions [directEffects, worldEffects, transitiveEffects, argPassEffects]

-- | Build transitive world-taint set: names that reference world directly or indirectly
inferTaintedNames :: Map.Map Text Binding -> Set.Set Text
inferTaintedNames bindingMap =
  let directTainted = Map.keysSet $ Map.filter (\b ->
        let fvs = exprFreeVars (wrapLambda (bindParams b) (bindBody b))
        in "world" `Set.member` fvs) bindingMap
  in fixpoint directTainted
  where
    fixpoint current =
      let next = Map.foldlWithKey' (\acc name b ->
            let fvs = exprFreeVars (wrapLambda (bindParams b) (bindBody b))
            in if not (Set.member name acc) && any (`Set.member` acc) (Set.toList fvs)
               then Set.insert name acc
               else acc
            ) current bindingMap
      in if next == current then current else fixpoint next

-- | Detect effects from passing world/tainted values as arguments.
-- When `f world` or `f taintedVar` appears, inline f's body with the param substituted
-- and check what world effects that produces.
inferArgPassEffects :: Set.Set Text -> Map.Map Text Binding -> Expr -> Effects
inferArgPassEffects tainted bindingMap = go
  where
    isWorldArg (Name "world") = True
    isWorldArg (Name n)       = Set.member n tainted
    isWorldArg _              = False

    go (App (Name fn) arg)
      | isWorldArg arg =
        case Map.lookup fn bindingMap of
          Just callee ->
            let params = bindParams callee
                -- Substitute the first param with "world" to detect effects
                body' = case params of
                  (p:_) -> substParam p (Name "world") (bindBody callee)
                  []    -> bindBody callee
            in inferWorldEffects tainted body'
          Nothing -> Set.empty
    go (App f x) = Set.union (go f) (go x)
    go (Lam _ body) = go body
    go (BinOp _ l r) = Set.union (go l) (go r)
    go (With body bs) = Set.union (go body) (Set.unions (map (go . bindBody) bs))
    go (Case _ alts) = Set.unions [go b | Alt _ _ b <- alts]
    go (Record _ bs) = Set.unions (map (go . bindBody) bs)
    go (Namespace bs) = Set.unions (map (go . bindBody) bs)
    go (Thunk e) = go e
    go (ListLit es) = Set.unions (map go es)
    go _ = Set.empty

-- | Simple expression substitution for trait analysis: replace Name occurrences
substParam :: Text -> Expr -> Expr -> Expr
substParam var replacement = go
  where
    go (Name n) | n == var = replacement
    go (App f x) = App (go f) (go x)
    go (Lam p body) | p == var = Lam p body  -- shadowed
                     | otherwise = Lam p (go body)
    go (BinOp op l r) = BinOp op (go l) (go r)
    go (FieldAccess e f) = FieldAccess (go e) f
    go (With body bs) = With (go body) [b { bindBody = go (bindBody b) } | b <- bs]
    go (Case s alts) = Case (go s) [Alt p g (go body) | Alt p g body <- alts]
    go (Record t bs) = Record t [b { bindBody = go (bindBody b) } | b <- bs]
    go (Namespace bs) = Namespace [b { bindBody = go (bindBody b) } | b <- bs]
    go (Thunk e) = Thunk (go e)
    go (ListLit es) = ListLit (map go es)
    go e = e

-- | Walk expression for world.* field access chains
-- Now also detects access through world-tainted names (w := world; w.io.println)
inferWorldEffects :: Set.Set Text -> Expr -> Effects
inferWorldEffects tainted = go
  where
    go (FieldAccess e f) =
      case worldChain e f of
        Just path ->
          let key = T.intercalate "." path
          in case Map.lookup key worldAliases of
               Just effs -> effs
               Nothing   -> Set.empty
        Nothing -> go e
    go (App f x)       = Set.union (go f) (go x)
    go (BinOp _ l r)   = Set.union (go l) (go r)
    go (Lam _ body)    = go body
    go (With body bs)  = Set.union (go body) (Set.unions (map (go . bindBody) bs))
    go (Case s alts)   = Set.union (go s) (Set.unions [go b | Alt _ _ b <- alts])
    go (Record _ bs)   = Set.unions (map (go . bindBody) bs)
    go (Namespace bs)  = Set.unions (map (go . bindBody) bs)
    go (Thunk e)       = go e
    go (ListLit es)    = Set.unions (map go es)
    go (Splice e)      = go e
    go _               = Set.empty

    worldChain :: Expr -> Text -> Maybe [Text]
    worldChain (Name "world") field = Just [field]
    worldChain (Name n) field
      | Set.member n tainted = Just [field]
    worldChain (FieldAccess inner f) field =
      case worldChain inner f of
        Just path -> Just (path ++ [field])
        Nothing   -> Nothing
    worldChain _ _ = Nothing

-- | Follow calls through non-annotated functions for transitive effects
inferTransitive :: TraitEnv -> Map.Map Text Binding -> Set.Set Text -> Set.Set Text -> Set.Set Text -> Effects
inferTransitive traitEnv bindingMap tainted visited fvs =
  let toFollow = [ b | fv <- Set.toList fvs
                     , not (Map.member fv traitEnv)
                     , not (Set.member fv visited)
                     , Just b <- [Map.lookup fv bindingMap] ]
  in Set.unions
       [ let body = wrapLambda (bindParams b) (bindBody b)
             innerFVs = exprFreeVars body
             visited' = Set.insert (bindName b) visited
             direct = Set.unions [ case Map.lookup fv traitEnv of
                                     Just effects -> effects
                                     Nothing      -> Set.empty
                                 | fv <- Set.toList innerFVs ]
             worldEffs = inferWorldEffects tainted (bindBody b)
             transitive = inferTransitive traitEnv bindingMap tainted visited' innerFVs
         in Set.unions [direct, worldEffs, transitive]
       | b <- toFollow ]

formatEffects :: Effects -> Text
formatEffects es
  | Set.null es = "[]"
  | otherwise   = "[" <> T.intercalate ", " (Set.toList es) <> "]"
