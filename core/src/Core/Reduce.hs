{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Core.Reduce
  ( reduce, Env, emptyEnv
  , Warning(..), warnings
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isUpper)
import Data.Graph (stronglyConnComp, SCC(..))
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
isResidual (IntLit _)    = False
isResidual (FloatLit _)  = False
isResidual (StringLit _) = False
isResidual (Record _ _)  = False
isResidual _             = True

isConcrete :: Expr -> Bool
isConcrete (IntLit _)    = True
isConcrete (FloatLit _)  = True
isConcrete (StringLit _) = True
isConcrete (Lam _ _)     = True
isConcrete (Record _ bs) = all (isConcrete . bindBody) bs
isConcrete _             = False

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

reduce :: Env -> Expr -> Expr
reduce env e = reduceD maxDepth env e

reduceD :: Int -> Env -> Expr -> Expr
reduceD _ _ e@(IntLit _)    = e
reduceD _ _ e@(FloatLit _)  = e
reduceD _ _ e@(StringLit _) = e
reduceD _ _ e@(Error _)     = e

reduceD d env (Name n)
  | d <= 0    = Name n
  | otherwise =
    case envLookup n env of
      Just (Name m) | m == n -> Name n  -- self-reference; don't recurse
      Just val@(Lam _ _) -> val
      Just val -> reduceD (d - 1) env val
      Nothing
        | isOperatorName n -> Lam "_a" (Lam "_b" (BinOp n (Name "_a") (Name "_b")))
        | not (T.null n) && isUpper (T.head n) ->
            -- Auto-constructor: bare uppercase name becomes zero-arg record
            Record n []
        | otherwise -> Name n

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
  case collectApp (App f x) of
    (Name n, args) | envIsRec n env ->
      let args' = map (reduceD d env) args
      in if all isConcrete args' && d >= minRecDepth
         then case envLookup n env of
                Just fn -> foldl (reduceApp d env) fn args'
                Nothing -> foldl App (Name n) args'
         else foldl App (Name n) args'
    _ ->
      let f' = reduceD d env f
          x' = reduceD d env x
      in reduceApp d env f' x'

reduceD d env (Lam p b) =
  -- Only alpha-rename if the param shadows an env key (fast check).
  -- The full free-var check is expensive for large envs; this is a safe
  -- over-approximation (rename more than strictly needed, but never miss).
  let pn = p
  in if pn `Map.member` envMap env
     then let allNames = Set.union (Map.keysSet (envMap env)) (exprFreeVars b)
              fresh = freshName pn allNames
              b' = substExpr pn (Name fresh) b
              env' = envDelete fresh env
          in Lam fresh (reduceD d env' b')
     else let env' = envDelete pn env
          in Lam p (reduceD d env' b)

reduceD d env (With body bindings) =
  let env' = evalBindings d env bindings
      body' = reduceD d env' body
      bs' = map (reduceBind d env') bindings
      keepBinding b = isResidual (bindBody b) || envIsImpure (bindName b) env'
      residualBs = filter keepBinding bs'
  in if null residualBs then body' else With body' residualBs

reduceD d env (Record tag bindings) =
  Record tag (map (reduceBind d env) bindings)

reduceD d env (FieldAccess e field) =
  let e' = forceThunk d env (reduceD d env e)
  in reduceFieldAccess e' field

reduceD d env (Namespace bindings) =
  let expanded = concatMap expandUnion bindings
      merged = mergeOpenDefs expanded
      env' = evalBindings d env expanded
      bs' = map (reduceBind d env') merged
  in Namespace bs'

reduceD d env (Case scrut alts) =
  let scrut' = forceThunk d env (reduceD d env scrut)
  in if isResidual scrut'
     then Case scrut' (map (\(Alt p g b) ->
            let env' = Set.foldl' (\e v -> envDelete v e) env (patVars p)
            in Alt p (fmap (reduceD d env') g) (reduceD d env' b)) alts)
     else reduceCase d env scrut' alts

reduceD d env (Thunk body) = Thunk (applyEnv d env body)

reduceD d env (ListLit es) = listToCons (map (reduceD d env) es)

reduceD _ _ (Error msg) = Error msg

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
  in foldl (evalSCC d) env1 sccs

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
evalAnnotation :: Int -> Env -> Binding -> Env
evalAnnotation d env b = case bindDomain b of
  Type  -> let ty = reduceD d env (bindBody b)
           in envInsertType (bindName b) ty env
  Trait -> let tr = reduceD d env (bindBody b)
           in envInsertTrait (bindName b) tr env
  Doc   -> env  -- docs are informational; no effect on reduction
  Parse -> env  -- parse config handled pre-parse; no effect on reduction
  _     -> env  -- shouldn't happen

-- | Evaluate one SCC group of value bindings
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
  in if isConcrete val
     then envInsert name val env1
     else env1

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
  Type  -> b { bindBody = reduceD d env (bindBody b) }
  Trait -> b { bindBody = reduceD d env (bindBody b) }
  Doc   -> b
  Parse -> b

-- ── Application reduction ─────────────────────────────────────────

reduceApp :: Int -> Env -> Expr -> Expr -> Expr
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
reduceApp _ _ f x = App f x

-- ── Binary operator reduction ─────────────────────────────────────

reduceBinOp :: Text -> Expr -> Expr -> Expr
-- Int ops
reduceBinOp "+" (IntLit a) (IntLit b)  = IntLit (a + b)
reduceBinOp "-" (IntLit a) (IntLit b)  = IntLit (a - b)
reduceBinOp "*" (IntLit a) (IntLit b)  = IntLit (a * b)
reduceBinOp "/" (IntLit a) (IntLit b)  | b /= 0 = IntLit (div a b)
reduceBinOp "%" (IntLit a) (IntLit b)  | b /= 0 = IntLit (mod a b)
reduceBinOp "**" (IntLit a) (IntLit b) | b >= 0  = IntLit (a ^ b)
-- Float ops
reduceBinOp "+" (FloatLit a) (FloatLit b) = FloatLit (a + b)
reduceBinOp "-" (FloatLit a) (FloatLit b) = FloatLit (a - b)
reduceBinOp "*" (FloatLit a) (FloatLit b) = FloatLit (a * b)
reduceBinOp "/" (FloatLit a) (FloatLit b) | b /= 0 = FloatLit (a / b)
-- String concat
reduceBinOp "+" (StringLit a) (StringLit b) = StringLit (a <> b)
-- Comparisons
reduceBinOp "==" (IntLit a) (IntLit b)     = IntLit (if a == b then 1 else 0)
reduceBinOp "/=" (IntLit a) (IntLit b)     = IntLit (if a /= b then 1 else 0)
reduceBinOp "<"  (IntLit a) (IntLit b)     = IntLit (if a < b then 1 else 0)
reduceBinOp ">"  (IntLit a) (IntLit b)     = IntLit (if a > b then 1 else 0)
reduceBinOp "<=" (IntLit a) (IntLit b)     = IntLit (if a <= b then 1 else 0)
reduceBinOp ">=" (IntLit a) (IntLit b)     = IntLit (if a >= b then 1 else 0)
reduceBinOp "==" (FloatLit a) (FloatLit b) = IntLit (if a == b then 1 else 0)
reduceBinOp "/=" (FloatLit a) (FloatLit b) = IntLit (if a /= b then 1 else 0)
reduceBinOp "<"  (FloatLit a) (FloatLit b) = IntLit (if a < b then 1 else 0)
reduceBinOp ">"  (FloatLit a) (FloatLit b) = IntLit (if a > b then 1 else 0)
reduceBinOp "<=" (FloatLit a) (FloatLit b) = IntLit (if a <= b then 1 else 0)
reduceBinOp ">=" (FloatLit a) (FloatLit b) = IntLit (if a >= b then 1 else 0)
reduceBinOp "==" (StringLit a) (StringLit b) = IntLit (if a == b then 1 else 0)
reduceBinOp "/=" (StringLit a) (StringLit b) = IntLit (if a /= b then 1 else 0)
-- String comparison
reduceBinOp "<"  (StringLit a) (StringLit b) = IntLit (if a < b then 1 else 0)
reduceBinOp ">"  (StringLit a) (StringLit b) = IntLit (if a > b then 1 else 0)
reduceBinOp "<=" (StringLit a) (StringLit b) = IntLit (if a <= b then 1 else 0)
reduceBinOp ">=" (StringLit a) (StringLit b) = IntLit (if a >= b then 1 else 0)
-- Cons
reduceBinOp ":" hd tl = Record "Cons" [mkBind "head" hd, mkBind "tail" tl]
-- Residual
reduceBinOp op l r = BinOp op l r

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
reduceFieldAccess e field = FieldAccess e field

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
matchPat (PLit (FloatLit a)) (FloatLit b)   | a == b = Just []
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
    go e@(Error _)     = e

    goBind b
      | bindName b == var = b
      | otherwise = b { bindBody = go (bindBody b) }

    goAlt (Alt p g body)
      | var `Set.member` patVars p = Alt p g body  -- shadowed by pattern
      | otherwise = Alt p (fmap go g) (go body)

-- | Generate a fresh name not in the given set
freshName :: Text -> Set.Set Text -> Text
freshName base used = head [n | i <- [0::Int ..],
                            let n = base <> T.pack (show i),
                            not (Set.member n used)]
