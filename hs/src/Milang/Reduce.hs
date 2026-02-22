{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Milang.Reduce (reduce, Env, emptyEnv, envImpure, warnings, Warning(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust)
import Data.Char (isUpper)
import Data.Graph (stronglyConnComp, SCC(..))
import Milang.Syntax

-- | Reduction environment: bindings + set of recursive (cyclic) names
data Env = Env
  { envMap :: !(Map.Map Text Expr)
  , envRec :: !(Set.Set Text)       -- names from CyclicSCC groups
  , envImpure :: !(Set.Set Text)    -- world-tainted names (auto-monad spine)
  } deriving (Show)

emptyEnv :: Env
emptyEnv = Env Map.empty Set.empty Set.empty

-- Env helpers
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

-- | Check if an expression is world-tainted (references any impure name)
isWorldTainted :: Env -> Expr -> Bool
isWorldTainted env expr =
  let fvs = exprFreeVars expr
  in "world" `Set.member` fvs || any (`envIsImpure` env) (Set.toList fvs)

-- | Maximum recursion depth for reduction (prevents infinite unrolling)
maxReduceDepth :: Int
maxReduceDepth = 128

-- | Minimum depth to enter recursive unrolling; below this, leave call residual
-- to avoid producing broken expressions when depth runs out mid-evaluation
minRecursiveDepth :: Int
minRecursiveDepth = 8

-- | Check if an expression is residual (not a concrete value)
isResidual :: Expr -> Bool
isResidual (IntLit _)    = False
isResidual (FloatLit _)  = False
isResidual (StringLit _) = False
isResidual (Record _ _)  = False
isResidual _             = True

-- | Check if an expression is a concrete value (suitable for recursive unrolling)
isConcrete :: Expr -> Bool
isConcrete (IntLit _)    = True
isConcrete (FloatLit _)  = True
isConcrete (StringLit _) = True
isConcrete (Lam _ _)     = True   -- functions are values
isConcrete (Record _ bs) = all (isConcrete . bindBody) bs
isConcrete _             = False

-- | Peel an App chain to get the head and all arguments (left to right)
collectApp :: Expr -> (Expr, [Expr])
collectApp (App f x) = let (h, args) = collectApp f in (h, args ++ [x])
collectApp e         = (e, [])

-- | Reduce an expression as far as possible given the environment.
-- Fully-known expressions become literals. Partially-known expressions
-- remain as residual AST fragments.
reduce :: Env -> Expr -> Expr
reduce env e = reduceD maxReduceDepth env e

-- | Depth-limited reduction.  When depth hits 0, return the expression
-- as residual (stop unrolling recursive definitions).
reduceD :: Int -> Env -> Expr -> Expr
reduceD _ _   e@(IntLit _)    = e
reduceD _ _   e@(FloatLit _)  = e
reduceD _ _   e@(StringLit _) = e

reduceD d env (Name n)
  | d <= 0    = Name n
  | otherwise =
    case envLookup n env of
      Just val@(Lam _ _) -> val  -- function value: return directly, reduce at call site
      Just val -> reduceD d env val
      Nothing
        -- Operator-as-function: (+) etc. become \a b -> a + b
        | isOperatorName n -> Lam "_a" (Lam "_b" (BinOp n (Name "_a") (Name "_b")))
        | otherwise -> Name n

reduceD d env (BinOp op l r) =
  let l' = forceThunkD d env (reduceD d env l)
      r' = forceThunkD d env (reduceD d env r)
      result = reduceBinOp op l' r'
  in case result of
    -- If reduceBinOp left it as a residual BinOp, try user-defined operator
    BinOp op' _ _ | op' == op ->
      case envLookup op env of
        Just fn -> reduceD d env (App (App fn l') r')
        Nothing -> result
    _ -> result

reduceD d env (App f x) =
  -- Concrete-args gate: for recursive function calls, only unroll if ALL args
  -- are concrete AND we have enough depth budget to complete at least one cycle
  case collectApp (App f x) of
    (Name n, args) | envIsRec n env ->
      let args' = map (reduceD d env) args
      in if all isConcrete args' && d >= minRecursiveDepth
         then case envLookup n env of
                Just fn -> foldl (reduceAppD d env) fn args'
                Nothing -> foldl App (Name n) args'
         else foldl App (Name n) args'  -- leave recursive call residual
    _ ->
      let f' = reduceD d env f
          x' = reduceD d env x
      in reduceAppD d env f' x'

reduceD d env (Lam p b) =
  -- Check if any env value has p as a free variable (capture risk)
  let envFVs = Set.unions (map exprFreeVars (Map.elems (envMap env)))
  in if p `Set.member` envFVs
     then -- Alpha-rename the lambda parameter to avoid capture
       let allNames = Set.unions [envFVs, exprFreeVars b, Map.keysSet (envMap env)]
           fresh = freshName p allNames
           b' = substExpr p (Name fresh) b
           env' = envDelete fresh env
       in Lam fresh (reduceD d env' b')
     else
       let env' = envDelete p env
       in Lam p (reduceD d env' b)

reduceD d env (With body bindings) =
  let env' = evalBindingsD d env bindings
      body' = reduceD d env' body
      bs' = map (reduceBindD d env') bindings
      -- Keep a binding if it's residual (not fully reduced) OR if it's impure
      -- (world-tainted). Impure bindings must be preserved even if they reduce
      -- to concrete values, because they represent side effects that must execute.
      keepBinding b = isResidual (bindBody b) || envIsImpure (bindName b) env'
      residualBs = filter keepBinding bs'
  in if null residualBs then body' else With body' residualBs

reduceD d env (Record tag bindings) =
  let bs' = map (reduceBindD d env) bindings
  in Record tag bs'

reduceD d env (FieldAccess e field) =
  let e' = forceThunkD d env (reduceD d env e)
  in reduceFieldAccess e' field

reduceD d env (Namespace bindings) =
  let env' = evalBindingsD d env bindings
      bs'  = map (reduceBindD d env') bindings
  in Namespace bs'

reduceD d env (Case scrut alts) =
  let scrut' = forceThunkD d env (reduceD d env scrut)
  in if isResidual scrut'
       then Case scrut' (map (\(Alt p g b) ->
              let env' = Set.foldl' (\e v -> envDelete v e) env (patVars p)
              in Alt p (fmap (reduceD d env') g) (reduceD d env' b)) alts)
       else reduceCaseD d env scrut' alts

reduceD _ _ e@(CFunction {}) = e  -- C FFI: irreducible

reduceD _ _ (Quote body) =
  quoteExpr body  -- quote captures the syntax, not the value

reduceD d env (Splice body) =
  let body' = reduceD d env body
  in case unquoteExpr body' of
       Just expr -> reduceD d env expr
       Nothing   -> Splice body'  -- residual: can't splice non-AST value

reduceD d env (Thunk body) = Thunk (applyEnvD d env body)  -- defer evaluation but capture bindings

reduceD d env (ListLit es) = listToCons (map (reduceD d env) es)

-- | Check if a name looks like an operator (composed of operator chars)
isOperatorName :: Text -> Bool
isOperatorName t = not (T.null t) && T.all (`elem` ("+-*/^<>=!&|@%?:" :: String)) t

-- ── Binding evaluation ────────────────────────────────────────────

-- Evaluate a sequence of bindings, extending the environment.
-- Strict bindings are reduced immediately; lazy bindings are deferred.
-- Self-referencing (recursive) bindings are NOT added to the env — they stay
-- as residual Names so the reducer doesn't try to inline them infinitely.
evalBindings :: Env -> [Binding] -> Env
evalBindings = evalBindingsD maxReduceDepth

evalBindingsD :: Int -> Env -> [Binding] -> Env
evalBindingsD d env bindings =
  let expanded = concatMap expandUnion bindings
      bindNames = Set.fromList [bindName b | b <- expanded]
      nodes = [ (b, bindName b,
                 Set.toList $ Set.intersection bindNames
                   (exprFreeVars (wrapLambda (bindParams b) (bindBody b))))
               | b <- expanded ]
      sccs = stronglyConnComp nodes
  in foldl (evalSCCD d) env sccs

-- | Expand union declarations: if a binding's body is a Namespace of all-uppercase
-- bindings, inject those as additional top-level bindings
expandUnion :: Binding -> [Binding]
expandUnion b = case bindBody b of
  Namespace ctors
    | all (\c -> let n = bindName c in not (T.null n) && isUpper (T.head n)) ctors ->
      b : ctors  -- keep the Shape binding + add Circle, Rect, etc.
  _ -> [b]

-- Evaluate one SCC group
evalSCCD :: Int -> Env -> SCC Binding -> Env
evalSCCD d env (AcyclicSCC b) =
  let name = bindName b
      body = wrapLambda (bindParams b) (bindBody b)
      val = if bindLazy b
            then bindBody b
            else reduceD d env body
      -- Track world-taintedness: if the body references world or any impure name,
      -- mark this binding as impure in the environment (auto-monad spine).
      env1 = if isWorldTainted env body then envMarkImpure name env else env
      -- Only inline value forms (literals, lambdas, concrete records).
      -- Non-concrete expressions (App, BinOp, FieldAccess etc.) stay as
      -- named bindings so the With block evaluates them once at runtime,
      -- preserving sharing and preventing duplication of side effects.
  in if isConcrete val
     then envInsert name val env1
     else env1
evalSCCD _ env (CyclicSCC bs) =
  -- Mutually recursive group: insert as unreduced lambdas, marked recursive.
  -- The concrete-args gate in reduceD (App) prevents exponential unrolling
  -- at definition sites, while still allowing unrolling at call sites with
  -- all-concrete arguments.
  let insertBind e b =
        let body = wrapLambda (bindParams b) (bindBody b)
            name = bindName b
            e1 = if isWorldTainted e body then envMarkImpure name e else e
        in e1 { envMap = Map.insert name body (envMap e1)
              , envRec = Set.insert name (envRec e1) }
  in foldl insertBind env bs

-- Wrap a body in lambdas for its parameters: f x y = e → \x -> \y -> e
wrapLambda :: [Text] -> Expr -> Expr
wrapLambda []     body = body
wrapLambda (p:ps) body = Lam p (wrapLambda ps body)

-- Reduce a single binding's body
reduceBind :: Env -> Binding -> Binding
reduceBind = reduceBindD maxReduceDepth

reduceBindD :: Int -> Env -> Binding -> Binding
reduceBindD d env b =
  b { bindBody = reduceD d env (wrapLambda (bindParams b) (bindBody b))
    , bindParams = []  -- params absorbed into lambdas
    }

-- ── Binary operator reduction ─────────────────────────────────────

reduceBinOp :: Text -> Expr -> Expr -> Expr
-- Cons operator: h : t → Record "Cons" [head=h, tail=t]
reduceBinOp ":" h t =
  Record "Cons" [ Binding "head" False [] h Nothing Nothing Nothing Nothing Nothing
                , Binding "tail" False [] t Nothing Nothing Nothing Nothing Nothing ]
-- Int × Int
reduceBinOp "+"  (IntLit a) (IntLit b) = IntLit (a + b)
reduceBinOp "-"  (IntLit a) (IntLit b) = IntLit (a - b)
reduceBinOp "*"  (IntLit a) (IntLit b) = IntLit (a * b)
reduceBinOp "/"  (IntLit a) (IntLit b)
  | b /= 0     = IntLit (a `div` b)
reduceBinOp "%"  (IntLit a) (IntLit b)
  | b /= 0     = IntLit (a `mod` b)
reduceBinOp "**" (IntLit a) (IntLit b)
  | b >= 0     = IntLit (a ^ b)

-- Float × Float
reduceBinOp "+"  (FloatLit a) (FloatLit b) = FloatLit (a + b)
reduceBinOp "-"  (FloatLit a) (FloatLit b) = FloatLit (a - b)
reduceBinOp "*"  (FloatLit a) (FloatLit b) = FloatLit (a * b)
reduceBinOp "/"  (FloatLit a) (FloatLit b)
  | b /= 0     = FloatLit (a / b)
reduceBinOp "**" (FloatLit a) (FloatLit b) = FloatLit (a ** b)

-- Int/Float promotion
reduceBinOp op (IntLit a) r@(FloatLit _) =
  reduceBinOp op (FloatLit (fromInteger a)) r
reduceBinOp op l@(FloatLit _) (IntLit b) =
  reduceBinOp op l (FloatLit (fromInteger b))

-- Comparison
reduceBinOp "==" (IntLit a) (IntLit b) = boolToExpr (a == b)
reduceBinOp "/=" (IntLit a) (IntLit b) = boolToExpr (a /= b)
reduceBinOp "<"  (IntLit a) (IntLit b) = boolToExpr (a < b)
reduceBinOp ">"  (IntLit a) (IntLit b) = boolToExpr (a > b)
reduceBinOp "<=" (IntLit a) (IntLit b) = boolToExpr (a <= b)
reduceBinOp ">=" (IntLit a) (IntLit b) = boolToExpr (a >= b)

-- Structural equality for strings, floats, lists, records
reduceBinOp "==" (StringLit a) (StringLit b) = boolToExpr (a == b)
reduceBinOp "/=" (StringLit a) (StringLit b) = boolToExpr (a /= b)
reduceBinOp "==" (FloatLit a) (FloatLit b) = boolToExpr (a == b)
reduceBinOp "/=" (FloatLit a) (FloatLit b) = boolToExpr (a /= b)
reduceBinOp "==" (Record t1 bs1) (Record t2 bs2) =
  boolToExpr (t1 == t2 && length bs1 == length bs2 &&
    all (\(b1, b2) -> bindName b1 == bindName b2 &&
      reduceBinOp "==" (bindBody b1) (bindBody b2) == IntLit 1) (zip bs1 bs2))
reduceBinOp "/=" a@(Record _ _) b@(Record _ _) =
  case reduceBinOp "==" a b of
    IntLit 1 -> IntLit 0
    IntLit 0 -> IntLit 1
    other    -> BinOp "/=" a b

-- String concatenation
reduceBinOp "+" (StringLit a) (StringLit b) = StringLit (a <> b)

-- Record merge: base <- overlay (override/add fields from overlay into base)
reduceBinOp "<-" (Record tag fields) (Record _ overlayBs) =
  let overrides = [(bindName b, bindBody b) | b <- overlayBs]
      updated = map (\f -> case lookup (bindName f) overrides of
                             Just newBody -> f { bindBody = newBody }
                             Nothing      -> f) fields
      existingNames = map bindName fields
      newFields = [b | b <- overlayBs, bindName b `notElem` existingNames]
  in Record tag (updated ++ newFields)

-- Residual: can't reduce
reduceBinOp op l r = BinOp op l r

boolToExpr :: Bool -> Expr
boolToExpr True  = IntLit 1
boolToExpr False = IntLit 0

-- ── Application reduction ─────────────────────────────────────────

reduceApp :: Expr -> Expr -> Expr
reduceApp = reduceAppD maxReduceDepth emptyEnv

reduceAppD :: Int -> Env -> Expr -> Expr -> Expr
-- Depth limit reached: return residual
reduceAppD d _ f x | d <= 0 = App f x
-- Built-in `not`: not 0 → 1, not nonzero → 0
reduceAppD _ _ (Name "not") (IntLit 0) = IntLit 1
reduceAppD _ _ (Name "not") (IntLit _) = IntLit 0
-- Built-in `if`: if cond ~then ~else
-- Fully applied: App (App (App (Name "if") cond) thenExpr) elseExpr
reduceAppD d env (App (App (Name "if") cond) thenExpr) elseExpr =
  case cond of
    IntLit 0 -> forceThunkD d env elseExpr  -- false
    IntLit _ -> forceThunkD d env thenExpr  -- true (any non-zero)
    _        -> App (App (App (Name "if") cond) thenExpr) elseExpr  -- residual
-- Beta reduction: (\x -> body) arg → substitute x with arg in body
-- Uses full environment so recursive calls and outer bindings are available
reduceAppD d env (Lam p body) arg =
  let argFV = exprFreeVars arg
      d' = d - 1
  in if p `Set.member` argFV
     then -- Alpha-rename: find a fresh name for p
       let fresh = freshName p (Set.union argFV (exprFreeVars body))
           body' = substExpr p (Name fresh) body
       in reduceD d' (envInsert fresh arg env) body'
     else reduceD d' (envInsert p arg env) body
-- Uppercase constructor application: Just 5 → Just {_0 = 5}
reduceAppD _ _ (Name n) arg
  | not (T.null n) && isUpper (T.head n) =
    Record n [Binding "_0" False [] arg Nothing Nothing Nothing Nothing Nothing]
-- Record introspection builtins
reduceAppD _ _ (Name "fields") (Record _ bs) =
  listToCons [bindBody b | b <- bs]
reduceAppD _ _ (Name "fieldNames") (Record _ bs) =
  listToCons [StringLit (bindName b) | b <- bs]
reduceAppD _ _ (Name "tag") (Record t _) = StringLit t
-- getField record "name" → field value
reduceAppD _ _ (App (Name "getField") (Record _ bs)) (StringLit name) =
  case [bindBody b | b <- bs, bindName b == name] of
    (v:_) -> v
    []    -> App (App (Name "getField") (Record "" bs)) (StringLit name)  -- residual
-- setField record "name" value → copy with field overridden
reduceAppD _ _ (App (App (Name "setField") (Record t bs)) (StringLit name)) val =
  let updated = map (\b -> if bindName b == name then b { bindBody = val } else b) bs
      exists = any (\b -> bindName b == name) bs
      result = if exists then updated else bs ++ [Binding name False [] val Nothing Nothing Nothing Nothing Nothing]
  in Record t result
-- Positional record extension: (Pair {_0=1}) 2 → Pair {_0=1, _1=2}
reduceAppD _ _ (Record tag bs) arg
  | isPositionalRecord bs =
    let nextIdx = "_" <> T.pack (show (length bs))
    in Record tag (bs ++ [Binding nextIdx False [] arg Nothing Nothing Nothing Nothing Nothing])
-- ── Compile-time builtin reductions ──
-- len on lists: walk the Cons chain
reduceAppD _ _ (Name "len") arg
  | Just n <- listLen arg = IntLit n
-- len / strlen on strings
reduceAppD _ _ (Name "len") (StringLit s) = IntLit (fromIntegral $ T.length s)
reduceAppD _ _ (Name "strlen") (StringLit s) = IntLit (fromIntegral $ T.length s)
-- String builtins on concrete args
reduceAppD _ _ (Name "toUpper") (StringLit s) = StringLit (T.toUpper s)
reduceAppD _ _ (Name "toLower") (StringLit s) = StringLit (T.toLower s)
reduceAppD _ _ (Name "trim") (StringLit s) = StringLit (T.strip s)
reduceAppD _ _ (Name "toString") (IntLit n) = StringLit (T.pack (show n))
reduceAppD _ _ (Name "toString") (FloatLit n) = StringLit (T.pack (show n))
reduceAppD _ _ (Name "toString") (StringLit s) = StringLit s
-- toInt / toFloat
reduceAppD _ _ (Name "toInt") (StringLit s) =
  case reads (T.unpack s) :: [(Integer, String)] of
    [(n, "")] -> IntLit n
    _         -> App (Name "toInt") (StringLit s)
reduceAppD _ _ (Name "toFloat") (StringLit s) =
  case reads (T.unpack s) :: [(Double, String)] of
    [(n, "")] -> FloatLit n
    _         -> App (Name "toFloat") (StringLit s)
-- charAt string index
reduceAppD _ _ (App (Name "charAt") (StringLit s)) (IntLit i)
  | i >= 0 && fromIntegral i < T.length s =
    StringLit (T.singleton (T.index s (fromIntegral i)))
-- indexOf string substring
reduceAppD _ _ (App (Name "indexOf") (StringLit hay)) (StringLit needle) =
  case T.breakOn needle hay of
    (before, match) | T.null match -> IntLit (-1)
                    | otherwise    -> IntLit (fromIntegral $ T.length before)
-- split string delimiter
reduceAppD _ _ (App (Name "split") (StringLit s)) (StringLit delim) =
  let parts = if T.null delim
              then map T.singleton (T.unpack s)
              else T.splitOn delim s
  in listToCons (map StringLit parts)
-- slice (string or list) — 2-arg curried: slice collection start
reduceAppD _ _ (App (App (Name "slice") (StringLit s)) (IntLit start)) (IntLit end) =
  let s' = T.take (fromIntegral (end - start)) (T.drop (fromIntegral start) s)
  in StringLit s'
-- replace old new string
reduceAppD _ _ (App (App (Name "replace") (StringLit old)) (StringLit new)) (StringLit s) =
  StringLit (T.replace old new s)
-- Residual
reduceAppD _ _ f x = App f x

-- | Check if all record fields are positional (_0, _1, ...)
isPositionalRecord :: [Binding] -> Bool
isPositionalRecord bs = all (\(b, i) -> bindName b == "_" <> T.pack (show i)) (zip bs [0::Int ..])

-- | Count elements in a compile-time Cons/Nil list
listLen :: Expr -> Maybe Integer
listLen (Record "Nil" _) = Just 0
listLen (Record "Cons" bs) =
  case [bindBody b | b <- bs, bindName b == "tail"] of
    [tl] -> (1 +) <$> listLen tl
    _    -> Nothing
listLen _ = Nothing

-- | Force a thunk: if the expression is a Thunk, reduce its body
forceThunk :: Env -> Expr -> Expr
forceThunk env (Thunk body) = reduce env body
forceThunk _   e            = e

forceThunkD :: Int -> Env -> Expr -> Expr
forceThunkD d env (Thunk body) = reduceD d env body
forceThunkD _   _ e            = e

-- | Find free variables of an expression (as a Set)
exprFreeVars :: Expr -> Set.Set Text
exprFreeVars (IntLit _)       = Set.empty
exprFreeVars (FloatLit _)     = Set.empty
exprFreeVars (StringLit _)    = Set.empty
exprFreeVars (Name n)         = Set.singleton n
exprFreeVars (BinOp op l r)   = Set.insert op $ Set.union (exprFreeVars l) (exprFreeVars r)
exprFreeVars (App f x)        = Set.union (exprFreeVars f) (exprFreeVars x)
exprFreeVars (Lam p b)        = Set.delete p (exprFreeVars b)
exprFreeVars (With body bs)   =
  let bnames = Set.fromList (map bindName bs)
  in Set.union (Set.difference (exprFreeVars body) bnames)
               (Set.unions (map (exprFreeVars . bindBody) bs))
exprFreeVars (Record _ bs)    = Set.unions (map (exprFreeVars . bindBody) bs)
exprFreeVars (FieldAccess e _) = exprFreeVars e
exprFreeVars (Namespace bs)   =
  let bnames = Set.fromList (map bindName bs)
  in Set.difference (Set.unions (map (exprFreeVars . bindBody) bs)) bnames
exprFreeVars (Case s alts)    =
  Set.union (exprFreeVars s) (Set.unions (map altFreeVars alts))
  where altFreeVars (Alt p mg b) =
          let bodyFVs = Set.union (exprFreeVars b) (maybe Set.empty exprFreeVars mg)
          in Set.difference bodyFVs (patVars p)
exprFreeVars (CFunction {})   = Set.empty
exprFreeVars (Thunk body)     = exprFreeVars body
exprFreeVars (ListLit es)     = Set.unions (map exprFreeVars es)
exprFreeVars (Quote _)          = Set.empty  -- quoted code has no free vars
exprFreeVars (Splice e)         = exprFreeVars e

-- | Generate a fresh name by appending primes
freshName :: Text -> Set.Set Text -> Text
freshName base used = head [n | n <- candidates, n `Set.notMember` used]
  where candidates = [base <> T.replicate i "'" | i <- [1..]]

-- | Syntactic substitution: replace all free occurrences of name with expr
substExpr :: Text -> Expr -> Expr -> Expr
substExpr n e (Name m) | m == n = e
substExpr _ _ v@(Name _) = v
substExpr _ _ v@(IntLit _) = v
substExpr _ _ v@(FloatLit _) = v
substExpr _ _ v@(StringLit _) = v
substExpr n e (BinOp op l r) = BinOp op (substExpr n e l) (substExpr n e r)
substExpr n e (App f x) = App (substExpr n e f) (substExpr n e x)
substExpr n e (Lam p b)
  | p == n    = Lam p b  -- shadowed
  | p `Set.member` exprFreeVars e =
    -- Alpha-rename to avoid capture
    let fresh = freshName p (Set.unions [exprFreeVars e, exprFreeVars b, Set.singleton n])
        b' = substExpr p (Name fresh) b
    in Lam fresh (substExpr n e b')
  | otherwise = Lam p (substExpr n e b)
substExpr n e (With body bs) =
  let shadowed = n `elem` map bindName bs
  in if shadowed then With body bs  -- simplified: n is rebound
     else With (substExpr n e body) (map (substBind n e) bs)
substExpr n e (Record tag bs) = Record tag (map (substBind n e) bs)
substExpr n e (FieldAccess ex f) = FieldAccess (substExpr n e ex) f
substExpr n e (Namespace bs) =
  let shadowed = n `elem` map bindName bs
  in if shadowed then Namespace bs
     else Namespace (map (substBind n e) bs)
substExpr n e (Case s alts) =
  Case (substExpr n e s) (map (substAlt n e) alts)
substExpr _ _ e@(CFunction {}) = e
substExpr n e (Thunk body) = Thunk (substExpr n e body)
substExpr n e (ListLit es) = ListLit (map (substExpr n e) es)
substExpr _ _ e@(Quote _) = e  -- don't substitute inside quotes
substExpr n e (Splice ex) = Splice (substExpr n e ex)

substBind :: Text -> Expr -> Binding -> Binding
substBind n e b = b { bindBody = substExpr n e (bindBody b) }

-- | Apply all environment bindings as substitutions (without reducing).
-- Skip recursive bindings — they should stay as Name references, not be inlined.
applyEnv :: Env -> Expr -> Expr
applyEnv env body =
  let nonRec = Map.filterWithKey (\k _ -> not (Set.member k (envRec env))) (envMap env)
  in Map.foldlWithKey' (\acc n e -> substExpr n e acc) body nonRec

applyEnvD :: Int -> Env -> Expr -> Expr
applyEnvD _ env body =
  let nonRec = Map.filterWithKey (\k _ -> not (Set.member k (envRec env))) (envMap env)
  in Map.foldlWithKey' (\acc n e -> substExpr n e acc) body nonRec

substAlt :: Text -> Expr -> Alt -> Alt
substAlt n e a = a { altBody = substExpr n e (altBody a)
                   , altGuard = fmap (substExpr n e) (altGuard a) }

-- ── Quote/Unquote (Metaprogramming) ───────────────────────────────

-- | Helper to build a binding with no source position
mkBind :: Text -> Expr -> Binding
mkBind n e = Binding n False [] e Nothing Nothing Nothing Nothing Nothing

-- | Convert a list of expressions to a cons-cell chain: Cons(head, Cons(..., Nil))
listToCons :: [Expr] -> Expr
listToCons []     = Record "Nil" []
listToCons (x:xs) = Record "Cons" [mkBind "head" x, mkBind "tail" (listToCons xs)]

-- | Detect a cons-cell chain and convert back to a flat list (for quoting etc.)
consToList :: Expr -> Maybe [Expr]
consToList (Record "Nil" []) = Just []
consToList (Record "Cons" bs) = do
  h <- lookup "head" [(bindName b, bindBody b) | b <- bs]
  t <- lookup "tail" [(bindName b, bindBody b) | b <- bs]
  rest <- consToList t
  Just (h : rest)
consToList _ = Nothing

-- | Convert an expression to its AST record representation
quoteExpr :: Expr -> Expr
quoteExpr (IntLit n)    = Record "Int"   [mkBind "val" (IntLit n)]
quoteExpr (FloatLit d)  = Record "Float" [mkBind "val" (FloatLit d)]
quoteExpr (StringLit s) = Record "Str"   [mkBind "val" (StringLit s)]
quoteExpr (Name n)      = Record "Var"   [mkBind "name" (StringLit n)]
quoteExpr (App f x)     = Record "App"   [mkBind "fn" (quoteExpr f),
                                           mkBind "arg" (quoteExpr x)]
quoteExpr (BinOp op l r) = Record "Op"   [mkBind "op" (StringLit op),
                                           mkBind "left" (quoteExpr l),
                                           mkBind "right" (quoteExpr r)]
quoteExpr (Lam p b)     = Record "Fn"    [mkBind "param" (StringLit p),
                                           mkBind "body" (quoteExpr b)]
quoteExpr (Record tag bs) = Record "Rec" [mkBind "tag" (StringLit tag),
                                           mkBind "fields" (quoteBindings bs)]
quoteExpr (Case s alts) = Record "Match" [mkBind "expr" (quoteExpr s),
                                           mkBind "alts" (listToCons (map quoteAlt alts))]
quoteExpr (Namespace bs) = Record "Let"  [mkBind "bindings" (quoteBindings bs)]
quoteExpr (With body bs) = Record "With" [mkBind "body" (quoteExpr body),
                                           mkBind "bindings" (quoteBindings bs)]
quoteExpr (ListLit es)  = Record "List"  [mkBind "elems" (listToCons (map quoteExpr es))]
quoteExpr (Thunk e)     = Record "Thunk" [mkBind "body" (quoteExpr e)]
quoteExpr (FieldAccess e f) = Record "Access" [mkBind "expr" (quoteExpr e),
                                                mkBind "field" (StringLit f)]
quoteExpr (Quote e)     = Record "Quote"  [mkBind "body" (quoteExpr e)]
quoteExpr (Splice e)    = Record "Splice" [mkBind "body" (quoteExpr e)]
quoteExpr (CFunction {}) = Record "CFunc" []

quoteBindings :: [Binding] -> Expr
quoteBindings bs = listToCons [Record "Field" [mkBind "name" (StringLit (bindName b)),
                                             mkBind "val" (quoteExpr (bindBody b))]
                           | b <- bs]

quoteAlt :: Alt -> Expr
quoteAlt (Alt pat guard body) =
  Record "MAlt" [mkBind "pat" (quotePat pat),
                  mkBind "guard" (maybe (IntLit 0) quoteExpr guard),
                  mkBind "body" (quoteExpr body)]

quotePat :: Pat -> Expr
quotePat (PVar v)      = Record "PVar"  [mkBind "name" (StringLit v)]
quotePat (PLit e)      = Record "PLit"  [mkBind "val" (quoteExpr e)]
quotePat (PRec t fs)   = Record "PRec"  [mkBind "tag" (StringLit t),
                                          mkBind "fields" (listToCons [Record "PField"
                                            [mkBind "name" (StringLit f),
                                             mkBind "pat" (quotePat p)]
                                           | (f,p) <- fs])]
quotePat (PList ps mr) = Record "PList" [mkBind "pats" (listToCons (map quotePat ps)),
                                          mkBind "rest" (maybe (IntLit 0) (StringLit) mr)]
quotePat PWild         = Record "PWild" []

-- | Convert an AST record representation back to an expression
unquoteExpr :: Expr -> Maybe Expr
unquoteExpr (Record "Int" bs)    = IntLit <$> getIntField "val" bs
unquoteExpr (Record "Float" bs)  = FloatLit <$> getFloatField "val" bs
unquoteExpr (Record "Str" bs)    = StringLit <$> getStrField "val" bs
unquoteExpr (Record "Var" bs)    = Name <$> getStrField "name" bs
unquoteExpr (Record "App" bs)    = do
  fn  <- getField "fn" bs >>= unquoteExpr
  arg <- getField "arg" bs >>= unquoteExpr
  Just (App fn arg)
unquoteExpr (Record "Op" bs)     = do
  op    <- getStrField "op" bs
  left  <- getField "left" bs >>= unquoteExpr
  right <- getField "right" bs >>= unquoteExpr
  Just (BinOp op left right)
unquoteExpr (Record "Fn" bs)     = do
  param <- getStrField "param" bs
  body  <- getField "body" bs >>= unquoteExpr
  Just (Lam param body)
unquoteExpr (Record "Rec" bs)    = do
  tag    <- getStrField "tag" bs
  fields <- getField "fields" bs >>= unquoteBindings
  Just (Record tag fields)
unquoteExpr (Record "Match" bs)  = do
  expr <- getField "expr" bs >>= unquoteExpr
  altsE <- getField "alts" bs
  case consToList altsE of
    Just es -> do
      alts <- mapM unquoteAlt es
      Just (Case expr alts)
    Nothing -> Nothing
unquoteExpr (Record "Let" bs)    = do
  bindsE <- getField "bindings" bs >>= unquoteBindings
  Just (Namespace bindsE)
unquoteExpr (Record "With" bs)   = do
  body   <- getField "body" bs >>= unquoteExpr
  bindsE <- getField "bindings" bs >>= unquoteBindings
  Just (With body bindsE)
unquoteExpr (Record "List" bs)   = do
  elemsE <- getField "elems" bs
  case consToList elemsE of
    Just es -> listToCons <$> mapM unquoteExpr es
    Nothing -> Nothing
unquoteExpr (Record "Thunk" bs)  = Thunk <$> (getField "body" bs >>= unquoteExpr)
unquoteExpr (Record "Access" bs) = do
  expr  <- getField "expr" bs >>= unquoteExpr
  field <- getStrField "field" bs
  Just (FieldAccess expr field)
unquoteExpr (Record "Quote" bs)  = Quote <$> (getField "body" bs >>= unquoteExpr)
unquoteExpr (Record "Splice" bs) = Splice <$> (getField "body" bs >>= unquoteExpr)
-- Raw values pass through: allows mixing AST records with concrete values
unquoteExpr e@(IntLit _)    = Just e
unquoteExpr e@(FloatLit _)  = Just e
unquoteExpr e@(StringLit _) = Just e
unquoteExpr e@(Name _)      = Just e
unquoteExpr e@(ListLit _)   = Just e  -- unreduced list literal passes through
unquoteExpr _ = Nothing

unquoteBindings :: Expr -> Maybe [Binding]
unquoteBindings expr = case consToList expr of
  Just es -> mapM unquoteField es
  Nothing -> case expr of
    ListLit es -> mapM unquoteField es  -- fallback for unreduced
    _ -> Nothing
  where
    unquoteField (Record "Field" bs) = do
      name <- getStrField "name" bs
      val  <- getField "val" bs >>= unquoteExpr
      Just (Binding name False [] val Nothing Nothing Nothing Nothing Nothing)
    unquoteField _ = Nothing

unquoteAlt :: Expr -> Maybe Alt
unquoteAlt (Record "MAlt" bs) = do
  patE  <- getField "pat" bs
  pat   <- unquotePat patE
  guardE <- getField "guard" bs
  let guard = case guardE of
        IntLit 0 -> Nothing
        _        -> unquoteExpr guardE >>= Just
  body  <- getField "body" bs >>= unquoteExpr
  Just (Alt pat guard body)
unquoteAlt _ = Nothing

unquotePat :: Expr -> Maybe Pat
unquotePat (Record "PVar" bs)  = PVar <$> getStrField "name" bs
unquotePat (Record "PLit" bs)  = PLit <$> (getField "val" bs >>= unquoteExpr)
unquotePat (Record "PRec" bs)  = do
  tag <- getStrField "tag" bs
  flds <- getField "fields" bs
  let tryList es = mapM (\e -> case e of
        Record "PField" fbs -> do
          name <- getStrField "name" fbs
          pat  <- getField "pat" fbs >>= unquotePat
          Just (name, pat)
        _ -> Nothing) es
  pfs <- case consToList flds of
    Just es -> tryList es
    Nothing -> case flds of
      ListLit es -> tryList es
      _ -> Nothing
  Just (PRec tag pfs)
unquotePat (Record "PList" bs) = do
  patsE <- getField "pats" bs
  pats <- case consToList patsE of
    Just es -> mapM unquotePat es
    Nothing -> case patsE of
      ListLit es -> mapM unquotePat es
      _ -> Nothing
  restE <- getField "rest" bs
  let rest = case restE of
        IntLit 0   -> Nothing
        StringLit s -> Just s
        _ -> Nothing
  Just (PList pats rest)
unquotePat (Record "PWild" _) = Just PWild
unquotePat _ = Nothing

-- | Field extraction helpers for unquote
getField :: Text -> [Binding] -> Maybe Expr
getField name bs = case [bindBody b | b <- bs, bindName b == name] of
  (v:_) -> Just v
  []    -> Nothing

getIntField :: Text -> [Binding] -> Maybe Integer
getIntField name bs = getField name bs >>= \case
  IntLit n -> Just n
  _        -> Nothing

getFloatField :: Text -> [Binding] -> Maybe Double
getFloatField name bs = getField name bs >>= \case
  FloatLit d -> Just d
  _          -> Nothing

getStrField :: Text -> [Binding] -> Maybe Text
getStrField name bs = getField name bs >>= \case
  StringLit s -> Just s
  _           -> Nothing

-- ── Field access reduction ────────────────────────────────────────

-- | Check if a name is a positional field (_0, _1, ...) and return the index
positionalIndex :: Text -> Maybe Int
positionalIndex t = case T.uncons t of
  Just ('_', rest) | not (T.null rest) && T.all (\c -> c >= '0' && c <= '9') rest ->
    Just (read (T.unpack rest))
  _ -> Nothing

reduceFieldAccess :: Expr -> Text -> Expr
reduceFieldAccess (Record _ bs) field =
  case positionalIndex field of
    Just i | i < length bs -> bindBody (bs !! i)
    _ -> case [bindBody b | b <- bs, bindName b == field] of
      (v:_) -> v
      []    -> FieldAccess (Record "" bs) field  -- field not found, residual
reduceFieldAccess (Namespace bs) field =
  case [bindBody b | b <- bs, bindName b == field] of
    (v:_) -> v
    []    -> FieldAccess (Namespace bs) field
reduceFieldAccess e field = FieldAccess e field

-- ── Case reduction ────────────────────────────────────────────────

reduceCase :: Env -> Expr -> [Alt] -> Expr
reduceCase = reduceCaseD maxReduceDepth

reduceCaseD :: Int -> Env -> Expr -> [Alt] -> Expr
reduceCaseD d env scrut alts =
  case tryMatchD d env scrut alts of
    Just (binds, body) ->
      let env' = env { envMap = Map.union (Map.fromList binds) (envMap env) }
      in reduceD d env' body
    Nothing -> Case scrut alts  -- residual

tryMatch :: Env -> Expr -> [Alt] -> Maybe ([(Text, Expr)], Expr)
tryMatch = tryMatchD maxReduceDepth

tryMatchD :: Int -> Env -> Expr -> [Alt] -> Maybe ([(Text, Expr)], Expr)
tryMatchD _ _ _ [] = Nothing
tryMatchD d env scrut (Alt pat guard body : rest) =
  case matchPat pat scrut of
    Just binds ->
      case guard of
        Nothing -> Just (binds, body)
        Just g  ->
          let env' = env { envMap = Map.union (Map.fromList binds) (envMap env) }
              g' = reduceD d env' g
          in case g' of
               IntLit 0 -> tryMatchD d env scrut rest  -- guard failed
               IntLit _ -> Just (binds, body)       -- guard passed
               _        -> Nothing                  -- guard residual, can't decide
    Nothing -> tryMatchD d env scrut rest

-- | Extract variable names bound by a pattern
patVars :: Pat -> Set.Set Text
patVars (PVar v)         = Set.singleton v
patVars PWild            = Set.empty
patVars (PLit _)         = Set.empty
patVars (PRec _ fields)  = Set.unions [patVars p | (_, p) <- fields]
patVars (PList pats mrest) =
  Set.unions (map patVars pats) `Set.union` maybe Set.empty Set.singleton mrest

matchPat :: Pat -> Expr -> Maybe [(Text, Expr)]
matchPat (PVar v) e = Just [(v, e)]
matchPat PWild    _ = Just []
matchPat (PLit (IntLit a)) (IntLit b)
  | a == b    = Just []
  | otherwise = Nothing
matchPat (PLit (StringLit a)) (StringLit b)
  | a == b    = Just []
  | otherwise = Nothing
matchPat (PRec tag fields) (Record rtag bs)
  | tag == rtag = matchFields fields bs
matchPat (PList pats mrest) expr = matchConsList pats mrest expr
matchPat _ _ = Nothing  -- can't match at compile time → residual

-- Match a PList pattern against a cons-cell chain (Cons/Nil records)
matchConsList :: [Pat] -> Maybe Text -> Expr -> Maybe [(Text, Expr)]
matchConsList [] Nothing (Record "Nil" []) = Just []  -- exact: [] matches Nil
matchConsList [] Nothing _ = Nothing                   -- exact: non-empty → fail
matchConsList [] (Just name) expr = Just [(name, expr)] -- spread: bind rest
matchConsList (p:ps) mrest (Record "Cons" bs) = do
  h <- lookup "head" [(bindName b, bindBody b) | b <- bs]
  t <- lookup "tail" [(bindName b, bindBody b) | b <- bs]
  binds1 <- matchPat p h
  binds2 <- matchConsList ps mrest t
  Just (binds1 ++ binds2)
matchConsList _ _ _ = Nothing  -- too few elements or not a list

matchFields :: [(Text, Pat)] -> [Binding] -> Maybe [(Text, Expr)]
matchFields [] _ = Just []
matchFields ((f, p):fps) bs =
  case positionalIndex f of
    Just i | i < length bs -> do
      binds1 <- matchPat p (bindBody (bs !! i))
      binds2 <- matchFields fps bs
      Just (binds1 ++ binds2)
    _ -> case [bindBody b | b <- bs, bindName b == f] of
      (v:_) -> do
        binds1 <- matchPat p v
        binds2 <- matchFields fps bs
        Just (binds1 ++ binds2)
      [] -> Nothing

-- ── Compile-time warnings ─────────────────────────────────────────

data Warning = Warning
  { warnPos :: Maybe SrcPos
  , warnMsg :: String
  } deriving (Show)

-- | Analyze a reduced AST for potential issues
warnings :: Expr -> [Warning]
warnings = warnExpr

warnExpr :: Expr -> [Warning]
-- Field access on a known record where field doesn't exist
warnExpr (FieldAccess (Record tag bs) field) =
  case positionalIndex field of
    Just i | i < length bs -> []  -- valid positional access
    _ -> case [() | b <- bs, bindName b == field] of
      [] -> [Warning Nothing $
              "field '" ++ T.unpack field ++ "' not found in record '" ++ T.unpack tag ++ "'"]
      _  -> []
-- Case on a known value where no pattern matches
warnExpr (Case scrut alts)
  | not (isResidual scrut) =
    case tryMatch emptyEnv scrut alts of
      Nothing -> [Warning Nothing "no pattern matches this value"]
      Just _  -> concatMap warnAlt alts
  | otherwise = warnExpr scrut ++ concatMap warnAlt alts
-- BinOp type mismatch on known values
warnExpr (BinOp op l r)
  | not (isResidual l) && not (isResidual r) = warnBinOp op l r
  | otherwise = warnExpr l ++ warnExpr r
-- Recurse into subexpressions
warnExpr (App f x) = warnExpr f ++ warnExpr x
warnExpr (Lam _ b) = warnExpr b
warnExpr (With body bs) = warnExpr body ++ concatMap warnBinding bs
warnExpr (Record _ bs) = concatMap warnBinding bs
warnExpr (FieldAccess e _) = warnExpr e
warnExpr (Namespace bs) = concatMap warnBinding bs
warnExpr (Thunk e) = warnExpr e
warnExpr (ListLit es) = concatMap warnExpr es
warnExpr (Quote _) = []
warnExpr (Splice e) = warnExpr e
warnExpr _ = []

warnAlt :: Alt -> [Warning]
warnAlt (Alt _ mg b) = maybe [] warnExpr mg ++ warnExpr b

warnBinding :: Binding -> [Warning]
warnBinding b =
  let ws = warnExpr (bindBody b)
  in map (\w -> w { warnPos = warnPos w <|> bindPos b }) ws
  where
    Nothing <|> y = y
    x       <|> _ = x

warnBinOp :: Text -> Expr -> Expr -> [Warning]
warnBinOp "+" (StringLit _) (IntLit _) = [Warning Nothing "cannot add string and int"]
warnBinOp "+" (IntLit _) (StringLit _) = [Warning Nothing "cannot add int and string"]
warnBinOp "+" (StringLit _) (FloatLit _) = [Warning Nothing "cannot add string and float"]
warnBinOp "+" (FloatLit _) (StringLit _) = [Warning Nothing "cannot add float and string"]
warnBinOp "-" (StringLit _) _ = [Warning Nothing "cannot subtract from string"]
warnBinOp "-" _ (StringLit _) = [Warning Nothing "cannot subtract string"]
warnBinOp "*" (StringLit _) _ = [Warning Nothing "cannot multiply string"]
warnBinOp "*" _ (StringLit _) = [Warning Nothing "cannot multiply by string"]
warnBinOp "/" _ (IntLit 0) = [Warning Nothing "division by zero"]
warnBinOp "/" _ (FloatLit 0) = [Warning Nothing "division by zero"]
warnBinOp _ _ _ = []
