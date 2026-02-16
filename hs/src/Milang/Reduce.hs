{-# LANGUAGE OverloadedStrings #-}
module Milang.Reduce (reduce, Env, emptyEnv) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust)
import Milang.Syntax

-- | Environment: maps names to (possibly residual) expressions
type Env = Map.Map Text Expr

emptyEnv :: Env
emptyEnv = Map.empty

-- | Check if an expression is residual (not a concrete value)
isResidual :: Expr -> Bool
isResidual (IntLit _)    = False
isResidual (FloatLit _)  = False
isResidual (StringLit _) = False
isResidual (ListLit _)   = False
isResidual (Record _ _)  = False
isResidual _             = True

-- | Reduce an expression as far as possible given the environment.
-- Fully-known expressions become literals. Partially-known expressions
-- remain as residual AST fragments.
reduce :: Env -> Expr -> Expr
reduce _   e@(IntLit _)    = e
reduce _   e@(FloatLit _)  = e
reduce _   e@(StringLit _) = e

reduce env (Name n) =
  case Map.lookup n env of
    Just val -> reduce env val   -- reduce on access (forces lazy bindings)
    Nothing  -> Name n  -- unknown / residual

reduce env (BinOp op l r) =
  let l' = forceThunk env (reduce env l)
      r' = forceThunk env (reduce env r)
  in reduceBinOp op l' r'

reduce env (App f x) =
  let f' = reduce env f
      x' = reduce env x
  in reduceApp f' x'

reduce env (Lam p b) =
  -- Reduce under lambda with param shadowing any outer binding
  let env' = Map.delete p env
  in Lam p (reduce env' b)

reduce env (With body bindings) =
  let env' = evalBindings env bindings
  in reduce env' body

reduce env (Record tag bindings) =
  let bs' = map (reduceBind env) bindings
  in Record tag bs'

reduce env (FieldAccess e field) =
  let e' = forceThunk env (reduce env e)
  in reduceFieldAccess e' field

reduce env (Namespace bindings) =
  let env' = evalBindings env bindings
      bs'  = map (reduceBind env') bindings
  in Namespace bs'

reduce env (Case scrut alts) =
  let scrut' = forceThunk env (reduce env scrut)
  in if isResidual scrut'
       then Case scrut' (map (\(Alt p b) -> Alt p (reduce env b)) alts)
       else reduceCase env scrut' alts

reduce _ e@(CFunction {}) = e  -- C FFI: irreducible

reduce env (Thunk body) = Thunk body  -- thunks stay deferred; forced on demand

reduce env (ListLit es) = ListLit (map (reduce env) es)

-- ── Binding evaluation ────────────────────────────────────────────

-- Evaluate a sequence of bindings, extending the environment.
-- Strict bindings are reduced immediately; lazy bindings are deferred.
-- Self-referencing (recursive) bindings are NOT added to the env — they stay
-- as residual Names so the reducer doesn't try to inline them infinitely.
evalBindings :: Env -> [Binding] -> Env
evalBindings env [] = env
evalBindings env (b:bs) =
  let name = bindName b
      body = wrapLambda (bindParams b) (bindBody b)
      isRecursive = isSelfReferencing name body
      val = if bindLazy b
            then bindBody b  -- lazy: store unevaluated
            else if isRecursive
                 then body  -- recursive: store without reducing
                 else reduce env body
      -- For recursive bindings, don't add to env so calls stay residual
      env' = if isRecursive
             then env
             else Map.insert name val env
  in evalBindings env' bs

-- | Check if an expression references a given name (used to detect recursion)
isSelfReferencing :: Text -> Expr -> Bool
isSelfReferencing name expr = name `Set.member` exprFreeVars expr

-- Wrap a body in lambdas for its parameters: f x y = e → \x -> \y -> e
wrapLambda :: [Text] -> Expr -> Expr
wrapLambda []     body = body
wrapLambda (p:ps) body = Lam p (wrapLambda ps body)

-- Reduce a single binding's body
reduceBind :: Env -> Binding -> Binding
reduceBind env b =
  b { bindBody = reduce env (wrapLambda (bindParams b) (bindBody b))
    , bindParams = []  -- params absorbed into lambdas
    }

-- ── Binary operator reduction ─────────────────────────────────────

reduceBinOp :: Text -> Expr -> Expr -> Expr
-- Int × Int
reduceBinOp "+"  (IntLit a) (IntLit b) = IntLit (a + b)
reduceBinOp "-"  (IntLit a) (IntLit b) = IntLit (a - b)
reduceBinOp "*"  (IntLit a) (IntLit b) = IntLit (a * b)
reduceBinOp "/"  (IntLit a) (IntLit b)
  | b /= 0     = IntLit (a `div` b)
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

-- String concatenation
reduceBinOp "+" (StringLit a) (StringLit b) = StringLit (a <> b)

-- Residual: can't reduce
reduceBinOp op l r = BinOp op l r

boolToExpr :: Bool -> Expr
boolToExpr True  = IntLit 1
boolToExpr False = IntLit 0

-- ── Application reduction ─────────────────────────────────────────

reduceApp :: Expr -> Expr -> Expr
-- Built-in `if`: if cond ~then ~else
-- Fully applied: App (App (App (Name "if") cond) thenExpr) elseExpr
reduceApp (App (App (Name "if") cond) thenExpr) elseExpr =
  case cond of
    IntLit 0 -> forceThunk emptyEnv elseExpr  -- false
    IntLit _ -> forceThunk emptyEnv thenExpr  -- true (any non-zero)
    _        -> App (App (App (Name "if") cond) thenExpr) elseExpr  -- residual
-- Beta reduction: (\x -> body) arg → substitute x with arg in body
-- Alpha-rename if arg contains free occurrences of x to avoid capture
reduceApp (Lam p body) arg =
  let argFV = exprFreeVars arg
  in if p `Set.member` argFV
     then -- Alpha-rename: find a fresh name for p
       let fresh = freshName p (Set.union argFV (exprFreeVars body))
           body' = substExpr p (Name fresh) body
       in reduce (Map.singleton fresh arg) body'
     else reduce (Map.singleton p arg) body
-- Residual
reduceApp f x = App f x

-- | Force a thunk: if the expression is a Thunk, reduce its body
forceThunk :: Env -> Expr -> Expr
forceThunk env (Thunk body) = reduce env body
forceThunk _   e            = e

-- | Find free variables of an expression (as a Set)
exprFreeVars :: Expr -> Set.Set Text
exprFreeVars (IntLit _)       = Set.empty
exprFreeVars (FloatLit _)     = Set.empty
exprFreeVars (StringLit _)    = Set.empty
exprFreeVars (Name n)         = Set.singleton n
exprFreeVars (BinOp _ l r)    = Set.union (exprFreeVars l) (exprFreeVars r)
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
  Set.union (exprFreeVars s) (Set.unions (map (exprFreeVars . altBody) alts))
exprFreeVars (CFunction {})   = Set.empty
exprFreeVars (Thunk body)     = exprFreeVars body
exprFreeVars (ListLit es)     = Set.unions (map exprFreeVars es)

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

substBind :: Text -> Expr -> Binding -> Binding
substBind n e b = b { bindBody = substExpr n e (bindBody b) }

substAlt :: Text -> Expr -> Alt -> Alt
substAlt n e a = a { altBody = substExpr n e (altBody a) }

-- ── Field access reduction ────────────────────────────────────────

reduceFieldAccess :: Expr -> Text -> Expr
reduceFieldAccess (Record _ bs) field =
  case [bindBody b | b <- bs, bindName b == field] of
    (v:_) -> v
    []    -> FieldAccess (Record "" bs) field  -- field not found, residual
reduceFieldAccess (Namespace bs) field =
  case [bindBody b | b <- bs, bindName b == field] of
    (v:_) -> v
    []    -> FieldAccess (Namespace bs) field
reduceFieldAccess e field = FieldAccess e field

-- ── Case reduction ────────────────────────────────────────────────

reduceCase :: Env -> Expr -> [Alt] -> Expr
reduceCase env scrut alts =
  case tryMatch scrut alts of
    Just (binds, body) ->
      let env' = Map.union (Map.fromList binds) env
      in reduce env' body
    Nothing -> Case scrut alts  -- residual

tryMatch :: Expr -> [Alt] -> Maybe ([(Text, Expr)], Expr)
tryMatch _ [] = Nothing
tryMatch scrut (Alt pat body : rest) =
  case matchPat pat scrut of
    Just binds -> Just (binds, body)
    Nothing    -> tryMatch scrut rest

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
matchPat (PList pats mrest) (ListLit es)
  | isNothing mrest && length pats /= length es = Nothing  -- exact match: wrong length
  | isJust mrest && length es < length pats = Nothing      -- spread: too few elements
  | otherwise = do
      let (headEs, tailEs) = splitAt (length pats) es
      binds <- matchListElems pats headEs
      restBinds <- case mrest of
        Nothing   -> Just []
        Just name -> Just [(name, ListLit tailEs)]
      Just (binds ++ restBinds)
matchPat _ _ = Nothing  -- can't match at compile time → residual

-- Match list elements pairwise
matchListElems :: [Pat] -> [Expr] -> Maybe [(Text, Expr)]
matchListElems [] [] = Just []
matchListElems (p:ps) (e:es) = do
  binds1 <- matchPat p e
  binds2 <- matchListElems ps es
  Just (binds1 ++ binds2)
matchListElems _ _ = Nothing

matchFields :: [(Text, Pat)] -> [Binding] -> Maybe [(Text, Expr)]
matchFields [] _ = Just []
matchFields ((f, p):fps) bs =
  case [bindBody b | b <- bs, bindName b == f] of
    (v:_) -> do
      binds1 <- matchPat p v
      binds2 <- matchFields fps bs
      Just (binds1 ++ binds2)
    [] -> Nothing
