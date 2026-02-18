{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Milang.Reduce (reduce, Env, emptyEnv, warnings, Warning(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust)
import Data.Char (isUpper)
import Data.Graph (stronglyConnComp, SCC(..))
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
    Just val -> reduce env val
    Nothing  -> Name n

reduce env (BinOp op l r) =
  let l' = forceThunk env (reduce env l)
      r' = forceThunk env (reduce env r)
  in reduceBinOp op l' r'

reduce env (App f x) =
  let f' = reduce env f
      x' = reduce env x
  in reduceApp f' x'

reduce env (Lam p b) =
  -- Check if any env value has p as a free variable (capture risk)
  let envFVs = Set.unions (map exprFreeVars (Map.elems env))
  in if p `Set.member` envFVs
     then -- Alpha-rename the lambda parameter to avoid capture
       let allNames = Set.unions [envFVs, exprFreeVars b, Map.keysSet env]
           fresh = freshName p allNames
           b' = substExpr p (Name fresh) b
           env' = Map.delete fresh env
       in Lam fresh (reduce env' b')
     else
       let env' = Map.delete p env
       in Lam p (reduce env' b)

reduce env (With body bindings) =
  let env' = evalBindings env bindings
      body' = reduce env' body
      bs' = map (reduceBind env') bindings
      residualBs = filter (isResidual . bindBody) bs'
  in if null residualBs then body' else With body' residualBs

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
       then Case scrut' (map (\(Alt p g b) -> Alt p (fmap (reduce env) g) (reduce env b)) alts)
       else reduceCase env scrut' alts

reduce _ e@(CFunction {}) = e  -- C FFI: irreducible

reduce env (Quote body) =
  quoteExpr body  -- quote captures the syntax, not the value

reduce env (Splice body) =
  let body' = reduce env body
  in case unquoteExpr body' of
       Just expr -> reduce env expr
       Nothing   -> Splice body'  -- residual: can't splice non-AST value

reduce env (Thunk body) = Thunk (applyEnv env body)  -- defer evaluation but capture bindings

reduce env (ListLit es) = listToCons (map (reduce env) es)

reduce env (RecordUpdate e bs) =
  let e' = reduce env e
      bs' = map (reduceBind env) bs
  in case e' of
    Record tag fields ->
      let overrides = [(bindName b, bindBody b) | b <- bs']
          updated = map (\f -> case lookup (bindName f) overrides of
                                 Just newBody -> f { bindBody = newBody }
                                 Nothing      -> f) fields
          -- Add any new fields not in the original
          existingNames = map bindName fields
          newFields = [b | b <- bs', bindName b `notElem` existingNames]
      in Record tag (updated ++ newFields)
    _ -> RecordUpdate e' bs'

-- ── Binding evaluation ────────────────────────────────────────────

-- Evaluate a sequence of bindings, extending the environment.
-- Strict bindings are reduced immediately; lazy bindings are deferred.
-- Self-referencing (recursive) bindings are NOT added to the env — they stay
-- as residual Names so the reducer doesn't try to inline them infinitely.
evalBindings :: Env -> [Binding] -> Env
evalBindings env bindings =
  let expanded = concatMap expandUnion bindings
      bindNames = Set.fromList [bindName b | b <- expanded]
      nodes = [ (b, bindName b,
                 Set.toList $ Set.intersection bindNames
                   (exprFreeVars (wrapLambda (bindParams b) (bindBody b))))
               | b <- expanded ]
      sccs = stronglyConnComp nodes
  in foldl evalSCC env sccs

-- | Expand union declarations: if a binding's body is a Namespace of all-uppercase
-- bindings, inject those as additional top-level bindings
expandUnion :: Binding -> [Binding]
expandUnion b = case bindBody b of
  Namespace ctors
    | all (\c -> let n = bindName c in not (T.null n) && isUpper (T.head n)) ctors ->
      b : ctors  -- keep the Shape binding + add Circle, Rect, etc.
  _ -> [b]

-- Evaluate one SCC group
evalSCC :: Env -> SCC Binding -> Env
evalSCC env (AcyclicSCC b) =
  let name = bindName b
      body = wrapLambda (bindParams b) (bindBody b)
      val = if bindLazy b
            then bindBody b
            else reduce env body
  in Map.insert name val env
evalSCC env (CyclicSCC _) =
  -- Mutually recursive group: don't add to env, keep calls residual in codegen
  env

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
-- Cons operator: h : t → Record "Cons" [head=h, tail=t]
reduceBinOp ":" h t =
  Record "Cons" [ Binding "head" False [] h Nothing Nothing
                , Binding "tail" False [] t Nothing Nothing ]
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
reduceBinOp "++" (StringLit a) (StringLit b) = StringLit (a <> b)

-- Residual: can't reduce
reduceBinOp op l r = BinOp op l r

boolToExpr :: Bool -> Expr
boolToExpr True  = IntLit 1
boolToExpr False = IntLit 0

-- ── Application reduction ─────────────────────────────────────────

reduceApp :: Expr -> Expr -> Expr
-- Built-in `not`: not 0 → 1, not nonzero → 0
reduceApp (Name "not") (IntLit 0) = IntLit 1
reduceApp (Name "not") (IntLit _) = IntLit 0
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
-- Uppercase constructor application: Just 5 → Just {_0 = 5}
reduceApp (Name n) arg
  | not (T.null n) && isUpper (T.head n) =
    Record n [Binding "_0" False [] arg Nothing Nothing]
-- Record introspection builtins
reduceApp (Name "fields") (Record _ bs) =
  listToCons [bindBody b | b <- bs]
reduceApp (Name "fieldNames") (Record _ bs) =
  listToCons [StringLit (bindName b) | b <- bs]
reduceApp (Name "tag") (Record t _) = StringLit t
-- getField record "name" → field value
reduceApp (App (Name "getField") (Record _ bs)) (StringLit name) =
  case [bindBody b | b <- bs, bindName b == name] of
    (v:_) -> v
    []    -> App (App (Name "getField") (Record "" bs)) (StringLit name)  -- residual
-- setField record "name" value → copy with field overridden
reduceApp (App (App (Name "setField") (Record t bs)) (StringLit name)) val =
  let updated = map (\b -> if bindName b == name then b { bindBody = val } else b) bs
      exists = any (\b -> bindName b == name) bs
      result = if exists then updated else bs ++ [Binding name False [] val Nothing Nothing]
  in Record t result
-- Positional record extension: (Pair {_0=1}) 2 → Pair {_0=1, _1=2}
reduceApp (Record tag bs) arg
  | isPositionalRecord bs =
    let nextIdx = "_" <> T.pack (show (length bs))
    in Record tag (bs ++ [Binding nextIdx False [] arg Nothing Nothing])
-- Residual
reduceApp f x = App f x

-- | Check if all record fields are positional (_0, _1, ...)
isPositionalRecord :: [Binding] -> Bool
isPositionalRecord bs = all (\(b, i) -> bindName b == "_" <> T.pack (show i)) (zip bs [0::Int ..])

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
  Set.union (exprFreeVars s) (Set.unions (map altFreeVars alts))
  where altFreeVars (Alt _ mg b) =
          Set.union (exprFreeVars b) (maybe Set.empty exprFreeVars mg)
exprFreeVars (CFunction {})   = Set.empty
exprFreeVars (Thunk body)     = exprFreeVars body
exprFreeVars (ListLit es)     = Set.unions (map exprFreeVars es)
exprFreeVars (RecordUpdate e bs) =
  Set.union (exprFreeVars e) (Set.unions (map (exprFreeVars . bindBody) bs))
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
substExpr n e (RecordUpdate ex bs) = RecordUpdate (substExpr n e ex) (map (substBind n e) bs)
substExpr _ _ e@(Quote _) = e  -- don't substitute inside quotes
substExpr n e (Splice ex) = Splice (substExpr n e ex)

substBind :: Text -> Expr -> Binding -> Binding
substBind n e b = b { bindBody = substExpr n e (bindBody b) }

-- | Apply all environment bindings as substitutions (without reducing)
applyEnv :: Env -> Expr -> Expr
applyEnv env body = Map.foldlWithKey' (\acc n e -> substExpr n e acc) body env

substAlt :: Text -> Expr -> Alt -> Alt
substAlt n e a = a { altBody = substExpr n e (altBody a)
                   , altGuard = fmap (substExpr n e) (altGuard a) }

-- ── Quote/Unquote (Metaprogramming) ───────────────────────────────

-- | Helper to build a binding with no source position
mkBind :: Text -> Expr -> Binding
mkBind n e = Binding n False [] e Nothing Nothing

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
quoteExpr (RecordUpdate e bs) = Record "Update" [mkBind "expr" (quoteExpr e),
                                                   mkBind "fields" (quoteBindings bs)]
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
unquoteExpr (Record "Update" bs) = do
  expr   <- getField "expr" bs >>= unquoteExpr
  fields <- getField "fields" bs >>= unquoteBindings
  Just (RecordUpdate expr fields)
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
      Just (Binding name False [] val Nothing Nothing)
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
reduceCase env scrut alts =
  case tryMatch env scrut alts of
    Just (binds, body) ->
      let env' = Map.union (Map.fromList binds) env
      in reduce env' body
    Nothing -> Case scrut alts  -- residual

tryMatch :: Env -> Expr -> [Alt] -> Maybe ([(Text, Expr)], Expr)
tryMatch _ _ [] = Nothing
tryMatch env scrut (Alt pat guard body : rest) =
  case matchPat pat scrut of
    Just binds ->
      case guard of
        Nothing -> Just (binds, body)
        Just g  ->
          let env' = Map.union (Map.fromList binds) env
              g' = reduce env' g
          in case g' of
               IntLit 0 -> tryMatch env scrut rest  -- guard failed
               IntLit _ -> Just (binds, body)       -- guard passed
               _        -> Nothing                  -- guard residual, can't decide
    Nothing -> tryMatch env scrut rest

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
    case tryMatch Map.empty scrut alts of
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
warnExpr (RecordUpdate e bs) = warnExpr e ++ concatMap warnBinding bs
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
