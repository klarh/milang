{-# LANGUAGE OverloadedStrings #-}
module Milang.Reduce (reduce, Env, emptyEnv, warnings, Warning(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (isNothing, isJust)
import Data.Char (isUpper)
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
  -- Reduce under lambda with param shadowing any outer binding
  let env' = Map.delete p env
  in Lam p (reduce env' b)

reduce env (With body bindings) =
  let env' = evalBindings env bindings
      body' = reduce env' body
      -- Keep residual bindings (might have side effects)
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

reduce env (Thunk body) = Thunk body  -- thunks stay deferred; forced on demand

reduce env (ListLit es) = ListLit (map (reduce env) es)

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
-- Uppercase constructor application: Just 5 → Just {_0 = 5}
reduceApp (Name n) arg
  | not (T.null n) && isUpper (T.head n) =
    Record n [Binding "_0" False [] arg Nothing]
-- Positional record extension: (Pair {_0=1}) 2 → Pair {_0=1, _1=2}
reduceApp (Record tag bs) arg
  | isPositionalRecord bs =
    let nextIdx = "_" <> T.pack (show (length bs))
    in Record tag (bs ++ [Binding nextIdx False [] arg Nothing])
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
substExpr n e (RecordUpdate ex bs) = RecordUpdate (substExpr n e ex) (map (substBind n e) bs)

substBind :: Text -> Expr -> Binding -> Binding
substBind n e b = b { bindBody = substExpr n e (bindBody b) }

substAlt :: Text -> Expr -> Alt -> Alt
substAlt n e a = a { altBody = substExpr n e (altBody a)
                   , altGuard = fmap (substExpr n e) (altGuard a) }

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
