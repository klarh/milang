{-# LANGUAGE OverloadedStrings #-}
-- | Stream fusion and other AST-to-AST optimizations.
-- Runs after reduction, before codegen.
module Core.Optimize (optimize) where

import Core.Syntax
import qualified Data.Text as T
import Data.List (nub)

-- | Apply all optimization passes to the top-level expression.
optimize :: Expr -> Expr
optimize = wrapTailRecBindings . everywhere betaReduce . everywhere fuseFoldMap

-- | Apply a transformation to every sub-expression (bottom-up).
everywhere :: (Expr -> Expr) -> Expr -> Expr
everywhere f = f . go
  where
    go (BinOp op l r)     = BinOp op (everywhere f l) (everywhere f r)
    go (App fn arg)       = App (everywhere f fn) (everywhere f arg)
    go (Lam p body)       = Lam p (everywhere f body)
    go (Record tag bs)    = Record tag (map goB bs)
    go (FieldAccess e fld) = FieldAccess (everywhere f e) fld
    go (Namespace bs)     = Namespace (map goB bs)
    go (Case scrut alts)  = Case (everywhere f scrut) (map goA alts)
    go (Thunk body)       = Thunk (everywhere f body)
    go (ListLit es)       = ListLit (map (everywhere f) es)
    go (With body bs)     = With (everywhere f body) (map goB bs)
    go (Quote e)          = Quote (everywhere f e)
    go (Splice e)         = Splice (everywhere f e)
    go e                  = e  -- IntLit, FloatLit, SizedInt, SizedFloat, StringLit, Name, Import, CFunction, Error

    goB b = b { bindBody = everywhere f (bindBody b) }
    goA (Alt pat guard body) = Alt pat (fmap (everywhere f) guard) (everywhere f body)

-- | Stream fusion rewrites:
--   fold f acc (map g xs)    → fold (\a x -> f a (g x)) acc xs
--   fold f acc (filter p xs) → fold (\a x -> if (p x) (f a x) a) acc xs
fuseFoldMap :: Expr -> Expr
fuseFoldMap
  -- fold f acc (map g xs) → fold (\a x -> f a (g x)) acc xs
  (App (App (App (Name "fold") f) acc)
       (App (App (Name "map") g) xs))
  = fuseFoldMap $
    App (App (App (Name "fold") (fusedMapBody f g)) acc) xs

fuseFoldMap
  -- fold f acc (filter p xs) → fold (\a x -> if (p x) (f a x) a) acc xs
  (App (App (App (Name "fold") f) acc)
       (App (App (Name "filter") p) xs))
  = fuseFoldMap $
    App (App (App (Name "fold") (fusedFilterBody f p)) acc) xs

fuseFoldMap e = e

-- | Beta-reduce: App (Lam x body) arg → body[x := arg]
betaReduce :: Expr -> Expr
betaReduce (App (Lam x body) arg) = betaReduce (subst x arg body)
betaReduce e = e

-- | Substitute all free occurrences of a variable with an expression.
subst :: T.Text -> Expr -> Expr -> Expr
subst x r (Name n) | n == x = r
subst _ _ e@(Name _) = e
subst x r (App fn arg) = App (subst x r fn) (subst x r arg)
subst x _ e@(Lam p _) | p == x = e  -- shadowed
subst x r (Lam p body) = Lam p (subst x r body)
subst x r (BinOp op l r2) = BinOp op (subst x r l) (subst x r r2)
subst x r (Case scrut alts) = Case (subst x r scrut) (map goA alts)
  where goA (Alt pat guard body)
          | bindsName x pat = Alt pat guard body  -- shadowed by pattern
          | otherwise = Alt pat (fmap (subst x r) guard) (subst x r body)
subst x r (With body bs) = With (subst x r body) (map goB bs)
  where goB b | bindName b == x = b  -- shadowed
              | otherwise = b { bindBody = subst x r (bindBody b) }
subst x r (Record tag bs) = Record tag (map (\b -> b { bindBody = subst x r (bindBody b) }) bs)
subst x r (FieldAccess e fld) = FieldAccess (subst x r e) fld
subst x r (Thunk body) = Thunk (subst x r body)
subst x r (ListLit es) = ListLit (map (subst x r) es)
subst x r (Quote e) = Quote (subst x r e)
subst x r (Splice e) = Splice (subst x r e)
subst _ _ e = e  -- IntLit, FloatLit, StringLit, etc.

-- | Check if a pattern binds a given variable name.
bindsName :: T.Text -> Pat -> Bool
bindsName n (PVar v) = n == v
bindsName n (PRec _ bs) = any (\(_, p) -> bindsName n p) bs
bindsName _ _ = False

-- | Build: \a x -> f a (g x)
fusedMapBody :: Expr -> Expr -> Expr
fusedMapBody f g =
  Lam "_fa" (Lam "_fx" (App (App f (Name "_fa")) (App g (Name "_fx"))))

-- | Build: \a x -> if (p x) (f a x) a
-- The if is encoded as the double-case truthiness pattern that the reducer produces.
fusedFilterBody :: Expr -> Expr -> Expr
fusedFilterBody f p =
  Lam "_fa" (Lam "_fx"
    (ifExpr (App p (Name "_fx"))
            (App (App f (Name "_fa")) (Name "_fx"))
            (Name "_fa")))

-- | Build the canonical if-then-else double-case pattern.
-- This matches what the reducer produces for `if cond then else`,
-- and will be detected by the EXPR_IF optimization in codegen.
ifExpr :: Expr -> Expr -> Expr -> Expr
ifExpr cond thenBr elseBr =
  Case (truthinessCheck cond)
    [ Alt (PLit (IntLit 0)) Nothing elseBr
    , Alt PWild Nothing thenBr
    ]

truthinessCheck :: Expr -> Expr
truthinessCheck cond = Case cond
  [ Alt (PRec "False" [])          Nothing (IntLit 0)
  , Alt (PRec "Nil" [])            Nothing (IntLit 0)
  , Alt (PRec "Nothing" [])        Nothing (IntLit 0)
  , Alt (PLit (IntLit 0))          Nothing (IntLit 0)
  , Alt (PLit (StringLit ""))      Nothing (IntLit 0)
  , Alt PWild                      Nothing (IntLit 1)
  ]

-- | Transform top-level self-recursive functions into With-wrapped form
-- so that matchTailLoop in codegen can detect them.
--
-- A binding like:
--   collatz = Lam n (Lam acc body)     -- body contains App(App(Name "collatz") ...) in tail position
--
-- Becomes:
--   collatz = Lam n (Lam acc (With (App(App(Name "_collatz_loop") (Name "n")) (Name "acc"))
--                                  [Binding "_collatz_loop" (Lam n (Lam acc body'))]))
--
-- where body' has "collatz" references replaced with "_collatz_loop".
wrapTailRecBindings :: Expr -> Expr
wrapTailRecBindings = go
  where
    go (Namespace bs) = Namespace (wrapBindingGroup bs)
    go (With body bs) = With (go body) (wrapBindingGroup bs)
    go (Lam p body) = Lam p (go body)
    go (App f x) = App (go f) (go x)
    go (Case scrut alts) = Case (go scrut) (map goAlt alts)
    go (BinOp op l r) = BinOp op (go l) (go r)
    go (Record tag bs) = Record tag (map goB bs)
    go (FieldAccess e f) = FieldAccess (go e) f
    go (Thunk e) = Thunk (go e)
    go (ListLit es) = ListLit (map go es)
    go e = e

    goAlt (Alt pat guard body) = Alt pat (fmap go guard) (go body)
    goB b = b { bindBody = go (bindBody b) }

    -- Process a group of bindings: try mutual TCO, then individual self-TCO
    wrapBindingGroup bs =
      let mutualGroups = findMutualGroups bs
          -- Apply mutual wrapping, collecting which bindings were handled
          (mutualWrapped, handledNames) = applyMutualGroups mutualGroups bs
          -- Apply self-TCO to remaining bindings
      in map (\b -> if bindName b `elem` handledNames
                    then b  -- already handled by mutual TCO
                    else wrapAndRecurse b) mutualWrapped

    wrapAndRecurse b
      | bindDomain b == Value
      , let name = bindName b
      , not (T.null name)
      , let body = bindBody b
      , (params, innerBody) <- unwrapLams' body
      , not (null params)
      , isTailRecursive name innerBody
      , not (hasNonTailSelfCalls name innerBody) =
          let loopName = "_" <> name <> "_loop"
              renamedBody = renameSelfCalls name loopName innerBody
              callArgs = map (Name . bindName) params
              callExpr = foldl App (Name loopName) callArgs
              loopBinding = Binding
                { bindName = loopName
                , bindParams = []
                , bindBody = wrapLams params renamedBody
                , bindDomain = Value
                , bindPos = Nothing
                }
          in b { bindBody = wrapLams params (With callExpr [loopBinding]) }
      | otherwise = b { bindBody = go (bindBody b) }

-- | Unwrap lambda chain into (params as bindings, inner body)
unwrapLams' :: Expr -> ([Binding], Expr)
unwrapLams' (Lam p body) =
  let paramBinding = Binding { bindName = p, bindParams = [], bindBody = Name p, bindDomain = Value, bindPos = Nothing }
      (rest, inner) = unwrapLams' body
  in (paramBinding : rest, inner)
unwrapLams' e = ([], e)

-- | Re-wrap a body in lambdas from a list of param bindings
wrapLams :: [Binding] -> Expr -> Expr
wrapLams [] body = body
wrapLams (b:bs) body = Lam (bindName b) (wrapLams bs body)

-- | Check if an expression contains self-calls in tail position
isTailRecursive :: T.Text -> Expr -> Bool
isTailRecursive name = go
  where
    go (App fn _) = isSelfCall name fn || go fn
    go (Case _ alts) = any (\(Alt _ _ body) -> go body) alts
    go (With body _) = go body
    go _ = False

    isSelfCall n (Name n') = n == n'
    isSelfCall n (App fn _) = isSelfCall n fn
    isSelfCall _ _ = False

-- | Check if self-calls appear in non-tail positions
hasNonTailSelfCalls :: T.Text -> Expr -> Bool
hasNonTailSelfCalls name = not . allTailOnly name

-- | Returns True if all occurrences of `name` are in tail-call position
allTailOnly :: T.Text -> Expr -> Bool
allTailOnly name = tailGo
  where
    -- In tail position: self-call is OK, recurse into tail subexpressions
    tailGo (App fn arg)
      | isSelfApp fn = not (containsName name arg) && argsOk fn
      | otherwise = tailGo fn && not (containsName name arg)
    tailGo (Case scrut alts) =
      not (containsName name scrut) &&
      all (\(Alt _ g body) -> maybe True (not . containsName name) g && tailGo body) alts
    tailGo (With body bs) = tailGo body && all (\b -> not (containsName name (bindBody b))) (filter (\b -> bindName b /= name) bs)
    tailGo (BinOp _ l r) = not (containsName name l) && not (containsName name r)
    tailGo (Lam _ body) = not (containsName name body)
    tailGo (Name n) = n /= name
    tailGo _ = True

    isSelfApp (Name n) = n == name
    isSelfApp (App fn _) = isSelfApp fn
    isSelfApp _ = False

    argsOk (Name _) = True
    argsOk (App fn arg) = argsOk fn && not (containsName name arg)
    argsOk _ = True

-- | Check if an expression contains a reference to the given name
containsName :: T.Text -> Expr -> Bool
containsName name = go
  where
    go (Name n) = n == name
    go (App fn arg) = go fn || go arg
    go (BinOp _ l r) = go l || go r
    go (Lam _ body) = go body
    go (Case scrut alts) = go scrut || any (\(Alt _ g body) -> maybe False go g || go body) alts
    go (With body bs) = go body || any (go . bindBody) bs
    go (Record _ bs) = any (go . bindBody) bs
    go (FieldAccess e _) = go e
    go (Thunk body) = go body
    go (ListLit es) = any go es
    go (Quote e) = go e
    go (Splice e) = go e
    go (Namespace bs) = any (go . bindBody) bs
    go _ = False

-- | Rename self-calls from oldName to newName
renameSelfCalls :: T.Text -> T.Text -> Expr -> Expr
renameSelfCalls oldName newName = go
  where
    go (Name n) | n == oldName = Name newName
    go (App fn arg) = App (go fn) (go arg)
    go (BinOp op l r) = BinOp op (go l) (go r)
    go (Lam p body) = Lam p (go body)
    go (Case scrut alts) = Case (go scrut) (map goA alts)
    go (With body bs) = With (go body) (map goB bs)
    go (Record tag bs) = Record tag (map goB bs)
    go (FieldAccess e fld) = FieldAccess (go e) fld
    go (Thunk body) = Thunk (go body)
    go (ListLit es) = ListLit (map go es)
    go (Quote e) = Quote (go e)
    go (Splice e) = Splice (go e)
    go e = e

    goB b = b { bindBody = go (bindBody b) }
    goA (Alt p g body) = Alt p (fmap go g) (go body)

-- ── Mutual Tail Call Optimization ──────────────────────────────────

-- | Find groups of mutually-recursive functions in a binding list.
-- Returns groups of 2+ functions that call each other in tail position only.
findMutualGroups :: [Binding] -> [[T.Text]]
findMutualGroups bs =
  let -- Only consider value bindings with lambda bodies
      candidates = [(bindName b, b) | b <- bs, bindDomain b == Value,
                    not (T.null (bindName b)),
                    let (ps, _) = unwrapLams' (bindBody b), not (null ps)]
      candNames = map fst candidates
      -- Build call graph: for each function, which other candidates does it call in tail position?
      callsOf name body =
        let (_, innerBody) = unwrapLams' body
        in [n | n <- candNames, n /= name, isTailRecursive n innerBody]
      edges = [(name, callsOf name (bindBody b)) | (name, b) <- candidates]
      -- Find connected components (simple: group functions that call each other)
      groups = findConnectedComponents edges
      -- Filter: only groups where every member calls at least one other member in tail position
      -- AND has no non-tail calls to any group member
      validGroup grp =
        length grp >= 2 &&
        all (\name -> case lookup name candidates of
          Just b ->
            let (_, innerBody) = unwrapLams' (bindBody b)
            in any (\other -> isTailRecursive other innerBody) (filter (/= name) grp)
               && all (\other -> not (hasNonTailSelfCalls other innerBody)) grp
          Nothing -> False) grp
  in filter validGroup groups

-- | Find connected components from a directed edge list (simple BFS/DFS)
findConnectedComponents :: [(T.Text, [T.Text])] -> [[T.Text]]
findConnectedComponents edges = go (map fst edges) []
  where
    -- Build undirected adjacency
    adjOf n = case lookup n edges of
      Just targets -> targets
      Nothing -> []

    go [] acc = acc
    go (n:rest) acc
      | any (n `elem`) acc = go rest acc
      | otherwise =
          let component = reachable [n] []
          in go rest (component : acc)

    reachable [] visited = nub visited
    reachable (n:queue) visited
      | n `elem` visited = reachable queue visited
      | otherwise =
          let neighbors = adjOf n
              -- Also find nodes that call n (reverse edges)
              callers = [name | (name, targets) <- edges, n `elem` targets]
              newNodes = filter (`notElem` visited) (neighbors ++ callers)
          in reachable (queue ++ newNodes) (n : visited)

-- | Apply mutual TCO groups to a binding list.
-- Returns (updated bindings, list of names that were handled).
applyMutualGroups :: [[T.Text]] -> [Binding] -> ([Binding], [T.Text])
applyMutualGroups groups bs = foldr applyGroup (bs, []) groups
  where
    applyGroup grp (curBs, handled) =
      let -- Get the bindings for this group, preserving order
          grpBs = [b | b <- curBs, bindName b `elem` grp]
          -- All must have the same number of params
          paramCounts = [(bindName b, length (fst (unwrapLams' (bindBody b)))) | b <- grpBs]
          allSameParams = case map snd paramCounts of
            [] -> False
            (n:ns) -> all (== n) ns && n > 0
      in if not allSameParams || length grpBs < 2
         then (curBs, handled)
         else let newBs = wrapMutualGroup grpBs curBs
              in (newBs, handled ++ map bindName grpBs)

-- | Wrap a group of mutually-recursive functions into a single dispatched function.
--
-- isEven n = n -> 0 = 1; _ = isOdd (n - 1)
-- isOdd n = n -> 0 = 0; _ = isEven (n - 1)
--
-- Becomes:
-- _isEven_isOdd_loop = \tag -> \n ->
--   tag -> 0 = (n -> 0 = 1; _ = _isEven_isOdd_loop 1 (n - 1))
--          1 = (n -> 0 = 0; _ = _isEven_isOdd_loop 0 (n - 1))
-- isEven = \n -> _isEven_isOdd_loop 0 n
-- isOdd = \n -> _isEven_isOdd_loop 1 n
wrapMutualGroup :: [Binding] -> [Binding] -> [Binding]
wrapMutualGroup grpBs allBs =
  let names = map bindName grpBs
      loopName = "_" <> T.intercalate "_" names <> "_loop"
      tagParam = "_mtag"

      -- Use params from first function (all have same count)
      (params, _) = unwrapLams' (bindBody (head grpBs))
      paramNames = map bindName params

      -- Build tag->index mapping
      nameToTag = zip names [0::Int ..]

      -- Rename all mutual calls in each body to use the loop function with tag
      renameMutualCalls body =
        foldr (\(name, tag) b -> renameToTagged name loopName tag b) body nameToTag

      -- Build Case alts: one per function in the group
      tagAlts = [Alt (PLit (IntLit (fromIntegral tag)))
                     Nothing
                     (let (_, innerBody) = unwrapLams' (bindBody b)
                      in renameMutualCalls innerBody)
                | (b, (_, tag)) <- zip grpBs nameToTag]

      -- The dispatch body: tag -> 0 = body0; 1 = body1; ...
      dispatchBody = Case (Name tagParam) tagAlts

      -- Full loop function: \tag -> \p1 -> \p2 -> ... -> dispatch
      loopBody = Lam tagParam (wrapLams params dispatchBody)
      loopBinding = Binding
        { bindName = loopName
        , bindParams = []
        , bindBody = loopBody
        , bindDomain = Value
        , bindPos = Nothing
        }

      -- Wrapper for each original function: \p1 -> ... -> _loop tag p1 ...
      mkWrapper name tag = Binding
        { bindName = name
        , bindParams = []
        , bindBody = wrapLams params
            (With (foldl App (Name loopName) (IntLit (fromIntegral tag) : map Name paramNames))
                  [loopBinding])
        , bindDomain = Value
        , bindPos = Nothing
        }

      wrappers = [mkWrapper name tag | (name, tag) <- nameToTag]

      -- Replace original bindings with wrappers
      replaceBinding b = case lookup (bindName b) (zip names wrappers) of
        Just w  -> w
        Nothing -> b
  in map replaceBinding allBs

-- | Rename calls to a function name into calls to a tagged loop function.
-- e.g. renameToTagged "isOdd" "_loop" 1 (App (Name "isOdd") x)
--   => App (App (Name "_loop") (IntLit 1)) x
renameToTagged :: T.Text -> T.Text -> Int -> Expr -> Expr
renameToTagged oldName loopName tag = go
  where
    go e | Just args <- matchCall oldName e =
      foldl App (Name loopName) (IntLit (fromIntegral tag) : map go args)
    go (App fn arg) = App (go fn) (go arg)
    go (BinOp op l r) = BinOp op (go l) (go r)
    go (Lam p body) = Lam p (go body)
    go (Case scrut alts) = Case (go scrut) (map goA alts)
    go (With body bs) = With (go body) (map goB bs)
    go (Record t bs) = Record t (map goB bs)
    go (FieldAccess e fld) = FieldAccess (go e) fld
    go (Thunk body) = Thunk (go body)
    go (ListLit es) = ListLit (map go es)
    go e = e

    goB b = b { bindBody = go (bindBody b) }
    goA (Alt p g body) = Alt p (fmap go g) (go body)

    -- Match a call chain: App(App(Name n, a1), a2) → Just [a1, a2]
    matchCall n expr = go' expr []
      where
        go' (App f x) args = go' f (x : args)
        go' (Name n') args | n' == n, not (null args) = Just args
        go' _ _ = Nothing
