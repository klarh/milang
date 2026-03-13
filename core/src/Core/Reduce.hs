{-# LANGUAGE OverloadedStrings #-}
module Core.Reduce
  ( reduce, reduceWithEnv, Env, emptyEnv
  , Warning(..), warnings
  , exprFreeVars
  , envInsert, envMap, envLookup
  , protectPrelude
  , substExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Graph (stronglyConnComp, SCC(..))
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Char (isUpper)
import Data.Maybe (isNothing)

import Core.Syntax
import Core.Prelude (ffiNamespace)

-- ── Environment ───────────────────────────────────────────────────

data Env = Env
  { envMap    :: !(Map.Map Text Expr)
  , envRec    :: !(Set.Set Text)      -- recursive (cyclic) names
  , envImpure :: !(Set.Set Text)      -- world-tainted names
  , envLetBound :: !(Set.Set Text)    -- names from With bindings (have runtime let-binding)
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
emptyEnv = Env Map.empty Set.empty Set.empty Set.empty Map.empty Map.empty []

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
isResidual (Record _ bs)   = any (isResidual . bindBody) bs
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

-- | Unwrap a chain of Lam constructors into param names and inner body.
collectLams :: Expr -> ([Text], Expr)
collectLams (Lam p body) = let (ps, b) = collectLams body in (p:ps, b)
collectLams e            = ([], e)

-- | True if the expression is a CFunction applied to at least one argument
-- (i.e., an actual C function call, not just a reference to the function).
isCFunctionCall :: Expr -> Bool
isCFunctionCall e = case collectApp e of
  (CFunction {}, _:_) -> True
  _ -> False

-- | True if a name has a :~ [pure] trait annotation, allowing inlining
-- of its value even when it's a CFunction call.
isPureTrait :: Text -> Env -> Bool
isPureTrait n env = case Map.lookup n (envTraits env) of
  Just traitExpr -> "pure" `Set.member` resolveEffects Map.empty traitExpr
  Nothing -> False

isOperatorName :: Text -> Bool
isOperatorName t = not (T.null t) && T.all (`elem` ("+-*/^<>=!&|@%?:" :: String)) t

-- ── Main reduce ───────────────────────────────────────────────────

-- | Top-level reduce: returns (reduced expr, warnings).
-- Type/trait checking is integrated into the unified reduction pass.
reduce :: Env -> Expr -> (Expr, [Warning])
reduce env e = case e of
  Namespace _ ->
    let letNames = Set.fromList [bindName b | b <- nsBindings e]
        envWithLet = env { envLetBound = Set.union letNames (envLetBound env) }
        env' = evalBindings maxDepth envWithLet (nsBindings e)
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
    let letNames = Set.fromList [bindName b | b <- nsBindings e]
        envWithLet = env { envLetBound = Set.union letNames (envLetBound env) }
        env' = evalBindings maxDepth envWithLet (nsBindings e)
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

reduceD d env (Name n) =
    case envLookup n env of
      Just (Name m) | m == n -> Name n  -- self-reference; don't recurse
      Just (Name m) | d <= 0 -> Name m  -- depth limit: substitute but don't recurse
      Just (Name m) -> reduceD (d - 1) env (Name m)  -- follow name aliases
      Just val@(Lam _ _) -> val
      Just val@(Namespace _) -> val  -- Namespace is a value, don't re-reduce
      Just val | isConcrete val -> if d <= 0 then val else reduceD (d - 1) env val
      -- Non-concrete value from a With/let binding: keep as Name so the
      -- runtime evaluates it once (prevents duplicating FFI side effects).
      Just _ | n `Set.member` envLetBound env -> Name n
      -- Non-concrete FFI call result: keep as Name to prevent duplicating
      -- side-effectful C function calls (even from lambda parameters).
      -- Functions annotated :~ [pure] are exempt and may be freely inlined.
      Just val | isCFunctionCall val, not (isPureTrait n env) -> Name n
      Just val | d <= 0 -> val  -- depth limit: return value without further reduction
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
                  Just fn ->
                    -- Apply all args at once by inserting into env, then reduce
                    -- the innermost body once.  This avoids intermediate Lam-body
                    -- reductions that would double-unfold mutual recursion.
                    let (params, body) = collectLams fn
                        nApply = min (length params) (length args')
                        applyParams = take nApply params
                        applyArgs = take nApply args'
                        -- Alpha-rename params that would capture free vars in args
                        argFVsUnion = Set.unions (map exprFreeVars applyArgs)
                        capturedSet = Set.intersection argFVsUnion
                                        (Set.fromList applyParams)
                        allNames = Set.unions [ argFVsUnion, exprFreeVars body
                                             , Map.keysSet (envMap env) ]
                        (applyParams', body') = if Set.null capturedSet
                          then (applyParams, body)
                          else foldr (\p (ps, b) ->
                            if p `Set.member` capturedSet
                              then let fresh = freshName p (Set.unions
                                         [allNames, Set.fromList ps])
                                   in (fresh:ps, substExpr p (Name fresh) b)
                              else (p:ps, b)) ([], body) applyParams
                        env' = foldl (\e (p, a) -> envInsert p a e) env
                                     (zip applyParams' applyArgs)
                        d' = d - nApply
                    in if nApply < length args'
                       -- More args than params: apply leftover via reduceApp
                       then let base = reduceD d' env' body'
                            in foldl (reduceApp d' env) base (drop nApply args')
                       else let base = reduceD d' env' body'
                                -- Re-wrap with any unapplied params
                                remaining = drop nApply params
                            in foldr Lam base remaining
                  Nothing -> foldl App (Name n) args'
           else foldl App (Name n) args'
      _ ->
        let f' = reduceD d env f
        in case f' of
          -- Call-by-name for lambda application: don't evaluate arg until needed.
          -- This prevents divergence in recursive branches (e.g., if cond t e).
          -- Exception: syntactic lambdas are always safe to reduce (they can't
          -- diverge), and must be reduced so their free variables are resolved
          -- before being captured in closures.
          Lam _ _ -> let x' = case x of
                           Lam _ _ -> reduceD d env x
                           _       -> x
                     in reduceApp d env f' x'
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
  let -- Mark With-bound names so the Name handler knows not to inline
      -- non-concrete values (prevents duplicating FFI side effects)
      letNames = Set.fromList [bindName b | b <- bindings]
      envWithLet = env { envLetBound = Set.union letNames (envLetBound env) }
      env' = evalBindings d envWithLet bindings
      body' = reduceD d env' body
      bs' = map (reduceBind d env') bindings
      keepBinding b = isResidual (bindBody b) || envIsImpure (bindName b) env'
      residualBs = filter keepBinding bs'
  in if null residualBs then body' else With body' residualBs

reduceD d env (Record tag bindings) =
  Record tag (map (reduceBind d env) bindings)

reduceD d env (FieldAccess e field) =
  let e' = forceThunk d env (reduceD d env e)
      result = reduceFieldAccess e' field
  in case result of
       With _ _ -> reduceD d env result
       _        -> result

reduceD d env (Namespace bindings) =
  let expanded = concatMap expandUnion bindings
      (valueBs, annotBs) = partitionBindings expanded
      mergedValues = mergeOpenDefs valueBs
      letNames = Set.fromList [bindName b | b <- expanded]
      envWithLet = env { envLetBound = Set.union letNames (envLetBound env) }
      env' = evalBindings d envWithLet expanded
      valBs' = map (reduceBind d env') mergedValues
      annBs' = map (reduceBind d env') annotBs
  in Namespace (valBs' ++ annBs')

reduceD d env (Case scrut alts) =
  let scrut' = forceThunk d env (reduceD d env scrut)
  in if isResidual scrut'
     then case scrut of
       -- When the scrutinee is a name from env and ANY alt body references it,
       -- shadow the name with a fresh binding to prevent double evaluation.
       -- For non-let-bound names (e.g. lambda params bound to FFI calls),
       -- wrap in a With binding so the scrutinee is evaluated once at runtime.
       Name n
         | Map.member n (envMap env)
         , any (\(Alt _ g b) ->
             n `Set.member` exprFreeVars b
             || maybe False ((n `Set.member`) . exprFreeVars) g) alts ->
           let allNames = Set.unions
                 [ Map.keysSet (envMap env), exprFreeVars scrut'
                 , Set.unions (concatMap (\(Alt _ g b) ->
                     exprFreeVars b : maybe [] ((:[]) . exprFreeVars) g) alts) ]
               fresh = freshName n allNames
               needsWrap = not (n `Set.member` envLetBound env)
               envShadow = envInsert n (Name fresh) (envDelete fresh env)
               envBase = if needsWrap
                         then envShadow { envLetBound = Set.insert fresh (envLetBound envShadow) }
                         else envShadow
               alts' = map (\alt@(Alt p g b) -> case p of
                 PWild | n `Set.member` exprFreeVars b
                         || maybe False ((n `Set.member`) . exprFreeVars) g ->
                   let envA = Set.foldl' (\e v -> envDelete v e) envBase (patVars (PVar fresh))
                   in Alt (PVar fresh) (fmap (reduceD d envA) g) (reduceD d envA b)
                 _ ->
                   -- For non-PWild patterns: when needsWrap, use envBase (fresh
                   -- will be bound by the With); otherwise use env (n is already
                   -- let-bound and safe to reference directly).
                   let envN = if needsWrap then envBase else env
                       envA = Set.foldl' (\e v -> envDelete v e) envN (patVars p)
                   in Alt p (fmap (reduceD d envA) g) (reduceD d envA b)) alts
           in if needsWrap
              then let binding = Binding { bindName = fresh, bindParams = []
                                         , bindBody = scrut'
                                         , bindDomain = Value, bindPos = Nothing }
                   in With (Case (Name fresh) alts') [binding]
              else Case scrut' alts'
       _ ->
         Case scrut' (map (\(Alt p g b) ->
            -- Alpha-rename pattern vars that collide with free vars in env
            -- values to prevent variable capture (e.g. fold callback capture).
            let pvs = patVars p
                envValFVs = Set.unions (map exprFreeVars (Map.elems (envMap env)))
                captured = Set.intersection pvs envValFVs
            in if Set.null captured
               then let env' = Set.foldl' (\e v -> envDelete v e) env pvs
                    in Alt p (fmap (reduceD d env') g) (reduceD d env' b)
               else let allNames = Set.unions [ pvs, envValFVs, Map.keysSet (envMap env)
                                              , exprFreeVars b
                                              , maybe Set.empty exprFreeVars g ]
                        (p', sub) = Set.foldl' (\(pat, s) v ->
                          let fresh = freshName v (Set.unions [allNames, Set.fromList (map fst s)])
                          in (substPat v fresh pat, (v, Name fresh) : s)
                          ) (p, []) captured
                        applyS e = foldl (\e' (old, new) -> substExpr old new e') e sub
                        env' = Set.foldl' (\e v -> envDelete v e) env (patVars p')
                    in Alt p' (fmap (reduceD d env' . applyS) g) (reduceD d env' (applyS b))
            ) alts)
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
      -- Bindings annotated :~ [pure] are never marked impure — the user asserts
      -- the call chain is side-effect-free, and the trait checker validates that
      -- independently.  For all other bindings, mark impure if world-tainted OR
      -- if the reduced value is a C FFI call.
      env1 = if isPureTrait name env
             then env
             else if isWorldTainted env body || isCFunctionCall val
                  then envMarkImpure name env else env
      -- Emit warnings for Error nodes in reduced value
      env2 = case val of
        Error msg -> envAddWarning (GeneralWarning (bindPos b) (name <> ": " <> msg)) env1
        _         -> env1
  in case val of
       Error _ -> env2  -- don't insert error nodes (type mismatches, etc.)
       _       -> envInsert name val env2

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
             env' = (envInsert fresh quoted env) { envLetBound = Set.delete fresh (envLetBound env) }
         in reduceD d' env' body'
       else let quoted = quoteExpr arg
                env' = (envInsert realName quoted env) { envLetBound = Set.delete realName (envLetBound env) }
            in reduceD d' env' body
reduceApp d env (Lam p body) arg =
  let d' = d - 1
      argFVs = exprFreeVars arg
      -- When the arg is a CFunction call, bind it via With so the Name handler's
      -- isCFunctionCall guard doesn't produce a dangling Name reference.
      needsLetBind = isCFunctionCall arg && not (isPureTrait p env)
      -- Check if overwriting p in env would capture vars in env-stored lambdas
      bodyFVs = exprFreeVars body
      envCapturesP = any (\fv -> case envLookup fv env of
        Just v  -> p `Set.member` exprFreeVars v
        Nothing -> False) (Set.toList bodyFVs)
      needsRename = p `Set.member` argFVs || envCapturesP
  in if needsRename
     -- Param name collides — alpha-rename to avoid capture
     then let allNames = Set.unions [argFVs, exprFreeVars body, Map.keysSet (envMap env)]
              fresh = freshName p allNames
              body' = substExpr p (Name fresh) body
          in if needsLetBind
             then let env' = (envInsert fresh (Name fresh) env) { envLetBound = Set.insert fresh (envLetBound env) }
                      result = reduceD d' env' body'
                      binding = Binding { bindName = fresh, bindParams = []
                                        , bindBody = arg, bindDomain = Value, bindPos = Nothing }
                  in if fresh `Set.member` exprFreeVars result
                     then With result [binding]
                     else result
             else let env' = (envInsert fresh arg env) { envLetBound = Set.delete fresh (envLetBound env) }
                  in reduceD d' env' body'
     else if needsLetBind
          then let env' = (envInsert p (Name p) env) { envLetBound = Set.insert p (envLetBound env) }
                   result = reduceD d' env' body
                   binding = Binding { bindName = p, bindParams = []
                                     , bindBody = arg, bindDomain = Value, bindPos = Nothing }
               in if p `Set.member` exprFreeVars result
                  then With result [binding]
                  else result
          else let -- Clear p from envLetBound: lambda params shadow let bindings
                   env' = (envInsert p arg env) { envLetBound = Set.delete p (envLetBound env) }
               in reduceD d' env' body
-- Sized type constructors: Int' N val, UInt' N val, Float' N val
-- Int'/UInt'/Float' are uppercase names, so they auto-construct as records.
-- After first arg (width), we get Record "Int'" [_0=width].
-- On second arg (value), we intercept and produce SizedInt/SizedFloat.
-- When val is a literal, we clamp at compile time.
-- When val is a residual, we emit a runtime clamp call.
reduceApp _ _ (Record "Int'" [b]) val
  | Just w <- intVal (bindBody b), Just n <- intVal val =
      let w' = fromInteger w in if w' == 0 then SizedInt n w' True
      else SizedInt (clampSigned w' n) w' True
  | Just w <- intVal (bindBody b) =
      App (App (Name "__sized_int") (IntLit w)) val
reduceApp _ _ (Record "UInt'" [b]) val
  | Just w <- intVal (bindBody b), Just n <- intVal val =
      let w' = fromInteger w in if w' == 0 then SizedInt n w' False
      else SizedInt (clampUnsigned w' n) w' False
  | Just w <- intVal (bindBody b) =
      App (App (Name "__sized_uint") (IntLit w)) val
reduceApp _ _ (Record "Float'" [b]) val
  | Just w <- intVal (bindBody b), Just d <- floatVal val = SizedFloat d (fromInteger w)
  | Just w <- intVal (bindBody b), Just n <- intVal val   = SizedFloat (fromInteger n) (fromInteger w)
  | Just _ <- intVal (bindBody b) = val  -- Float coercion is a no-op at runtime
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
-- Builtin: __sized_int w val — runtime signed int clamping
reduceApp _ _ (App (Name "__sized_int") (IntLit w)) val
  | Just n <- intVal val = let w' = fromInteger w in if w' == 0 then SizedInt n w' True
    else SizedInt (clampSigned w' n) w' True
-- Builtin: __sized_uint w val — runtime unsigned int clamping
reduceApp _ _ (App (Name "__sized_uint") (IntLit w)) val
  | Just n <- intVal val = let w' = fromInteger w in if w' == 0 then SizedInt n w' False
    else SizedInt (clampUnsigned w' n) w' False
-- Builtin: __ffi_apply annotateExpr namespace
-- Injects the backend-specific ffi object as first arg to the annotate function,
-- applies it to the namespace, processes descriptors, and returns a modified namespace.
reduceApp d env (App (Name "__ffi_apply") annotateExpr) ns@(Namespace _) =
  let ffiObj = reduceD d env ffiNamespace
      annotateWithFfi = reduceApp d env annotateExpr ffiObj
      descriptors = reduceApp d env annotateWithFfi ns
  in ffiApply descriptors ns
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
  in if w == 0 then SizedInt v w s
     else SizedInt (if s then clampSigned w v else clampUnsigned w v) w s
sizedIntResult v (SizedInt _ w s) _ =
  if w == 0 then SizedInt v w s
  else SizedInt (if s then clampSigned w v else clampUnsigned w v) w s
sizedIntResult v _ (SizedInt _ w s) =
  if w == 0 then SizedInt v w s
  else SizedInt (if s then clampSigned w v else clampUnsigned w v) w s
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
    (v:_) ->
      let fvs = exprFreeVars v
          nsNames = Set.fromList [bindName b | b <- bs,
                                  bindDomain b == Value || bindDomain b == Lazy]
          directDeps = Set.intersection fvs nsNames
          -- Compute transitive closure of namespace dependencies
          allDeps = closureNsDeps bs directDeps nsNames
      in if Set.null allDeps
         then v
         else With v [b | b <- bs, bindName b `Set.member` allDeps]
    []    -> FieldAccess (Namespace bs) field
reduceFieldAccess e field = FieldAccess e field

-- | Compute transitive closure of namespace binding dependencies.
-- Given a set of directly needed names and the full set of namespace names,
-- expand to include all transitively referenced namespace bindings.
closureNsDeps :: [Binding] -> Set.Set Text -> Set.Set Text -> Set.Set Text
closureNsDeps bs pending nsNames = go pending Set.empty
  where
    bMap = Map.fromList [(bindName b, b) | b <- bs]
    go todo visited
      | Set.null todo = visited
      | otherwise =
          let new = Set.difference todo visited
              visited' = Set.union visited new
              newFvs = Set.unions [ exprFreeVars (wrapLambda (bindParams b) (bindBody b))
                                  | n <- Set.toList new
                                  , Just b <- [Map.lookup n bMap] ]
              todo' = Set.intersection newFvs nsNames
          in go todo' visited'

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
reduceRecordUpdate (Namespace bs) updates =
  let overrides = case updates of
        Record _ obs   -> obs
        Namespace obs  -> obs
        _              -> []
      overrideMap = Map.fromList [(bindName o, o) | o <- overrides]
      updated = map (\b -> case Map.lookup (bindName b) overrideMap of
                       Just o  -> o
                       Nothing -> b) bs
      existingNames = Set.fromList [bindName b | b <- bs]
      newFields = filter (\o -> not (bindName o `Set.member` existingNames)) overrides
  in Namespace (updated ++ newFields)
reduceRecordUpdate l r = BinOp "<-" l r

-- ── Case/pattern matching ─────────────────────────────────────────

reduceCase :: Int -> Env -> Expr -> [Alt] -> Expr
reduceCase _ _ scrut [] = Error $ "non-exhaustive pattern match on: " <> T.pack (prettyExpr 0 scrut)
reduceCase d env scrut (Alt pat mGuard body : rest) =
  case matchPat pat scrut of
    Nothing -> reduceCase d env scrut rest
    Just binds ->
      -- Alpha-rename bound names that collide with free vars in env values
      -- to prevent variable capture (e.g. filter's Cons 't' capturing outer 't').
      let boundNames = Set.fromList (map fst binds)
          envValFVs = Set.unions (map exprFreeVars (Map.elems (envMap env)))
          captured = Set.intersection boundNames envValFVs
          (binds', body', mGuard') =
            if Set.null captured then (binds, body, mGuard)
            else let allNames = Set.unions [ boundNames, envValFVs
                                           , Map.keysSet (envMap env)
                                           , exprFreeVars body
                                           , maybe Set.empty exprFreeVars mGuard ]
                     renames = Set.foldl' (\s v ->
                       let fresh = freshName v (Set.unions [allNames, Set.fromList (map snd s)])
                       in (v, fresh) : s) [] captured
                     applyR e = foldl (\e' (old, new) -> substExpr old (Name new) e') e renames
                     renameBind (n, v) = case lookup n renames of
                       Just n' -> (n', v)
                       Nothing -> (n, v)
                 in (map renameBind binds, applyR body, fmap applyR mGuard)
          env' = foldl (\e (n,v) -> envInsert n v e) env binds'
      in case mGuard' of
        Nothing -> reduceD d env' body'
        Just g  ->
          let g' = forceThunk d env' (reduceD d env' g)
          in case g' of
            IntLit 0 -> reduceCase d env scrut rest
            IntLit _ -> reduceD d env' body'
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

-- ── FFI annotation processing ──────────────────────────────────────

-- | Process FFI descriptors and modify a namespace.
-- Extracts struct descriptors from a Cons list, generates constructor
-- bindings, patches CPtr→CStruct in existing CFunction signatures.
ffiApply :: Expr -> Expr -> Expr
ffiApply descriptors (Namespace bindings) =
  case consToList descriptors of
    Just items ->
      let structDescs = concatMap extractStructDesc items
          outDescs = concatMap extractOutDesc items
          opaqueDescs = concatMap extractOpaqueDesc items
          ctorBindings = concatMap ffiCtorBindings structDescs
          -- Extract header from existing CFunction bindings for accessor generation
          hdr = case [h | Binding { bindBody = CFunction h _ _ _ _ } <- bindings] of
                  (h:_) -> h
                  []    -> ""
          opaqueBindings = concatMap (ffiOpaqueBindings hdr) opaqueDescs
          patchedBindings = map (patchCFunBinding structDescs . patchOutParams outDescs) bindings
      in Namespace (patchedBindings ++ ctorBindings ++ opaqueBindings)
    Nothing -> Namespace bindings  -- couldn't parse descriptors, return as-is
ffiApply _ ns = ns

-- | Extract a struct descriptor from a Record.
-- Expected: {_ffi = "struct"; name = "Name"; fields = [{name = "x"; ctype = "int32"}; ...]}
extractStructDesc :: Expr -> [(Text, [(Text, CType)])]
extractStructDesc (Record _ bs) = case fieldLookup "_ffi" bs of
  Just (StringLit "struct") -> case fieldLookup "name" bs of
    Just (StringLit name) -> case fieldLookup "fields" bs >>= consToList of
      Just fieldExprs ->
        let fields = concatMap extractCField fieldExprs
        in [(name, fields)]
      Nothing -> []
    _ -> []
  _ -> []
extractStructDesc _ = []

-- | Extract a (name, CType) pair from a field descriptor Record.
extractCField :: Expr -> [(Text, CType)]
extractCField (Record _ bs) =
  case (fieldLookup "name" bs, fieldLookup "ctype" bs) of
    (Just (StringLit n), Just (StringLit ct)) ->
      case parseCTypeStr ct of
        Just ctype -> [(n, ctype)]
        Nothing    -> []
    _ -> []
extractCField _ = []

-- | Parse a C type string to CType.
parseCTypeStr :: Text -> Maybe CType
parseCTypeStr "int8"    = Just (CInt 8)
parseCTypeStr "int16"   = Just (CInt 16)
parseCTypeStr "int32"   = Just (CInt 32)
parseCTypeStr "int"     = Just (CInt 32)
parseCTypeStr "int64"   = Just (CInt 64)
parseCTypeStr "long"    = Just CLong
parseCTypeStr "uint8"   = Just (CUInt 8)
parseCTypeStr "uint16"  = Just (CUInt 16)
parseCTypeStr "uint32"  = Just (CUInt 32)
parseCTypeStr "uint64"  = Just (CUInt 64)
parseCTypeStr "float"   = Just CFloat
parseCTypeStr "float32" = Just CFloat32
parseCTypeStr "float64" = Just CFloat
parseCTypeStr "double"  = Just CFloat
parseCTypeStr "string"  = Just CString
parseCTypeStr "void"    = Just CVoid
parseCTypeStr s
  | "ptr:" `T.isPrefixOf` s = Just (CPtr (T.drop 4 s))
  | otherwise = Nothing

-- | Generate constructor binding(s) for a struct descriptor.
-- make_Name x y = {x = x; y = y}
ffiCtorBindings :: (Text, [(Text, CType)]) -> [Binding]
ffiCtorBindings (name, fields) =
  let ctorName = "make_" <> name
      fieldNames = map fst fields
      recordExpr = Record "" [ mkBind fn (Name fn) | fn <- fieldNames ]
      body = foldr Lam recordExpr fieldNames
  in [ mkBind ctorName body ]

-- | Patch CFunction bindings: replace CPtr with CStruct where struct is known.
patchCFunBinding :: [(Text, [(Text, CType)])] -> Binding -> Binding
patchCFunBinding structs b = b { bindBody = patchCFunExpr structs (bindBody b) }

patchCFunExpr :: [(Text, [(Text, CType)])] -> Expr -> Expr
patchCFunExpr structs (CFunction hdr fn ret params isStd) =
  CFunction hdr fn (patchCType structs ret) (map (patchCType structs) params) isStd
patchCFunExpr _ e = e

-- | Replace CPtr "Name" with CStructPtr when a matching struct descriptor exists.
-- Strips const/struct qualifiers and trailing parameter names so that
-- e.g. CPtr "const SDL_Rect p" matches "SDL_Rect".
patchCType :: [(Text, [(Text, CType)])] -> CType -> CType
patchCType structs (CPtr ptrName) =
  let stripped = stripPtrQualifiers ptrName
  in case lookup stripped structs of
    Just fields -> CStructPtr stripped fields
    Nothing -> CPtr ptrName
patchCType structs (CStruct sname oldFields) =
  case lookup sname structs of
    Just fields -> CStruct sname fields
    Nothing -> CStruct sname oldFields
patchCType _ t = t

-- | Strip C type qualifiers and trailing parameter name from a pointer type.
-- "const Point p" → "Point", "const SDL_Rect" → "SDL_Rect", "Point" → "Point"
stripPtrQualifiers :: Text -> Text
stripPtrQualifiers t =
  let ws = T.words t
      noQuals = filter (\w -> w /= "const" && w /= "struct" && w /= "volatile") ws
  in case noQuals of
    []  -> t
    [w] -> w
    -- If multiple words remain after removing qualifiers, the last one is likely
    -- the parameter name (e.g. "Point p" → "Point")
    _   -> T.unwords (init noQuals)

-- | Extract out-parameter descriptors.
-- Expected: {_ffi = "out"; func = "name"; params = [{index = 1; ctype = "int32"}; ...]}
extractOutDesc :: Expr -> [(Text, [(Int, CType)])]
extractOutDesc (Record _ bs) = case fieldLookup "_ffi" bs of
  Just (StringLit "out") -> case fieldLookup "func" bs of
    Just (StringLit funcName) -> case fieldLookup "params" bs >>= consToList of
      Just paramExprs ->
        let params = concatMap extractOutParam paramExprs
        in [(funcName, params)]
      Nothing -> []
    _ -> []
  _ -> []
extractOutDesc _ = []

-- | Extract a (index, CType) pair from an out-param descriptor.
extractOutParam :: Expr -> [(Int, CType)]
extractOutParam (Record _ bs) =
  case (fieldLookup "index" bs, fieldLookup "ctype" bs) of
    (Just (IntLit idx), Just (StringLit ct)) ->
      case parseCTypeStr ct of
        Just ctype -> [(fromIntegral idx, ctype)]
        Nothing    -> []
    _ -> []
extractOutParam _ = []

-- | Patch CFunction bindings to mark specified params as out-params.
patchOutParams :: [(Text, [(Int, CType)])] -> Binding -> Binding
patchOutParams outDescs b =
  let fn = bindName b
  in case lookup fn outDescs of
    Just paramPatches -> b { bindBody = patchOutExpr paramPatches (bindBody b) }
    Nothing -> b

patchOutExpr :: [(Int, CType)] -> Expr -> Expr
patchOutExpr patches (CFunction hdr fn ret params isStd) =
  let params' = zipWith (\i p -> case lookup i patches of
                                   Just ct -> COut ct
                                   Nothing -> p) [0..] params
  in CFunction hdr fn ret params' isStd
patchOutExpr _ e = e

-- | Extract opaque type descriptors.
-- Expected: {_ffi = "opaque"; name = "Name"; accessors = [{field = "x"; ctype = "int32"}; ...]; include = "<header.h>" (optional)}
extractOpaqueDesc :: Expr -> [(Text, Text, [(Text, CType)])]
extractOpaqueDesc (Record _ bs) = case fieldLookup "_ffi" bs of
  Just (StringLit "opaque") -> case fieldLookup "name" bs of
    Just (StringLit name) ->
      let incl = case fieldLookup "include" bs of
                   Just (StringLit i) -> i
                   _                  -> ""
      in case fieldLookup "accessors" bs >>= consToList of
        Just accExprs ->
          let accs = concatMap extractCField accExprs
          in [(name, incl, accs)]
        Nothing -> [(name, incl, [])]
    _ -> []
  _ -> []
extractOpaqueDesc _ = []

-- | Generate accessor bindings for an opaque type.
-- Each accessor becomes a CFunction with C name "__acc:StructType:field.path".
-- The codegen detects this prefix and generates an inline C accessor function.
-- If an include path is specified, it overrides the default header.
ffiOpaqueBindings :: Text -> (Text, Text, [(Text, CType)]) -> [Binding]
ffiOpaqueBindings defaultHdr (typeName, incl, accessors) =
  map mkAccessor accessors
  where
    accHdr = if T.null incl then defaultHdr else stripAngleBrackets incl
    isStd = not (T.null incl) && (T.head incl == '<')
    stripAngleBrackets t
      | T.head t == '<' && T.last t == '>' = T.init (T.tail t)
      | otherwise = t
    mkAccessor (fieldPath, ctype) =
      let accName = typeName <> "_" <> T.replace "." "_" fieldPath
          cName = "__acc:" <> typeName <> ":" <> fieldPath
      in mkBind accName (CFunction accHdr cName ctype [CPtr typeName] isStd)

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

-- ── Prelude protection ────────────────────────────────────────────
-- When a user fully overrides a prelude function (chainBodies fails),
-- other prelude functions that reference it would break.  We preserve
-- the prelude version under __p_<name> and rewrite prelude-internal
-- cross-references so they use the protected name.  Open-function
-- extensions (chainBodies succeeds) are left alone.

protectPrelude :: [Binding] -> [Binding] -> [Binding]
protectPrelude preludeBs userBs =
  let preludeValueNames = Set.fromList
        [bindName b | b <- preludeBs, bindDomain b `elem` [Value, Lazy]]
      userValueNames = Set.fromList
        [bindName b | b <- userBs, bindDomain b `elem` [Value, Lazy]]
      conflicts = Set.intersection preludeValueNames userValueNames
  in if Set.null conflicts
     then preludeBs
     else
       let preludeMap = Map.fromList
             [(bindName b, b) | b <- preludeBs, bindDomain b `elem` [Value, Lazy]]
           userMap = Map.fromList
             [(bindName b, b) | b <- userBs, bindDomain b `elem` [Value, Lazy]]
           isOverride name = case (Map.lookup name preludeMap, Map.lookup name userMap) of
             (Just pb, Just ub) ->
               isNothing (chainBodies
                 (wrapLambda (bindParams ub) (bindBody ub))
                 (wrapLambda (bindParams pb) (bindBody pb)))
             _ -> False
           overrides = Set.filter isOverride conflicts
       in if Set.null overrides
          then preludeBs
          else
            let renameMap = Map.fromList
                  [(n, "__p_" <> n) | n <- Set.toList overrides]
                modifyB b
                  | bindDomain b `elem` [Value, Lazy] =
                    b { bindBody = renameOverrides renameMap
                          (Set.fromList (bindParams b)) (bindBody b) }
                  | otherwise = b
                modified = map modifyB preludeBs
                internals = [ (modifyB (preludeMap Map.! n))
                                { bindName = "__p_" <> n }
                            | n <- Set.toList overrides ]
            in modified ++ internals

-- | Rename references to overridden prelude names, respecting scoping.
renameOverrides :: Map.Map Text Text -> Set.Set Text -> Expr -> Expr
renameOverrides renames = go
  where
    rename bound n
      | Set.member n bound = n
      | otherwise = Map.findWithDefault n n renames
    go bound (Name n)          = Name (rename bound n)
    go bound (App f x)         = App (go bound f) (go bound x)
    go bound (BinOp op l r)    = BinOp op (go bound l) (go bound r)
    go bound (Lam p body)      = Lam p (go (Set.insert (lamParamName p) bound) body)
    go bound (Case scrut alts) = Case (go bound scrut) (map (goAlt bound) alts)
    go bound (Record tag bs)   = Record tag (map (goBind bound) bs)
    go bound (FieldAccess e f) = FieldAccess (go bound e) f
    go bound (Namespace bs)    = Namespace (map (goBind bound) bs)
    go bound (With ns bs)      = With (go bound ns) (map (goBind bound) bs)
    go bound (ListLit es)      = ListLit (map (go bound) es)
    go bound (Quote e)         = Quote (go bound e)
    go bound (Splice e)        = Splice (go bound e)
    go bound (Thunk e)         = Thunk (go bound e)
    go _     e                 = e
    goAlt bound (Alt pat guard body) =
      let bound' = Set.union bound (patVars pat)
      in Alt pat (fmap (go bound') guard) (go bound' body)
    goBind bound b =
      let bound' = Set.union bound (Set.fromList (bindParams b))
      in b { bindBody = go bound' (bindBody b) }

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
exprFreeVars (Lam p b)       = Set.delete (lamParamName p) (Set.delete p (exprFreeVars b))
exprFreeVars (Record _ bs)   = Set.unions (map bindingFreeVars bs)
exprFreeVars (FieldAccess e _) = exprFreeVars e
exprFreeVars (Namespace bs)  = bindingsFreeVars bs
exprFreeVars (Case s alts)   = Set.union (exprFreeVars s) (Set.unions (map altFreeVars alts))
exprFreeVars (Thunk e)       = exprFreeVars e
exprFreeVars (ListLit es)    = Set.unions (map exprFreeVars es)
exprFreeVars (With e bs)     = let defined = Set.fromList (map bindName bs)
                               in Set.union (bindingsFreeVars bs) (Set.difference (exprFreeVars e) defined)
exprFreeVars (Import _)      = Set.empty
exprFreeVars (Quote e)       = exprFreeVars e
exprFreeVars (Splice e)      = exprFreeVars e
exprFreeVars (CFunction {})  = Set.empty
exprFreeVars (Error _)       = Set.empty

bindingFreeVars :: Binding -> Set.Set Text
bindingFreeVars b =
  let bodyFVs = exprFreeVars (bindBody b)
      -- Include both raw and stripped param names so quoted params (#t) also
      -- remove their bare splice name (t) from the free variable set.
      paramSet = Set.fromList (bindParams b ++ map lamParamName (bindParams b))
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

-- | Substitute a variable name in a pattern (alpha-rename a bound variable).
substPat :: Text -> Text -> Pat -> Pat
substPat old new = go
  where
    go (PVar v)     = PVar (if v == old then new else v)
    go (PRec t fs)  = PRec t [(f, go p) | (f, p) <- fs]
    go (PList ps mr) = PList (map go ps) (fmap (\v -> if v == old then new else v) mr)
    go p            = p  -- PWild, PLit unchanged

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
      | p == var || lamParamName p == var = Lam p b  -- shadowed
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
  | TSizedInt !Int !Bool   -- bit width, isSigned
  | TSizedFloat !Int       -- bit width
  | TFun MiType MiType
  | TRecord Text [(Text, MiType)]
  | TUnion Text [Text]
  | TVar Text
  | TAny
  | TOverload [MiType]  -- multiple valid type annotations (additive ::)
  deriving (Show, Eq)

type TypeEnv = Map.Map Text MiType

-- | Pretty-print a type for error messages
prettyType :: MiType -> Text
prettyType TInt     = "Int"
prettyType TFloat   = "Float"
prettyType TStr     = "Str"
prettyType (TSizedInt w True)  = "Int' " <> T.pack (show w)
prettyType (TSizedInt w False) = "UInt' " <> T.pack (show w)
prettyType (TSizedFloat w) = "Float' " <> T.pack (show w)
prettyType (TVar v) = v
prettyType TAny     = "?"
prettyType (TFun a b) = prettyType a <> " : " <> prettyType b
prettyType (TUnion name _) = name
prettyType (TOverload ts) = T.intercalate " | " (map prettyType ts)
prettyType (TRecord tag fields) =
  let t = if T.null tag then "" else tag <> " "
      fs = T.intercalate "; " [n <> " = " <> prettyType ty | (n, ty) <- fields]
  in t <> "{" <> fs <> "}"

-- | Check if a type variable occurs in a type (for occurs check)
occursIn :: Text -> MiType -> Bool
occursIn v (TVar v')       = False  -- TVar-to-TVar is just aliasing, not recursive
occursIn v (TFun a b)      = occursInAny v a || occursInAny v b
occursIn v (TRecord _ fs)  = any (occursInAny v . snd) fs
occursIn v (TOverload ts)  = any (occursIn v) ts
occursIn _ _               = False

-- Helper that includes TVar matches (for sub-expression checking)
occursInAny :: Text -> MiType -> Bool
occursInAny v (TVar v')       = v == v'
occursInAny v (TFun a b)      = occursInAny v a || occursInAny v b
occursInAny v (TRecord _ fs)  = any (occursInAny v . snd) fs
occursInAny v (TOverload ts)  = any (occursInAny v) ts
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
exprToType _ (Name "Int")   = TSizedInt 64 True
exprToType _ (Name "UInt")  = TSizedInt 64 False
exprToType _ (Name "Str")   = TStr
exprToType _ (Name "Float") = TSizedFloat 64
exprToType _ (Name "Byte")  = TSizedInt 8 False
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
exprToType _ (App (Name "Int'") (IntLit n))   = TSizedInt (fromInteger n) True
exprToType _ (App (Name "UInt'") (IntLit n))  = TSizedInt (fromInteger n) False
exprToType _ (App (Name "Float'") (IntLit n)) = TSizedFloat (fromInteger n)
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
      -- Group annotations by name, accumulating all exprs
      annotMap = Map.fromListWith (++) [(n, [(e, fromPrelude)]) | (n, e, fromPrelude) <- allAnnots]
  in foldl (go userValues annotMap) builtinTypeEnv bindings
  where
    go userValues annotMap tenv b =
      case Map.lookup (bindName b) annotMap of
        -- Skip prelude annotations when user has redefined the name
        Just pairs | all snd pairs, Set.member (bindName b) userValues -> inferOrDetect tenv b
        Just pairs ->
          let types = [exprToType tenv e | (e, _) <- pairs]
          in case types of
               [t] -> Map.insert (bindName b) t tenv
               ts  -> Map.insert (bindName b) (TOverload ts) tenv
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
    findConstraint (Lam lp e)
      | lp == param = TAny  -- nested lambda shadows the param
      | otherwise = findConstraint e
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
inferExprE _ (IntLit _)        = TInt
inferExprE _ (FloatLit _)      = TFloat
inferExprE _ (SizedInt _ w s)  = TSizedInt (fromInteger w) s
inferExprE _ (SizedFloat _ w)  = TSizedFloat (fromInteger w)
inferExprE _ (StringLit _)     = TStr
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
  let fty = inferExprE tenv f
      xty = inferExprE tenv x
  in case fty of
    TFun _ ret -> ret
    TOverload alts ->
      case [(argTy, retTy) | TFun argTy retTy <- alts] of
        [] -> TAny
        pairs ->
          -- Find overloads whose argument type matches
          let matching = [ret | (argTy, ret) <- pairs, typeMatches argTy xty]
          in case matching of
               []    -> snd (head pairs)  -- fallback
               [ret] -> ret               -- unique match
               rets  -> TOverload rets    -- multiple match: keep overloaded
    -- If f is a Name, check the type env for its return type
    _ -> case f of
      Name n -> case Map.lookup n tenv of
                  Just ty -> inferReturnType ty xty
                  _ -> TAny
      _ -> TAny
  where
    inferReturnType (TFun _ ret) _ = ret
    inferReturnType (TOverload alts) argTy =
      case [(a, r) | TFun a r <- alts] of
        [] -> TAny
        pairs ->
          let matching = [ret | (a, ret) <- pairs, typeMatches a argTy]
          in case matching of
               []    -> snd (head pairs)
               [ret] -> ret
               rets  -> TOverload rets
    inferReturnType _ _ = TAny
    -- Check if expected type is compatible with actual (simple structural match)
    typeMatches TAny _ = True
    typeMatches _ TAny = True
    typeMatches (TVar _) _ = True
    typeMatches _ (TVar _) = True
    typeMatches TInt TInt = True
    typeMatches TFloat TFloat = True
    typeMatches TStr TStr = True
    typeMatches (TSizedInt _ _) (TSizedInt _ _) = True
    typeMatches (TSizedFloat _) (TSizedFloat _) = True
    typeMatches TInt (TSizedInt _ _) = True
    typeMatches (TSizedInt _ _) TInt = True
    typeMatches TFloat (TSizedFloat _) = True
    typeMatches (TSizedFloat _) TFloat = True
    typeMatches (TFun a1 r1) (TFun a2 r2) = typeMatches a1 a2 && typeMatches r1 r2
    typeMatches (TUnion n1 _) (TUnion n2 _) = n1 == n2
    typeMatches (TRecord t1 _) (TUnion _ tags) = t1 `elem` tags
    typeMatches (TUnion _ tags) (TRecord t2 _) = t2 `elem` tags
    typeMatches _ _ = False
inferExprE tenv (Lam p body) =
  -- Infer param type from body usage (e.g., 2*x → x :: Int)
  let inferred = inferParamFromBody tenv body p
      paramTy = if inferred /= TAny then inferred
                else TAny  -- lambda params are locally scoped, don't inherit from outer env
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
    -- Overloaded types: succeed if ANY alternative matches
    go subst (TOverload alts) act =
      case filter (null . snd) [go subst alt act | alt <- alts] of
        ((s, _):_) -> (s, [])      -- at least one matched
        []         -> go subst (head alts) act  -- none matched; report against first
    go subst exp (TOverload alts) =
      case filter (null . snd) [go subst exp alt | alt <- alts] of
        ((s, _):_) -> (s, [])
        []         -> go subst exp (head alts)
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
    -- Sized types: compatible with each other and with TInt/TFloat
    go subst (TSizedInt _ _) (TSizedInt _ _) = (subst, [])
    go subst (TSizedFloat _) (TSizedFloat _) = (subst, [])
    go subst TInt (TSizedInt _ _) = (subst, [])
    go subst (TSizedInt _ _) TInt = (subst, [])
    go subst TFloat (TSizedFloat _) = (subst, [])
    go subst (TSizedFloat _) TFloat = (subst, [])
    go subst (TSizedInt _ _) TFloat = (subst, [])
    go subst TFloat (TSizedInt _ _) = (subst, [])
    go subst (TSizedInt _ _) (TSizedFloat _) = (subst, [])
    go subst (TSizedFloat _) (TSizedInt _ _) = (subst, [])
    go subst TInt (TSizedFloat _) = (subst, [])
    go subst (TSizedFloat _) TInt = (subst, [])
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

-- | Check a binding's body against its LOCAL type annotation(s)
-- Multiple :: annotations for the same name are additive: the body
-- must satisfy at least one of them.
checkTypeBinding :: TypeEnv -> Map.Map Text [Expr] -> Binding -> [Warning]
checkTypeBinding tenv localAnnots b =
  let (annotErrs, tenv') = case Map.lookup (bindName b) localAnnots of
        Nothing -> ([], tenv)
        Just tyExprs ->
          case bindBody b of
            IntLit 0 | null (bindParams b) -> ([], tenv)
            _ -> tryAnnotations tenv tyExprs
      -- When there's no local type annotation, shadow parameter names to
      -- prevent false positives from prelude types (e.g., using `lines` as a
      -- parameter name shouldn't trigger type errors from the prelude's
      -- `lines :: Str : List` annotation).
      paramEnv = case Map.lookup (bindName b) localAnnots of
        Just _  -> tenv'  -- annotation provides proper param types via peelParams
        Nothing -> foldl (\e p -> Map.delete p e) tenv' (bindParams b)
      operandErrs = checkOperands paramEnv (bindPos b) (bindName b) (bindBody b)
  in annotErrs ++ operandErrs
  where
    -- Try each annotation; succeed if ANY matches (no errors)
    tryAnnotations tenv0 [] = ([], tenv0)
    tryAnnotations tenv0 [tyExpr] =
      let expectedType = exprToType tenv0 tyExpr
          (peeledEnv, bodyType) = peelParams tenv0 (bindParams b) expectedType
      in (checkExprAgainst peeledEnv (bindPos b) (bindName b) bodyType (bindBody b), peeledEnv)
    tryAnnotations tenv0 tyExprs =
      let results = [(checkExprAgainst peeledEnv (bindPos b) (bindName b) bodyType (bindBody b), peeledEnv)
                    | tyExpr <- tyExprs
                    , let expectedType = exprToType tenv0 tyExpr
                    , let (peeledEnv, bodyType) = peelParams tenv0 (bindParams b) expectedType]
      in case filter (null . fst) results of
           ((_, env'):_) -> ([], env')     -- at least one matched
           []            -> head results   -- none matched; report first error

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
checkExprAgainst tenv pos name (TOverload alts) expr =
  -- Succeed if ANY overload matches
  let results = [checkExprAgainst tenv pos name alt expr | alt <- alts]
  in case filter null results of
       (_:_) -> []         -- at least one matched
       []    -> head results  -- none matched; report first error
checkExprAgainst tenv pos name expected expr =
  let actual = inferExprE tenv expr
  in snd (checkCompatWith Map.empty pos name expected actual)

-- | Walk an expression and report operand type mismatches
checkOperands :: TypeEnv -> Maybe SrcPos -> Text -> Expr -> [Warning]
checkOperands tenv0 pos name expr0 = snd (go tenv0 0 expr0)
  where
    go tenv c (BinOp op l r) =
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
          (c2, errs1) = go tenv c1 l
          (c3, errs2) = go tenv c2 r
      in (c3, opErrs ++ errs1 ++ errs2)
    go tenv c (App f x) =
      let fty0 = inferExprE tenv f
          xty = inferExprE tenv x
          (fty, c1) = freshenType c fty0
          argErrs = case fty of
            TFun argTy _ -> snd (checkCompatWith Map.empty pos name argTy xty)
            TOverload alts ->
              -- Try each overload; succeed if any function alternative accepts the arg
              let results = [checkCompatWith Map.empty pos name argTy xty
                            | TFun argTy _ <- alts]
              in if null results || any (null . snd) results then [] else snd (head results)
            _ -> []
          (c2, errs1) = go tenv c1 f
          (c3, errs2) = go tenv c2 x
      in (c3, argErrs ++ errs1 ++ errs2)
    go tenv c (Lam p body) = go (Map.delete p tenv) c body
    go tenv c (With body bs)  =
      let (c1, e1) = go tenv c body
          (c2, e2) = foldl (\(ci, ei) b -> let (ci', ei') = go tenv ci (bindBody b) in (ci', ei ++ ei')) (c1, []) bs
      in (c2, e1 ++ e2)
    go tenv c (Case scrut alts) =
      let (c1, bodyErrs) = foldl (\(ci, ei) (Alt _ _ body) -> let (ci', ei') = go tenv ci body in (ci', ei ++ ei')) (c, []) alts
          exhaustErrs = checkExhaustiveness tenv pos name scrut alts
      in (c1, bodyErrs ++ exhaustErrs)
    go tenv c (Record _ bs) =
      foldl (\(ci, ei) b -> let (ci', ei') = go tenv ci (bindBody b) in (ci', ei ++ ei')) (c, []) bs
    go tenv c (Thunk e)       = go tenv c e
    go _ c _               = (c, [])

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
collectTVars (TOverload ts) = Set.unions (map collectTVars ts)
collectTVars _              = Set.empty

applySubst :: Map.Map Text MiType -> MiType -> MiType
applySubst s (TVar v)       = Map.findWithDefault (TVar v) v s
applySubst s (TFun a b)     = TFun (applySubst s a) (applySubst s b)
applySubst s (TRecord t fs) = TRecord t [(n, applySubst s ty) | (n, ty) <- fs]
applySubst s (TOverload ts) = TOverload (map (applySubst s) ts)
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
      localAnnots = Map.fromListWith (++) [(bindName b, [bindBody b])
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
          Just val | not (isPreludePos (bindPos b)), isConcrete val ->
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

-- | Check trait annotations on bindings.
-- Unannotated bindings have their effects inferred (not checked);
-- only explicitly annotated bindings are validated.
traitCheckBindings :: Env -> [Binding] -> [Warning]
traitCheckBindings env bindings =
  let explicitTraitEnv = collectTraitEnv env bindings
      bindingMap = Map.fromList [(bindName b, b) | b <- bindings]
      -- Only check value/lazy bindings
      valueBinds = filter (\b -> bindDomain b == Value || bindDomain b == Lazy) bindings
      -- Augment trait env with inferred effects for unannotated bindings
      augTraitEnv = inferUnannotatedTraits explicitTraitEnv bindingMap valueBinds
  in concatMap (checkTrait explicitTraitEnv augTraitEnv bindingMap valueBinds) valueBinds

-- | Infer effects for unannotated bindings and add to trait env.
-- Iterates to fixpoint so transitive inferred effects propagate.
inferUnannotatedTraits :: TraitEnv -> Map.Map Text Binding -> [Binding] -> TraitEnv
inferUnannotatedTraits explicit bindingMap valueBinds = go explicit
  where
    go tenv =
      let next = foldl (\te b ->
            let name = bindName b
            in if Map.member name explicit
               then te  -- has explicit annotation, don't override
               else let inferred = inferBindingEffects te bindingMap valueBinds b
                    in if Set.null inferred
                       then te
                       else Map.insert name inferred te
            ) tenv valueBinds
      in if next == tenv then tenv else go next

-- | Check a single binding against its trait annotation.
-- Only explicitly annotated bindings are checked; unannotated ones are skipped
-- (their effects are inferred and visible to callers via the augmented trait env).
checkTrait :: TraitEnv -> TraitEnv -> Map.Map Text Binding -> [Binding] -> Binding -> [Warning]
checkTrait explicitTraitEnv augTraitEnv bindingMap allBindings b =
  let name = bindName b
      isMain = name == "main" || name == "_main"
      hasExplicit = Map.member name explicitTraitEnv
      -- Only check explicitly annotated bindings (skip unannotated and main)
      skip = not hasExplicit || (isMain && not hasExplicit)
  in if skip
     then []
     else let declared = case Map.lookup name explicitTraitEnv of
                           Just d  -> d
                           Nothing -> Set.empty
              inferred = inferBindingEffects augTraitEnv bindingMap allBindings b
              excess = Set.difference inferred declared
          in if Set.null excess
             then []
             else [TraitWarning (bindPos b) (bindName b)
                    ("effect violation: " <> name
                     <> " declared :~ " <> formatEffects declared
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
