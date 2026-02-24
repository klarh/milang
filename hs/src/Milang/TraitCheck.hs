{-# LANGUAGE OverloadedStrings #-}
module Milang.TraitCheck (traitCheck, TraitError(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Milang.Syntax
import Milang.Reduce (exprFreeVars)

-- | A trait error: a function's inferred effects exceed its declared traits
data TraitError = TraitError
  { traitPos     :: Maybe SrcPos
  , traitName    :: Text
  , traitMessage :: Text
  } deriving (Show)

-- | Effect set: a set of effect names (e.g., "console", "fs.read")
type Effects = Set.Set Text

-- | Trait environment: maps names to their declared effect sets
type TraitEnv = Map.Map Text Effects

-- | Run trait checking on top-level bindings.
-- Returns a list of trait errors (empty = all good).
traitCheck :: [Binding] -> [TraitError]
traitCheck bindings =
  let -- Step 1: Collect declared traits into an environment
      traitEnv = collectTraits bindings
      -- Step 2: For each binding with a trait annotation, check it
  in concatMap (checkBinding traitEnv bindings) bindings

-- | Collect trait declarations into a map of name → effect set.
-- Resolves trait aliases transitively: if `io :~ [console]` and `greet :~ io`,
-- then greet's effects = {console}.
collectTraits :: [Binding] -> TraitEnv
collectTraits bindings =
  let raw = [(bindName b, bindTraits b) | b <- bindings, bindTraits b /= Nothing]
      -- First pass: resolve each trait expr to an effect set
      env0 = Map.empty
  in foldl (\env (name, Just expr) -> Map.insert name (resolveEffects env expr) env) env0 raw

-- | Resolve a trait expression to a set of effect names.
-- - ListLit [a, b, c] → set of resolved effects
-- - Name n → look up n in the trait env (alias)
-- - [] (Nil) → empty set (pure)
-- - Other → treat as a single effect name
resolveEffects :: TraitEnv -> Expr -> Effects
resolveEffects env (ListLit es) = Set.unions (map (resolveEffects env) es)
resolveEffects env (Record "Nil" _) = Set.empty  -- [] = pure
resolveEffects env (Record "Cons" bs) =
  -- Cons-cell list: resolve head + tail
  let hd = case [bindBody b | b <- bs, bindName b == "head"] of
              [e] -> resolveEffects env e
              _   -> Set.empty
      tl = case [bindBody b | b <- bs, bindName b == "tail"] of
              [e] -> resolveEffects env e
              _   -> Set.empty
  in Set.union hd tl
resolveEffects env (Name n) =
  case Map.lookup n env of
    Just effects -> effects       -- alias: use its resolved effects
    Nothing      -> Set.singleton n  -- leaf effect name
resolveEffects env (FieldAccess e f) =
  -- e.g., fs.read → treat as dotted effect name
  Set.singleton (dotName e f)
resolveEffects _ _ = Set.empty

-- | Convert a field access chain to a dotted name: fs.read → "fs.read"
dotName :: Expr -> Text -> Text
dotName (Name n) f = n <> "." <> f
dotName (FieldAccess e f1) f2 = dotName e f1 <> "." <> f2
dotName _ f = f

-- | Check a single binding against its declared traits.
checkBinding :: TraitEnv -> [Binding] -> Binding -> [TraitError]
checkBinding traitEnv allBindings b =
  case bindTraits b of
    Nothing -> []  -- no trait annotation, nothing to check
    Just _ ->
      case Map.lookup (bindName b) traitEnv of
        Nothing -> []  -- shouldn't happen, but be safe
        Just declared ->
          let -- Infer effects by finding all trait-annotated names this function calls
              inferred = inferEffects traitEnv allBindings b
              -- Check: inferred must be ⊆ declared
              excess = Set.difference inferred declared
          in if Set.null excess
             then []
             else [TraitError
                    { traitPos = bindPos b
                    , traitName = bindName b
                    , traitMessage = "effect violation: " <> bindName b
                        <> " declared :~ " <> formatEffects declared
                        <> " but uses " <> formatEffects excess
                    }]

-- | Infer the actual effects of a binding by walking its body and collecting
-- effects from any trait-annotated names it references, plus world.* accesses.
inferEffects :: TraitEnv -> [Binding] -> Binding -> Effects
inferEffects traitEnv allBindings b =
  let body = wrapLambda (bindParams b) (bindBody b)
      fvs = exprFreeVars body
      -- Effects from calling trait-annotated names
      directEffects = Set.unions
        [ case Map.lookup fv traitEnv of
            Just effects -> effects
            Nothing      -> Set.empty
        | fv <- Set.toList fvs ]
      -- Effects from world.* field access chains in the body
      worldEffects = inferWorldEffects (bindBody b)
      -- Transitive effects through non-annotated callees
      transitiveEffects = inferTransitive traitEnv allBindings (Set.singleton (bindName b)) fvs
  in Set.unions [directEffects, worldEffects, transitiveEffects]

-- | Walk an expression and find world.* field access chains.
-- Maps top-level world capabilities to effect names:
--   world.io.*    → console
--   world.fs.*    → fs
--   world.net.*   → net
--   world.env.*   → env
--   world.argv    → argv (reading args is a minor effect)
--   world.*       → the field name as a generic effect
inferWorldEffects :: Expr -> Effects
inferWorldEffects = go
  where
    go (FieldAccess e f) =
      case worldPath e f of
        Just effects -> Set.union effects (go e)
        Nothing      -> go e
    go (App f x)        = Set.union (go f) (go x)
    go (BinOp _ l r)    = Set.union (go l) (go r)
    go (Lam _ body)     = go body
    go (With body bs)   = Set.union (go body) (Set.unions (map (go . bindBody) bs))
    go (Case s alts)    = Set.union (go s) (Set.unions [go b | Alt _ _ b <- alts])
    go (Record _ bs)    = Set.unions (map (go . bindBody) bs)
    go (Namespace bs)   = Set.unions (map (go . bindBody) bs)
    go (Thunk e)        = go e
    go (ListLit es)     = Set.unions (map go es)
    go (Splice e)       = go e
    go _                = Set.empty

    -- Extract the world capability from a field access chain
    worldPath :: Expr -> Text -> Maybe Effects
    worldPath (Name "world") field = Just (capabilityEffect field)
    worldPath (FieldAccess inner f) _ =
      case worldRoot inner f of
        True  -> Just Set.empty  -- deeper chain, effect already captured at top level
        False -> Nothing
    worldPath _ _ = Nothing

    -- Check if an expression chain is rooted at world
    worldRoot :: Expr -> Text -> Bool
    worldRoot (Name "world") _ = True
    worldRoot (FieldAccess e _) f = worldRoot e f
    worldRoot _ _ = False

    -- Map a top-level world field to effect names
    capabilityEffect :: Text -> Effects
    capabilityEffect "io"   = Set.singleton "console"
    capabilityEffect "fs"   = Set.fromList ["fs.read", "fs.write"]
    capabilityEffect "net"  = Set.singleton "net"
    capabilityEffect "env"  = Set.singleton "env"
    capabilityEffect "argv" = Set.empty  -- reading argv is pure
    capabilityEffect other  = Set.singleton other

-- | Follow calls through non-annotated functions to find transitive effects.
-- Annotated functions act as "boundaries" — their declared effects are used.
-- Non-annotated functions are transparent: we look through them.
inferTransitive :: TraitEnv -> [Binding] -> Set.Set Text -> Set.Set Text -> Effects
inferTransitive traitEnv allBindings visited fvs =
  let bindingMap = Map.fromList [(bindName b, b) | b <- allBindings]
      -- Only follow non-annotated, non-visited names
      toFollow = [ b | fv <- Set.toList fvs
                     , not (Map.member fv traitEnv)  -- skip annotated (boundary)
                     , not (Set.member fv visited)    -- skip visited (prevent cycles)
                     , Just b <- [Map.lookup fv bindingMap] ]
  in Set.unions
       [ let body = wrapLambda (bindParams b) (bindBody b)
             innerFVs = exprFreeVars body
             visited' = Set.insert (bindName b) visited
             -- Direct effects from annotated callees
             direct = Set.unions [ case Map.lookup fv traitEnv of
                                     Just effects -> effects
                                     Nothing      -> Set.empty
                                 | fv <- Set.toList innerFVs ]
             -- World effects from body
             worldEffs = inferWorldEffects (bindBody b)
             -- Continue through non-annotated callees
             transitive = inferTransitive traitEnv allBindings visited' innerFVs
         in Set.unions [direct, worldEffs, transitive]
       | b <- toFollow ]

-- | Wrap a body in lambdas for its parameters (same as in Reduce.hs)
wrapLambda :: [Text] -> Expr -> Expr
wrapLambda []     body = body
wrapLambda (p:ps) body = Lam p (wrapLambda ps body)

-- | Format an effect set for display
formatEffects :: Effects -> Text
formatEffects es
  | Set.null es = "[]"
  | otherwise   = "[" <> T.intercalate ", " (Set.toList es) <> "]"
