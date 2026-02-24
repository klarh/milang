{-# LANGUAGE OverloadedStrings #-}
module Milang.TraitCheck (traitCheck, TraitError(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (inits)
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

-- ---------------------------------------------------------------------------
-- World capability map (data-driven)
--
-- Each world primitive is tagged with its leaf effect(s).  Group aliases
-- (e.g. "io" → {console}, "fs" → {fs.read, fs.write}) are derived
-- automatically from the leaf entries via prefix unions.
-- ---------------------------------------------------------------------------

-- | Leaf capabilities: (path segments from world root, leaf effects)
worldCapabilities :: [([Text], Effects)]
worldCapabilities =
  [ (["io", "println"],         s "console")
  , (["io", "print"],           s "console")
  , (["io", "readLine"],        s "console")
  , (["fs", "read", "file"],    s "fs.read")
  , (["fs", "read", "exists"],  s "fs.read")
  , (["fs", "write", "file"],   s "fs.write")
  , (["fs", "write", "append"], s "fs.write")
  , (["fs", "write", "remove"], s "fs.write")
  , (["process", "exec"],       s "process")
  , (["process", "exit"],       s "process")
  , (["getEnv"],                s "env")
  , (["argv"],                  Set.empty)  -- pure
  ]
  where s = Set.singleton

-- | Derived alias map: every non-empty prefix of every capability path maps
-- to the union of matching leaf effects.
-- E.g. "io" → {console}, "fs" → {fs.read, fs.write}, "fs.read" → {fs.read}
worldAliases :: Map.Map Text Effects
worldAliases =
  Map.fromListWith Set.union
    [ (T.intercalate "." prefix, effs)
    | (path, effs) <- worldCapabilities
    , prefix <- tail (inits path)  -- all non-empty prefixes including full path
    ]

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Run trait checking on top-level bindings.
-- Returns a list of trait errors (empty = all good).
traitCheck :: [Binding] -> [TraitError]
traitCheck bindings =
  let traitEnv = collectTraits bindings
  in concatMap (checkBinding traitEnv bindings) bindings

-- | Collect trait declarations into a map of name → effect set.
-- Resolves trait aliases transitively: if `io :~ [console]` and `greet :~ io`,
-- then greet's effects = {console}.
collectTraits :: [Binding] -> TraitEnv
collectTraits bindings =
  let raw = [(bindName b, bindTraits b) | b <- bindings, bindTraits b /= Nothing]
  in foldl (\env (name, Just expr) -> Map.insert name (resolveEffects env expr) env) Map.empty raw

-- | Resolve a trait expression to a set of effect names.
-- Checks user trait env first, then world aliases, then treats as leaf.
resolveEffects :: TraitEnv -> Expr -> Effects
resolveEffects env (ListLit es) = Set.unions (map (resolveEffects env) es)
resolveEffects env (Record "Nil" _) = Set.empty
resolveEffects env (Record "Cons" bs) =
  let hd = case [bindBody b | b <- bs, bindName b == "head"] of
              [e] -> resolveEffects env e
              _   -> Set.empty
      tl = case [bindBody b | b <- bs, bindName b == "tail"] of
              [e] -> resolveEffects env e
              _   -> Set.empty
  in Set.union hd tl
resolveEffects env (Name n) = resolveName env n
resolveEffects env (FieldAccess e f) =
  let dotted = dotName e f
  in resolveName env dotted
resolveEffects _ _ = Set.empty

-- | Resolve a name/dotted-name: user env → world aliases → leaf effect
resolveName :: TraitEnv -> Text -> Effects
resolveName env n =
  case Map.lookup n env of
    Just effects -> effects
    Nothing -> case Map.lookup n worldAliases of
      Just effects -> effects
      Nothing      -> Set.singleton n

-- | Convert a field access chain to a dotted name
dotName :: Expr -> Text -> Text
dotName (Name n) f = n <> "." <> f
dotName (FieldAccess e f1) f2 = dotName e f1 <> "." <> f2
dotName _ f = f

-- ---------------------------------------------------------------------------
-- Trait checking
-- ---------------------------------------------------------------------------

-- | Check a single binding against its declared traits.
checkBinding :: TraitEnv -> [Binding] -> Binding -> [TraitError]
checkBinding traitEnv allBindings b =
  case bindTraits b of
    Nothing -> []
    Just _ ->
      case Map.lookup (bindName b) traitEnv of
        Nothing -> []
        Just declared ->
          let inferred = inferEffects traitEnv allBindings b
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
-- effects from trait-annotated callees, world.* accesses, and transitive calls.
inferEffects :: TraitEnv -> [Binding] -> Binding -> Effects
inferEffects traitEnv allBindings b =
  let body = wrapLambda (bindParams b) (bindBody b)
      fvs = exprFreeVars body
      directEffects = Set.unions
        [ case Map.lookup fv traitEnv of
            Just effects -> effects
            Nothing      -> Set.empty
        | fv <- Set.toList fvs ]
      worldEffects = inferWorldEffects (bindBody b)
      transitiveEffects = inferTransitive traitEnv allBindings (Set.singleton (bindName b)) fvs
  in Set.unions [directEffects, worldEffects, transitiveEffects]

-- | Walk an expression and find world.* field access chains.
-- Looks up each chain in worldAliases to derive effects.
inferWorldEffects :: Expr -> Effects
inferWorldEffects = go
  where
    go (FieldAccess e f) =
      case worldChain e f of
        Just path ->
          let key = T.intercalate "." path
          in case Map.lookup key worldAliases of
               Just effs -> effs
               Nothing   -> Set.empty  -- unknown world field → pure
        Nothing -> go e
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

    -- Extract field path from a world-rooted access chain
    worldChain :: Expr -> Text -> Maybe [Text]
    worldChain (Name "world") field = Just [field]
    worldChain (FieldAccess inner f) field =
      case worldChain inner f of
        Just path -> Just (path ++ [field])
        Nothing   -> Nothing
    worldChain _ _ = Nothing

-- | Follow calls through non-annotated functions to find transitive effects.
inferTransitive :: TraitEnv -> [Binding] -> Set.Set Text -> Set.Set Text -> Effects
inferTransitive traitEnv allBindings visited fvs =
  let bindingMap = Map.fromList [(bindName b, b) | b <- allBindings]
      toFollow = [ b | fv <- Set.toList fvs
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
             worldEffs = inferWorldEffects (bindBody b)
             transitive = inferTransitive traitEnv allBindings visited' innerFVs
         in Set.unions [direct, worldEffs, transitive]
       | b <- toFollow ]

wrapLambda :: [Text] -> Expr -> Expr
wrapLambda []     body = body
wrapLambda (p:ps) body = Lam p (wrapLambda ps body)

formatEffects :: Effects -> Text
formatEffects es
  | Set.null es = "[]"
  | otherwise   = "[" <> T.intercalate ", " (Set.toList es) <> "]"
