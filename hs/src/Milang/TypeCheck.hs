{-# LANGUAGE OverloadedStrings #-}
module Milang.TypeCheck (typeCheck, TypeError(..)) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Milang.Syntax

-- | Type representation
data MiType
  = TNum                             -- integer type
  | TFloat                           -- floating point
  | TStr                             -- string type
  | TFun MiType MiType               -- function type: a -> b
  | TRecord Text [(Text, MiType)]    -- tagged record with field types
  | TVar Text                        -- type variable (for polymorphism)
  | TAny                             -- unconstrained / unknown
  deriving (Show, Eq)

-- | Type error with source location
data TypeError = TypeError
  { tePos     :: Maybe SrcPos
  , teName    :: Text
  , teExpected :: MiType
  , teActual   :: MiType
  , teMessage  :: Text
  } deriving (Show)

-- | Type environment: maps names to their types
type TypeEnv = Map.Map Text MiType

-- | Run type checking on a list of bindings.
-- Returns a list of type errors (empty = all good).
typeCheck :: [Binding] -> [TypeError]
typeCheck bindings =
  let -- First pass: collect all :: annotations
      typeEnv = collectTypes bindings
      -- Second pass: check each annotated binding
  in concatMap (checkBinding typeEnv) bindings

-- | Collect type annotations into an environment
collectTypes :: [Binding] -> TypeEnv
collectTypes = foldl go Map.empty
  where
    go env b = case bindType b of
      Just tyExpr -> Map.insert (bindName b) (exprToTypeWith env tyExpr) env
      Nothing     -> env

-- | Convert a type expression (milang Expr) to a MiType.
-- Type expressions are regular milang expressions used in the type domain:
--   Num, Str, Float         → primitive types
--   a : b                   → function type (BinOp ":")
--   {x = Num; y = Str}      → record type
--   lowercase name           → type variable
exprToType :: Expr -> MiType
exprToType = exprToTypeWith Map.empty

exprToTypeWith :: TypeEnv -> Expr -> MiType
exprToTypeWith _ (Name "Num")   = TNum
exprToTypeWith _ (Name "Str")   = TStr
exprToTypeWith _ (Name "Float") = TFloat
exprToTypeWith env (Name n)
  | not (T.null n) && isLower (T.head n) = TVar n
  | otherwise = case Map.lookup n env of
      Just t  -> t   -- resolve type alias
      Nothing -> TAny
  where isLower c = c >= 'a' && c <= 'z'
exprToTypeWith env (BinOp ":" l r) = TFun (exprToTypeWith env l) (exprToTypeWith env r)
exprToTypeWith env (Record tag fields) =
  TRecord tag [(bindName b, exprToTypeWith env (bindBody b)) | b <- fields]
-- Anonymous record from parser (tag = "")
exprToTypeWith env (Namespace bindings) =
  TRecord "" [(bindName b, exprToTypeWith env (bindBody b)) | b <- bindings]
exprToTypeWith _ _ = TAny

-- | Check a single binding against its type annotation
checkBinding :: TypeEnv -> Binding -> [TypeError]
checkBinding typeEnv b = case bindType b of
  Nothing -> []  -- no annotation, nothing to check
  Just tyExpr ->
    -- Skip type-only bindings (:: with no value body)
    case bindBody b of
      IntLit 0 | null (bindParams b) -> []
      _ ->
        let expectedType = exprToTypeWith typeEnv tyExpr
        in checkExprAgainst typeEnv (bindPos b) (bindName b) expectedType (bindBody b)

-- | Bidirectional check: push expected type into expression
checkExprAgainst :: TypeEnv -> Maybe SrcPos -> Text -> MiType -> Expr -> [TypeError]
-- For lambdas with function type annotation, push arg type into param and check body
checkExprAgainst env pos name (TFun argTy retTy) (Lam p body) =
  let env' = Map.insert p argTy env
  in checkExprAgainst env' pos name retTy body
-- Otherwise, infer and compare
checkExprAgainst env pos name expected expr =
  let actual = inferExpr env expr
  in checkCompat pos name expected actual

-- | Infer the type of a single expression
inferExpr :: TypeEnv -> Expr -> MiType
inferExpr _ (IntLit _)    = TNum
inferExpr _ (FloatLit _)  = TFloat
inferExpr _ (StringLit _) = TStr
inferExpr env (Name n) =
  case Map.lookup n env of
    Just t  -> t
    Nothing -> TAny
inferExpr env (BinOp op l r)
  | op `elem` ["-", "*", "/", "^", "%"] =
    let lt = inferExpr env l
        rt = inferExpr env r
    in case (lt, rt) of
         (TFloat, _) -> TFloat
         (_, TFloat) -> TFloat
         _           -> TNum
  | op == "+" =
    let lt = inferExpr env l
        rt = inferExpr env r
    in case (lt, rt) of
         (TStr, _)   -> TStr
         (_, TStr)   -> TStr
         (TFloat, _) -> TFloat
         (_, TFloat) -> TFloat
         _           -> TNum
  | op `elem` ["==", "/=", "<", ">", "<=", ">="] = TNum
  | op == "++" = TStr
  | op == ":" =
    let ht = inferExpr env l
    in TRecord "Cons" [("head", ht), ("tail", TAny)]
  | op `elem` ["&&", "||"] = TNum
  | op == "|>" =
    -- pipe: x |> f  =  f x
    case inferExpr env r of
      TFun _ ret -> ret
      _          -> TAny
  | otherwise = TAny
inferExpr env (App f x) =
  case inferExpr env f of
    TFun argTy ret ->
      -- If function expects a specific type, we could check arg here too
      -- For now just return the result type
      case ret of
        TFun _ _ -> ret  -- partially applied
        _        -> ret
    _ -> TAny
inferExpr _ (Lam _ _) = TAny
inferExpr env (Record tag fields) =
  TRecord tag [(bindName b, inferExpr env (bindBody b)) | b <- fields]
inferExpr env (FieldAccess e field) =
  -- Try to resolve field type from record type
  case inferExpr env e of
    TRecord _ fields ->
      case lookup field fields of
        Just t  -> t
        Nothing -> TAny
    _ -> TAny
inferExpr _ (Thunk e) = inferExpr Map.empty e  -- thunks: infer underlying type
inferExpr env (With e bs) =
  -- Extend env with local bindings, then infer body
  let env' = foldl addLocal env bs
  in inferExpr env' e
  where
    addLocal acc b = case bindType b of
      Just tyExpr -> Map.insert (bindName b) (exprToTypeWith acc tyExpr) acc
      Nothing     -> Map.insert (bindName b) (inferExpr acc (bindBody b)) acc
inferExpr env (Case _ alts) =
  -- Infer from first alternative's body (all should agree)
  case alts of
    (Alt _ _ body : _) -> inferExpr env body
    []                  -> TAny
inferExpr env (ListLit es) =
  -- Infer element type from first element
  case es of
    (e:_) -> TRecord "Cons" [("head", inferExpr env e), ("tail", TAny)]
    []    -> TRecord "Nil" []
inferExpr _ _ = TAny

-- | Check structural compatibility between expected and actual types.
-- Type variables in expected type match anything.
-- TAny in either position is always compatible.
checkCompat :: Maybe SrcPos -> Text -> MiType -> MiType -> [TypeError]
checkCompat pos name expected actual = go Map.empty expected actual
  where
    go _ TAny _ = []
    go _ _ TAny = []
    go subst (TVar v) act =
      case Map.lookup v subst of
        Just prev -> go subst prev act  -- check consistency
        Nothing   -> []  -- first occurrence, always matches
    go _ _ (TVar _) = []  -- actual is a type var (from polymorphic function), compatible
    go _ TNum TNum = []
    go _ TFloat TFloat = []
    go _ TStr TStr = []
    go _ TNum TFloat = []  -- allow float where num expected (numeric compat)
    go _ TFloat TNum = []
    go subst (TFun ea er) (TFun aa ar) =
      go subst ea aa ++ go subst er ar
    go subst (TRecord etag efields) (TRecord atag afields)
      | etag /= "" && atag /= "" && etag /= atag =
        [mkErr ("tag mismatch: expected " <> etag <> ", got " <> atag)]
      | otherwise =
        -- Check that all expected fields exist with compatible types
        concatMap (checkField subst afields) efields
    go _ exp' act' =
      if exp' == act'
        then []
        else [mkErr ("expected " <> prettyType exp' <> ", got " <> prettyType act')]

    checkField subst actualFields (ename, etype) =
      case lookup ename actualFields of
        Just atype -> go subst etype atype
        Nothing    -> [mkErr ("missing field: " <> ename)]

    mkErr msg = TypeError
      { tePos = pos
      , teName = name
      , teExpected = expected
      , teActual = actual
      , teMessage = msg
      }

-- | Pretty-print a type for error messages
prettyType :: MiType -> Text
prettyType TNum = "Num"
prettyType TFloat = "Float"
prettyType TStr = "Str"
prettyType (TVar v) = v
prettyType TAny = "?"
prettyType (TFun a b) = prettyType a <> " : " <> prettyType b
prettyType (TRecord tag fields) =
  let t = if T.null tag then "" else tag <> " "
      fs = T.intercalate "; " [n <> " = " <> prettyType ty | (n, ty) <- fields]
  in t <> "{" <> fs <> "}"
