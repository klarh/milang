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
  let -- Collect type annotations (resolved via typeEnv, not reducer)
      typeEnv = collectTypes bindings
      -- Check each binding
  in concatMap (checkBinding typeEnv) bindings

-- | Collect type annotations into an environment.
-- Type expressions are interpreted directly (not reduced, to avoid : → cons).
-- Type aliases are resolved via the typeEnv built incrementally.
-- All type aliases are structural (untagged) — tags are runtime concepts.
collectTypes :: [Binding] -> TypeEnv
collectTypes = foldl go Map.empty
  where
    go env b = case bindType b of
      Just tyExpr ->
        Map.insert (bindName b) (exprToTypeWith env tyExpr) env
      Nothing ->
        -- Infer type from body (for non-annotated bindings)
        let bodyType = inferBodyType env b
        in case bodyType of
             TAny -> env  -- don't pollute env with unknown types
             _    -> Map.insert (bindName b) bodyType env

-- | Infer a binding's type from its body and parameters
inferBodyType :: TypeEnv -> Binding -> MiType
inferBodyType env b =
  let bodyTy = inferExpr env (bindBody b)
      -- Wrap in function types for each parameter
  in foldr (\_ ty -> TFun TAny ty) bodyTy (bindParams b)

-- | Convert a type expression (milang Expr) to a MiType.
-- Type expressions are regular milang expressions that have been reduced
-- in the type domain. Handles:
--   Num, Str, Float         → primitive types
--   a : b                   → function type (BinOp ":")
--   {x = Num; y = Str}      → record type (Record or anonymous)
--   App f x                 → type application (reduced if possible)
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
-- Lam in type position: represents a type-level function (keep as TAny for now,
-- these get applied via the reducer before reaching here)
exprToTypeWith _ (Lam _ _) = TAny
-- Tag {fields} in type position: With (Name tag) bindings → tagged record type
exprToTypeWith env (With (Name tag) fields)
  | not (T.null tag) && isUpper (T.head tag) =
    TRecord tag [(bindName b, exprToTypeWith env (bindBody b)) | b <- fields]
  where isUpper c = c >= 'A' && c <= 'Z'
exprToTypeWith env (With body bs) =
  -- General With: extend env with local bindings, then infer body type
  let env' = foldl (\acc b -> Map.insert (bindName b) (exprToTypeWith acc (bindBody b)) acc) env bs
  in exprToTypeWith env' body
-- App that wasn't fully reduced — try to interpret anyway
exprToTypeWith env (App f x) =
  case exprToTypeWith env f of
    TFun _ ret -> ret  -- function type applied → return type
    _          -> TAny
exprToTypeWith _ _ = TAny

-- | Check a single binding against its type annotation and for operand errors
checkBinding :: TypeEnv -> Binding -> [TypeError]
checkBinding typeEnv b =
  let annotErrs = case bindType b of
        Nothing -> []
        Just tyExpr ->
          case bindBody b of
            IntLit 0 | null (bindParams b) -> []
            _ -> let expectedType = exprToTypeWith typeEnv tyExpr
                     -- Peel function types for each param, binding param types in env
                     (env', bodyType) = peelParams typeEnv (bindParams b) expectedType
                 in checkExprAgainst env' (bindPos b) (bindName b) bodyType (bindBody b)
      -- Also check for operand type errors in the body (even without annotation)
      operandErrs = checkOperands typeEnv (bindPos b) (bindName b) (bindBody b)
  in annotErrs ++ operandErrs

-- | Peel function type layers for binding parameters, extending the type env
peelParams :: TypeEnv -> [Text] -> MiType -> (TypeEnv, MiType)
peelParams env [] ty = (env, ty)
peelParams env (p:ps) (TFun argTy retTy) =
  peelParams (Map.insert p argTy env) ps retTy
peelParams env (_:ps) ty =
  -- More params than type layers — remaining params are TAny
  peelParams env ps ty

-- | Walk an expression and report operand type mismatches.
-- E.g. "hello" * 3 → Str used with arithmetic operator
checkOperands :: TypeEnv -> Maybe SrcPos -> Text -> Expr -> [TypeError]
checkOperands env pos name = go
  where
    go (BinOp op l r)
      | op `elem` ["-", "*", "/", "^", "%"] =
        let lt = inferExpr env l
            rt = inferExpr env r
            errs = checkNumericOp op lt rt
        in errs ++ go l ++ go r
      | op `elem` ["<", ">", "<=", ">="] =
        let lt = inferExpr env l
            rt = inferExpr env r
            errs = checkNumericOp op lt rt
        in errs ++ go l ++ go r
      | otherwise = go l ++ go r
    go (App f x) =
      let fty = inferExpr env f
          xty = inferExpr env x
          argErrs = case fty of
            TFun argTy _ -> checkCompat pos name argTy xty
            _ -> []
      in argErrs ++ go f ++ go x
    go (Lam _ body) = go body
    go (With body bs) = go body ++ concatMap (go . bindBody) bs
    go (Case _ alts) = concatMap (\(Alt _ _ body) -> go body) alts
    go (Record _ bs) = concatMap (go . bindBody) bs
    go (Thunk e) = go e
    go _ = []

    checkNumericOp op lt rt
      | lt == TStr = [mkOpErr op "Str" "numeric"]
      | rt == TStr = [mkOpErr op "Str" "numeric"]
      | lt == TRecord "" [] || rt == TRecord "" [] = []
      | otherwise = []  -- Num, Float, TAny, TVar all OK

    mkOpErr op actual expected = TypeError
      { tePos = pos
      , teName = name
      , teExpected = TAny
      , teActual = TAny
      , teMessage = "operator " <> op <> ": " <> actual <> " used where " <> expected <> " expected"
      }

-- | Bidirectional check: push expected type into expression.
-- Pushes function types into nested lambdas, binding param types in env.
checkExprAgainst :: TypeEnv -> Maybe SrcPos -> Text -> MiType -> Expr -> [TypeError]
-- For lambdas with function type, push arg type into param and check body
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
  | op == ":" =
    let ht = inferExpr env l
    in TRecord "Cons" [("head", ht), ("tail", TAny)]
  | otherwise = TAny
inferExpr env (App f x) =
  case inferExpr env f of
    TFun _argTy ret -> ret
    _ -> TAny
inferExpr env (Lam p body) =
  -- Can't infer arg type without annotation; infer body with param as TAny
  let env' = Map.insert p TAny env
  in TFun TAny (inferExpr env' body)
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
  -- Detect Tag {field = val; ...} pattern → tagged record type
  case e of
    Name tag | not (T.null tag) && isUpperWith (T.head tag)
             , all hasNamedField bs ->
      TRecord tag [(bindName b, inferExpr env (bindBody b)) | b <- bs]
    _ ->
      -- General With: extend env with local bindings, then infer body
      let env' = foldl addLocal env bs
      in inferExpr env' e
  where
    addLocal acc b = case bindType b of
      Just tyExpr -> Map.insert (bindName b) (exprToTypeWith acc tyExpr) acc
      Nothing     -> Map.insert (bindName b) (inferExpr acc (bindBody b)) acc
    isUpperWith c = c >= 'A' && c <= 'Z'
    hasNamedField b = not (T.null (bindName b)) && bindName b /= "_"
                      && null (bindParams b)
inferExpr env (Case scrutinee alts) =
  -- Infer type from all alternatives; use first non-TAny
  let altTypes = [inferExpr (extendWithPat env pat) body | Alt pat _ body <- alts]
      concreteTypes = filter (/= TAny) altTypes
  in case concreteTypes of
       (t:_) -> t
       []    -> TAny
  where
    extendWithPat e (PRec _ fields) =
      foldl (\acc (n, _) -> Map.insert n TAny acc) e fields
    extendWithPat e (PVar n) = Map.insert n (inferExpr env scrutinee) e
    extendWithPat e _ = e
inferExpr env (ListLit es) =
  -- Infer element type from first element
  case es of
    (e:_) -> TRecord "Cons" [("head", inferExpr env e), ("tail", TAny)]
    []    -> TRecord "Nil" []
inferExpr _ _ = TAny

-- | Check structural compatibility between expected and actual types.
-- Type variables in expected type match anything, with consistency checking.
-- TAny in either position is always compatible.
checkCompat :: Maybe SrcPos -> Text -> MiType -> MiType -> [TypeError]
checkCompat pos name expected actual =
  case go Map.empty expected actual of
    (_, errs) -> errs
  where
    go subst TAny _ = (subst, [])
    go subst _ TAny = (subst, [])
    go subst (TVar v) act =
      case Map.lookup v subst of
        Just prev -> go subst prev act  -- check consistency with previous binding
        Nothing   -> (Map.insert v act subst, [])  -- bind type var
    go subst _ (TVar _) = (subst, [])  -- actual is a type var, compatible
    go subst TNum TNum = (subst, [])
    go subst TFloat TFloat = (subst, [])
    go subst TStr TStr = (subst, [])
    go subst TNum TFloat = (subst, [])  -- numeric compatibility
    go subst TFloat TNum = (subst, [])
    go subst (TFun ea er) (TFun aa ar) =
      let (subst', errs1) = go subst ea aa
          (subst'', errs2) = go subst' er ar
      in (subst'', errs1 ++ errs2)
    go subst (TRecord etag efields) (TRecord atag afields)
      | etag /= "" && etag /= atag =
        (subst, [mkErr ("tag mismatch: expected " <> etag <> ", got " <> (if T.null atag then "(untagged)" else atag))])
      | otherwise =
        foldl (\(s, errs) (ename, etype) ->
          case lookup ename afields of
            Just atype -> let (s', e') = go s etype atype in (s', errs ++ e')
            Nothing    -> (s, errs ++ [mkErr ("missing field: " <> ename)])
          ) (subst, []) efields
    go subst exp' act' =
      if exp' == act'
        then (subst, [])
        else (subst, [mkErr ("expected " <> prettyType exp' <> ", got " <> prettyType act')])

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
