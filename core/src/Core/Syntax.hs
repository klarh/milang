{-# LANGUAGE OverloadedStrings #-}
module Core.Syntax where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

-- | Source position for error reporting
data SrcPos = SrcPos
  { srcFile :: !String
  , srcLine :: !Int
  , srcCol  :: !Int
  } deriving (Show, Eq)

prettySrcPos :: SrcPos -> String
prettySrcPos (SrcPos f l c) = f ++ ":" ++ show l ++ ":" ++ show c

-- | Annotation domain — the key architectural concept.
-- Each binding belongs to exactly one domain. The reducer dispatches
-- on this tag to apply domain-specific reduction rules.
data Domain
  = Value       -- name = expr        (eager value binding)
  | Lazy        -- name := expr       (deferred/thunk binding)
  | Type        -- name :: expr       (type constraint)
  | Trait       -- name :~ expr       (effect/capability annotation)
  | Doc         -- name :? expr       (documentation)
  | Parse       -- (op) :! expr       (operator precedence/associativity)
  deriving (Show, Eq)

-- | Core expression type — both AST and value representation.
-- After partial evaluation, fully-reduced nodes are literals/records;
-- residual nodes are operations that couldn't be resolved at compile time.
data Expr
  = IntLit !Integer
  | FloatLit !Double
  | StringLit !Text
  | Name !Text
  | BinOp !Text Expr Expr
  | App Expr Expr
  | Lam !Text Expr              -- \x -> body (single-param lambda)
  | Record !Text [Binding]      -- Tag { bindings } or anonymous { bindings }
  | FieldAccess Expr !Text      -- expr.field
  | Namespace [Binding]         -- top-level or scoped block of bindings
  | Case Expr [Alt]             -- expr -> { pat = body; ... }
  | Thunk Expr                  -- ~expr: deferred evaluation
  | ListLit [Expr]              -- [a, b, c]
  | With Expr [Binding]         -- expr <- { bindings } (record update)
  | Error !Text                 -- reduction error (type error, trait violation, etc.)
  deriving (Show, Eq)

-- | A binding in any domain. The domain tag determines how the
-- reducer processes it.
data Binding = Binding
  { bindDomain :: !Domain
  , bindName   :: !Text
  , bindParams :: ![Text]       -- f x y = ... → params = [x, y]
  , bindBody   :: Expr
  , bindPos    :: !(Maybe SrcPos)
  } deriving (Show, Eq)

-- | Convenience: make a simple value binding
mkBind :: Text -> Expr -> Binding
mkBind name body = Binding Value name [] body Nothing

-- | Convenience: make a function binding
mkFun :: Text -> [Text] -> Expr -> Binding
mkFun name params body = Binding Value name params body Nothing

-- | Pattern for case/destructuring
data Pat
  = PVar !Text               -- x (binds anything)
  | PLit Expr                -- literal match
  | PRec !Text [(Text, Pat)] -- Tag { field = pat, ... }
  | PList [Pat] (Maybe Text) -- [a, b] or [a, ...rest]
  | PWild                    -- _ (match anything, don't bind)
  deriving (Show, Eq)

-- | Case alternative
data Alt = Alt
  { altPat   :: Pat
  , altGuard :: Maybe Expr
  , altBody  :: Expr
  } deriving (Show, Eq)

-- ── Pretty printing ───────────────────────────────────────────────

prettyExpr :: Int -> Expr -> String
prettyExpr _ (IntLit n)      = show n
prettyExpr _ (FloatLit d)    = show d
prettyExpr _ (StringLit s)   = show s
prettyExpr _ (Name n)        = T.unpack n
prettyExpr i (BinOp op l r)  =
  "(" ++ prettyExpr i l ++ " " ++ T.unpack op ++ " " ++ prettyExpr i r ++ ")"
prettyExpr i (App f x)       =
  "(" ++ prettyExpr i f ++ " " ++ prettyExpr i x ++ ")"
prettyExpr i (Lam p b)       =
  "(\\" ++ T.unpack p ++ " -> " ++ prettyExpr i b ++ ")"
prettyExpr i (Record tag bs) =
  T.unpack tag ++ " {" ++ prettyBindings i bs ++ "}"
prettyExpr i (FieldAccess e f) =
  prettyExpr i e ++ "." ++ T.unpack f
prettyExpr i (Namespace bs)  =
  "{\n" ++ prettyBindings (i+2) bs ++ "\n" ++ replicate i ' ' ++ "}"
prettyExpr i (Case e alts)   =
  prettyExpr i e ++ " ->\n" ++
  concatMap (\a -> replicate (i+2) ' ' ++ prettyAlt (i+2) a ++ "\n") alts
prettyExpr i (Thunk e)       = "~(" ++ prettyExpr i e ++ ")"
prettyExpr i (ListLit es)    = "[" ++ intercalate ", " (map (prettyExpr i) es) ++ "]"
prettyExpr i (With e bs)     =
  prettyExpr i e ++ " <- {" ++ prettyBindings i bs ++ "}"
prettyExpr _ (Error msg)     = "<error: " ++ T.unpack msg ++ ">"

prettyBindings :: Int -> [Binding] -> String
prettyBindings i = concatMap (\b -> replicate i ' ' ++ prettyBinding i b ++ "\n")

prettyBinding :: Int -> Binding -> String
prettyBinding i (Binding dom n ps body _) =
  let op = case dom of
        Value -> " = "
        Lazy  -> " := "
        Type  -> " :: "
        Trait -> " :~ "
        Doc   -> " :? "
        Parse -> " :! "
  in T.unpack n ++ concatMap ((" " ++) . T.unpack) ps ++ op ++ prettyExpr i body

prettyAlt :: Int -> Alt -> String
prettyAlt i (Alt p mg b) =
  let guardStr = case mg of
        Nothing -> ""
        Just g  -> " | " ++ prettyExpr i g
  in prettyPat p ++ guardStr ++ " = " ++ prettyExpr i b

prettyPat :: Pat -> String
prettyPat (PVar v)     = T.unpack v
prettyPat (PLit e)     = prettyExpr 0 e
prettyPat (PRec t fs)  =
  T.unpack t ++ " {" ++
  unwords [T.unpack f ++ " = " ++ prettyPat p | (f,p) <- fs] ++ "}"
prettyPat (PList ps mrest) =
  "[" ++ intercalate ", " (map prettyPat ps) ++
  maybe "" (\r -> ", ..." ++ T.unpack r) mrest ++ "]"
prettyPat PWild        = "_"
