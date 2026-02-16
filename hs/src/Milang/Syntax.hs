{-# LANGUAGE OverloadedStrings #-}
module Milang.Syntax where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List (intercalate)

-- | C type representation for FFI
data CType
  = CInt          -- int, long, short, etc.
  | CFloat        -- double, float
  | CString       -- const char *
  | CVoid         -- void
  | CPtr          -- opaque pointer (void *, FILE *, struct foo *, etc.)
  | COutInt       -- output param: int * — caller allocates, callee fills
  | COutFloat     -- output param: double * / float *
  deriving (Show, Eq)

-- | Core expression type — this is both the AST and the value representation.
-- After partial evaluation, fully-reduced nodes are literals/records;
-- residual nodes are operations that couldn't be resolved at compile time.
data Expr
  = IntLit !Integer
  | FloatLit !Double
  | StringLit !Text
  | Name !Text
  | BinOp !Text Expr Expr
  | App Expr Expr
  | Lam !Text Expr
  | With Expr [Binding]        -- expr { bindings }
  | Record !Text [Binding]     -- Tag { bindings }
  | FieldAccess Expr !Text     -- expr.field
  | Namespace [Binding]        -- top-level or block of bindings
  | Case Expr [Alt]            -- expr -> { Pat = body; ... }
  | Thunk Expr                 -- ~expr: deferred evaluation
  | ListLit [Expr]             -- [a, b, c]: list literal
  | CFunction !Text !Text CType [CType]  -- C FFI: header, c_name, return type, param types
  deriving (Show, Eq)

data Binding = Binding
  { bindName :: !Text
  , bindLazy :: !Bool      -- True = :=, False = =
  , bindParams :: ![Text]  -- f x y = ... → params = [x, y]
  , bindBody :: Expr
  } deriving (Show, Eq)

-- | Pattern for case expressions / destructuring
data Pat
  = PVar !Text               -- x (binds anything)
  | PLit Expr                -- literal match
  | PRec !Text [(Text, Pat)] -- Tag { field = pat, ... }
  | PList [Pat] (Maybe Text) -- [a, b] or [a, ...rest]
  | PWild                    -- _ (match anything, don't bind)
  deriving (Show, Eq)

-- | Case alternative
data Alt = Alt
  { altPat  :: Pat
  , altBody :: Expr
  } deriving (Show, Eq)

-- | Pretty-print an expression (for dump/debug)
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
prettyExpr i (With e bs)     =
  prettyExpr i e ++ " { " ++ prettyBindings i bs ++ " }"
prettyExpr i (Record tag bs) =
  T.unpack tag ++ " { " ++ prettyBindings i bs ++ " }"
prettyExpr i (FieldAccess e f) =
  prettyExpr i e ++ "." ++ T.unpack f
prettyExpr i (Namespace bs)  =
  "{\n" ++ prettyBindings (i+2) bs ++ "\n" ++ replicate i ' ' ++ "}"
prettyExpr i (Case e alts)   =
  prettyExpr i e ++ " ->\n" ++
  concatMap (\a -> replicate (i+2) ' ' ++ prettyAlt (i+2) a ++ "\n") alts
prettyExpr i (Thunk e)       = "~(" ++ prettyExpr i e ++ ")"
prettyExpr i (ListLit es)    = "[" ++ intercalate ", " (map (prettyExpr i) es) ++ "]"
prettyExpr _ (CFunction hdr name _ _) =
  "<cfn:" ++ T.unpack hdr ++ ":" ++ T.unpack name ++ ">"

prettyBindings :: Int -> [Binding] -> String
prettyBindings i = concatMap (\b -> replicate i ' ' ++ prettyBinding i b ++ "\n")

prettyBinding :: Int -> Binding -> String
prettyBinding i (Binding n lz ps body) =
  T.unpack n ++ concatMap ((" " ++) . T.unpack) ps ++
  (if lz then " := " else " = ") ++ prettyExpr i body

prettyAlt :: Int -> Alt -> String
prettyAlt i (Alt p b) = prettyPat p ++ " = " ++ prettyExpr i b

prettyPat :: Pat -> String
prettyPat (PVar v)     = T.unpack v
prettyPat (PLit e)     = prettyExpr 0 e
prettyPat (PRec t fs)  =
  T.unpack t ++ " { " ++
  unwords [T.unpack f ++ " = " ++ prettyPat p | (f,p) <- fs] ++ " }"
prettyPat (PList ps mrest) =
  "[" ++ intercalate ", " (map prettyPat ps) ++
  maybe "" (\r -> ", ..." ++ T.unpack r) mrest ++ "]"
prettyPat PWild        = "_"
