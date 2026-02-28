{-# LANGUAGE OverloadedStrings #-}
module Core.IR (exprToJSON) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Text as T

import Core.Syntax

-- | Convert an Expr to a JSON Value suitable for consumption by
-- external backends (Python, C++, etc.).
exprToJSON :: Expr -> Value
exprToJSON (IntLit n) = object
  [ "tag" .= ("int" :: T.Text), "value" .= n ]
exprToJSON (FloatLit d) = object
  [ "tag" .= ("float" :: T.Text), "value" .= d ]
exprToJSON (SizedInt n w s) = object
  [ "tag" .= ("sized_int" :: T.Text)
  , "value" .= n, "bits" .= w, "signed" .= s ]
exprToJSON (SizedFloat d w) = object
  [ "tag" .= ("sized_float" :: T.Text)
  , "value" .= d, "bits" .= w ]
exprToJSON (StringLit s) = object
  [ "tag" .= ("string" :: T.Text), "value" .= s ]
exprToJSON (Name n) = object
  [ "tag" .= ("name" :: T.Text), "value" .= n ]
exprToJSON (BinOp op l r) = object
  [ "tag" .= ("binop" :: T.Text)
  , "op" .= op
  , "left" .= exprToJSON l
  , "right" .= exprToJSON r ]
exprToJSON (App f x) = object
  [ "tag" .= ("app" :: T.Text)
  , "func" .= exprToJSON f
  , "arg" .= exprToJSON x ]
exprToJSON (Lam p b) = object
  [ "tag" .= ("lam" :: T.Text)
  , "param" .= p
  , "body" .= exprToJSON b ]
exprToJSON (Record t bs) = object
  [ "tag" .= ("record" :: T.Text)
  , "label" .= t
  , "bindings" .= map bindingToJSON bs ]
exprToJSON (FieldAccess e f) = object
  [ "tag" .= ("field_access" :: T.Text)
  , "expr" .= exprToJSON e
  , "field" .= f ]
exprToJSON (Namespace bs) = object
  [ "tag" .= ("namespace" :: T.Text)
  , "bindings" .= map bindingToJSON bs ]
exprToJSON (Case e alts) = object
  [ "tag" .= ("case" :: T.Text)
  , "scrutinee" .= exprToJSON e
  , "alts" .= map altToJSON alts ]
exprToJSON (Thunk e) = object
  [ "tag" .= ("thunk" :: T.Text)
  , "expr" .= exprToJSON e ]
exprToJSON (ListLit es) = object
  [ "tag" .= ("list" :: T.Text)
  , "elements" .= map exprToJSON es ]
exprToJSON (With e bs) = object
  [ "tag" .= ("with" :: T.Text)
  , "expr" .= exprToJSON e
  , "bindings" .= map bindingToJSON bs ]
exprToJSON (Import path) = object
  [ "tag" .= ("import" :: T.Text)
  , "path" .= path ]
exprToJSON (Quote e) = object
  [ "tag" .= ("quote" :: T.Text)
  , "expr" .= exprToJSON e ]
exprToJSON (Splice e) = object
  [ "tag" .= ("splice" :: T.Text)
  , "expr" .= exprToJSON e ]
exprToJSON (CFunction hdr name ret params stdImport) = object
  [ "tag" .= ("c_function" :: T.Text)
  , "header" .= hdr
  , "name" .= name
  , "return_type" .= ctypeToJSON ret
  , "param_types" .= map ctypeToJSON params
  , "standard_import" .= stdImport ]
exprToJSON (Error msg) = object
  [ "tag" .= ("error" :: T.Text)
  , "message" .= msg ]

bindingToJSON :: Binding -> Value
bindingToJSON (Binding dom name params body pos) = object $
  [ "domain" .= domainToJSON dom
  , "name" .= name
  , "params" .= params
  , "body" .= exprToJSON body
  ] ++ maybe [] (\p -> ["pos" .= posToJSON p]) pos

domainToJSON :: Domain -> Value
domainToJSON Value = String "value"
domainToJSON Lazy  = String "lazy"
domainToJSON Type  = String "type"
domainToJSON Trait = String "trait"
domainToJSON Doc   = String "doc"
domainToJSON Parse = String "parse"

posToJSON :: SrcPos -> Value
posToJSON (SrcPos f l c) = object
  [ "file" .= f, "line" .= l, "col" .= c ]

patToJSON :: Pat -> Value
patToJSON (PVar v) = object
  [ "tag" .= ("var" :: T.Text), "name" .= v ]
patToJSON (PLit e) = object
  [ "tag" .= ("lit" :: T.Text), "expr" .= exprToJSON e ]
patToJSON (PRec t fields) = object
  [ "tag" .= ("record" :: T.Text)
  , "label" .= t
  , "fields" .= map (\(f, p) -> object ["name" .= f, "pat" .= patToJSON p]) fields ]
patToJSON (PList ps mrest) = object $
  [ "tag" .= ("list" :: T.Text)
  , "elements" .= map patToJSON ps
  ] ++ maybe [] (\r -> ["rest" .= r]) mrest
patToJSON PWild = object
  [ "tag" .= ("wildcard" :: T.Text) ]

altToJSON :: Alt -> Value
altToJSON (Alt pat guard body) = object $
  [ "pat" .= patToJSON pat
  , "body" .= exprToJSON body
  ] ++ maybe [] (\g -> ["guard" .= exprToJSON g]) guard

ctypeToJSON :: CType -> Value
ctypeToJSON CInt       = String "int"
ctypeToJSON CFloat     = String "float"
ctypeToJSON CString    = String "string"
ctypeToJSON CVoid      = String "void"
ctypeToJSON (CPtr t)   = object [ "tag" .= ("ptr" :: T.Text), "to" .= t ]
ctypeToJSON COutInt    = String "out_int"
ctypeToJSON COutFloat  = String "out_float"
