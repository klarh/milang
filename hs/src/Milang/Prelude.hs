{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Milang.Prelude (preludeBindings) where

import Milang.Syntax (Binding(..), Expr(..))
import Milang.Parser (parseProgram)
import qualified Data.Text as T

-- | Standard prelude source code
preludeSrc :: T.Text
preludeSrc = T.unlines
  [ "List = {Nil; Cons head tail}"
  , "id x = x"
  , "const x y = x"
  , "flip f a b = f b a"
  , "null lst = lst -> Nil = 1; _ = 0"
  , "head lst = lst.head"
  , "tail lst = lst.tail"
  , "fold f acc lst = lst -> Nil = acc; Cons = fold f (f acc lst.head) lst.tail"
  , "map f lst = lst -> Nil = []; Cons = Cons (f lst.head) (map f lst.tail)"
  , "filter f lst = lst -> Nil = []; Cons = if (f lst.head) ~(Cons lst.head (filter f lst.tail)) ~(filter f lst.tail)"
  , "concat a b = a -> Nil = b; Cons = Cons a.head (concat a.tail b)"
  , "push lst x = concat lst [x]"
  , "get lst i = if (i == 0) ~lst.head ~(get lst.tail (i - 1))"
  , "sum lst = fold (\\acc x -> acc + x) 0 lst"
  , "product lst = fold (\\acc x -> acc * x) 1 lst"
  , "any f lst = fold (\\acc x -> acc || f x) 0 lst"
  , "all f lst = fold (\\acc x -> acc && f x) 1 lst"
  , "contains lst x = any (\\el -> el == x) lst"
  , "range start end = range_helper start end []"
  , "range_helper start end acc = if (start >= end) ~acc ~(range_helper start (end - 1) (Cons (end - 1) acc))"
  , "zip a b = if (null a || null b) ~[] ~(Cons [a.head, b.head] (zip a.tail b.tail))"
  , "last lst = if (null lst.tail) ~lst.head ~(last lst.tail)"
  , "init lst = if (null lst.tail) ~[] ~(Cons lst.head (init lst.tail))"
  , "reverse lst = fold (\\acc x -> Cons x acc) [] lst"
  , "take n lst = if (n == 0 || null lst) ~[] ~(Cons lst.head (take (n - 1) lst.tail))"
  , "drop n lst = if (n == 0) ~lst ~(drop (n - 1) lst.tail)"
  , "enumerate lst = zip (range 0 (len lst)) lst"
  , "join sep lst = lst -> Nil = \"\"; Cons = if (null lst.tail) ~lst.head ~(lst.head ++ sep ++ join sep lst.tail)"
  , "abs x = if (x < 0) ~(0 - x) ~x"
  , "min a b = if (a < b) ~a ~b"
  , "max a b = if (a > b) ~a ~b"
  , "not x = if x ~0 ~1"
  ]

-- | Parse the prelude source into bindings.
--   Crashes at compile time if the prelude has a syntax error.
preludeBindings :: [Binding]
preludeBindings =
  case parseProgram "<prelude>" preludeSrc of
    Right (Namespace bs) -> bs
    Right _ -> error "prelude: expected namespace"
    Left err -> error $ "prelude parse error: " ++ show err
