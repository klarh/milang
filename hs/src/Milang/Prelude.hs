{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Milang.Prelude (preludeBindings) where

import Milang.Syntax (Binding(..), Expr(..))
import Milang.Parser (parseProgram)
import qualified Data.Text as T

-- | Standard prelude source code
preludeSrc :: T.Text
preludeSrc = T.unlines
  [ "Bool = {True; False}"
  , "List = {Nil; Cons head tail}"
  , "truthy val = val -> False = 0; Nil = 0; 0 = 0; \"\" = 0; _ = 1"
  , "id :: a : a"
  , "id x = x"
  , "const :: a : b : a"
  , "const x y = x"
  , "flip :: (a : b : c) : b : a : c"
  , "flip f a b = f b a"
  , "null :: List : Num"
  , "null lst = lst -> Nil = 1; _ = 0"
  , "head :: List : a"
  , "head lst = lst.head"
  , "tail :: List : List"
  , "tail lst = lst.tail"
  , "fold :: (a : b : a) : a : List : a"
  , "fold f acc lst = lst -> Nil = acc; Cons = fold f (f acc lst.head) lst.tail"
  , "map :: (a : b) : List : List"
  , "map f lst = lst -> Nil = []; Cons = Cons (f lst.head) (map f lst.tail)"
  , "filter :: (a : Num) : List : List"
  , "filter f lst = lst -> Nil = []; Cons = if (f lst.head) ~(Cons lst.head (filter f lst.tail)) ~(filter f lst.tail)"
  , "concat :: List : List : List"
  , "concat a b = a -> Nil = b; Cons = Cons a.head (concat a.tail b)"
  , "push :: List : a : List"
  , "push lst x = concat lst [x]"
  , "get :: List : Num : a"
  , "get lst i = if (i == 0) ~lst.head ~(get lst.tail (i - 1))"
  , "sum :: List : Num"
  , "sum lst = fold (\\acc x -> acc + x) 0 lst"
  , "product :: List : Num"
  , "product lst = fold (\\acc x -> acc * x) 1 lst"
  , "any :: (a : Num) : List : Num"
  , "any f lst = fold (\\acc x -> acc || f x) 0 lst"
  , "all :: (a : Num) : List : Num"
  , "all f lst = fold (\\acc x -> acc && f x) 1 lst"
  , "contains :: List : a : Num"
  , "contains lst x = any (\\el -> el == x) lst"
  , "range :: Num : Num : List"
  , "range start end = range_helper start end []"
  , "range_helper start end acc = if (start >= end) ~acc ~(range_helper start (end - 1) (Cons (end - 1) acc))"
  , "zip :: List : List : List"
  , "zip a b = if (null a || null b) ~[] ~(Cons [a.head, b.head] (zip a.tail b.tail))"
  , "last :: List : a"
  , "last lst = if (null lst.tail) ~lst.head ~(last lst.tail)"
  , "init :: List : List"
  , "init lst = if (null lst.tail) ~[] ~(Cons lst.head (init lst.tail))"
  , "reverse :: List : List"
  , "reverse lst = fold (\\acc x -> Cons x acc) [] lst"
  , "take :: Num : List : List"
  , "take n lst = if (n == 0 || null lst) ~[] ~(Cons lst.head (take (n - 1) lst.tail))"
  , "drop :: Num : List : List"
  , "drop n lst = if (n == 0) ~lst ~(drop (n - 1) lst.tail)"
  , "enumerate :: List : List"
  , "enumerate lst = zip (range 0 (len lst)) lst"
  , "join :: Str : List : Str"
  , "join sep lst = lst -> Nil = \"\"; Cons = if (null lst.tail) ~lst.head ~(lst.head + sep + join sep lst.tail)"
  , "abs :: Num : Num"
  , "abs x = if (x < 0) ~(0 - x) ~x"
  , "neg :: Num : Num"
  , "neg x = 0 - x"
  , "min :: Num : Num : Num"
  , "min a b = if (a < b) ~a ~b"
  , "max :: Num : Num : Num"
  , "max a b = if (a > b) ~a ~b"
  , "not x = if x ~0 ~1"
  , "(<-) base overlay = fold (\\acc pair -> setField acc (head pair) (head (tail pair))) base (zip (fieldNames overlay) (fields overlay))"
  , "(|>) x f = f x"
  , "(>>) f g x = g (f x)"
  , "(<<) f g x = f (g x)"
  , "(&&) a b = if a ~b ~0"
  , "(||) a b = if a ~1 ~b"
  ]

-- | Parse the prelude source into bindings.
--   Crashes at compile time if the prelude has a syntax error.
preludeBindings :: [Binding]
preludeBindings =
  case parseProgram "<prelude>" preludeSrc of
    Right (Namespace bs) -> bs
    Right _ -> error "prelude: expected namespace"
    Left err -> error $ "prelude parse error: " ++ show err
