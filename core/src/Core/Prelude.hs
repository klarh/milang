{-# LANGUAGE OverloadedStrings #-}
module Core.Prelude (preludeBindings) where

import Data.Text (Text)
import qualified Data.Text as T
import Core.Syntax
import Core.Parser (parseProgram)

-- | The milang prelude as source text. Parsed at compiler startup.
-- | Bootstrap prelude — strict evaluation only (no #/$ quote/splice yet).
preludeSrc :: Text
preludeSrc = T.unlines
  [ "Bool = {True; False}"
  , "List = {Nil; Cons head tail}"
  , "Maybe = {Nothing; Just val}"
  , "if cond t e = (truthy cond) -> 0 = e; _ = t"
  , "truthy val = val -> False = 0; Nil = 0; Nothing = 0; 0 = 0; \"\" = 0; _ = 1"
  , "toString val = val -> True = \"True\"; False = \"False\"; Nil = \"[]\"; Nothing = \"Nothing\"; Just x = \"Just(\" + toString x + \")\"; _ = _toString val"
  , "fromMaybe def m = m -> Just x = x; Nothing = def"
  , "id x = x"
  , "const x y = x"
  , "flip f a b = f b a"
  , "null lst = lst -> Nil = 1; _ = 0"
  , "head lst = lst -> Nil = Nothing; Cons h t = Just h"
  , "tail lst = lst -> Nil = Nothing; Cons h t = Just t"
  , "fold f acc lst = lst -> Nil = acc; Cons h t = fold f (f acc h) t"
  , "map f lst = lst -> Nil = []; Cons h t = Cons (f h) (map f t)"
  , "filter f lst = lst -> Nil = []; Cons h t = if (f h) (Cons h (filter f t)) (filter f t)"
  , "concat a b = a -> Nil = b; Cons h t = Cons h (concat t b)"
  , "push lst x = concat lst [x]"
  , "at lst i = lst -> Nil = Nothing; Cons h t = if (i == 0) (Just h) (at t (i - 1))"
  , "at' i lst = at lst i"
  , "sum lst = fold (\\acc x -> acc + x) 0 lst"
  , "product lst = fold (\\acc x -> acc * x) 1 lst"
  , "any f lst = fold (\\acc x -> if (f x) 1 acc) 0 lst"
  , "all f lst = fold (\\acc x -> if (f x) acc 0) 1 lst"
  , "contains lst x = any (\\el -> el == x) lst"
  , "range start end = range_helper start end []"
  , "range_helper start end acc = if (start >= end) acc (range_helper start (end - 1) (Cons (end - 1) acc))"
  , "zip a b = if (null a) [] (if (null b) [] (Cons [head a, head b] (zip (tail a) (tail b))))"
  , "last lst = lst -> Nil = Nothing; Cons h t = if (null t) (Just h) (last t)"
  , "init lst = lst -> Nil = Nothing; Cons h t = if (null t) (Just []) (Just (Cons h (fromMaybe [] (init t))))"
  , "reverse lst = fold (\\acc x -> Cons x acc) [] lst"
  , "take n lst = if (n == 0) [] (take_inner n lst)"
  , "take_inner n lst = lst -> Nil = []; Cons h t = Cons h (take (n - 1) t)"
  , "drop n lst = if (n == 0) lst (drop_inner n lst)"
  , "drop_inner n lst = lst -> Nil = []; Cons h t = drop (n - 1) t"
  , "enumerate lst = zip (range 0 (len lst)) lst"
  , "join sep lst = lst -> Nil = \"\"; Cons h t = if (null t) h (h + sep + join sep t)"
  , "abs x = if (x < 0) (0 - x) x"
  , "neg x = 0 - x"
  , "min a b = if (a < b) a b"
  , "max a b = if (a > b) a b"
  , "not x = if x 0 1"
  , "(|>) x f = f x"
  , "(>>) f g x = g (f x)"
  , "(<<) f g x = f (g x)"
  , "(&&) a b = if a b 0"
  , "(||) a b = if a 1 b"
  ]

-- | Parse the prelude source into bindings.
preludeBindings :: [Binding]
preludeBindings = case parseProgram "<prelude>" preludeSrc of
  Right (Namespace bs) -> bs
  Right _  -> error "prelude did not parse as Namespace"
  Left err -> error $ "prelude parse error: " ++ show err
