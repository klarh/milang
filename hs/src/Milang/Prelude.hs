{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Milang.Prelude (preludeBindings) where

import Milang.Syntax (Binding(..), Expr(..))
import Milang.Parser (parseProgram)
import qualified Data.Text as T

-- | Standard prelude source code
preludeSrc :: T.Text
preludeSrc = T.unlines
  [ "/* Standard prelude */"
  , ""
  , "id x = x"
  , "const x _ = x"
  , "flip f x y = f y x"
  , ""
  , "sum lst = fold (\\acc x -> acc + x) 0 lst"
  , "product lst = fold (\\acc x -> acc * x) 1 lst"
  , ""
  , "any pred lst = fold (\\acc x -> acc || pred x) 0 lst"
  , "all pred lst = fold (\\acc x -> acc && pred x) 1 lst"
  , ""
  , "contains lst x = fold (\\acc el -> acc || (el == x)) 0 lst"
  , ""
  , "range a b = if (a > b) ~[] ~(concat [a] (range (a + 1) b))"
  , ""
  , "zip xs ys = if (len xs == 0 || len ys == 0) ~[] ~(concat [[get xs 0, get ys 0]] (zip (slice xs 1 (len xs)) (slice ys 1 (len ys))))"
  , ""
  , "head lst = get lst 0"
  , "tail lst = slice lst 1 (len lst)"
  , "last lst = get lst (len lst - 1)"
  , "init lst = slice lst 0 (len lst - 1)"
  , ""
  , "reverse lst = fold (\\acc x -> concat [x] acc) [] lst"
  , ""
  , "take n lst = if (n <= 0 || len lst == 0) ~[] ~(concat [get lst 0] (take (n - 1) (slice lst 1 (len lst))))"
  , ""
  , "drop n lst = if (n <= 0) ~lst ~(drop (n - 1) (slice lst 1 (len lst)))"
  , ""
  , "enumerate lst = zip (range 0 (len lst - 1)) lst"
  ]

-- | Parse the prelude source into bindings.
--   Crashes at compile time if the prelude has a syntax error.
preludeBindings :: [Binding]
preludeBindings =
  case parseProgram "<prelude>" preludeSrc of
    Right (Namespace bs) -> bs
    Right _ -> error "prelude: expected namespace"
    Left err -> error $ "prelude parse error: " ++ show err
