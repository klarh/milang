{-# LANGUAGE OverloadedStrings #-}
module Core.WebREPL
  ( evalProgram
  , evalExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)

import Core.Parser (parseProgram, parseExpr)
import Core.Reduce (reduce, emptyEnv)
import Core.Prelude (preludeBindings)
import Core.Syntax (Expr(..), Binding(..), mkBind, prettyExpr)

-- | Inject the prelude before user bindings. Mirrors Main.injectPrelude True
injectPrelude :: Expr -> Expr
injectPrelude (Namespace bs) = Namespace (preludeBindings ++ bs)
injectPrelude e = Namespace (preludeBindings ++ [mkBind "_main" e])

-- | Parse+reduce a full program (possibly with top-level bindings).
-- Does NOT resolve imports; imports will remain as Import nodes.
evalProgram :: Text -> Either String String
evalProgram src = case parseProgram "<input>" src of
  Left err -> Left (errorBundlePretty err)
  Right expr ->
    let withPrelude = injectPrelude expr
        (reduced, _ws) = reduce emptyEnv withPrelude
    in Right (prettyExpr 0 reduced)

-- | Parse+reduce a single expression (treat as a main expression)
evalExpr :: Text -> Either String String
evalExpr src = case parseExpr "<input>" src of
  Left err -> Left (errorBundlePretty err)
  Right expr ->
    let withPrelude = injectPrelude expr
        (reduced, _ws) = reduce emptyEnv withPrelude
        -- If reduce returned a Namespace, prefer printing the value bound to
        -- the synthetic _main binding we created; otherwise print the whole
        -- reduced expression.
        resultExpr = case reduced of
          Namespace bs -> case filter (\b -> bindName b == "_main") bs of
            (b:_) -> bindBody b
            _     -> reduced
          _ -> reduced
    in Right (prettyExpr 0 resultExpr)
