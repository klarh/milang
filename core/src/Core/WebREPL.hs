{-# LANGUAGE OverloadedStrings #-}
module Core.WebREPL
  ( evalProgram
  , evalExpr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec (errorBundlePretty)

import Core.Parser (parseProgramWithMain, parseExpr)
import Core.Reduce (reduce, emptyEnv)
import Core.Prelude (preludeBindings)
import Core.Syntax (Expr(..), Binding(..), mkBind, prettyExpr)

-- | Inject the prelude before user bindings. Mirrors Main.injectPrelude True
injectPrelude :: Expr -> Expr
injectPrelude (Namespace bs) = Namespace (preludeBindings ++ bs)
injectPrelude e = Namespace (preludeBindings ++ [mkBind "_main" e])

-- | Parse+reduce a full program (possibly with top-level bindings).
-- Accept an optional trailing expression after bindings; print its value if present.
evalProgram :: Text -> Either String String
evalProgram src = case parseProgramWithMain "<input>" src of
  Left err -> Left (errorBundlePretty err)
  Right expr ->
    let withPrelude = injectPrelude expr
        (reduced, _ws) = reduce emptyEnv withPrelude
        -- Prefer printing an explicit _main binding if provided; otherwise
        -- fall back to the last user-defined binding (useful for REPL-style
        -- use where users define a binding and expect its value to be shown).
        resultExpr = case reduced of
          Namespace bs ->
            case filter (\b -> bindName b == "_main") bs of
              (b:_) -> bindBody b
              [] ->
                let userBs = filter (\b -> bindPos b /= Nothing) bs
                in case reverse userBs of
                     (lastb:_) -> bindBody lastb
                     _ -> reduced
          _ -> reduced
    in Right (prettyExpr 0 resultExpr)

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
