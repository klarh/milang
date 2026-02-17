{-# LANGUAGE OverloadedStrings #-}
module Milang.Lexer
  ( Parser
  , sc
  , lexeme
  , symbol
  , identifier
  , operator
  , intLiteral
  , floatLiteral
  , stringLiteral
  , indentGuard
  , nonIndented
  , indentBlock
  , IndentOpt(..)
  ) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char.Lexer (IndentOpt(..))

type Parser = Parsec Void Text

-- | Line comment (-- style, like Haskell)
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- | Block comment (/* ... */ style, nestable)
blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

-- | Single-line block comment for inline use (no newlines allowed inside)
inlineBlockComment :: Parser ()
inlineBlockComment = try $ do
  _ <- string "/*"
  _ <- manyTill (satisfy (/= '\n')) (string "*/")
  pure ()

-- | Space consumer: eats spaces/tabs but NOT newlines (important for indentation)
sc :: Parser ()
sc = L.space
  (void $ some (char ' ' <|> char '\t'))
  lineComment
  inlineBlockComment

-- | Space consumer that also eats newlines (for inside braces)
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | Wrap a parser to consume trailing whitespace (not newlines)
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse a specific string and consume trailing whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a line continuation (backslash + newline + optional space)
lineContinuation :: Parser ()
lineContinuation = try $ do
  _ <- char '\\'
  _ <- newline
  sc

-- | Parse an identifier (lowercase start or _)
identifier :: Parser Text
identifier = lexeme $ do
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs)

-- | Parse an uppercase identifier (for record tags)
upperIdentifier :: Parser Text
upperIdentifier = lexeme $ do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs)

-- | Parse an operator (sequence of operator chars)
operator :: Parser Text
operator = lexeme $ do
  op <- some (oneOf ("+-*/^<>=!&|@#$%~?." :: String))
  pure $ T.pack op

-- | Integer literal
intLiteral :: Parser Integer
intLiteral = lexeme L.decimal

-- | Float literal
floatLiteral :: Parser Double
floatLiteral = lexeme L.float

-- | String literal with escape sequences
stringLiteral :: Parser Text
stringLiteral = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  pure $ T.pack s

-- | Indentation helpers (re-exported from Megaparsec)
indentGuard :: Ordering -> Pos -> Parser Pos
indentGuard = L.indentGuard sc

nonIndented :: Parser a -> Parser a
nonIndented = L.nonIndented sc

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn
