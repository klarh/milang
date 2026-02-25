{-# LANGUAGE OverloadedStrings #-}
module Core.Lexer
  ( Parser
  , sc, scn
  , lexeme, lexemeN
  , symbol, symbolN
  , identifier, upperIdentifier
  , operator
  , intLiteral, floatLiteral, stringLiteral
  ) where

import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | Line comment
lineComment :: Parser ()
lineComment = L.skipLineComment "--"

-- | Nestable block comment
blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

-- | Inline block comment (no newlines inside)
inlineBlockComment :: Parser ()
inlineBlockComment = try $ do
  _ <- string "/*"
  _ <- manyTill (satisfy (/= '\n')) (string "*/")
  pure ()

-- | Line continuation: backslash at end of line
lineContinuation :: Parser ()
lineContinuation = try $ do
  _ <- char '\\'
  _ <- many (char ' ' <|> char '\t')
  _ <- newline
  pure ()

-- | Space consumer: spaces/tabs only (preserves newlines for indentation)
sc :: Parser ()
sc = L.space
  (void $ some (char ' ' <|> char '\t') <|> (lineContinuation *> pure ""))
  lineComment
  inlineBlockComment

-- | Space consumer that also eats newlines (for inside delimiters)
scn :: Parser ()
scn = L.space space1 lineComment blockComment

-- | Wrap parser to consume trailing same-line whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Wrap parser to consume trailing whitespace including newlines
lexemeN :: Parser a -> Parser a
lexemeN = L.lexeme scn

-- | Parse exact string, consume same-line whitespace
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse exact string, consume whitespace including newlines
symbolN :: Text -> Parser Text
symbolN = L.symbol scn

-- | Lowercase identifier
identifier :: Parser Text
identifier = lexeme $ do
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs)

-- | Uppercase identifier (record tags, constructors)
upperIdentifier :: Parser Text
upperIdentifier = lexeme $ do
  c <- upperChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs)

-- | Operator: sequence of operator characters
operator :: Parser Text
operator = lexeme $ do
  op <- some (oneOf ("+-*/^<>=!&|@%~?." :: String))
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
