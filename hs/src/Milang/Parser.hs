{-# LANGUAGE OverloadedStrings #-}
module Milang.Parser (parseProgram, parseExpr) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

import Milang.Syntax
import Milang.Lexer (Parser, sc, lexeme, symbol)

-- ── Entry points ──────────────────────────────────────────────────

parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseProgram = parse (scn *> pProgram <* eof)

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = parse (sc *> pExpr <* eof)

-- ── Whitespace handling ───────────────────────────────────────────

-- Space consumer that also eats newlines
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") empty

-- ── Program: sequence of top-level bindings ───────────────────────

pProgram :: Parser Expr
pProgram = do
  bss <- many (pTopBindings <* skipNewlines)
  pure $ Namespace (concat bss)

skipNewlines :: Parser ()
skipNewlines = void $ many (lexeme newline)

-- ── Bindings ──────────────────────────────────────────────────────

-- Top-level binding(s): must start at column 1
-- Returns a list because destructuring produces multiple bindings
pTopBindings :: Parser [Binding]
pTopBindings = do
  pos <- L.indentGuard sc EQ pos1
  pBindingsAt pos

-- Parse binding(s) at a given indent level.
-- Tries destructuring ({a; b} = expr) first, then falls back to normal binding.
pBindingsAt :: Pos -> Parser [Binding]
pBindingsAt ref = try (pDestructBinding ref) <|> (pure <$> pBindingAt ref)

-- Parse a destructuring binding: {field1; field2 = alias; ...} = expr
-- Desugars to: _tmp_N = expr; field1 = _tmp_N.field1; alias = _tmp_N.field2; ...
pDestructBinding :: Pos -> Parser [Binding]
pDestructBinding _ref = do
  off <- getOffset
  _ <- symbol "{"
  fields <- pDestructField `sepEndBy1` pSep
  _ <- symbol "}"
  _ <- symbol "="
  body <- pExpr
  let tmpName = T.pack ("_destruct_" ++ show off)
      tmpBind = Binding tmpName False [] body
      fieldBinds = map (\(localName, fieldName) ->
        Binding localName False [] (FieldAccess (Name tmpName) fieldName)
        ) fields
  pure (tmpBind : fieldBinds)

-- Parse a single destructuring field: either "name" (shorthand) or "localName = fieldName"
pDestructField :: Parser (Text, Text)
pDestructField = do
  skipBraceWhitespace
  name <- pIdentifier
  mField <- optional (symbol "=" *> pIdentifier)
  pure $ case mField of
    Just field -> (name, field)   -- {myA = a} means bind myA from field a
    Nothing    -> (name, name)    -- {a} means bind a from field a

-- Parse a binding given its indent level
pBindingAt :: Pos -> Parser Binding
pBindingAt ref = do
  name <- pBindingName
  params <- many (try pIdentifier)
  lazy <- pBindOp
  body <- pExpr
  -- Check for trailing -> (match scope)
  mMatch <- optional (try pMatchArrow)
  let body' = case mMatch of
        Just alts -> Case body alts
        Nothing   -> body
  -- Check for trailing brace scope: expr {bindings}
  mWith <- optional (try pBraceWith)
  let body'' = maybe body' (With body') mWith
  -- Check for indented child bindings or match alts
  children <- pIndentedChildren ref body''
  pure $ Binding name lazy params children

-- Name on the LHS of a binding: lowercase or uppercase (for module aliases)
-- Uppercase binding must be followed by = or := (not {, which would be a record)
pBindingName :: Parser Text
pBindingName = try $ do
  n <- pAnyIdentifier
  -- Peek ahead: must be followed by params or =/:=
  -- If uppercase and next char is '{', this isn't a binding — fail
  if isUpper (T.head n)
    then do
      _ <- lookAhead (try pIdentifier <|> try (symbol "=" *> pure "") <|> try (symbol ":=" *> pure ""))
      pure n
    else pure n

-- After parsing the body, check for indented children.
-- If body is a Case (from ->), indented lines are match alternatives.
-- Otherwise, indented lines are regular bindings (With).
pIndentedChildren :: Pos -> Expr -> Parser Expr
pIndentedChildren ref body = do
  case body of
    Case scrut existingAlts -> do
      -- Parse indented match alternatives
      alts <- many $ try $ do
        skipNewlines
        _ <- L.indentGuard sc GT ref
        pMatchAlt
      pure $ Case scrut (existingAlts ++ alts)
    _ -> do
      -- Parse indented bindings
      children <- pIndentedBindings ref
      if null children then pure body else pure $ With body children

pBindOp :: Parser Bool
pBindOp = (False <$ symbol "=") <|> (True <$ symbol ":=")

-- Parse indented child bindings (deeper than ref)
pIndentedBindings :: Pos -> Parser [Binding]
pIndentedBindings ref = do
  bss <- many $ try $ do
    skipNewlines
    pos <- L.indentGuard sc GT ref
    pBindingsAt pos
  pure (concat bss)

-- Parse -> (signals a match scope follows)
pMatchArrow :: Parser [Alt]
pMatchArrow = do
  _ <- symbol "->"
  -- Optionally parse inline alternatives on same line
  -- (for single-arm matches or brace-delimited)
  pure []

-- Parse a single match alternative: Pattern = body [{ scope }]
pMatchAlt :: Parser Alt
pMatchAlt = do
  pat <- pPattern
  _ <- symbol "="
  body <- pExpr
  -- Allow trailing brace scope on the body
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
  pure $ Alt pat body'

-- Parse a trailing {bindings} block (for With expressions)
pBraceWith :: Parser [Binding]
pBraceWith = do
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure bs

-- ── Expressions ───────────────────────────────────────────────────

pExpr :: Parser Expr
pExpr = pInfix

-- Infix operators with precedence climbing
pInfix :: Parser Expr
pInfix = pPrec 0

pPrec :: Int -> Parser Expr
pPrec minPrec = do
  left <- pApp
  pInfixRest minPrec left

pInfixRest :: Int -> Expr -> Parser Expr
pInfixRest minPrec left = do
  -- Try backslash continuation before checking for operator
  _ <- many (try $ char '\\' *> newline *> sc)
  mop <- optional (try $ lookAhead pOperator)
  case mop of
    Nothing -> pure left
    Just op ->
      let (prec, assoc) = opInfo op
      in if prec < minPrec
         then pure left
         else do
           _ <- pOperator  -- consume the operator
           -- Allow continuation after operator too
           _ <- many (try $ char '\\' *> newline *> sc)
           let nextPrec = if assoc == RightAssoc then prec else prec + 1
           right <- pPrec nextPrec
           pInfixRest minPrec (BinOp op left right)

data Assoc = LeftAssoc | RightAssoc deriving (Eq)

opInfo :: Text -> (Int, Assoc)
opInfo "**" = (150, RightAssoc)
opInfo "*"  = (100, LeftAssoc)
opInfo "/"  = (100, LeftAssoc)
opInfo "+"  = (50,  LeftAssoc)
opInfo "-"  = (50,  LeftAssoc)
opInfo "==" = (30,  LeftAssoc)
opInfo "/=" = (30,  LeftAssoc)
opInfo "<"  = (30,  LeftAssoc)
opInfo ">"  = (30,  LeftAssoc)
opInfo "<=" = (30,  LeftAssoc)
opInfo ">=" = (30,  LeftAssoc)
opInfo _    = (50,  LeftAssoc)

-- Parse an operator token (but not = or := or -> or .)
pOperator :: Parser Text
pOperator = try $ lexeme $ do
  op <- some (oneOf ("+-*/^<>=!&|@#$%?" :: String))
  let t = T.pack op
  -- Don't consume binding operators or match arrow
  if t == "=" || t == ":=" || t == "->"
    then fail "reserved operator"
    else pure t

-- Function application by juxtaposition
-- Field access (.) binds tighter than application
pApp :: Parser Expr
pApp = do
  f <- pAtomDot
  args <- many (try pAtomDot)
  pure $ foldl App f args

-- An atom possibly followed by .field chains
pAtomDot :: Parser Expr
pAtomDot = do
  e <- pAtom
  e' <- pDotChain e
  pRecordUpdates e'

pDotChain :: Expr -> Parser Expr
pDotChain e = do
  mDot <- optional (try (symbol "."))
  case mDot of
    Nothing -> pure e
    Just _  -> do
      field <- pAnyIdentifier
      pDotChain (FieldAccess e field)

-- Parse zero or more record updates: expr:{f1=v1} :{f2=v2} ...
pRecordUpdates :: Expr -> Parser Expr
pRecordUpdates e = do
  mUpd <- optional (try pRecordUpdate)
  case mUpd of
    Nothing -> pure e
    Just bs -> pRecordUpdates (RecordUpdate e bs)

-- Parse :{bindings}
pRecordUpdate :: Parser [Binding]
pRecordUpdate = do
  _ <- symbol ":"
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure bs

-- ── Atoms ─────────────────────────────────────────────────────────

pAtom :: Parser Expr
pAtom = choice
  [ pParens
  , pListLit
  , pThunk
  , pStringLit
  , try pFloatLit
  , pIntLit
  , pLambda
  , pNameOrRecord
  ]

pParens :: Parser Expr
pParens = between (symbol "(") (symbol ")") pExpr

pThunk :: Parser Expr
pThunk = do
  _ <- symbol "~"
  Thunk <$> pAtomDot

pListLit :: Parser Expr
pListLit = do
  _ <- symbol "["
  es <- pExpr `sepEndBy` symbol ","
  _ <- symbol "]"
  pure $ ListLit es

pBraceBindings :: Parser [Binding]
pBraceBindings = concat <$> (pBraceBindingOrDestruct `sepEndBy` pSep)

-- Either a destructuring {a; b} = expr or a normal binding inside braces
pBraceBindingOrDestruct :: Parser [Binding]
pBraceBindingOrDestruct = try pBraceDestruct <|> (pure <$> pBraceBinding)

pBraceDestruct :: Parser [Binding]
pBraceDestruct = do
  skipBraceWhitespace
  off <- getOffset
  _ <- symbol "{"
  fields <- pDestructField `sepEndBy1` pSep
  _ <- symbol "}"
  _ <- symbol "="
  body <- pExpr
  let tmpName = T.pack ("_destruct_" ++ show off)
      tmpBind = Binding tmpName False [] body
      fieldBinds = map (\(localName, fieldName) ->
        Binding localName False [] (FieldAccess (Name tmpName) fieldName)
        ) fields
  pure (tmpBind : fieldBinds)

pBraceBinding :: Parser Binding
pBraceBinding = do
  skipBraceWhitespace
  name <- pIdentifier
  params <- many (try pIdentifier)
  lazy <- pBindOp
  body <- pExpr
  -- Allow nested brace scopes
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
  pure $ Binding name lazy params body'

pSep :: Parser ()
pSep = void (symbol ";") <|> void (some (lexeme newline))

skipBraceWhitespace :: Parser ()
skipBraceWhitespace = void $ many (char ' ' <|> char '\t' <|> newline)

pStringLit :: Parser Expr
pStringLit = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  pure $ StringLit (T.pack s)

pIntLit :: Parser Expr
pIntLit = IntLit <$> lexeme L.decimal

pFloatLit :: Parser Expr
pFloatLit = FloatLit <$> lexeme L.float

pLambda :: Parser Expr
pLambda = do
  _ <- symbol "\\"
  params <- some pIdentifier
  _ <- symbol "->"
  body <- pExpr
  pure $ foldr Lam body params

pNameOrRecord :: Parser Expr
pNameOrRecord = do
  n <- pAnyIdentifier
  if isUpper (T.head n)
    then do
      -- Uppercase: could be a record Tag { ... } or just a name
      mbs <- optional (try pBraceRecord)
      case mbs of
        Just bs -> pure $ Record n bs
        Nothing -> pure $ Name n
    else pure $ Name n

pBraceRecord :: Parser [Binding]
pBraceRecord = do
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure bs

-- ── Identifiers ───────────────────────────────────────────────────

pIdentifier :: Parser Text
pIdentifier = try $ lexeme $ do
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs)

pAnyIdentifier :: Parser Text
pAnyIdentifier = lexeme $ do
  c <- letterChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs)

-- ── Patterns ──────────────────────────────────────────────────────

pPattern :: Parser Pat
pPattern = choice
  [ pPatRecord
  , pPatList
  , pPatLitInt
  , pPatLitStr
  , pPatWild
  , pPatVar
  ]

-- Tag {x; y} or Tag {x = pat; y = pat}
pPatRecord :: Parser Pat
pPatRecord = try $ do
  tag <- pAnyIdentifier
  if isUpper (T.head tag)
    then do
      _ <- symbol "{"
      fields <- pPatField `sepEndBy` pSep
      _ <- symbol "}"
      pure $ PRec tag fields
    else fail "expected uppercase tag for record pattern"

-- [a, b, c] or [head, ...rest]
pPatList :: Parser Pat
pPatList = try $ do
  _ <- symbol "["
  (pats, mrest) <- pPatListElements
  _ <- symbol "]"
  pure $ PList pats mrest

-- Parse list pattern elements, detecting ...rest at the end
pPatListElements :: Parser ([Pat], Maybe Text)
pPatListElements = do
  -- Try empty list
  mEnd <- optional (lookAhead (symbol "]"))
  case mEnd of
    Just _ -> pure ([], Nothing)
    Nothing -> do
      -- Check for ...rest (spread at start with no elements before)
      mSpread <- optional (try (symbol "..." *> pIdentifier))
      case mSpread of
        Just rest -> pure ([], Just rest)
        Nothing -> do
          p <- pPattern
          mComma <- optional (symbol ",")
          case mComma of
            Nothing -> pure ([p], Nothing)
            Just _ -> do
              (rest, mrestName) <- pPatListElements
              pure (p : rest, mrestName)

-- Field pattern: either "name = pat" or just "name" (shorthand for name = PVar name)
pPatField :: Parser (Text, Pat)
pPatField = do
  skipBraceWhitespace
  name <- pIdentifier
  mpat <- optional (symbol "=" *> pPattern)
  pure $ case mpat of
    Just p  -> (name, p)
    Nothing -> (name, PVar name)  -- shorthand: {x} means {x = x}

pPatLitInt :: Parser Pat
pPatLitInt = PLit . IntLit <$> lexeme L.decimal

pPatLitStr :: Parser Pat
pPatLitStr = do
  s <- pStringLit
  pure $ PLit s

pPatWild :: Parser Pat
pPatWild = PWild <$ symbol "_"

pPatVar :: Parser Pat
pPatVar = PVar <$> pIdentifier
