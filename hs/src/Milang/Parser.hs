{-# LANGUAGE OverloadedStrings #-}
module Milang.Parser (parseProgram, parseExpr, parseBinding) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isUpper, isDigit)
import Data.Void (Void)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

import Milang.Syntax
import Milang.Lexer (Parser, sc, lexeme, symbol)

-- | Operator associativity
data Assoc = LeftAssoc | RightAssoc deriving (Eq, Show)

-- | User-defined operator table
type OpTable = Map.Map Text (Int, Assoc)

-- | Global operator table, set before parsing by parseProgram.
-- Uses unsafePerformIO to avoid threading state through every parser.
{-# NOINLINE globalOpTable #-}
globalOpTable :: IORef OpTable
globalOpTable = unsafePerformIO (newIORef Map.empty)

-- | Capture source text between two offsets
captureSource :: Int -> Int -> Parser (Maybe Text)
captureSource startOff endOff = do
  st <- getParserState
  let input = pstateInput (statePosState st)
      srcText = T.take (endOff - startOff) (T.drop startOff input)
  pure $ Just (T.stripEnd srcText)
grabPos :: Parser SrcPos
grabPos = do
  sp <- getSourcePos
  pure $ SrcPos (sourceName sp) (unPos (sourceLine sp)) (unPos (sourceColumn sp))

-- ── Entry points ──────────────────────────────────────────────────

parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseProgram name src =
  let opTable = scanParseDecls src
  in unsafePerformIO (writeIORef globalOpTable opTable) `seq`
     runParser (scn *> pProgram <* eof) name src

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr <* eof)

parseBinding :: String -> Text -> Either (ParseErrorBundle Text Void) Binding
parseBinding = runParser (sc *> pBindingAt pos1 <* eof)

-- | Pre-scan source text for :! parse-domain declarations.
-- Looks for lines like: (op) :! {prec = N; assoc = Left}
-- Returns an operator table for use during parsing.
scanParseDecls :: Text -> OpTable
scanParseDecls src = Map.fromList $ mapMaybe scanLine (T.lines src)
  where
    scanLine line =
      let stripped = T.strip line
      in case T.stripPrefix "(" stripped of
           Just rest -> case T.breakOn ")" rest of
             (op, afterOp) | not (T.null op) && not (T.null afterOp) ->
               let afterClose = T.strip (T.drop 1 afterOp)
               in case T.stripPrefix ":!" afterClose of
                    Just declBody -> parseOpDecl op (T.strip declBody)
                    Nothing -> Nothing
             _ -> Nothing
           Nothing -> Nothing

    parseOpDecl :: Text -> Text -> Maybe (Text, (Int, Assoc))
    parseOpDecl op body =
      let prec = extractField "prec" body >>= readInt
          assoc = extractField "assoc" body >>= readAssoc
      in case (prec, assoc) of
           (Just p, Just a) -> Just (op, (p, a))
           (Just p, Nothing) -> Just (op, (p, LeftAssoc))
           (Nothing, Just a) -> Just (op, (50, a))
           _                 -> Nothing

    extractField :: Text -> Text -> Maybe Text
    extractField key txt =
      case T.breakOn (key <> " = ") txt of
        (_, rest) | not (T.null rest) ->
          let afterEq = T.drop (T.length key + 3) rest
              val = T.takeWhile (\c -> c /= ';' && c /= '}' && c /= '\n') afterEq
          in Just (T.strip val)
        _ -> case T.breakOn (key <> "=") txt of
          (_, rest2) | not (T.null rest2) ->
            let afterEq2 = T.drop (T.length key + 1) rest2
                val2 = T.takeWhile (\c -> c /= ';' && c /= '}' && c /= '\n') afterEq2
            in Just (T.strip val2)
          _ -> Nothing

    readInt :: Text -> Maybe Int
    readInt t = case reads (T.unpack t) of
      [(n, _)] -> Just n
      _        -> Nothing

    readAssoc :: Text -> Maybe Assoc
    readAssoc t
      | T.isPrefixOf "Right" t = Just RightAssoc
      | T.isPrefixOf "Left" t  = Just LeftAssoc
      | T.isPrefixOf "right" t = Just RightAssoc
      | T.isPrefixOf "left" t  = Just LeftAssoc
      | otherwise = Nothing

-- ── Whitespace handling ───────────────────────────────────────────

-- Space consumer that also eats newlines
scn :: Parser ()
scn = L.space space1 (L.skipLineComment "--") (L.skipBlockCommentNested "/*" "*/")

-- ── Program: sequence of top-level bindings ───────────────────────

pProgram :: Parser Expr
pProgram = do
  bss <- many (pTopBindings <* skipNewlines)
  pure $ Namespace (mergeTypeAnnotations (concat bss))

skipNewlines :: Parser ()
skipNewlines = scn

-- | Merge type annotations (::) with their value bindings (=).
-- A type-only binding (bindType = Just t, body = IntLit 0) followed by
-- a value binding with the same name gets merged into one binding.
mergeTypeAnnotations :: [Binding] -> [Binding]
mergeTypeAnnotations [] = []
mergeTypeAnnotations [b] = [b]
mergeTypeAnnotations (b1 : b2 : rest)
  | isTypeOnly b1 && bindName b1 == bindName b2 && not (isTypeOnly b2) =
    -- Merge: attach b1's type to b2
    b2 { bindType = bindType b1 } : mergeTypeAnnotations rest
  | otherwise = b1 : mergeTypeAnnotations (b2 : rest)
  where
    isTypeOnly b = case (bindType b, bindBody b) of
      (Just _, IntLit 0) -> True
      _                  -> False

-- ── Bindings ──────────────────────────────────────────────────────

-- Top-level binding(s): must start at column 1
-- Returns a list because destructuring produces multiple bindings
pTopBindings :: Parser [Binding]
pTopBindings = do
  pos <- L.indentGuard sc EQ pos1
  pBindingsAt pos

-- Parse binding(s) at a given indent level.
-- Tries type annotation (::) first, then destructuring, then normal binding.
pBindingsAt :: Pos -> Parser [Binding]
pBindingsAt ref = try (pure <$> pTypeBindingAt ref)
             <|> try (pParseDomainBinding ref)
             <|> try (pDestructBinding ref)
             <|> (pure <$> pBindingAt ref)

-- Parse a parse-domain binding: (op) :! {prec = N; assoc = Left/Right}
-- These are consumed by scanParseDecls pre-scan; here we just skip them.
pParseDomainBinding :: Pos -> Parser [Binding]
pParseDomainBinding _ref = do
  _ <- symbol "("
  _op <- pOperator
  _ <- symbol ")"
  _ <- symbol ":!"
  -- Parse the body as a brace-delimited record: {prec = N; assoc = Left/Right}
  _ <- symbol "{"
  _ <- pBraceBindings
  _ <- symbol "}"
  pure []  -- no binding emitted; info already in op table via scanParseDecls

-- Parse a destructuring binding: {field1; field2 = alias; ...} = expr
-- Desugars to: _tmp_N = expr; field1 = _tmp_N.field1; alias = _tmp_N.field2; ...
pDestructBinding :: Pos -> Parser [Binding]
pDestructBinding _ref = do
  pos <- grabPos
  off <- getOffset
  _ <- symbol "{"
  fields <- pDestructField `sepEndBy1` pSep
  _ <- symbol "}"
  _ <- symbol "="
  body <- pExpr
  let tmpName = T.pack ("_destruct_" ++ show off)
      tmpBind = Binding tmpName False [] body (Just pos) Nothing Nothing
      fieldBinds = map (\(localName, fieldName) ->
        Binding localName False [] (FieldAccess (Name tmpName) fieldName) (Just pos) Nothing Nothing
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

-- Parse a type annotation: name :: typeExpr
-- In type context, {x = Num; y = Num} is allowed as an anonymous record type.
pTypeBindingAt :: Pos -> Parser Binding
pTypeBindingAt _ref = do
  pos <- grabPos
  name <- pBindingName
  _ <- symbol "::"
  typeExpr <- try pAnonRecord <|> pExpr
  pure $ Binding name False [] (IntLit 0) (Just pos) (Just typeExpr) Nothing

-- Parse a binding given its indent level
pBindingAt :: Pos -> Parser Binding
pBindingAt ref = do
  startOff <- getOffset
  pos <- grabPos
  name <- pBindingName
  params <- many (try pIdentifier)
  lazy <- pBindOp
  body <- try pExpr <|> pure (IntLit 0)
  mMatch <- optional (try pMatchArrow)
  let body' = case mMatch of
        Just alts -> Case body alts
        Nothing   -> body
  mWith <- optional (try pBraceWith)
  let body'' = maybe body' (With body') mWith
  children <- pIndentedChildren ref body''
  endOff <- getOffset
  srcText <- captureSource startOff endOff
  pure $ Binding name lazy params children (Just pos) Nothing srcText

-- Name on the LHS of a binding: lowercase or uppercase (for module aliases)
-- Uppercase binding must be followed by = or := (not {, which would be a record)
-- Positional names (_0, _1, ...) are reserved and cannot be used as field names
pBindingName :: Parser Text
pBindingName = try pOpBindingName <|> try pRegularBindingName

-- Parse an operator in parens as a binding name: (+), (<=>), etc.
pOpBindingName :: Parser Text
pOpBindingName = try $ do
  _ <- symbol "("
  op <- pOperator
  _ <- symbol ")"
  -- Must be followed by params or = / := / ::
  _ <- lookAhead (try pIdentifier <|> try (symbol "::" *> pure "") <|> try (symbol "=" *> pure "") <|> try (symbol ":=" *> pure ""))
  pure op

pRegularBindingName :: Parser Text
pRegularBindingName = try $ do
  n <- pAnyIdentifier
  -- Reject positional field names (_0, _1, ...)
  case T.uncons n of
    Just ('_', rest) | not (T.null rest) && T.all isDigit rest ->
      fail $ "'" ++ T.unpack n ++ "' is reserved for positional field access"
    _ -> pure ()
  -- Peek ahead: must be followed by params or =/:=
  -- If uppercase and next char is '{', this isn't a binding — fail
  if isUpper (T.head n)
    then do
      _ <- lookAhead (try pIdentifier <|> try (symbol "::" *> pure "") <|> try (symbol "=" *> pure "") <|> try (symbol ":=" *> pure ""))
      pure n
    else pure n

-- After parsing the body, check for indented children.
-- If body is a Case (from ->), indented lines are match alternatives.
-- Otherwise, indented lines are regular bindings or bare expressions (With).
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
      -- Parse indented bindings or bare expressions
      children <- pIndentedStatements ref
      if null children then pure body
      else
        -- The last bare expression (if any) becomes the body
        let (stmts, result) = splitLastExpr children body
        in pure $ With result stmts

pBindOp :: Parser Bool
pBindOp = (False <$ symbol "=") <|> (True <$ symbol ":=")

-- Parse indented child bindings or bare expressions (deeper than ref)
pIndentedStatements :: Pos -> Parser [Binding]
pIndentedStatements ref = do
  items <- many $ try $ do
    skipNewlines
    pos <- L.indentGuard sc GT ref
    pStatementAt pos
  pure (concat items)

-- Parse either a binding (name = expr) or a bare expression (auto-named)
pStatementAt :: Pos -> Parser [Binding]
pStatementAt pos = try (pBindingsAt pos) <|> pBareExpr

-- Parse a bare expression and wrap it in an auto-named binding
pBareExpr :: Parser [Binding]
pBareExpr = do
  pos <- grabPos
  off <- getOffset
  e <- pExpr
  let name = T.pack ("_stmt_" ++ show off)
  pure [Binding name False [] e (Just pos) Nothing Nothing]

-- Split bindings: if the last item is an auto-generated statement binding,
-- use its body as the With result; otherwise use the original body.
splitLastExpr :: [Binding] -> Expr -> ([Binding], Expr)
splitLastExpr [] fallback = ([], fallback)
splitLastExpr bs fallback =
  let lastB = last bs
      initBs = init bs
  in if "_stmt_" `T.isPrefixOf` bindName lastB
     then (initBs, bindBody lastB)
     else (bs, fallback)

-- Parse -> (signals a match scope follows)
pMatchArrow :: Parser [Alt]
pMatchArrow = do
  _ <- symbol "->"
  -- Parse optional inline alternatives separated by ;
  try (pMatchAlt `sepBy1` symbol ";") <|> pure []

-- Parse a single match alternative: Pattern = body, or | guard = body
pMatchAlt :: Parser Alt
pMatchAlt = try pGuardAlt <|> pPatternAlt

-- Guard alternative: | condition = body
pGuardAlt :: Parser Alt
pGuardAlt = do
  _ <- symbol "|"
  cond <- pExpr
  _ <- symbol "="
  body <- pExpr
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
  -- | _ = body means "always match" (no guard needed)
  let guard = case cond of
        Name "_" -> Nothing
        _        -> Just cond
  pure $ Alt PWild guard body'

-- Pattern alternative: Pattern = body
pPatternAlt :: Parser Alt
pPatternAlt = do
  pat <- pPattern
  _ <- symbol "="
  body <- pExpr
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
  pure $ Alt pat Nothing body'

-- Parse a trailing {bindings} block (for With expressions)
pBraceWith :: Parser [Binding]
pBraceWith = do
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure bs

-- | User-defined operator table — now in Lexer module, re-imported

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
  let tbl = unsafePerformIO (readIORef globalOpTable)
  -- Try backslash continuation before checking for operator
  _ <- many (try $ char '\\' *> newline *> sc)
  -- Try backtick infix: a `div` b → div a b
  mBacktick <- optional (try $ lookAhead pBacktickOp)
  case mBacktick of
    Just name -> do
      -- Backtick operators use user table or default precedence 50, left-associative
      let (bPrec, _) = opInfoWith tbl name
      if bPrec < minPrec
        then pure left
        else do
          _ <- pBacktickOp  -- consume
          _ <- many (try $ char '\\' *> newline *> sc)
          right <- pPrec (bPrec + 1)
          let result = App (App (Name name) left) right
          pInfixRest minPrec result
    Nothing -> do
      mop <- optional (try $ lookAhead pOperator)
      case mop of
        Nothing -> pure left
        Just op ->
          let (prec, assoc) = opInfoWith tbl op
          in if prec < minPrec
             then pure left
             else do
               _ <- pOperator  -- consume the operator
               -- Allow continuation after operator too
               _ <- many (try $ char '\\' *> newline *> sc)
               let nextPrec = if assoc == RightAssoc then prec else prec + 1
               right <- pPrec nextPrec
               let result
                     | op == "|>" = App right left
                     | op == ">>" = Lam "_c" (App right (App left (Name "_c")))
                     | op == "<<" = Lam "_c" (App left (App right (Name "_c")))
                     | op == "&&" = App (App (App (Name "if") left) (Thunk right)) (Thunk (IntLit 0))
                     | op == "||" = App (App (App (Name "if") left) (Thunk (IntLit 1))) (Thunk right)
                     | otherwise  = BinOp op left right
               pInfixRest minPrec result

-- Parse a backtick-quoted identifier: `name`
pBacktickOp :: Parser Text
pBacktickOp = try $ lexeme $ do
  _ <- char '`'
  name <- some (alphaNumChar <|> char '_' <|> char '\'')
  _ <- char '`'
  pure $ T.pack name

-- | Look up operator info: user table overrides built-in defaults
opInfoWith :: OpTable -> Text -> (Int, Assoc)
opInfoWith tbl op = case Map.lookup op tbl of
  Just info -> info
  Nothing   -> opInfo op

opInfo :: Text -> (Int, Assoc)
opInfo "|>" = (5,   LeftAssoc)
opInfo "||" = (15,  RightAssoc)
opInfo "&&" = (20,  RightAssoc)
opInfo ">>" = (10,  LeftAssoc)
opInfo "<<" = (10,  RightAssoc)
opInfo ":"  = (25,  RightAssoc)
opInfo "**" = (150, RightAssoc)
opInfo "*"  = (100, LeftAssoc)
opInfo "/"  = (100, LeftAssoc)
opInfo "++" = (40,  RightAssoc)
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
  op <- some (oneOf ("+-*/^<>=!&|@%?:" :: String))
  let t = T.pack op
  -- Don't consume binding operators, match arrow, or type annotation
  if t == "=" || t == ":=" || t == "->" || t == "::" || t == ":!"
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
  , pQuote
  , pSplice
  , pStringLit
  , try pFloatLit
  , pIntLit
  , pLambda
  , try pUnionDecl
  , pNameOrRecord
  ]

pParens :: Parser Expr
pParens = do
  _ <- symbol "("
  -- Try operator-as-function: (+), (<=>), etc.
  mop <- optional (try (pOperator <* symbol ")"))
  case mop of
    Just op -> pure $ Name op
    Nothing -> do
      e <- pExpr
      _ <- symbol ")"
      pure e

pThunk :: Parser Expr
pThunk = do
  _ <- symbol "~"
  Thunk <$> pAtomDot

pQuote :: Parser Expr
pQuote = do
  _ <- char '#'
  Quote <$> pAtomDot

pSplice :: Parser Expr
pSplice = do
  _ <- char '$'
  Splice <$> pAtomDot

pListLit :: Parser Expr
pListLit = do
  _ <- symbol "["
  es <- pExpr `sepEndBy` symbol ","
  _ <- symbol "]"
  pure $ ListLit es

-- | Parse an anonymous record literal: {x = 1; y = 2}
-- Distinguished from union decl by having lowercase first field name.
pAnonRecord :: Parser Expr
pAnonRecord = do
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure $ Record "" bs

-- | Parse a union/enum declaration: {Just _; None; Pair _ _}
-- Produces a Namespace of constructor bindings.
pUnionDecl :: Parser Expr
pUnionDecl = do
  _ <- symbol "{"
  skipBraceWhitespace
  ctors <- pConstructorDecl `sepEndBy1` pSep
  skipBraceWhitespace
  _ <- symbol "}"
  pure $ Namespace (map ctorToBinding ctors)

-- Parse a single constructor: UpperName followed by zero or more field names or _
pConstructorDecl :: Parser (Text, [Text])
pConstructorDecl = do
  skipBraceWhitespace
  name <- pAnyIdentifier
  if T.null name || not (isUpper (T.head name))
    then fail "expected uppercase constructor name"
    else do
      fields <- many pConstructorField
      let named = zipWith (\f i -> if f == "_" then T.pack ("_" ++ show i) else f)
                          fields [0::Int ..]
      pure (name, named)

-- Parse a constructor field: either _ or a lowercase name
pConstructorField :: Parser Text
pConstructorField = symbol "_" <|> lexeme (do
  c <- lowerChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs))

-- Convert a constructor declaration to a binding
ctorToBinding :: (Text, [Text]) -> Binding
ctorToBinding (name, []) = Binding name False [] (Record name []) Nothing Nothing Nothing
ctorToBinding (name, params) =
  let fields = [Binding p False [] (Name p) Nothing Nothing Nothing | p <- params]
      body = foldr (\p b -> Lam p b) (Record name fields) params
  in Binding name False [] body Nothing Nothing Nothing

pBraceBindings :: Parser [Binding]
pBraceBindings = concat <$> (pBraceBindingOrDestruct `sepEndBy` pSep)

-- Either a destructuring {a; b} = expr, a normal binding, or a bare expression
pBraceBindingOrDestruct :: Parser [Binding]
pBraceBindingOrDestruct = try pBraceDestruct <|> try (pure <$> pBraceBinding) <|> pBraceBareExpr

-- Parse a bare expression inside braces, auto-naming it
pBraceBareExpr :: Parser [Binding]
pBraceBareExpr = do
  skipBraceWhitespace
  pos <- grabPos
  off <- getOffset
  e <- pExpr
  let name = T.pack ("_stmt_" ++ show off)
  pure [Binding name False [] e (Just pos) Nothing Nothing]

pBraceDestruct :: Parser [Binding]
pBraceDestruct = do
  skipBraceWhitespace
  pos <- grabPos
  off <- getOffset
  _ <- symbol "{"
  fields <- pDestructField `sepEndBy1` pSep
  _ <- symbol "}"
  _ <- symbol "="
  body <- pExpr
  let tmpName = T.pack ("_destruct_" ++ show off)
      tmpBind = Binding tmpName False [] body (Just pos) Nothing Nothing
      fieldBinds = map (\(localName, fieldName) ->
        Binding localName False [] (FieldAccess (Name tmpName) fieldName) (Just pos) Nothing Nothing
        ) fields
  pure (tmpBind : fieldBinds)

pBraceBinding :: Parser Binding
pBraceBinding = do
  skipBraceWhitespace
  pos <- grabPos
  name <- pIdentifier
  -- Reject positional field names (_0, _1, ...)
  case T.uncons name of
    Just ('_', rest) | not (T.null rest) && T.all isDigit rest ->
      fail $ "'" ++ T.unpack name ++ "' is reserved for positional field access"
    _ -> pure ()
  params <- many (try pIdentifier)
  lazy <- pBindOp
  body <- pExpr
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
  pure $ Binding name lazy params body' (Just pos) Nothing Nothing

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
  , pPatBareConstructor
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

-- Bare uppercase name as zero-field constructor pattern: Point = ...
pPatBareConstructor :: Parser Pat
pPatBareConstructor = try $ do
  tag <- pAnyIdentifier
  if not (T.null tag) && isUpper (T.head tag)
    then pure $ PRec tag []
    else fail "expected uppercase constructor"
