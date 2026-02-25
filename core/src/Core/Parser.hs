{-# LANGUAGE OverloadedStrings #-}
module Core.Parser (parseProgram, parseExpr) where

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
import Control.Monad (void, when)

import Core.Syntax
import Core.Lexer (Parser, sc, scn, lexeme, symbol, symbolN)

-- ── Operator table ────────────────────────────────────────────────

data Assoc = LeftAssoc | RightAssoc deriving (Eq, Show)
type OpTable = Map.Map Text (Int, Assoc)

{-# NOINLINE globalOpTable #-}
globalOpTable :: IORef OpTable
globalOpTable = unsafePerformIO (newIORef Map.empty)

-- ── Entry points ──────────────────────────────────────────────────

parseProgram :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseProgram name src =
  let opTable = scanParseDecls src
  in unsafePerformIO (writeIORef globalOpTable opTable) `seq`
     runParser (scn *> pProgram <* eof) name src

parseExpr :: String -> Text -> Either (ParseErrorBundle Text Void) Expr
parseExpr = runParser (sc *> pExpr <* eof)

-- Pre-scan for :! parse declarations
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

    parseOpDecl op body =
      let prec = extractField "prec" body >>= readInt
          assoc = extractField "assoc" body >>= readAssoc
      in case (prec, assoc) of
           (Just p, Just a)  -> Just (op, (p, a))
           (Just p, Nothing) -> Just (op, (p, LeftAssoc))
           (Nothing, Just a) -> Just (op, (50, a))
           _                 -> Nothing

    extractField key txt =
      case T.breakOn (key <> " = ") txt of
        (_, rest) | not (T.null rest) ->
          let afterEq = T.drop (T.length key + 3) rest
          in Just . T.strip $ T.takeWhile (\c -> c /= ';' && c /= '}' && c /= '\n') afterEq
        _ -> Nothing

    readInt t = case reads (T.unpack t) of { [(n,_)] -> Just n; _ -> Nothing }
    readAssoc t
      | T.isPrefixOf "Right" t || T.isPrefixOf "right" t = Just RightAssoc
      | T.isPrefixOf "Left" t || T.isPrefixOf "left" t = Just LeftAssoc
      | otherwise = Nothing

-- ── Helpers ───────────────────────────────────────────────────────

grabPos :: Parser SrcPos
grabPos = do
  sp <- getSourcePos
  pure $ SrcPos (sourceName sp) (unPos (sourceLine sp)) (unPos (sourceColumn sp))

pSep :: Parser ()
pSep = void (symbol ";") <|> void (some (lexeme newline))

skipBraceWhitespace :: Parser ()
skipBraceWhitespace = scn

-- ── Program ───────────────────────────────────────────────────────

pProgram :: Parser Expr
pProgram = do
  bss <- many (pTopBindings <* scn)
  pure $ Namespace (mergeAnnotations (concat bss))

-- | Merge annotation-only bindings (::, :~, :?) into subsequent value bindings
-- with the same name. In the unified architecture, annotations stay as separate
-- bindings in the Namespace — merging just groups them for the reducer.
mergeAnnotations :: [Binding] -> [Binding]
mergeAnnotations [] = []
mergeAnnotations [b] = [b]
mergeAnnotations (b1 : b2 : rest)
  | isAnnotationOnly b1 && bindName b1 == bindName b2 && not (isAnnotationOnly b2) =
    -- Keep both: the annotation binding stays, and the value binding follows
    b1 : mergeAnnotations (b2 : rest)
  | otherwise = b1 : mergeAnnotations (b2 : rest)
  where
    isAnnotationOnly b = bindDomain b `elem` [Type, Trait, Doc]

-- ── Top-level bindings ────────────────────────────────────────────

pTopBindings :: Parser [Binding]
pTopBindings = do
  _ <- L.indentGuard sc EQ pos1
  pBindingsAt pos1

pBindingsAt :: Pos -> Parser [Binding]
pBindingsAt ref = try (pure <$> pDomainBinding ref Type "::")
             <|> try (pure <$> pDomainBinding ref Trait ":~")
             <|> try (pure <$> pDomainBinding ref Doc ":?")
             <|> try (pParseDomainBinding ref)
             <|> try (pDestructBinding ref)
             <|> (pure <$> pValueBinding ref)

-- | Parse an annotation-domain binding: name :: expr, name :~ expr, name :? expr
-- In the unified architecture, these produce Binding with the appropriate domain tag.
pDomainBinding :: Pos -> Domain -> Text -> Parser Binding
pDomainBinding _ref dom op = do
  pos <- grabPos
  name <- pBindingName
  _ <- symbol op
  body <- pExpr
  pure $ Binding dom name [] body (Just pos)

-- | Parse :! (parse domain) — consumed by scanParseDecls, just skip here
pParseDomainBinding :: Pos -> Parser [Binding]
pParseDomainBinding _ref = do
  pos <- grabPos
  _ <- symbol "("
  op <- pOperator
  _ <- symbol ")"
  _ <- symbol ":!"
  body <- pExpr
  pure [Binding Parse op [] body (Just pos)]

-- | Parse a destructuring binding: {field1; field2} = expr
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
      tmpBind = Binding Value tmpName [] body (Just pos)
      fieldBinds = map (\(localName, fieldName) ->
        Binding Value localName [] (FieldAccess (Name tmpName) fieldName) (Just pos)
        ) fields
  pure (tmpBind : fieldBinds)

pDestructField :: Parser (Text, Text)
pDestructField = do
  skipBraceWhitespace
  name <- pIdentifier
  mField <- optional (symbol "=" *> pIdentifier)
  pure $ case mField of
    Just field -> (name, field)
    Nothing    -> (name, name)

-- | Parse a value binding: name [params] = expr or name [params] := expr
pValueBinding :: Pos -> Parser Binding
pValueBinding ref = do
  pos <- grabPos
  name <- pBindingName
  params <- many (try pParam)
  -- Check for -> (match form)
  matchForm <- optional (try (symbol "->"))
  case matchForm of
    Just _ | not (null params) -> do
      let lastParam = last params
      inlineAlts <- try (pMatchAlt `sepBy1` symbol ";") <|> pure []
      let body = Case (Name lastParam) inlineAlts
      body' <- pIndentedChildren ref body
      pure $ Binding Value name params body' (Just pos)
    _ -> do
      dom <- pBindOp
      body <- try pExpr <|> pure (IntLit 0)
      mMatch <- optional (try pMatchArrow)
      let body' = case mMatch of
            Just alts -> Case body alts
            Nothing   -> body
      mWith <- optional (try pBraceWith)
      let body'' = case mWith of
            Nothing -> body'
            Just bs
              | IntLit 0 <- body', all isNamedBind bs -> Record "" bs
              | otherwise -> With body' bs
      body''' <- pIndentedChildren ref body''
      pure $ Binding dom name params body''' (Just pos)

pBindOp :: Parser Domain
pBindOp = (Value <$ symbol "=") <|> (Lazy <$ symbol ":=")

isNamedBind :: Binding -> Bool
isNamedBind b = not ("_stmt_" `T.isPrefixOf` bindName b)

-- ── Indented children ─────────────────────────────────────────────

pIndentedChildren :: Pos -> Expr -> Parser Expr
pIndentedChildren ref body = case body of
  Case scrut existingAlts -> do
    alts <- many $ try $ do
      scn
      _ <- L.indentGuard sc GT ref
      pMatchAlt
    pure $ Case scrut (existingAlts ++ alts)
  _ -> do
    children <- pIndentedStatements ref
    if null children then pure body
    else pure $ buildScope children body

pIndentedStatements :: Pos -> Parser [Binding]
pIndentedStatements ref = do
  items <- many $ try $ do
    scn
    pos <- L.indentGuard sc GT ref
    pStatementAt pos
  pure (concat items)

pStatementAt :: Pos -> Parser [Binding]
pStatementAt pos = try (pBindingsAt pos) <|> pBareExpr

pBareExpr :: Parser [Binding]
pBareExpr = do
  pos <- grabPos
  off <- getOffset
  e <- pExpr
  let name = T.pack ("_stmt_" ++ show off)
  pure [Binding Value name [] e (Just pos)]

buildScope :: [Binding] -> Expr -> Expr
buildScope children body = case body of
  IntLit 0 ->
    let named = filter isNamedBind children
        result = if null named then IntLit 0 else Record "" named
    in With result children
  _ -> With body children

-- ── Match alternatives ────────────────────────────────────────────

pMatchArrow :: Parser [Alt]
pMatchArrow = do
  _ <- symbol "->"
  try (pMatchAlt `sepBy1` symbol ";") <|> pure []

pMatchAlt :: Parser Alt
pMatchAlt = try pGuardAlt <|> pPatternAlt

pGuardAlt :: Parser Alt
pGuardAlt = do
  _ <- symbol "|"
  cond <- pExpr
  _ <- symbol "="
  body <- pExpr
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
      guard' = case cond of { Name "_" -> Nothing; _ -> Just cond }
  pure $ Alt PWild guard' body'

pPatternAlt :: Parser Alt
pPatternAlt = do
  pat <- pPattern
  mGuard <- optional (try (symbol "|" *> pExpr))
  _ <- symbol "="
  body <- pExpr
  mWith <- optional (try pBraceWith)
  let body' = maybe body (With body) mWith
  pure $ Alt pat mGuard body'

pBraceWith :: Parser [Binding]
pBraceWith = do
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure bs

-- ── Expressions ───────────────────────────────────────────────────

pExpr :: Parser Expr
pExpr = pInfix False

pExprML :: Parser Expr
pExprML = pInfix True

pInfix :: Bool -> Parser Expr
pInfix ml = pPrec ml 0

pPrec :: Bool -> Int -> Parser Expr
pPrec ml minPrec = do
  left <- pApp ml
  pInfixRest ml minPrec left

pInfixRest :: Bool -> Int -> Expr -> Parser Expr
pInfixRest ml minPrec left = do
  let tbl = unsafePerformIO (readIORef globalOpTable)
  when ml (void scn)
  -- Try backtick infix
  mBacktick <- optional (try $ lookAhead pBacktickOp)
  case mBacktick of
    Just name -> do
      let (bPrec, _) = opInfoWith tbl name
      if bPrec < minPrec then pure left
      else do
        _ <- pBacktickOp
        if ml then scn else sc
        right <- pPrec ml (bPrec + 1)
        pInfixRest ml minPrec (App (App (Name name) left) right)
    Nothing -> do
      mop <- optional (try $ lookAhead pOperator)
      case mop of
        Nothing -> pure left
        Just op ->
          let (prec, assoc) = opInfoWith tbl op
          in if prec < minPrec then pure left
             else do
               _ <- pOperator
               if ml then scn else sc
               let nextPrec = if assoc == RightAssoc then prec else prec + 1
               right <- pPrec ml nextPrec
               pInfixRest ml minPrec (BinOp op left right)

pBacktickOp :: Parser Text
pBacktickOp = try $ lexeme $ do
  _ <- char '`'
  name <- some (alphaNumChar <|> char '_' <|> char '\'')
  _ <- char '`'
  pure $ T.pack name

opInfoWith :: OpTable -> Text -> (Int, Assoc)
opInfoWith tbl op = case Map.lookup op tbl of
  Just info -> info
  Nothing   -> opInfo op

opInfo :: Text -> (Int, Assoc)
opInfo "<-" = (8,   LeftAssoc)
opInfo "|>" = (5,   LeftAssoc)
opInfo "||" = (15,  RightAssoc)
opInfo "&&" = (20,  RightAssoc)
opInfo ">>" = (10,  LeftAssoc)
opInfo "<<" = (10,  RightAssoc)
opInfo ":"  = (25,  RightAssoc)
opInfo "**" = (150, RightAssoc)
opInfo "*"  = (100, LeftAssoc)
opInfo "/"  = (100, LeftAssoc)
opInfo "%"  = (100, LeftAssoc)
opInfo "+"  = (50,  LeftAssoc)
opInfo "-"  = (50,  LeftAssoc)
opInfo "==" = (30,  LeftAssoc)
opInfo "/=" = (30,  LeftAssoc)
opInfo "<"  = (30,  LeftAssoc)
opInfo ">"  = (30,  LeftAssoc)
opInfo "<=" = (30,  LeftAssoc)
opInfo ">=" = (30,  LeftAssoc)
opInfo _    = (50,  LeftAssoc)

pOperator :: Parser Text
pOperator = try $ lexeme $ do
  op <- some (oneOf ("+-*/^<>=!&|@%?:" :: String))
  let t = T.pack op
  if t `elem` ["=", ":=", "->", "::", ":!", ":~", ":?"]
    then fail "reserved operator"
    else pure t

-- ── Application ───────────────────────────────────────────────────

pApp :: Bool -> Parser Expr
pApp ml = do
  when ml (void scn)
  f <- pAtomDot ml
  args <- many (try (pAppArg ml))
  pure $ foldl App f args

pAppArg :: Bool -> Parser Expr
pAppArg ml
  | ml = (scn *> pAtomDotArg ml) <|> pAtomDotArg ml
  | otherwise = pAtomDotArg ml

pAtomDot :: Bool -> Parser Expr
pAtomDot ml = do
  e <- pAtomML ml
  pDotChain e

pAtomDotArg :: Bool -> Parser Expr
pAtomDotArg ml = do
  e <- pAtomNoRecordML ml
  pDotChain e

pDotChain :: Expr -> Parser Expr
pDotChain e = do
  mDot <- optional (try (symbol "."))
  case mDot of
    Nothing -> pure e
    Just _  -> do
      field <- pAnyIdentifier
      pDotChain (FieldAccess e field)

-- ── Atoms ─────────────────────────────────────────────────────────

pAtomML :: Bool -> Parser Expr
pAtomML ml = choice
  [ pParens, pListLit, pThunk
  , try pStringLit, try pFloatLit, pIntLit
  , pLambdaML ml, try pUnionDecl, try pAnonRecord, pNameOrRecord
  ]

pAtomNoRecordML :: Bool -> Parser Expr
pAtomNoRecordML ml = choice
  [ pParens, pListLit, pThunk
  , try pStringLit, try pFloatLit, pIntLit
  , pLambdaML ml, try pUnionDecl, pNameOrRecord
  ]

pParens :: Parser Expr
pParens = do
  _ <- symbol "("
  scn
  mop <- optional (try (pOperator <* scn <* symbol ")"))
  case mop of
    Just op -> pure $ Name op
    Nothing -> do
      e <- pExprML
      scn
      _ <- symbol ")"
      pure e

pThunk :: Parser Expr
pThunk = symbol "~" *> (Thunk <$> pAtomDot False)

pListLit :: Parser Expr
pListLit = do
  _ <- symbol "["
  scn
  es <- pExprML `sepEndBy` (symbol "," *> scn)
  scn
  _ <- symbol "]"
  pure $ ListLit es

pAnonRecord :: Parser Expr
pAnonRecord = do
  _ <- symbol "{"
  bs <- pBraceBindings
  _ <- symbol "}"
  pure $ Record "" bs

pUnionDecl :: Parser Expr
pUnionDecl = do
  _ <- symbol "{"
  skipBraceWhitespace
  ctors <- pConstructorDecl `sepEndBy1` pSep
  skipBraceWhitespace
  _ <- symbol "}"
  pure $ Namespace (map ctorToBinding ctors)

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

pConstructorField :: Parser Text
pConstructorField = symbol "_" <|> lexeme (do
  c <- lowerChar
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack (c : cs))

ctorToBinding :: (Text, [Text]) -> Binding
ctorToBinding (name, []) = Binding Value name [] (Record name []) Nothing
ctorToBinding (name, params) =
  let fields = [Binding Value p [] (Name p) Nothing | p <- params]
      body = foldr Lam (Record name fields) params
  in Binding Value name [] body Nothing

pBraceBindings :: Parser [Binding]
pBraceBindings = do
  bs <- concat <$> (pBraceBindingOrDestruct `sepEndBy` pSep)
  skipBraceWhitespace
  pure bs

pBraceBindingOrDestruct :: Parser [Binding]
pBraceBindingOrDestruct = try pBraceDestruct <|> try (pure <$> pBraceBinding) <|> try pBraceBareExpr

pBraceBareExpr :: Parser [Binding]
pBraceBareExpr = do
  skipBraceWhitespace
  pos <- grabPos
  off <- getOffset
  e <- pExpr
  let name = T.pack ("_stmt_" ++ show off)
  pure [Binding Value name [] e (Just pos)]

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
      tmpBind = Binding Value tmpName [] body (Just pos)
      fieldBinds = map (\(localName, fieldName) ->
        Binding Value localName [] (FieldAccess (Name tmpName) fieldName) (Just pos)
        ) fields
  pure (tmpBind : fieldBinds)

pBraceBinding :: Parser Binding
pBraceBinding = do
  skipBraceWhitespace
  pos <- grabPos
  ref <- L.indentLevel
  name <- pIdentifier
  -- Reject positional field names
  case T.uncons name of
    Just ('_', rest) | not (T.null rest) && T.all isDigit rest ->
      fail $ "'" ++ T.unpack name ++ "' is reserved for positional field access"
    _ -> pure ()
  params <- many (try pIdentifier)
  dom <- pBindOp
  body <- pExpr
  mMatch <- optional (try pMatchArrow)
  let body' = case mMatch of
        Just alts -> Case body alts
        Nothing   -> body
  mWith <- optional (try pBraceWith)
  let body'' = maybe body' (With body') mWith
  body''' <- case body'' of
    Case scrut existingAlts -> do
      alts <- many $ try $ do
        scn
        _ <- L.indentGuard sc GT ref
        pMatchAlt
      pure $ Case scrut (existingAlts ++ alts)
    _ -> do
      children <- pIndentedStatements ref
      if null children then pure body''
      else pure $ buildScope children body''
  pure $ Binding dom name params body''' (Just pos)

pStringLit :: Parser Expr
pStringLit = lexeme $ do
  _ <- char '"'
  s <- manyTill L.charLiteral (char '"')
  pure $ StringLit (T.pack s)

pIntLit :: Parser Expr
pIntLit = IntLit <$> lexeme (L.signed (pure ()) L.decimal)

pFloatLit :: Parser Expr
pFloatLit = FloatLit <$> lexeme (L.signed (pure ()) L.float)

pLambdaML :: Bool -> Parser Expr
pLambdaML ml = do
  _ <- symbol "\\"
  params <- some pParam
  _ <- symbol "->"
  body <- if ml then pExprML else pExpr
  pure $ foldr Lam body params

pNameOrRecord :: Parser Expr
pNameOrRecord = do
  n <- pAnyIdentifier
  if not (T.null n) && isUpper (T.head n)
    then do
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

pParam :: Parser Text
pParam = try pQuotedParam <|> pIdentifier

pQuotedParam :: Parser Text
pQuotedParam = try $ lexeme $ do
  _ <- char '#'
  c <- lowerChar <|> char '_'
  cs <- many (alphaNumChar <|> char '_' <|> char '\'')
  pure $ T.pack ('#' : c : cs)

-- ── Binding names ─────────────────────────────────────────────────

pBindingName :: Parser Text
pBindingName = try pOpBindingName <|> try pRegularBindingName

pOpBindingName :: Parser Text
pOpBindingName = try $ do
  _ <- symbol "("
  op <- pOperator
  _ <- symbol ")"
  _ <- lookAhead (try pIdentifier <|> try (symbol "::" *> pure "")
              <|> try (symbol ":~" *> pure "") <|> try (symbol ":?" *> pure "")
              <|> try (symbol "=" *> pure "") <|> try (symbol ":=" *> pure ""))
  pure op

pRegularBindingName :: Parser Text
pRegularBindingName = try $ do
  n <- pAnyIdentifier
  case T.uncons n of
    Just ('_', rest) | not (T.null rest) && T.all isDigit rest ->
      fail $ "'" ++ T.unpack n ++ "' is reserved for positional field access"
    _ -> pure ()
  if not (T.null n) && isUpper (T.head n)
    then do
      _ <- lookAhead (try (symbol "=" *> symbol "{")
                  <|> try (symbol "=" *> pAnyIdentifier *> symbol "{" *> pure "{")
                  <|> try (symbol "=" *> pAnyIdentifier *> pure "")
                  <|> try (symbol "::" *> pure "")
                  <|> try (symbol ":~" *> pure "")
                  <|> try (symbol ":?" *> pure ""))
      pure n
    else pure n

-- ── Patterns ──────────────────────────────────────────────────────

pPattern :: Parser Pat
pPattern = choice
  [ pPatRecord, pPatConstructor, pPatList
  , pPatLitInt, pPatLitStr, pPatWild, pPatVar
  ]

pPatAtom :: Parser Pat
pPatAtom = choice
  [ pPatRecord, pPatList, pPatLitInt, pPatLitStr, pPatWild
  , try (between (symbol "(") (symbol ")") pPattern)
  , pPatVar
  ]

pPatRecord :: Parser Pat
pPatRecord = try $ do
  tag <- pAnyIdentifier
  if not (T.null tag) && isUpper (T.head tag)
    then do
      _ <- symbol "{"
      fields <- pPatField `sepEndBy` pSep
      _ <- symbol "}"
      pure $ PRec tag fields
    else fail "expected uppercase tag"

pPatList :: Parser Pat
pPatList = try $ do
  _ <- symbol "["
  (pats, mrest) <- pPatListElements
  _ <- symbol "]"
  pure $ PList pats mrest

pPatListElements :: Parser ([Pat], Maybe Text)
pPatListElements = do
  mEnd <- optional (lookAhead (symbol "]"))
  case mEnd of
    Just _ -> pure ([], Nothing)
    Nothing -> do
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

pPatField :: Parser (Text, Pat)
pPatField = do
  skipBraceWhitespace
  name <- pIdentifier
  mpat <- optional (symbol "=" *> pPattern)
  pure $ case mpat of
    Just p  -> (name, p)
    Nothing -> (name, PVar name)

pPatLitInt :: Parser Pat
pPatLitInt = PLit . IntLit <$> lexeme (L.signed (pure ()) L.decimal)

pPatLitStr :: Parser Pat
pPatLitStr = PLit <$> pStringLit

pPatWild :: Parser Pat
pPatWild = PWild <$ symbol "_"

pPatVar :: Parser Pat
pPatVar = PVar <$> pIdentifier

pPatConstructor :: Parser Pat
pPatConstructor = try $ do
  tag <- pAnyIdentifier
  if not (T.null tag) && isUpper (T.head tag)
    then do
      args <- many (try pPatAtom)
      let fields = zipWith (\i p -> (T.pack ("_" ++ show i), p)) [(0::Int)..] args
      pure $ PRec tag fields
    else fail "expected uppercase constructor"
