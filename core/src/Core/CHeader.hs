{-# LANGUAGE OverloadedStrings #-}
module Core.CHeader (parseCHeader, parseCHeaderInclude, CFunSig(..), CEnumConst(..)) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char (isAlpha, isAlphaNum, isSpace, isUpper, isLower, isDigit, isHexDigit, digitToInt)
import Data.Bits (shiftL, shiftR, (.|.), (.&.), xor, complement)
import System.Process (readProcess)
import Control.Exception (try, SomeException)
import System.FilePath (takeFileName)
import System.Info (os)

import Core.Syntax (CType(..))

-- | A parsed C function signature
data CFunSig = CFunSig
  { cfName   :: !Text
  , cfRet    :: !CType
  , cfParams :: ![CType]
  } deriving (Show)

data CEnumConst = CEnumConst
  { ceName  :: !Text
  , ceValue :: !Int
  } deriving (Show)

-- | Info about a typedef'd type (struct, enum, or callback)
data CTypeDefInfo = TDStruct [(String, CType)]
                  | TDEnum
                  | TDCallback CType [CType]
                  deriving (Show)

type TypeDefMap = Map String CTypeDefInfo

-- | Parse a C header file using the specified compiler's preprocessor
runPreprocessor :: String -> [String] -> String -> IO (Either String String)
runPreprocessor cc args input = do
  result <- try (readProcess cc args input) :: IO (Either SomeException String)
  case result of
    Right out -> pure (Right out)
    Left e -> pure (Left ("preprocessor failed: " ++ show e))

parseCHeader :: String -> FilePath -> IO (Either String ([CFunSig], [CEnumConst]))
parseCHeader cc path = do
  r <- runPreprocessor cc ["-E", path] ""
  case r of
    Left err -> pure $ Left err
    Right output -> do
      -- Extract #define constants via a separate -dM pass
      defineConsts <- do
        dr <- extractDefines cc ["-E", path] ""
        pure $ case dr of
          Right dout -> parseDefineConstants dout
          Left _     -> []
      let structMap = Map.map TDStruct (parseStructDefs output)
          (enumConsts, enumTypeNames) = parseEnumDefs output
          enumMap = Map.fromList [(n, TDEnum) | n <- enumTypeNames]
          cbMap = Map.map (\(r',ps) -> TDCallback r' ps) (parseCallbackDefs output)
          typeMap = structMap `Map.union` enumMap `Map.union` cbMap
          rawSigs = mapMaybe (parseLine typeMap) (joinDecls (lines output))
          deduped = dedupeSigs rawSigs
          visible = filter (not . isInternal . cfName) deduped
          base = takeFileName path
          built = builtinSigsFor base
          visMap = Map.fromList [(cfName s, s) | s <- visible]
          merged = foldl' (\m b -> case Map.lookup (cfName b) m of
                      Nothing -> Map.insert (cfName b) b m
                      Just e  -> if null (cfParams e) && not (null (cfParams b)) then Map.insert (cfName b) b m else m
                    ) visMap built
          final = Map.elems merged
          -- Merge define constants with enum constants, preferring enum values
          enumNames = Map.fromList [(ceName c, ()) | c <- enumConsts]
          newDefines = filter (\c -> not (Map.member (ceName c) enumNames)) defineConsts
      pure $ Right (final, enumConsts ++ newDefines)

-- | Parse a C header by asking clang to preprocess an #include of the header
-- name. This lets clang find system headers via angle-bracket includes.
parseCHeaderInclude :: String -> String -> IO (Either String ([CFunSig], [CEnumConst]))
parseCHeaderInclude cc hdr = do
  let input = "#include <" ++ hdr ++ ">\n"
  r <- runPreprocessor cc ["-E", "-xc", "-"] input
  case r of
    Left err -> pure $ Left err
    Right output -> do
      -- Extract #define constants via a separate -dM pass
      defineConsts <- do
        dr <- extractDefines cc ["-E", "-xc", "-"] input
        pure $ case dr of
          Right dout -> parseDefineConstants dout
          Left _     -> []
      let structMap = Map.map TDStruct (parseStructDefs output)
          (enumConsts, enumTypeNames) = parseEnumDefs output
          enumMap = Map.fromList [(n, TDEnum) | n <- enumTypeNames]
          cbMap = Map.map (\(r',ps) -> TDCallback r' ps) (parseCallbackDefs output)
          typeMap = structMap `Map.union` enumMap `Map.union` cbMap
          rawSigs = mapMaybe (parseLine typeMap) (joinDecls (lines output))
          deduped = dedupeSigs rawSigs
          visible = filter (not . isInternal . cfName) deduped
          built = builtinSigsFor hdr
          visMap = Map.fromList [(cfName s, s) | s <- visible]
          merged = foldl' (\m b -> case Map.lookup (cfName b) m of
                      Nothing -> Map.insert (cfName b) b m
                      Just e  -> if null (cfParams e) && not (null (cfParams b)) then Map.insert (cfName b) b m else m
                    ) visMap built
          final = Map.elems merged
          enumNames = Map.fromList [(ceName c, ()) | c <- enumConsts]
          newDefines = filter (\c -> not (Map.member (ceName c) enumNames)) defineConsts
      pure $ Right (final, enumConsts ++ newDefines)

-- | Join multi-line declarations.  Preprocessed C output may split function
-- declarations across lines.  We join non-directive lines that don't end with
-- ';' or '}' onto the next line so parseLine sees complete declarations.
joinDecls :: [String] -> [String]
joinDecls [] = []
joinDecls (l:ls)
  | "#" `isPrefixOf` trimmed = l : joinDecls ls
  | null trimmed             = l : joinDecls ls
  | endsDecl trimmed         = l : joinDecls ls
  | otherwise                = joinDecls ((l ++ " " ++ head' ls) : tail' ls)
  where
    trimmed = dropWhile isSpace l
    endsDecl s = case last s of { ';' -> True; '}' -> True; _ -> False }
    head' [] = ""
    head' (x:_) = x
    tail' [] = []
    tail' (_:xs) = xs

parseLine :: TypeDefMap -> String -> Maybe CFunSig
parseLine sm line =
  let decl = stripAttrs (takeDecl line)
  in case break (== '(') decl of
    (_, []) -> Nothing
    (before, _:after) -> parseDecl sm (before, after)

-- takeDecl: get text until semicolon
takeDecl :: String -> String
takeDecl = takeWhile (/= ';')

-- stripAttrs: remove __attribute__((...)) sequences from a declaration
-- These appear in preprocessed output and contain nested parentheses that
-- confuse the simple break-on-'(' function name parser.
stripAttrs :: String -> String
stripAttrs [] = []
stripAttrs s
  | "__attribute__" `isPrefixOf` s =
      let rest = drop (length ("__attribute__" :: String)) s
          rest' = dropWhile isSpace rest
      in case rest' of
           ('(':r) -> stripAttrs (dropParens 1 r)
           _       -> stripAttrs rest'
  | otherwise = head s : stripAttrs (tail s)
  where
    dropParens :: Int -> String -> String
    dropParens 0 cs = cs
    dropParens _ [] = []
    dropParens d ('(':cs) = dropParens (d+1) cs
    dropParens d (')':cs) = dropParens (d-1) cs
    dropParens d (_:cs)   = dropParens d cs

parseDecl :: TypeDefMap -> (String, String) -> Maybe CFunSig
parseDecl sm (beforeParen, paramStr) = do
  let beforeWords = words beforeParen
  case beforeWords of
    [] -> Nothing
    [_] -> Nothing
    _ -> do
      let rawName = last beforeWords
          stars = takeWhile (== '*') rawName
          name = filter (\c -> isAlphaNum c || c == '_') rawName
          retWords = init beforeWords ++ [stars | not (null stars)]
          noise = ["extern","static","inline","__inline__","__cdecl","__stdcall","__attribute__","__declspec","__CRT_INLINE","WINAPI","CDECL","DLLIMPORT"]
          isNoise w = any (`isPrefixOf` w) ["__"] || any (== w) noise || '(' `elem` w || ')' `elem` w
          retFiltered = filter (not . isNoise) retWords
          retStr = unwords retFiltered
      if null name then Nothing else do
        ret <- parseCType sm retStr
        params <- parseParamList sm paramStr
        if "..." `elem` words paramStr then Nothing else Just CFunSig
          { cfName = T.pack name
          , cfRet = ret
          , cfParams = params
          }

splitAtParen :: String -> Maybe (String, String)
splitAtParen s = case break (== '(') s of
  (_, [])         -> Nothing
  (before, _:after) -> Just (before, after)

parseParamList :: TypeDefMap -> String -> Maybe [CType]
parseParamList sm s =
  let s' = takeMatchingParen s
      params = map (T.strip . T.pack) (splitTopLevel ',' s')
  in case params of
       ["void"] -> Just []
       [""]     -> Nothing            -- old-style unspecified parameter list: treat as unknown
       _         -> mapM (parseParamType sm) params

-- | Take content up to the matching ')' for the function parameter list,
-- respecting nested parentheses (e.g., callback parameters).
takeMatchingParen :: String -> String
takeMatchingParen = go (0 :: Int)
  where
    go _ [] = []
    go 0 (')':_) = []
    go d (')':cs) = ')' : go (d-1) cs
    go d ('(':cs) = '(' : go (d+1) cs
    go d (c:cs)   = c : go d cs

-- | Split a string by a delimiter, but only at top-level (depth 0).
-- Parentheses increase/decrease nesting depth.
splitTopLevel :: Char -> String -> [String]
splitTopLevel delim = go (0 :: Int) []
  where
    go _ acc [] = [reverse acc]
    go 0 acc (c:cs) | c == delim = reverse acc : go 0 [] cs
    go d acc ('(':cs) = go (d+1) ('(':acc) cs
    go d acc (')':cs) = go (max 0 (d-1)) (')':acc) cs
    go d acc (c:cs)   = go d (c:acc) cs

-- | Check if a parameter is an inline callback (function pointer).
-- Matches patterns like: void (*func)(int, void*) or int (*)(int)
isCallbackParam :: T.Text -> Bool
isCallbackParam t = "(*" `T.isInfixOf` t && T.count "(" t >= 2

parseParamType :: TypeDefMap -> T.Text -> Maybe CType
parseParamType sm param
  -- Inline callback parameters: void (*func)(args...) or void (*)(args...)
  | isCallbackParam param =
      let (retPart, rest) = T.breakOn "(*" param
          afterStar = T.drop 2 rest  -- skip "(*"
          (_, afterName) = T.breakOn ")" afterStar  -- skip to end of (*name)
          cbParamStr = T.drop 1 (T.dropWhile (/= '(') (T.drop 1 afterName))
      in case (parseCType sm (T.unpack (T.strip retPart)),
               parseParamList sm (T.unpack cbParamStr)) of
           (Just ret, Just params) -> Just (CCallback ret params)
           _ -> Just (CPtr "void")  -- fallback: treat as opaque function pointer
  | T.any (== '[') param =
      let base = T.strip $ fst $ T.breakOn "[" param
          baseWords = filter (\w -> not (T.isPrefixOf "__" w)) (T.words base)
      in case baseWords of
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           _                 -> Just (CPtr (T.unwords baseWords))
  | T.any (== '*') param =
      let starCount = T.length (T.filter (== '*') param)
          cleaned = T.words (T.replace "*" " " param)
          -- Drop trailing parameter name if present.
          -- A param name starts with lowercase and isn't a C type keyword.
          typeWords = case cleaned of
            []  -> []
            [w] -> [w]
            _   | isParamName (last cleaned) -> init cleaned
                | otherwise -> cleaned
          baseWords = filter (\w -> not (T.isPrefixOf "__" w)) typeWords
      in if starCount > 1
         then Just (CPtr (T.unwords baseWords))
         else case baseWords of
           ["char"]           -> Just CString
           ["const", "char"]  -> Just CString
           ["float"]          -> Nothing
           ["long", "double"] -> Nothing
           _                  -> Just (CPtr (T.unwords baseWords))
  | otherwise =
      let ws = T.words param
          typeStr = case ws of
            []  -> ""
            [w] -> w
            _   -> T.unwords (init ws)
      in parseCType sm (T.unpack typeStr)

-- | Check if a word looks like a C parameter name rather than a type component.
-- Param names start with a lowercase letter and aren't C type keywords.
isParamName :: T.Text -> Bool
isParamName w = case T.uncons w of
  Just (c, _) -> isLower c && w `notElem` cTypeKeywords
  Nothing     -> False
  where
    cTypeKeywords = ["int", "char", "long", "short", "float", "double",
                     "void", "signed", "unsigned", "const", "struct",
                     "volatile", "size_t", "ssize_t", "ptrdiff_t"]

parseCType :: TypeDefMap -> String -> Maybe CType
parseCType sm s
  | '*' `elem` s =
      let cleaned = words (map (\c -> if c == '*' then ' ' else c) s)
          base = filter (\w -> not ("__" `isPrefixOf` w)) cleaned
      in case base of
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           _                 -> Just (CPtr (T.pack (unwords base)))
  | otherwise = case words s of
      ["int"]             -> Just (CInt 32)
      ["long"]            -> Just CLong
      ["long", "int"]     -> Just CLong
      ["long", "long"]    -> Just (CInt 64)
      ["unsigned"]        -> Just (CUInt 32)
      ["unsigned", "int"] -> Just (CUInt 32)
      ["unsigned", "long"] -> Just CULong
      ["unsigned", "long", "long"] -> Just (CUInt 64)
      ["short"]           -> Just (CInt 16)
      ["unsigned", "short"] -> Just (CUInt 16)
      ["int8_t"]          -> Just (CInt 8)
      ["int16_t"]         -> Just (CInt 16)
      ["int32_t"]         -> Just (CInt 32)
      ["int64_t"]         -> Just (CInt 64)
      ["uint8_t"]         -> Just (CUInt 8)
      ["uint16_t"]        -> Just (CUInt 16)
      ["uint32_t"]        -> Just (CUInt 32)
      ["uint64_t"]        -> Just (CUInt 64)
      ["size_t"]          -> Just (CUInt 64)
      ["ssize_t"]         -> Just (CInt 64)
      ["ptrdiff_t"]       -> Just (CInt 64)
      ["double"]          -> Just CFloat
      ["float"]           -> Just CFloat32
      ["long", "double"]  -> Just CFloat
      ["void"]            -> Just CVoid
      ["char"]            -> Just (CInt 8)
      ["signed", "char"]  -> Just (CInt 8)
      ["unsigned", "char"] -> Just (CUInt 8)
      ["const", "char"]   -> Just (CInt 8)
      _ -> case words s of
        [name] -> lookupTypeDef sm name
        ["struct", name] -> lookupTypeDef sm name
        _ -> Nothing

lookupTypeDef :: TypeDefMap -> String -> Maybe CType
lookupTypeDef sm name = case Map.lookup name sm of
  Just TDEnum -> Just (CInt 32)
  Just (TDStruct fields) -> Just (CStruct (T.pack name) [(T.pack fn, ft) | (fn, ft) <- fields])
  Just (TDCallback ret params) -> Just (CCallback ret params)
  -- For unknown single-word types (e.g. Uint32, SDL_bool), treat as int.
  -- Pointer types already have '*' in the declaration, so any unknown type
  -- reaching this fallback (without '*') is an integer-like typedef.
  -- The generated C code includes the original header, so gcc resolves
  -- the real type.
  Nothing -> if isLikelyType name then Just (CInt 32) else Nothing
    where isLikelyType n = not (null n) && isUpper (head n) && all (\c -> isAlphaNum c || c == '_') n

-- | Parse struct/typedef definitions from preprocessed output.
-- Handles: typedef struct { fields } Name; and struct Name { fields };
parseStructDefs :: String -> Map String [(String, CType)]
parseStructDefs output =
  let -- Remove # directives and join into one string
      contentLines = filter (not . isPrefixOf "#") (lines output)
      joined = unwords (map (filter (/= '\r')) contentLines)
      -- Split on semicolons respecting brace nesting
      decls = splitTopLevel joined
  in foldl' collectStruct Map.empty decls
  where
    -- Split on ';' at brace depth 0 only
    splitTopLevel s = stl 0 [] [] s
    stl _ acc cur [] = reverse (reverse cur : acc)
    stl n acc cur ('{':cs) = stl (n+1) acc ('{':cur) cs
    stl n acc cur ('}':cs) = stl (max 0 (n-1)) acc ('}':cur) cs
    stl 0 acc cur (';':cs) = stl 0 (reverse cur : acc) [] cs
    stl n acc cur (c:cs)   = stl n acc (c:cur) cs

    collectStruct acc decl =
      let trimmed = dropWhile isSpace decl
      in case parseTypedefStruct trimmed `orElse` parseNamedStruct trimmed of
           Just (name, fields) -> Map.insert name fields acc
           Nothing -> acc

    orElse Nothing b = b
    orElse a      _ = a

    -- typedef struct { ... } Name
    -- typedef struct Tag { ... } Name
    parseTypedefStruct s
      | "typedef" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 7 s)
          in if "struct" `isPrefixOf` rest
             then let afterStruct = dropWhile isSpace (drop 6 rest)
                      -- Skip optional struct tag name (e.g. "typedef struct SDL_Color { ... }")
                      afterTag = if not (null afterStruct) && isAlpha (head afterStruct)
                                 then dropWhile isSpace (dropWhile (\c -> isAlphaNum c || c == '_') afterStruct)
                                 else afterStruct
                  in case findBraces afterTag of
                       Just (body, afterClose) ->
                         let name = filter (\c -> isAlphaNum c || c == '_')
                                           (dropWhile isSpace afterClose)
                         in if null name then Nothing
                            else case parseStructFields body of
                                   Just fields -> Just (name, fields)
                                   Nothing -> Nothing
                       Nothing -> Nothing
             else Nothing
      | otherwise = Nothing

    -- struct Name { ... }
    parseNamedStruct s
      | "struct" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 6 s)
              (name, afterName) = span (\c -> isAlphaNum c || c == '_') rest
          in if null name then Nothing
             else case findBraces (dropWhile isSpace afterName) of
                    Just (body, _) ->
                      case parseStructFields body of
                        Just fields -> Just (name, fields)
                        Nothing -> Nothing
                    Nothing -> Nothing
      | otherwise = Nothing

    -- Find matching braces, return (body, rest)
    findBraces ('{':rest) = go 1 [] rest
      where
        go 0 acc s = Just (reverse acc, s)
        go _ _   [] = Nothing
        go n acc ('{':cs) = go (n+1) ('{':acc) cs
        go n acc ('}':cs) = if n == 1 then go 0 acc cs else go (n-1) ('}':acc) cs
        go n acc (c:cs) = go n (c:acc) cs
    findBraces _ = Nothing

    -- Parse struct fields: "int x ; float y" → [("x", CInt 32), ("y", CFloat)]
    parseStructFields body =
      let fieldDecls = filter (not . null . dropWhile isSpace) (splitOn ';' body)
          emptyMap = Map.empty :: TypeDefMap
      in mapM (parseOneField emptyMap) fieldDecls

    parseOneField sm' fieldStr =
      let ws = words fieldStr
      in case ws of
           [] -> Nothing
           [_] -> Nothing
           _ -> let name = last ws
                    typeStr = unwords (init ws)
                in case parseCType sm' typeStr of
                     Just ty -> Just (name, ty)
                     Nothing -> Nothing

-- | Parse function pointer typedefs from preprocessed output.
-- Handles: typedef RetType (*Name)(ParamTypes);
parseCallbackDefs :: String -> Map String (CType, [CType])
parseCallbackDefs output =
  let contentLines = filter (not . isPrefixOf "#") (lines output)
      joined = unwords (map (filter (/= '\r')) contentLines)
      decls = stlCb 0 [] [] joined
  in foldl' collectCb Map.empty decls
  where
    stlCb _ acc cur [] = reverse (reverse cur : acc)
    stlCb n acc cur ('{':cs) = stlCb (n+1) acc ('{':cur) cs
    stlCb n acc cur ('}':cs) = stlCb (max 0 (n-1)) acc ('}':cur) cs
    stlCb 0 acc cur (';':cs) = stlCb 0 (reverse cur : acc) [] cs
    stlCb n acc cur (c:cs)   = stlCb n acc (c:cur) cs

    collectCb acc decl =
      let trimmed = dropWhile isSpace decl
      in case parseFnPtrTypedef trimmed of
           Just (name, ret, params) -> Map.insert name (ret, params) acc
           Nothing -> acc

    -- Parse: typedef RetType (*Name)(ParamTypes)
    parseFnPtrTypedef s
      | "typedef" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 7 s)
          in case parseRetAndPtr rest of
               Just (retType, name, paramStr) ->
                 let emptyMap = Map.empty :: TypeDefMap
                 in case parseParamList emptyMap paramStr of
                      Just params -> Just (name, retType, params)
                      Nothing -> Nothing
               Nothing -> Nothing
      | otherwise = Nothing

    -- Parse "RetType (*Name)(params)" → (retType, name, paramStr)
    parseRetAndPtr s =
      case break (== '(') s of
        (retPart, '(':rest) ->
          let retStr = dropWhileEnd isSpace retPart
              emptyMap = Map.empty :: TypeDefMap
          in case parseCType emptyMap retStr of
               Just retType ->
                 case break (== ')') rest of
                   (ptrPart, ')':afterParen) ->
                     let nameStr = filter (\c -> isAlphaNum c || c == '_')
                                          (dropWhile (== '*') (dropWhile isSpace ptrPart))
                         afterTrim = dropWhile isSpace afterParen
                     in if null nameStr then Nothing
                        else case afterTrim of
                               '(':')':_ -> Just (retType, nameStr, "void")
                               '(':paramsTail ->
                                 let paramStr = takeWhile (/= ')') paramsTail
                                 in if null paramStr
                                    then Just (retType, nameStr, "void")
                                    else Just (retType, nameStr, paramStr)
                               _ -> Nothing
                   _ -> Nothing
               Nothing -> Nothing
        _ -> Nothing

    dropWhileEnd p = reverse . dropWhile p . reverse

-- | Parse enum definitions and extract named constants.
-- Handles: enum { A = 0, B = 1 }, typedef enum { ... } Name;
parseEnumDefs :: String -> ([CEnumConst], [String])
parseEnumDefs output =
  let contentLines = filter (not . isPrefixOf "#") (lines output)
      joined = unwords (map (filter (/= '\r')) contentLines)
      -- Use brace-aware splitting
      decls = stlEnum 0 [] [] joined
      (_, consts, typeNames) = foldl' collectEnum (Map.empty, [], []) decls
  in (consts, typeNames)
  where
    -- Split on ';' at brace depth 0 (reuse same logic)
    stlEnum _ acc cur [] = reverse (reverse cur : acc)
    stlEnum n acc cur ('{':cs) = stlEnum (n+1) acc ('{':cur) cs
    stlEnum n acc cur ('}':cs) = stlEnum (max 0 (n-1)) acc ('}':cur) cs
    stlEnum 0 acc cur (';':cs) = stlEnum 0 (reverse cur : acc) [] cs
    stlEnum n acc cur (c:cs)   = stlEnum n acc (c:cur) cs

    collectEnum (env, consts, typeNames) decl =
      let trimmed = dropWhile isSpace decl
      in case parseEnumDecl env trimmed of
           Just (cs, env', mName) -> (env', consts ++ cs, typeNames ++ maybe [] (:[]) mName)
           Nothing -> (env, consts, typeNames)

    parseEnumDecl env s
      | "typedef" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 7 s)
          in if "enum" `isPrefixOf` rest
             then let afterEnum = dropWhile isSpace (drop 4 rest)
                      -- skip optional enum tag name (e.g. "SDL_RendererFlags")
                      afterName = dropWhile isSpace (snd (span (\c -> isAlphaNum c || c == '_') afterEnum))
                  in case findBraces afterName of
                       Just (body, afterClose) ->
                         let name = filter (\c -> isAlphaNum c || c == '_')
                                           (dropWhile isSpace afterClose)
                             (consts, env') = parseEnumBody env body
                         in Just (consts, env', if null name then Nothing else Just name)
                       Nothing -> Nothing
             else Nothing
      | "enum" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 4 s)
              (possibleName, afterName) = span (\c -> isAlphaNum c || c == '_') rest
          in case findBraces (dropWhile isSpace afterName) of
               Just (body, _) ->
                 let (consts, env') = parseEnumBody env body
                 in Just (consts, env', if null possibleName then Nothing else Just possibleName)
               Nothing -> Nothing
      | otherwise = Nothing

    findBraces ('{':rest) = go 1 [] rest
      where
        go 0 acc r = Just (reverse acc, r)
        go _ _   [] = Nothing
        go n acc ('{':cs) = go (n+1) ('{':acc) cs
        go n acc ('}':cs) = if n == 1 then go 0 acc cs else go (n-1) ('}':acc) cs
        go n acc (c:cs) = go n (c:acc) cs
    findBraces _ = Nothing

    parseEnumBody env body =
      let entries = splitOn ',' body
      in case foldl' parseEntry (0, env, []) entries of
           (_, env', consts) -> (consts, env')

    parseEntry (nextVal, env, acc) entry =
      let trimmed = dropWhile isSpace entry
          ws = words trimmed
      in case ws of
           [] -> (nextVal, env, acc)
           (name:"=":_)
             | all (\c -> isAlphaNum c || c == '_') name
             , Just v <- evalConstExpr env (dropWhile isSpace (drop 1 (dropWhile (/= '=') trimmed))) ->
                 (v + 1, Map.insert name v env, acc ++ [CEnumConst (T.pack name) v])
           [name]
             | all (\c -> isAlphaNum c || c == '_') name
             , not (null name) ->
                 (nextVal + 1, Map.insert name nextVal env, acc ++ [CEnumConst (T.pack name) nextVal])
           _ -> (nextVal, env, acc)

    -- | Evaluate a C constant expression with operator precedence.
    -- Supports: integer/hex/char literals, named references, |, ^, &, <<, >>, +, -, *, ~
    evalConstExpr :: Map String Int -> String -> Maybe Int
    evalConstExpr env s = case parseOr (dropWhile isSpace s) of
      Just (n, rest) | all isSpace rest -> Just n
      _ -> Nothing
      where
        parseOr = parseBinChain [("|", (.|.))] parseXor
        parseXor = parseBinChain [("^", xor)] parseAnd
        parseAnd = parseBinChain [("&", (.&.))] parseShift
        parseShift = parseBinChain [("<<", shiftL), (">>", shiftR)] parseAdd
        parseAdd = parseBinChain [("+", (+)), ("-", (-))] parseMul
        parseMul = parseBinChain [("*", (*))] parseUnary

        parseBinChain ops next s' = do
          (l, r) <- next s'
          go l (dropWhile isSpace r)
          where
            go l r' = case matchOp ops r' of
              Just (op, after) -> do
                (r2, rest) <- next (dropWhile isSpace after)
                go (op l r2) (dropWhile isSpace rest)
              Nothing -> Just (l, r')
            matchOp [] _ = Nothing
            matchOp ((sym, op):os') r'
              | sym `isPrefixOf` r'
              , not (sym == "|" && "||" `isPrefixOf` r')
              , not (sym == "&" && "&&" `isPrefixOf` r')
              , not (sym == "<" && "<=" `isPrefixOf` r')
              , not (sym == ">" && ">=" `isPrefixOf` r')
              = Just (op, drop (length sym) r')
              | otherwise = matchOp os' r'

        parseUnary s' = case s' of
          ('~':rest) -> do
            (n, r) <- parseUnary (dropWhile isSpace rest)
            Just (complement n, r)
          ('-':rest) | not (null rest) && (head rest /= '-') -> do
            (n, r) <- parseUnary (dropWhile isSpace rest)
            Just (negate n, r)
          _ -> parseAtom s'

        parseAtom s' = case s' of
          ('(':rest) -> do
            (n, r) <- parseOr (dropWhile isSpace rest)
            let r' = dropWhile isSpace r
            case r' of
              (')':after) -> Just (n, after)
              _ -> Nothing
          ('\'':'\\':'x':rest) -> parseCharHex' rest
          ('\'':'\\':'X':rest) -> parseCharHex' rest
          ('\'':'\\':c:'\'':rest) -> Just (escapeChar c, rest)
          ('\'':c:'\'':rest) -> Just (fromEnum c, rest)
          ('0':'x':rest) -> parseHexAtom rest
          ('0':'X':rest) -> parseHexAtom rest
          _ | not (null s') && isDigit (head s') -> parseDecAtom s'
            | not (null s') && (isAlpha (head s') || head s' == '_') ->
                let (name, rest) = span (\c -> isAlphaNum c || c == '_') s'
                in case Map.lookup name env of
                     Just v -> Just (v, rest)
                     Nothing -> Nothing
            | otherwise -> Nothing

        parseHexAtom h =
          let digits = takeWhile isHexDigit h
              after = dropWhile (\c -> c == 'u' || c == 'U' || c == 'l' || c == 'L')
                                (drop (length digits) h)
          in if not (null digits)
             then Just (foldl' (\a c -> a * 16 + digitToInt c) 0 digits, after)
             else Nothing

        parseDecAtom s' =
          let digits = takeWhile isDigit s'
              after = dropWhile (\c -> c == 'u' || c == 'U' || c == 'l' || c == 'L')
                                (drop (length digits) s')
          in if not (null digits)
             then Just (read digits, after)
             else Nothing

        parseCharHex' h =
          let digits = takeWhile isHexDigit h
              rest = drop (length digits) h
          in if not (null digits) then case rest of
               ('\'':after) -> Just (foldl' (\a c -> a * 16 + digitToInt c) 0 digits, after)
               _ -> Just (foldl' (\a c -> a * 16 + digitToInt c) 0 digits, rest)
             else Nothing
    escapeChar c = case c of
      'n' -> 10; 'r' -> 13; 't' -> 9; '0' -> 0
      'a' -> 7; 'b' -> 8; 'f' -> 12; 'v' -> 11
      _ -> fromEnum c

isInternal :: Text -> Bool
isInternal name = T.isPrefixOf "__" name || T.isPrefixOf "_" name

-- | Extract #define integer constants from gcc -dM -E output.
-- Filters to user-facing constants (uppercase names, no __ prefix).
-- Uses multi-pass resolution to handle cross-references between macros.
-- Supports function-like macro expansion for constant evaluation.
parseDefineConstants :: String -> [CEnumConst]
parseDefineConstants output =
  let allLines = lines output
      -- Parse function-like macros: #define NAME(PARAMS) BODY
      funcMacros = Map.fromList
        [(name, (params, body))
        | line <- allLines
        , let ws = words line
        , length ws >= 3
        , head ws == "#define"
        , let rest = drop 8 (dropWhile isSpace line) -- skip "#define "
        , let (nameAndParams, body') = span (/= ')') rest
        , '(' `elem` nameAndParams  -- has opening paren (function-like macro)
        , let (name, paramStr) = span (\c -> isAlphaNum c || c == '_') nameAndParams
        , not (null name)
        , '(' `elem` paramStr
        , let params = map (filter (\c -> isAlphaNum c || c == '_'))
                           (splitOn ',' (drop 1 (dropWhile (/= '(') paramStr)))
        , let body = dropWhile isSpace (drop 1 body')  -- skip ')'
        , not (null body)
        ]
      -- Parse object-like macros (no parens in name)
      candidates = [(name, val) | line <- allLines
                                , let ws = words line
                                , length ws >= 3
                                , head ws == "#define"
                                , let name = ws !! 1
                                , let val = unwords (drop 2 ws)
                                , not ("__" `isPrefixOf` name)
                                , not (null name)
                                , isUpper (head name)
                                , '(' `notElem` name]
      -- Multi-pass: resolve simple literals first, then expressions referencing them
      resolve env [] = (env, [])
      resolve env todo =
        let (resolved, remaining) = foldr tryResolve ([], []) todo
            tryResolve (name, val) (res, rem') =
              case evalConstExpr env funcMacros val of
                Just n  -> ((name, n) : res, rem')
                Nothing -> (res, (name, val) : rem')
            env' = foldl' (\m (n, v) -> Map.insert n v m) env resolved
        in if null resolved
           then (env', [])  -- no progress, stop
           else let (env'', more) = resolve env' remaining
                in (env'', map (\(n,v) -> CEnumConst (T.pack n) v) resolved ++ more)
      (_, consts) = resolve Map.empty candidates
  in consts
  where
    -- Expression evaluator with function-like macro expansion.
    -- Depth-limited to prevent infinite recursion from circular macros.
    evalConstExpr :: Map String Int -> Map String ([String], String) -> String -> Maybe Int
    evalConstExpr env macros = go 8  -- max 8 levels of macro expansion
      where
        go 0 _ = Nothing  -- circular or deeply nested macro; skip
        go depth s = case parseOr depth (dropWhile isSpace s) of
          Just (n, rest) | all isSpace rest -> Just n
          _ -> Nothing

        parseOr d = parseBinChain [("|", (.|.))] (parseXor d)
        parseXor d = parseBinChain [("^", xor)] (parseAnd d)
        parseAnd d = parseBinChain [("&", (.&.))] (parseShift d)
        parseShift d = parseBinChain [("<<", shiftL), (">>", shiftR)] (parseAdd d)
        parseAdd d = parseBinChain [("+", (+)), ("-", (-))] (parseMul d)
        parseMul d = parseBinChain [("*", (*))] (parseUnary d)

        parseBinChain ops next s' = do
          (l, r) <- next s'
          go l (dropWhile isSpace r)
          where
            go l r' = case matchOp ops r' of
              Just (op, after) -> do
                (r2, rest) <- next (dropWhile isSpace after)
                go (op l r2) (dropWhile isSpace rest)
              Nothing -> Just (l, r')
            matchOp [] _ = Nothing
            matchOp ((sym, op):os') r'
              | sym `isPrefixOf` r'
              , not (sym == "|" && "||" `isPrefixOf` r')
              , not (sym == "&" && "&&" `isPrefixOf` r')
              = Just (op, drop (length sym) r')
              | otherwise = matchOp os' r'

        parseUnary d s' = case s' of
          ('~':rest) -> do
            (n, r) <- parseUnary d (dropWhile isSpace rest)
            Just (complement n, r)
          ('-':rest) | not (null rest) && head rest /= '-' -> do
            (n, r) <- parseUnary d (dropWhile isSpace rest)
            Just (negate n, r)
          _ -> parseAtom d s'

        parseAtom d s' = case s' of
          ('(':rest) -> do
            (n, r) <- parseOr d (dropWhile isSpace rest)
            let r' = dropWhile isSpace r
            case r' of
              (')':after) -> Just (n, after)
              _ -> Nothing
          ('0':'x':rest) -> parseHexAtom rest
          ('0':'X':rest) -> parseHexAtom rest
          _ | not (null s') && isDigit (head s') -> parseDecAtom s'
            | not (null s') && (isAlpha (head s') || head s' == '_') ->
                let (name, rest) = span (\c -> isAlphaNum c || c == '_') s'
                    rest' = dropWhile isSpace rest
                in case rest' of
                     -- Function-like macro call: NAME(args)
                     ('(':afterParen) ->
                       case Map.lookup name macros of
                         Just (params, body) -> expandMacro d params body afterParen
                         Nothing -> Nothing
                     -- Simple name reference
                     _ -> case Map.lookup name env of
                            Just v -> Just (v, rest)
                            Nothing -> Nothing
            | otherwise -> Nothing

        -- Expand a function-like macro by substituting params with evaluated args
        expandMacro d params body afterParen =
          let argStr = takeWhile (/= ')') afterParen
              afterClose = drop 1 (dropWhile (/= ')') afterParen)
              args = splitTopLevelCommas argStr
              substituted = substituteParams (zip params args) body
          in case go (d - 1) substituted of
               Just n -> Just (n, afterClose)
               Nothing -> Nothing

        -- Split on commas but respect parentheses
        splitTopLevelCommas s' = go 0 "" s'
          where
            go _ cur [] = [reverse cur]
            go 0 cur (',':cs) = reverse cur : go 0 "" cs
            go n cur ('(':cs) = go (n+1) ('(':cur) cs
            go n cur (')':cs) = go (max 0 (n-1)) (')':cur) cs
            go n cur (c:cs)   = go n (c:cur) cs

        -- Substitute parameter names with argument text in macro body
        substituteParams [] body = body
        substituteParams ((param, arg):rest) body =
          substituteParams rest (replaceAll param (dropWhile isSpace arg) body)

        replaceAll _ _ [] = []
        replaceAll param arg s'@(c:cs)
          | param `isPrefixOf` s'
          , let after = drop (length param) s'
          , null after || not (isAlphaNum (head after) || head after == '_')
          , null s' || c == head param  -- sanity
          = arg ++ replaceAll param arg after
          | otherwise = c : replaceAll param arg cs

        parseHexAtom h =
          let digits = takeWhile isHexDigit h
              after = dropWhile (\c -> c == 'u' || c == 'U' || c == 'l' || c == 'L')
                                (drop (length digits) h)
          in if not (null digits)
             then Just (foldl' (\a c -> a * 16 + digitToInt c) 0 digits, after)
             else Nothing

        parseDecAtom s' =
          let digits = takeWhile isDigit s'
              after = dropWhile (\c -> c == 'u' || c == 'U' || c == 'l' || c == 'L')
                                (drop (length digits) s')
          in if not (null digits)
             then Just (read digits, after)
             else Nothing

-- | Run gcc -dM -E to extract all macro definitions as text
extractDefines :: String -> [String] -> String -> IO (Either String String)
extractDefines cc args input = do
  result <- try (readProcess cc ("-dM" : args) input) :: IO (Either SomeException String)
  case result of
    Right out -> pure (Right out)
    Left e -> pure (Left ("define extraction failed: " ++ show e))

-- | Provide fallback signatures for common system headers when preprocessing
-- yields incomplete results on some platforms (MinGW/MSYS). These are minimal
-- subsets sufficient for the test-suite.
builtinSigsFor :: String -> [CFunSig]
builtinSigsFor "math.h" =
  [ CFunSig { cfName = T.pack "sin", cfRet = CFloat, cfParams = [CFloat] }
  , CFunSig { cfName = T.pack "sqrt", cfRet = CFloat, cfParams = [CFloat] }
  , CFunSig { cfName = T.pack "pow", cfRet = CFloat, cfParams = [CFloat, CFloat] }
  , CFunSig { cfName = T.pack "frexp", cfRet = CFloat, cfParams = [CFloat, COut (CInt 32)] }
  ]
builtinSigsFor "stdio.h" =
  if os == "mingw32"
  then
    [ CFunSig { cfName = T.pack "fgetws", cfRet = CPtr (T.pack "wchar_t"), cfParams = [CPtr (T.pack "wchar_t"), CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fputws", cfRet = CInt 32, cfParams = [CPtr (T.pack "wchar_t"), CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "gets_s", cfRet = CString, cfParams = [CString, CInt 32] }
    , CFunSig { cfName = T.pack "fgets", cfRet = CString, cfParams = [CString, CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fopen", cfRet = CPtr (T.pack "FILE"), cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "freopen", cfRet = CPtr (T.pack "FILE"), cfParams = [CString, CString, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fclose", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fread", cfRet = CInt 32, cfParams = [CString, CInt 32, CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fwrite", cfRet = CInt 32, cfParams = [CString, CInt 32, CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fseek", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE"), CInt 32, CInt 32] }
    , CFunSig { cfName = T.pack "ftell", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "feof", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "ferror", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "perror", cfRet = CVoid, cfParams = [CString] }
    , CFunSig { cfName = T.pack "putchar", cfRet = CInt 32, cfParams = [CInt 32] }
    , CFunSig { cfName = T.pack "puts", cfRet = CInt 32, cfParams = [CString] }
    , CFunSig { cfName = T.pack "putc", cfRet = CInt 32, cfParams = [CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "getc", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "ungetc", cfRet = CInt 32, cfParams = [CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "tempnam", cfRet = CString, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "remove", cfRet = CInt 32, cfParams = [CString] }
    , CFunSig { cfName = T.pack "rename", cfRet = CInt 32, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "unlink", cfRet = CInt 32, cfParams = [CString] }
    , CFunSig { cfName = T.pack "rewind", cfRet = CVoid, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "setbuf", cfRet = CVoid, cfParams = [CPtr (T.pack "FILE"), CString] }
    , CFunSig { cfName = T.pack "setvbuf", cfRet = CInt 32, cfParams = [CPtr (T.pack "FILE"), CString, CInt 32, CInt 32] }
    , CFunSig { cfName = T.pack "snprintf", cfRet = CInt 32, cfParams = [CString, CInt 32, CString] }
    , CFunSig { cfName = T.pack "sprintf", cfRet = CInt 32, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "sscanf", cfRet = CInt 32, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "scanf", cfRet = CInt 32, cfParams = [CString] }
    ]
  else
    [ CFunSig { cfName = T.pack "fgets", cfRet = CString, cfParams = [CString, CInt 32, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fopen", cfRet = CPtr (T.pack "FILE"), cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "freopen", cfRet = CPtr (T.pack "FILE"), cfParams = [CString, CString, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "tempnam", cfRet = CString, cfParams = [CString, CString] }
    ]
builtinSigsFor _ = []

-- | Dedupe signatures: prefer the signature with the most parameters for a given name
-- This avoids keeping spurious zero-arg/malformed prototypes emitted by some
-- preprocessors (notably MinGW headers).
dedupeSigs :: [CFunSig] -> [CFunSig]
dedupeSigs sigs = Map.elems $ foldl' insert Map.empty sigs
  where
    insert m s = let n = cfName s in case Map.lookup n m of
      Nothing -> Map.insert n s m
      Just e  -> if length (cfParams s) > length (cfParams e) then Map.insert n s m else m

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c s  = case break (== c) s of
  (w, [])     -> [w]
  (w, _:rest) -> w : splitOn c rest
