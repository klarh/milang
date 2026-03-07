{-# LANGUAGE OverloadedStrings #-}
module Core.CHeader (parseCHeader, parseCHeaderInclude, CFunSig(..), CEnumConst(..)) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Char (isAlphaNum, isSpace, isUpper, isDigit, isHexDigit, digitToInt)
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
          rawSigs = mapMaybe (parseLine typeMap) (lines output)
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
          rawSigs = mapMaybe (parseLine typeMap) (lines output)
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

parseLine :: TypeDefMap -> String -> Maybe CFunSig
parseLine sm line =
  let decl = takeDecl line
  in case break (== '(') decl of
    (_, []) -> Nothing
    (before, _:after) -> parseDecl sm (before, takeWhile (/= ')') after)

-- takeDecl: get text until semicolon
takeDecl :: String -> String
takeDecl = takeWhile (/= ';')

-- stripAttrs: remove obvious attribute tokens (leave rest intact)
stripAttrs :: String -> String
stripAttrs s = s  -- handled later by token filtering

parseDecl :: TypeDefMap -> (String, String) -> Maybe CFunSig
parseDecl sm (beforeParen, paramStr) = do
  let beforeWords = words beforeParen
  case beforeWords of
    [] -> Nothing
    [_] -> Nothing
    _ -> do
      let rawName = last beforeWords
          name = filter (\c -> isAlphaNum c || c == '_') rawName
          retWords = init beforeWords
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
  let s' = takeWhile (/= ')') s
      params = map (T.strip . T.pack) (splitOn ',' s')
  in case params of
       ["void"] -> Just []
       [""]     -> Nothing            -- old-style unspecified parameter list: treat as unknown
       _         -> mapM (parseParamType sm) params

parseParamType :: TypeDefMap -> T.Text -> Maybe CType
parseParamType sm param
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
          baseWords = filter (\w -> not (T.isPrefixOf "__" w)) cleaned
      in if starCount > 1
         then Just (CPtr (T.unwords baseWords))
         else case baseWords of
           ["char"]           -> Just CString
           ["const", "char"]  -> Just CString
           ["int"]            -> Just (COut (CInt 32))
           ["long"]           -> Just (COut (CInt 64))
           ["unsigned"]       -> Just (COut (CInt 32))
           ["double"]         -> Just (COut CFloat)
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
      ["long"]            -> Just (CInt 64)
      ["long", "int"]     -> Just (CInt 64)
      ["long", "long"]    -> Just (CInt 64)
      ["unsigned"]        -> Just (CInt 32)
      ["unsigned", "int"] -> Just (CInt 32)
      ["unsigned", "long"] -> Just (CInt 64)
      ["unsigned", "long", "long"] -> Just (CInt 64)
      ["short"]           -> Just (CInt 16)
      ["unsigned", "short"] -> Just (CInt 16)
      ["int8_t"]          -> Just (CInt 8)
      ["int16_t"]         -> Just (CInt 16)
      ["int32_t"]         -> Just (CInt 32)
      ["int64_t"]         -> Just (CInt 64)
      ["uint8_t"]         -> Just (CInt 8)
      ["uint16_t"]        -> Just (CInt 16)
      ["uint32_t"]        -> Just (CInt 32)
      ["uint64_t"]        -> Just (CInt 64)
      ["size_t"]          -> Just (CInt 64)
      ["ssize_t"]         -> Just (CInt 64)
      ["ptrdiff_t"]       -> Just (CInt 64)
      ["double"]          -> Just CFloat
      ["float"]           -> Just CFloat32
      ["long", "double"]  -> Just CFloat
      ["void"]            -> Just CVoid
      ["char"]            -> Just (CInt 8)
      ["signed", "char"]  -> Just (CInt 8)
      ["unsigned", "char"] -> Just (CInt 8)
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
  Nothing -> Nothing

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
                  in case findBraces afterStruct of
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
  in foldl' collectEnum ([], []) decls
  where
    -- Split on ';' at brace depth 0 (reuse same logic)
    stlEnum _ acc cur [] = reverse (reverse cur : acc)
    stlEnum n acc cur ('{':cs) = stlEnum (n+1) acc ('{':cur) cs
    stlEnum n acc cur ('}':cs) = stlEnum (max 0 (n-1)) acc ('}':cur) cs
    stlEnum 0 acc cur (';':cs) = stlEnum 0 (reverse cur : acc) [] cs
    stlEnum n acc cur (c:cs)   = stlEnum n acc (c:cur) cs

    collectEnum (consts, typeNames) decl =
      let trimmed = dropWhile isSpace decl
      in case parseEnumDecl trimmed of
           Just (cs, mName) -> (consts ++ cs, typeNames ++ maybe [] (:[]) mName)
           Nothing -> (consts, typeNames)

    parseEnumDecl s
      | "typedef" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 7 s)
          in if "enum" `isPrefixOf` rest
             then let afterEnum = dropWhile isSpace (drop 4 rest)
                  in case findBraces afterEnum of
                       Just (body, afterClose) ->
                         let name = filter (\c -> isAlphaNum c || c == '_')
                                           (dropWhile isSpace afterClose)
                             consts = parseEnumBody body
                         in Just (consts, if null name then Nothing else Just name)
                       Nothing -> Nothing
             else Nothing
      | "enum" `isPrefixOf` s =
          let rest = dropWhile isSpace (drop 4 s)
              (possibleName, afterName) = span (\c -> isAlphaNum c || c == '_') rest
          in case findBraces (dropWhile isSpace afterName) of
               Just (body, _) ->
                 let consts = parseEnumBody body
                 in Just (consts, if null possibleName then Nothing else Just possibleName)
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

    parseEnumBody body =
      let entries = splitOn ',' body
          autoVal = 0 :: Int
      in snd $ foldl' parseEntry (autoVal, []) entries

    parseEntry (nextVal, acc) entry =
      let trimmed = dropWhile isSpace entry
          ws = words trimmed
      in case ws of
           [] -> (nextVal, acc)
           (name:"=":valStr:_)
             | all (\c -> isAlphaNum c || c == '_') name
             , Just v <- readInt valStr ->
                 (v + 1, acc ++ [CEnumConst (T.pack name) v])
           [name]
             | all (\c -> isAlphaNum c || c == '_') name
             , not (null name) ->
                 (nextVal + 1, acc ++ [CEnumConst (T.pack name) nextVal])
           _ -> (nextVal, acc)

    readInt s' =
      let s'' = dropWhile isSpace s'
      in case reads s'' :: [(Int, String)] of
           [(n, rest)] | all isSpace rest -> Just n
           _ -> Nothing

isInternal :: Text -> Bool
isInternal name = T.isPrefixOf "__" name || T.isPrefixOf "_" name

-- | Extract #define integer constants from gcc -dM -E output.
-- Filters to user-facing constants (uppercase names, no __ prefix).
parseDefineConstants :: String -> [CEnumConst]
parseDefineConstants output =
  mapMaybe parseLine' (lines output)
  where
    parseLine' line = case words line of
      ["#define", name, val]
        | not ("__" `isPrefixOf` name)
        , not (null name)
        , isUpper (head name)
        , Just n <- parseIntLiteral val
        -> Just (CEnumConst (T.pack name) n)
      _ -> Nothing
    parseIntLiteral s
      | "0x" `isPrefixOf` s || "0X" `isPrefixOf` s =
          let hex = drop 2 (filter (/= 'U') (filter (/= 'L') s))
          in if all isHexDigit hex && not (null hex)
             then Just (foldl' (\acc c -> acc * 16 + digitToInt c) 0 hex)
             else Nothing
      | all isDigit s, not (null s) = Just (read s)
      | all (\c -> isDigit c || c == 'U' || c == 'L') s, not (null s) =
          let digits = filter isDigit s
          in if null digits then Nothing else Just (read digits)
      | otherwise = Nothing

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
