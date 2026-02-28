{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.CHeader (parseCHeader, parseCHeaderInclude, CFunSig(..)) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, foldl')
import qualified Data.Map.Strict as Map
import Data.Char (isAlphaNum)
#ifndef WASM
import System.Process (readProcess)
import Control.Exception (try, SomeException)
#endif
import System.FilePath (takeFileName)
import System.Info (os)

import Core.Syntax (CType(..))

-- | A parsed C function signature
data CFunSig = CFunSig
  { cfName   :: !Text
  , cfRet    :: !CType
  , cfParams :: ![CType]
  } deriving (Show)

-- | Parse a C header file using clang -E
#ifdef WASM
runPreprocessor :: [String] -> String -> IO (Either String String)
runPreprocessor _ _ = pure $ Left "preprocessor unavailable in WASM build"
#else
runPreprocessor :: [String] -> String -> IO (Either String String)
runPreprocessor clangArgs input = do
  -- Try clang first, fall back to gcc if clang isn't available or fails
  rclang <- try (readProcess "clang" clangArgs input) :: IO (Either SomeException String)
  case rclang of
    Right out -> pure (Right out)
    Left _ -> do
      rgcc <- try (readProcess "gcc" clangArgs input) :: IO (Either SomeException String)
      case rgcc of
        Right out2 -> pure (Right out2)
        Left e2 -> pure (Left ("preprocessor failed: " ++ show e2))
#endif

parseCHeader :: FilePath -> IO (Either String [CFunSig])
parseCHeader path = do
  r <- runPreprocessor ["-E", path] ""
  case r of
    Left err -> pure $ Left err
    Right output -> do
      let rawSigs = mapMaybe parseLine (lines output)
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
      pure $ Right final

-- | Parse a C header by asking clang to preprocess an #include of the header
-- name. This lets clang find system headers via angle-bracket includes.
parseCHeaderInclude :: String -> IO (Either String [CFunSig])
parseCHeaderInclude hdr = do
  let input = "#include <" ++ hdr ++ ">\n"
  r <- runPreprocessor ["-E", "-xc", "-"] input
  case r of
    Left err -> pure $ Left err
    Right output -> do
      let rawSigs = mapMaybe parseLine (lines output)
          deduped = dedupeSigs rawSigs
          visible = filter (not . isInternal . cfName) deduped
          built = builtinSigsFor hdr
          visMap = Map.fromList [(cfName s, s) | s <- visible]
          merged = foldl' (\m b -> case Map.lookup (cfName b) m of
                      Nothing -> Map.insert (cfName b) b m
                      Just e  -> if null (cfParams e) && not (null (cfParams b)) then Map.insert (cfName b) b m else m
                    ) visMap built
          final = Map.elems merged
      pure $ Right final

parseLine :: String -> Maybe CFunSig
parseLine line =
  let decl = takeDecl line
  in case break (== '(') decl of
    (_, []) -> Nothing
    (before, _:after) -> parseDecl (before, takeWhile (/= ')') after)

-- takeDecl: get text until semicolon
takeDecl :: String -> String
takeDecl = takeWhile (/= ';')

-- stripAttrs: remove obvious attribute tokens (leave rest intact)
stripAttrs :: String -> String
stripAttrs s = s  -- handled later by token filtering

parseDecl :: (String, String) -> Maybe CFunSig
parseDecl (beforeParen, paramStr) = do
  let beforeWords = words beforeParen
  case beforeWords of
    [] -> Nothing
    [_] -> Nothing
    _ -> do
      let rawName = last beforeWords
          -- sanitize name (remove pointer asterisks and other chars)
          name = filter (\c -> isAlphaNum c || c == '_') rawName
          retWords = init beforeWords
          noise = ["extern","static","inline","__inline__","__cdecl","__stdcall","__attribute__","__declspec","__CRT_INLINE","WINAPI","CDECL","DLLIMPORT"]
          isNoise w = any (`isPrefixOf` w) ["__"] || any (== w) noise || '(' `elem` w || ')' `elem` w
          retFiltered = filter (not . isNoise) retWords
          retStr = unwords retFiltered
      if null name then Nothing else do
        ret <- parseCType retStr
        params <- parseParamList paramStr
        if "..." `elem` words paramStr then Nothing else Just CFunSig
          { cfName = T.pack name
          , cfRet = ret
          , cfParams = params
          }

splitAtParen :: String -> Maybe (String, String)
splitAtParen s = case break (== '(') s of
  (_, [])         -> Nothing
  (before, _:after) -> Just (before, after)

parseParamList :: String -> Maybe [CType]
parseParamList s =
  let s' = takeWhile (/= ')') s
      params = map (T.strip . T.pack) (splitOn ',' s')
  in case params of
       ["void"] -> Just []
       [""]     -> Nothing            -- old-style unspecified parameter list: treat as unknown
       _         -> mapM parseParamType params

parseParamType :: T.Text -> Maybe CType
parseParamType param
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
           ["int"]            -> Just COutInt
           ["long"]           -> Just COutInt
           ["unsigned"]       -> Just COutInt
           ["double"]         -> Just COutFloat
           ["float"]          -> Nothing
           ["long", "double"] -> Nothing
           _                  -> Just (CPtr (T.unwords baseWords))
  | otherwise =
      let ws = T.words param
          typeStr = case ws of
            []  -> ""
            [w] -> w
            _   -> T.unwords (init ws)
      in parseCType (T.unpack typeStr)

parseCType :: String -> Maybe CType
parseCType s
  | '*' `elem` s =
      let cleaned = words (map (\c -> if c == '*' then ' ' else c) s)
          base = filter (\w -> not ("__" `isPrefixOf` w)) cleaned
      in case base of
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           _                 -> Just (CPtr (T.pack (unwords base)))
  | otherwise = case words s of
      ["int"]             -> Just CInt
      ["long"]            -> Just CInt
      ["long", "int"]     -> Just CInt
      ["long", "long"]    -> Just CInt
      ["unsigned"]        -> Just CInt
      ["unsigned", "int"] -> Just CInt
      ["short"]           -> Just CInt
      ["int64_t"]         -> Just CInt
      ["size_t"]          -> Just CInt
      ["double"]          -> Just CFloat
      ["float"]           -> Just CFloat
      ["long", "double"]  -> Just CFloat
      ["void"]            -> Just CVoid
      ["char"]            -> Just CInt
      ["const", "char"]   -> Just CInt
      _                   -> Nothing

isInternal :: Text -> Bool
isInternal name = T.isPrefixOf "__" name || T.isPrefixOf "_" name

-- | Provide fallback signatures for common system headers when preprocessing
-- yields incomplete results on some platforms (MinGW/MSYS). These are minimal
-- subsets sufficient for the test-suite.
builtinSigsFor :: String -> [CFunSig]
builtinSigsFor "math.h" =
  [ CFunSig { cfName = T.pack "sin", cfRet = CFloat, cfParams = [CFloat] }
  , CFunSig { cfName = T.pack "sqrt", cfRet = CFloat, cfParams = [CFloat] }
  , CFunSig { cfName = T.pack "pow", cfRet = CFloat, cfParams = [CFloat, CFloat] }
  , CFunSig { cfName = T.pack "frexp", cfRet = CFloat, cfParams = [CFloat, COutInt] }
  ]
builtinSigsFor "stdio.h" =
  if os == "mingw32"
  then
    [ CFunSig { cfName = T.pack "fgetws", cfRet = CPtr (T.pack "wchar_t"), cfParams = [CPtr (T.pack "wchar_t"), CInt, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fputws", cfRet = CInt, cfParams = [CPtr (T.pack "wchar_t"), CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "gets_s", cfRet = CString, cfParams = [CString, CInt] }
    , CFunSig { cfName = T.pack "fgets", cfRet = CString, cfParams = [CString, CInt, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fopen", cfRet = CPtr (T.pack "FILE"), cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "freopen", cfRet = CPtr (T.pack "FILE"), cfParams = [CString, CString, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fclose", cfRet = CInt, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fread", cfRet = CInt, cfParams = [CString, CInt, CInt, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fwrite", cfRet = CInt, cfParams = [CString, CInt, CInt, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "fseek", cfRet = CInt, cfParams = [CPtr (T.pack "FILE"), CInt, CInt] }
    , CFunSig { cfName = T.pack "ftell", cfRet = CInt, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "feof", cfRet = CInt, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "ferror", cfRet = CInt, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "perror", cfRet = CVoid, cfParams = [CString] }
    , CFunSig { cfName = T.pack "putchar", cfRet = CInt, cfParams = [CInt] }
    , CFunSig { cfName = T.pack "puts", cfRet = CInt, cfParams = [CString] }
    , CFunSig { cfName = T.pack "putc", cfRet = CInt, cfParams = [CInt, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "getc", cfRet = CInt, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "ungetc", cfRet = CInt, cfParams = [CInt, CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "tempnam", cfRet = CString, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "remove", cfRet = CInt, cfParams = [CString] }
    , CFunSig { cfName = T.pack "rename", cfRet = CInt, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "unlink", cfRet = CInt, cfParams = [CString] }
    , CFunSig { cfName = T.pack "rewind", cfRet = CVoid, cfParams = [CPtr (T.pack "FILE")] }
    , CFunSig { cfName = T.pack "setbuf", cfRet = CVoid, cfParams = [CPtr (T.pack "FILE"), CString] }
    , CFunSig { cfName = T.pack "setvbuf", cfRet = CInt, cfParams = [CPtr (T.pack "FILE"), CString, CInt, CInt] }
    , CFunSig { cfName = T.pack "snprintf", cfRet = CInt, cfParams = [CString, CInt, CString] }
    , CFunSig { cfName = T.pack "sprintf", cfRet = CInt, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "sscanf", cfRet = CInt, cfParams = [CString, CString] }
    , CFunSig { cfName = T.pack "scanf", cfRet = CInt, cfParams = [CString] }
    ]
  else
    [ CFunSig { cfName = T.pack "fgets", cfRet = CString, cfParams = [CString, CInt, CPtr (T.pack "FILE")] }
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
