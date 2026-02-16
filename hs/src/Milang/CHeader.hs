{-# LANGUAGE OverloadedStrings #-}
module Milang.CHeader (parseCHeader, CFunSig(..)) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf)
import System.Process (readProcess)

import Milang.Syntax

-- | A parsed C function signature
data CFunSig = CFunSig
  { cfName   :: !Text
  , cfRet    :: !CType
  , cfParams :: ![CType]
  } deriving (Show)

-- | Parse a C header file using clang -E, returning (header_path, [function_sig])
parseCHeader :: FilePath -> IO (Either String [CFunSig])
parseCHeader path = do
  -- Run clang preprocessor
  output <- readProcess "clang" ["-E", path] ""
  let sigs = mapMaybe parseLine (lines output)
  -- Filter out internal/compiler symbols
      visible = filter (not . isInternal . cfName) sigs
  pure $ Right visible

-- | Try to parse a function declaration from a preprocessed line.
-- Handles: "extern TYPE NAME (ARGS) ..." and "TYPE NAME (ARGS) ..."
parseLine :: String -> Maybe CFunSig
parseLine line =
  case words (stripAttrs (takeDecl line)) of
    ("extern" : rest) -> parseDecl rest
    rest              -> parseDecl rest

-- | Take everything up to the first semicolon
takeDecl :: String -> String
takeDecl = takeWhile (/= ';')

-- | Strip __attribute__((...)) and similar noise
stripAttrs :: String -> String
stripAttrs [] = []
stripAttrs ('_':'_':'a':'t':'t':'r':_) = []  -- truncate at __attribute__
stripAttrs (c:cs) = c : stripAttrs cs

-- | Parse "TYPE NAME (PARAM_TYPES)" from word list
parseDecl :: [String] -> Maybe CFunSig
parseDecl ws = do
  let joined = unwords ws
  (beforeParen, afterParen) <- splitAtParen joined
  let beforeWords = words beforeParen
  case reverse beforeWords of
    [] -> Nothing
    [_] -> Nothing  -- just a name, no return type
    (rawName : retWords) -> do
      -- Handle C style "TYPE *name" — strip leading *s from name, add to ret type
      let (stars, name) = span (== '*') rawName
          retStr = unwords (reverse retWords) ++ replicate (length stars) '*'
      ret <- parseCType retStr
      params <- parseParamList afterParen
      -- Skip variadic functions
      if "..." `elem` words afterParen || null name
        then Nothing
        else Just CFunSig
          { cfName   = T.pack name
          , cfRet    = ret
          , cfParams = params
          }

-- | Split "stuff (params)" into ("stuff ", "params)")
splitAtParen :: String -> Maybe (String, String)
splitAtParen s = case break (== '(') s of
  (_, [])     -> Nothing
  (before, _:after) -> Just (before, after)

-- | Parse a C parameter list like "int __x, double __y)"
-- Returns Nothing if any param has an unsupported type (pointers, etc.)
parseParamList :: String -> Maybe [CType]
parseParamList s =
  let s' = takeWhile (/= ')') s
      params = map (T.strip . T.pack) (splitOn ',' s')
  in if params == [T.pack "void"] || params == [T.pack ""]
     then Just []
     else mapM parseParamType params

-- | Parse a single parameter declaration like "double __x" or "int"
-- Returns Nothing for unsupported types only
parseParamType :: T.Text -> Maybe CType
parseParamType param
  -- Array params like "char __s[20]" → treat as pointer
  | T.any (== '[') param =
      let base = T.strip $ fst $ T.breakOn (T.pack "[") param
          baseWords = filter (\w -> not (T.isPrefixOf (T.pack "__") w)) (T.words base)
          baseStrs = map T.unpack baseWords
      in case baseStrs of
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           _                 -> Just CPtr
  | T.any (== '*') param =
      let starCount = T.length (T.filter (== '*') param)
          cleaned = T.words (T.replace (T.pack "*") (T.pack " ") param)
          base = filter (\w -> not (T.isPrefixOf (T.pack "__") w)) cleaned
          baseStrs = map T.unpack base
      in if starCount > 1
         then Just CPtr  -- double/triple pointers → opaque
         else case baseStrs of
           -- const char * → CString
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           -- int * → output parameter
           ["int"]           -> Just COutInt
           ["long"]          -> Just COutInt
           ["unsigned"]      -> Just COutInt
           -- double * / float * → output parameter
           ["double"]        -> Just COutFloat
           -- float* and long double* output params are tricky (type mismatch)
           -- Skip them — the double variant works fine
           ["float"]         -> Nothing
           ["long", "double"] -> Nothing
           -- everything else → opaque pointer
           _                 -> Just CPtr
  | otherwise =
      let ws = T.words param
          typeStr = case ws of
            []  -> ""
            [w] -> w
            _   -> T.unwords (init ws)
      in parseCType (T.unpack typeStr)

-- | Map a C type string to our CType
parseCType :: String -> Maybe CType
parseCType s
  | '*' `elem` s =
      let cleaned = words (map (\c -> if c == '*' then ' ' else c) s)
          base = filter (\w -> not ("__" `isPrefixOf` w)) cleaned
      in case base of
           -- const char * → CString
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           -- everything else with a pointer → CPtr (opaque)
           _                 -> Just CPtr
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
      _                   -> Nothing      -- skip structs passed by value, etc.

-- | Filter out internal/compiler-generated symbols
isInternal :: Text -> Bool
isInternal name = T.isPrefixOf "__" name || T.isPrefixOf "_" name

-- | Split a string on a character
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c s  = case break (== c) s of
  (w, [])   -> [w]
  (w, _:rest) -> w : splitOn c rest
