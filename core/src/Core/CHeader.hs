{-# LANGUAGE OverloadedStrings #-}
module Core.CHeader (parseCHeader, CFunSig(..)) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf)
import System.Process (readProcess)

import Core.Syntax (CType(..))

-- | A parsed C function signature
data CFunSig = CFunSig
  { cfName   :: !Text
  , cfRet    :: !CType
  , cfParams :: ![CType]
  } deriving (Show)

-- | Parse a C header file using clang -E
parseCHeader :: FilePath -> IO (Either String [CFunSig])
parseCHeader path = do
  output <- readProcess "clang" ["-E", path] ""
  let sigs = mapMaybe parseLine (lines output)
      visible = filter (not . isInternal . cfName) sigs
  pure $ Right visible

parseLine :: String -> Maybe CFunSig
parseLine line =
  case words (stripAttrs (takeDecl line)) of
    ("extern" : rest) -> parseDecl rest
    rest              -> parseDecl rest

takeDecl :: String -> String
takeDecl = takeWhile (/= ';')

stripAttrs :: String -> String
stripAttrs [] = []
stripAttrs ('_':'_':'a':'t':'t':'r':_) = []
stripAttrs (c:cs) = c : stripAttrs cs

parseDecl :: [String] -> Maybe CFunSig
parseDecl ws = do
  let joined = unwords ws
  (beforeParen, afterParen) <- splitAtParen joined
  let beforeWords = words beforeParen
  case reverse beforeWords of
    [] -> Nothing
    [_] -> Nothing
    (rawName : retWords) -> do
      let (stars, name) = span (== '*') rawName
          retStr = unwords (reverse retWords) ++ replicate (length stars) '*'
      ret <- parseCType retStr
      params <- parseParamList afterParen
      if "..." `elem` words afterParen || null name
        then Nothing
        else Just CFunSig
          { cfName   = T.pack name
          , cfRet    = ret
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
  in if params == ["void"] || params == [""]
     then Just []
     else mapM parseParamType params

parseParamType :: T.Text -> Maybe CType
parseParamType param
  | T.any (== '[') param =
      let base = T.strip $ fst $ T.breakOn "[" param
          baseWords = filter (\w -> not (T.isPrefixOf "__" w)) (T.words base)
          baseStrs = map T.unpack baseWords
      in case baseStrs of
           ["char"]          -> Just CString
           ["const", "char"] -> Just CString
           _                 -> Just CPtr
  | T.any (== '*') param =
      let starCount = T.length (T.filter (== '*') param)
          cleaned = T.words (T.replace "*" " " param)
          base = filter (\w -> not (T.isPrefixOf "__" w)) cleaned
          baseStrs = map T.unpack base
      in if starCount > 1
         then Just CPtr
         else case baseStrs of
           ["char"]           -> Just CString
           ["const", "char"]  -> Just CString
           ["int"]            -> Just COutInt
           ["long"]           -> Just COutInt
           ["unsigned"]       -> Just COutInt
           ["double"]         -> Just COutFloat
           ["float"]          -> Nothing
           ["long", "double"] -> Nothing
           _                  -> Just CPtr
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
      _                   -> Nothing

isInternal :: Text -> Bool
isInternal name = T.isPrefixOf "__" name || T.isPrefixOf "_" name

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c s  = case break (== c) s of
  (w, [])     -> [w]
  (w, _:rest) -> w : splitOn c rest
