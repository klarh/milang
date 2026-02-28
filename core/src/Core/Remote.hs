{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Remote
  ( fetchRemote
  , hashFile
  , hashBytes
  , isURL
  , urlDirName
  , resolveURL
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
#ifndef WASM
import qualified Crypto.Hash.SHA256 as SHA256
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
#endif
import Data.List (isPrefixOf, intercalate)

-- | Check if a path looks like a URL
isURL :: String -> Bool
isURL s = "https://" `isPrefixOf` s || "http://" `isPrefixOf` s

-- | Get the "directory" part of a URL (everything up to the last /)
urlDirName :: String -> String
urlDirName url =
  let parts = wordsBy (== '/') url
  in if length parts <= 3
     then url
     else intercalate "/" (init parts)

-- | Resolve a relative path against a URL base directory
resolveURL :: String -> String -> String
resolveURL base rel
  | isURL rel = rel
  | otherwise = normalizeURL (base ++ "/" ++ rel)

normalizeURL :: String -> String
normalizeURL url =
  let allParts = wordsBy (== '/') url
      (prefix, pathParts) = splitAt 3 allParts
      normalized = collapse [] pathParts
  in intercalate "/" (prefix ++ normalized)
  where
    collapse acc [] = reverse acc
    collapse acc (".":rest) = collapse acc rest
    collapse [] ("..":rest) = collapse [] rest
    collapse (_:acc) ("..":rest) = collapse acc rest
    collapse acc (x:rest) = collapse (x:acc) rest

-- | SHA-256 hash of a file as lowercase hex string
#ifdef WASM
hashFile :: FilePath -> IO String
hashFile _ = pure ""
#else
hashFile :: FilePath -> IO String
hashFile path = do
  contents <- BS.readFile path
  pure $ BS8.unpack $ B16.encode $ SHA256.hash contents
#endif

-- | SHA-256 hash of raw bytes as lowercase hex string
#ifdef WASM
hashBytes :: BS.ByteString -> String
hashBytes _ = ""
#else
hashBytes :: BS.ByteString -> String
hashBytes = BS8.unpack . B16.encode . SHA256.hash
#endif

-- | Fetch a URL, cache locally under ~/.cache/milang/<sha256-of-url>/<basename>
#ifdef WASM
fetchRemote :: String -> IO (Either String FilePath)
fetchRemote _ = pure $ Left "fetchRemote: unsupported in WASM build (imports disabled)"
#else
fetchRemote :: String -> IO (Either String FilePath)
fetchRemote url = do
  cacheDir <- getXdgDirectory XdgCache "milang"
  let urlHash = BS8.unpack $ B16.encode $ SHA256.hash $ BS8.pack url
      cacheEntry = cacheDir </> urlHash
      baseName = urlBaseName url
      cachedFile = cacheEntry </> baseName
  createDirectoryIfMissing True cacheEntry
  exists <- doesFileExist cachedFile
  if exists
    then pure $ Right cachedFile
    else do
      (ec, _, cerr) <- readProcessWithExitCode
        "curl" ["-fsSL", "-o", cachedFile, "--", url] ""
      case ec of
        ExitSuccess   -> pure $ Right cachedFile
        ExitFailure c -> pure $ Left $
          "Failed to fetch " ++ url ++ " (curl exit " ++ show c ++ ")\n" ++ cerr
#endif

urlBaseName :: String -> String
urlBaseName url =
  let parts = wordsBy (== '/') url
      name  = if null parts then "module.mi" else last parts
  in if null name then "module.mi" else name

wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy _ [] = []
wordsBy p s  = let (w, rest) = break p s
               in w : case rest of
                        []     -> []
                        (_:rs) -> wordsBy p rs
