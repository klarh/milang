{-# LANGUAGE OverloadedStrings #-}
module Milang.Remote
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
import qualified Crypto.Hash.SHA256 as SHA256
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Data.List (isPrefixOf, intercalate)

-- | Check if a path looks like a URL
isURL :: String -> Bool
isURL s = "https://" `isPrefixOf` s || "http://" `isPrefixOf` s

-- | Get the "directory" part of a URL (everything up to the last /)
-- e.g. "https://example.com/pkg/main.mi" -> "https://example.com/pkg"
urlDirName :: String -> String
urlDirName url =
  let parts = wordsBy (== '/') url
  in if length parts <= 3  -- "https:" "" "host"
     then url
     else intercalate "/" (init parts)

-- | Resolve a relative path against a URL base directory
-- e.g. resolveURL "https://example.com/pkg" "utils.mi" -> "https://example.com/pkg/utils.mi"
--      resolveURL "https://example.com/pkg" "../lib.mi" -> "https://example.com/lib.mi"
resolveURL :: String -> String -> String
resolveURL base rel
  | isURL rel = rel  -- already absolute
  | otherwise = normalizeURL (base ++ "/" ++ rel)

-- | Normalize a URL path (collapse ../ and ./)
normalizeURL :: String -> String
normalizeURL url =
  let allParts = wordsBy (== '/') url
      -- allParts for "http://host/path" = ["http:", "", "host", "path"]
      -- Keep scheme + "" + host as prefix, normalize the rest
      (prefix, pathParts) = splitAt 3 allParts
      normalized = collapse [] pathParts
  in intercalate "/" (prefix ++ normalized)
  where
    collapse acc [] = reverse acc
    collapse acc (".":rest) = collapse acc rest
    collapse [] ("..":rest) = collapse [] rest
    collapse (_:acc) ("..":rest) = collapse acc rest
    collapse acc (x:rest) = collapse (x:acc) rest

-- | Compute SHA-256 hash of a file, returned as lowercase hex string
hashFile :: FilePath -> IO String
hashFile path = do
  contents <- BS.readFile path
  pure $ BS8.unpack $ B16.encode $ SHA256.hash contents

-- | Compute SHA-256 hash of raw bytes, returned as lowercase hex string
hashBytes :: BS.ByteString -> String
hashBytes = BS8.unpack . B16.encode . SHA256.hash

-- | Fetch a remote URL, cache it locally, verify hash.
--
-- Returns the local file path to the cached content.
--
-- expectedHash is the Merkle hash (content + transitive imports).
-- For leaf files (no imports), this equals the content hash.
-- Verification of the Merkle hash happens at the import resolution level,
-- not here — this function only does content-level caching.
--
-- Cache layout: ~/.cache/milang/<sha256-of-url>/<basename>
fetchRemote :: String -> Maybe String -> IO (Either String FilePath)
fetchRemote url _expectedHash = do
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
      result <- curlFetch url cachedFile
      case result of
        Left err -> pure $ Left err
        Right () -> pure $ Right cachedFile

-- | Fetch a URL to a local file using curl
curlFetch :: String -> FilePath -> IO (Either String ())
curlFetch url dest = do
  (ec, _, cerr) <- readProcessWithExitCode
    "curl" ["-fsSL", "-o", dest, "--", url] ""
  case ec of
    ExitSuccess   -> pure $ Right ()
    ExitFailure c -> pure $ Left $
      "Failed to fetch " ++ url ++ " (curl exit " ++ show c ++ ")\n" ++ cerr

-- | Extract a reasonable filename from a URL
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
