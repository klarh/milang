{-# LANGUAGE OverloadedStrings #-}
module Milang.Remote
  ( fetchRemote
  , hashFile
  , isURL
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16
import qualified Crypto.Hash.SHA256 as SHA256
import System.Directory (createDirectoryIfMissing, doesFileExist, getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)
import Data.List (isPrefixOf)

-- | Check if a path looks like a URL
isURL :: String -> Bool
isURL s = "https://" `isPrefixOf` s || "http://" `isPrefixOf` s

-- | Compute SHA-256 hash of a file, returned as lowercase hex string
hashFile :: FilePath -> IO String
hashFile path = do
  contents <- BS.readFile path
  pure $ BS8.unpack $ B16.encode $ SHA256.hash contents

-- | Fetch a remote URL, cache it locally, verify hash.
--
-- Returns the local file path to the cached content.
--
-- If expectedHash is provided, verifies the cached/fetched content matches.
-- If no hash is provided, warns and prints the hash for pinning.
--
-- Cache layout: ~/.cache/milang/<sha256-of-url>/<basename>
fetchRemote :: String -> Maybe String -> IO (Either String FilePath)
fetchRemote url expectedHash = do
  cacheDir <- getXdgDirectory XdgCache "milang"
  -- Use hash of URL as directory name to avoid filesystem issues
  let urlHash = BS8.unpack $ B16.encode $ SHA256.hash $ BS8.pack url
      cacheEntry = cacheDir </> urlHash
      -- Preserve the filename from the URL for readability
      baseName = urlBaseName url
      cachedFile = cacheEntry </> baseName
  createDirectoryIfMissing True cacheEntry

  exists <- doesFileExist cachedFile
  if exists
    then verifyCached cachedFile url expectedHash
    else do
      result <- curlFetch url cachedFile
      case result of
        Left err -> pure $ Left err
        Right () -> verifyCached cachedFile url expectedHash

-- | Verify a cached file's hash
verifyCached :: FilePath -> String -> Maybe String -> IO (Either String FilePath)
verifyCached path url expectedHash = do
  actualHash <- hashFile path
  case expectedHash of
    Nothing -> do
      hPutStrLn stderr $ "WARNING: no sha256 for import \"" ++ url ++ "\""
      hPutStrLn stderr $ "  sha256 = \"" ++ actualHash ++ "\""
      pure $ Right path
    Just expected
      | expected == actualHash -> pure $ Right path
      | otherwise -> pure $ Left $
          "Hash mismatch for " ++ url ++ "\n" ++
          "  expected: " ++ expected ++ "\n" ++
          "  actual:   " ++ actualHash

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
