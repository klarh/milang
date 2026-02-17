{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Data.List (nub)
import System.Exit (exitFailure, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout)
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath (replaceExtension)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Milang.Syntax (prettyExpr, Expr)
import Milang.Parser (parseProgram)
import Milang.Import (resolveImports, resolveAndPin, findURLImports, LinkInfo(..))
import Milang.Reduce (reduce, emptyEnv, warnings, Warning(..))
import Milang.Syntax (prettySrcPos)
import Milang.Codegen (codegen)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", file]            -> cmdRun file
    ["compile", file]        -> cmdCompile file Nothing
    ["compile", file, outf]  -> cmdCompile file (Just outf)
    ["dump", file]           -> cmdDump file
    ["reduce", file]         -> cmdReduce file
    ["pin", file]            -> cmdPin file
    _                        -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr "Usage: milang <command> <file.mi>"
  hPutStrLn stderr ""
  hPutStrLn stderr "Commands:"
  hPutStrLn stderr "  run <file>              Parse, reduce, compile to C, run"
  hPutStrLn stderr "  compile <file> [out.c]  Emit generated C"
  hPutStrLn stderr "  dump <file>             Show parsed AST"
  hPutStrLn stderr "  reduce <file>           Show AST after partial evaluation"
  hPutStrLn stderr "  pin <file>              Fetch URL imports and print/update sha256 hashes"
  exitFailure

-- | Parse a milang source file (raw, no import resolution)
loadAndParseRaw :: FilePath -> IO (Either String Milang.Syntax.Expr)
loadAndParseRaw file = do
  src <- TIO.readFile file
  case parseProgram file src of
    Left err  -> pure $ Left (show err)
    Right ast -> pure $ Right ast

-- | Parse + resolve imports
loadAndParse :: FilePath -> IO (Either String (Milang.Syntax.Expr, LinkInfo))
loadAndParse file = do
  result <- loadAndParseRaw file
  case result of
    Left err  -> pure $ Left err
    Right ast -> resolveImports file ast

-- | Parse + resolve imports + reduce
loadAndReduce :: FilePath -> IO (Either String (Milang.Syntax.Expr, LinkInfo))
loadAndReduce file = do
  result <- loadAndParse file
  case result of
    Left err  -> pure $ Left err
    Right (ast, li) -> do
      let reduced = reduce emptyEnv ast
      let ws = warnings reduced
      mapM_ (printWarning file) ws
      pure $ Right (reduced, li)

printWarning :: FilePath -> Warning -> IO ()
printWarning _file (Warning mpos msg) =
  let loc = case mpos of
              Just pos -> prettySrcPos pos
              Nothing  -> _file
  in hPutStrLn stderr $ "warning: " ++ loc ++ ": " ++ msg

-- | dump: show parsed AST (before import resolution)
cmdDump :: FilePath -> IO ()
cmdDump file = do
  result <- loadAndParseRaw file
  case result of
    Left err  -> hPutStrLn stderr err >> exitFailure
    Right ast -> putStrLn (prettyExpr 0 ast)

-- | reduce: show AST after partial evaluation
cmdReduce :: FilePath -> IO ()
cmdReduce file = do
  result <- loadAndReduce file
  case result of
    Left err      -> hPutStrLn stderr err >> exitFailure
    Right (ast,_) -> putStrLn (prettyExpr 0 ast)

-- | compile: emit C source
cmdCompile :: FilePath -> Maybe FilePath -> IO ()
cmdCompile file mout = do
  result <- loadAndReduce file
  case result of
    Left err      -> hPutStrLn stderr err >> exitFailure
    Right (ast,_) -> do
      let outf = maybe (replaceExtension file ".c") id mout
      if outf == "-"
        then codegen stdout ast
        else withFile outf WriteMode (\h -> codegen h ast)
      hPutStrLn stderr $ "Wrote " ++ outf

-- | run: compile to C, invoke gcc, run the binary
cmdRun :: FilePath -> IO ()
cmdRun file = do
  result <- loadAndReduce file
  case result of
    Left err       -> hPutStrLn stderr err >> exitFailure
    Right (ast, li) -> do
      cwd <- getCurrentDirectory
      let cFile  = "/tmp/milang_out.c"
          binFile = "/tmp/milang_out"
          extraFlags = nub (linkFlags li)
          extraSrcs  = nub (linkSources li)
          gccArgs = ["-O2", "-o", binFile, cFile, "-I" ++ cwd]
                    ++ extraSrcs ++ extraFlags ++ ["-lm"]
      withFile cFile WriteMode (\h -> codegen h ast)
      (ec, _, cerr) <- readProcessWithExitCode "gcc" gccArgs ""
      case ec of
        ExitFailure _ -> do
          hPutStrLn stderr "gcc compilation failed:"
          hPutStrLn stderr cerr
          exitFailure
        ExitSuccess -> do
          callProcess binFile []
          removeFile cFile
          removeFile binFile

-- | pin: fetch URL imports, print sha256 hashes, rewrite file with hashes
cmdPin :: FilePath -> IO ()
cmdPin file = do
  result <- loadAndParseRaw file
  case result of
    Left err -> hPutStrLn stderr err >> exitFailure
    Right ast -> do
      let imports = findURLImports ast
          unpinned = filter (isNothing . snd) imports
          allUrls  = map fst imports
      if null allUrls
        then putStrLn "No URL imports found."
        else do
          -- Resolve imports to compute Merkle hashes
          resolved <- resolveAndPin file ast
          case resolved of
            Left err -> hPutStrLn stderr err >> exitFailure
            Right (_, _, merkleMap) -> do
              let successes = [(url, h) | url <- allUrls
                                        , Just h <- [Map.lookup url merkleMap]]
              -- Read file content for rewriting
              src <- TIO.readFile file
              let src' = foldl pinOne src successes
              if src' == src
                then putStrLn "All URL imports already pinned."
                else do
                  TIO.writeFile file src'
                  putStrLn $ "Pinned " ++ show (length unpinned) ++ " import(s) in " ++ file
              -- Print summary
              mapM_ (\(url, h) -> putStrLn $ "  " ++ url ++ "\n    sha256 = \"" ++ h ++ "\"") successes
  where
    isNothing Nothing = True
    isNothing _       = False

    -- Insert sha256 into an import that doesn't have one
    pinOne :: T.Text -> (String, String) -> T.Text
    pinOne src (url, hash) =
      let hashLine = T.pack $ "sha256 = \"" ++ hash ++ "\""
          bare = T.pack $ "import \"" ++ url ++ "\""
          withHash = T.pack $ "import \"" ++ url ++ "\" { " ++ T.unpack hashLine ++ " }"
      in if bare `T.isInfixOf` src && not (hasOpts src (T.pack url))
           then T.replace bare withHash src
           else src

    -- Check if import "url" already has { ... } options
    hasOpts :: T.Text -> T.Text -> Bool
    hasOpts src url =
      let importStr = T.pack "import \"" <> url <> T.pack "\""
          after = snd $ T.breakOn importStr src
          rest  = T.drop (T.length importStr) after
          trimmed = T.dropWhile (== ' ') rest
      in not (T.null trimmed) && T.head trimmed == '{'
