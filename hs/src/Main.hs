{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Data.List (nub)
import System.Exit (exitFailure, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout)
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (removeFile, getCurrentDirectory)
import System.FilePath (replaceExtension)
import qualified Data.Text.IO as TIO

import Milang.Syntax (prettyExpr, Expr)
import Milang.Parser (parseProgram)
import Milang.Import (resolveImports, LinkInfo(..))
import Milang.Reduce (reduce, emptyEnv)
import Milang.Codegen (codegen)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["run", file]            -> cmdRun file
    ["compile", file]        -> cmdCompile file Nothing
    ["compile", file, outf]  -> cmdCompile file (Just outf)
    ["dump", file]           -> cmdDump file
    ["reduce", file]         -> cmdReduce file
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
    Right (ast, li) -> pure $ Right (reduce emptyEnv ast, li)

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
                    ++ extraSrcs ++ extraFlags
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
