{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile)
import System.FilePath (dropExtension)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set

import Core.Syntax
import Core.Parser (parseProgram)
import Core.Reduce (reduce, emptyEnv, Env, warnings, Warning(..))
import Core.Codegen (codegen)
import Core.Prelude (preludeBindings)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run" : file : runArgs)     -> cmdRun file runArgs
    ["compile", file]            -> cmdCompile file Nothing
    ["compile", file, out]       -> cmdCompile file (Just out)
    ["dump", file]               -> cmdDump file
    ["reduce", file]             -> cmdReduce file
    ["raw-reduce", file]         -> cmdRawReduce file
    _ -> do
      hPutStrLn stderr "Usage: milang-core <command> <file>"
      hPutStrLn stderr "Commands: run, compile, dump, reduce, raw-reduce"
      exitFailure

-- | Load and parse a file
loadAndParse :: String -> IO Expr
loadAndParse file = do
  src <- TIO.readFile file
  case parseProgram file src of
    Right expr -> pure expr
    Left err -> do
      hPutStrLn stderr (show err)
      exitFailure

-- | Inject prelude bindings before user bindings
injectPrelude :: Bool -> Expr -> Expr
injectPrelude True (Namespace bs) = Namespace (preludeBindings ++ bs)
injectPrelude True e = Namespace (preludeBindings ++ [mkBind "_main" e])
injectPrelude False e = e

-- | Load, parse, inject prelude, and reduce
loadAndReduce :: String -> IO (Expr, Env)
loadAndReduce file = do
  ast <- loadAndParse file
  let withPrelude = injectPrelude True ast
      env = emptyEnv
      reduced = reduce env withPrelude
      ws = warnings env
  mapM_ (printWarning file) ws
  pure (reduced, env)

printWarning :: String -> Warning -> IO ()
printWarning _file (TypeWarning pos name msg) =
  hPutStrLn stderr $ maybe "" (\p -> prettySrcPos p ++ ": ") pos
                   ++ "type warning: " ++ T.unpack name ++ ": " ++ T.unpack msg
printWarning _file (TraitWarning pos name msg) =
  hPutStrLn stderr $ maybe "" (\p -> prettySrcPos p ++ ": ") pos
                   ++ "trait warning: " ++ T.unpack name ++ ": " ++ T.unpack msg
printWarning _file (GeneralWarning pos msg) =
  hPutStrLn stderr $ maybe "" (\p -> prettySrcPos p ++ ": ") pos
                   ++ "warning: " ++ T.unpack msg

-- | Set of prelude names to hide in script mode output
preludeNames :: Set.Set T.Text
preludeNames = Set.fromList names
  where
    names = concatMap expandName preludeBindings
    expandName b = bindName b : case bindBody b of
      Namespace ctors -> map bindName ctors
      _ -> []

-- | raw-reduce: reduce without prelude
cmdRawReduce :: String -> IO ()
cmdRawReduce file = do
  ast <- loadAndParse file
  let reduced = reduce emptyEnv ast
  putStrLn (prettyExpr 0 reduced)

-- | dump: show parsed AST
cmdDump :: String -> IO ()
cmdDump file = do
  ast <- loadAndParse file
  putStrLn (prettyExpr 0 ast)

-- | reduce: show AST after partial evaluation
cmdReduce :: String -> IO ()
cmdReduce file = do
  (reduced, _) <- loadAndReduce file
  putStrLn (prettyExpr 0 reduced)

-- | compile: emit C
cmdCompile :: String -> Maybe String -> IO ()
cmdCompile file mOut = do
  (reduced, _) <- loadAndReduce file
  let outFile = case mOut of
        Just "-" -> Nothing  -- stdout
        Just f   -> Just f
        Nothing  -> Just (dropExtension file ++ ".c")
  case outFile of
    Nothing -> codegen stdout preludeNames reduced
    Just f  -> withFile f WriteMode $ \h -> codegen h preludeNames reduced

-- | run: compile to C, invoke gcc, execute
cmdRun :: String -> [String] -> IO ()
cmdRun file runArgs = do
  (reduced, _) <- loadAndReduce file
  let cFile = dropExtension file ++ "_core.c"
      binFile = dropExtension file ++ "_core"
  withFile cFile WriteMode $ \h -> codegen h preludeNames reduced
  (gccExit, _, gccErr) <- readProcessWithExitCode "gcc"
    ["-O2", "-o", binFile, cFile] ""
  case gccExit of
    ExitSuccess -> pure ()
    ExitFailure _ -> do
      hPutStrLn stderr $ "gcc failed:\n" ++ gccErr
      removeFile cFile
      exitFailure
  (runExit, runOut, runErr) <- readProcessWithExitCode binFile runArgs ""
  removeFile cFile
  removeFile binFile
  putStr runOut
  if not (null runErr) then hPutStrLn stderr runErr else pure ()
  exitWith runExit
