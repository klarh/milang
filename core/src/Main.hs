{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, doesFileExist)
import System.FilePath (dropExtension, takeDirectory, (</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IORef

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
  resolved <- resolveImports file ast
  let withPrelude = injectPrelude True resolved
      env = emptyEnv
      reduced = reduce env withPrelude
      ws = warnings env
  mapM_ (printWarning file) ws
  pure (reduced, env)

-- | Resolve all imports in an AST, replacing Import nodes with parsed content
resolveImports :: String -> Expr -> IO Expr
resolveImports file expr = do
  cache <- newIORef (Map.empty :: Map.Map FilePath Expr)
  resolveExpr cache (takeDirectory file) expr

resolveExpr :: IORef (Map.Map FilePath Expr) -> FilePath -> Expr -> IO Expr
resolveExpr cache dir (Import path) = do
  let relPath = dir </> T.unpack path
  cached <- readIORef cache
  case Map.lookup relPath cached of
    Just e  -> pure e
    Nothing -> do
      exists <- doesFileExist relPath
      if not exists
        then pure $ Error ("import not found: " <> path)
        else do
          src <- TIO.readFile relPath
          case parseProgram relPath src of
            Left err -> pure $ Error (T.pack (show err))
            Right ast -> do
              -- Mark as in-progress for circular import detection
              modifyIORef cache (Map.insert relPath (Error "circular import"))
              resolved <- resolveExpr cache (takeDirectory relPath) ast
              modifyIORef cache (Map.insert relPath resolved)
              pure resolved
resolveExpr cache dir (Namespace bs) =
  Namespace <$> mapM (resolveBinding cache dir) bs
resolveExpr cache dir (App f x) =
  App <$> resolveExpr cache dir f <*> resolveExpr cache dir x
resolveExpr cache dir (BinOp op l r) =
  BinOp op <$> resolveExpr cache dir l <*> resolveExpr cache dir r
resolveExpr cache dir (Lam p b) =
  Lam p <$> resolveExpr cache dir b
resolveExpr cache dir (Record t bs) =
  Record t <$> mapM (resolveBinding cache dir) bs
resolveExpr cache dir (FieldAccess e f) =
  (\e' -> FieldAccess e' f) <$> resolveExpr cache dir e
resolveExpr cache dir (Case s alts) =
  Case <$> resolveExpr cache dir s <*> mapM (resolveAlt cache dir) alts
resolveExpr cache dir (Thunk e) = Thunk <$> resolveExpr cache dir e
resolveExpr cache dir (ListLit es) = ListLit <$> mapM (resolveExpr cache dir) es
resolveExpr cache dir (With e bs) =
  With <$> resolveExpr cache dir e <*> mapM (resolveBinding cache dir) bs
resolveExpr _ _ e = pure e  -- literals, names, errors

resolveBinding :: IORef (Map.Map FilePath Expr) -> FilePath -> Binding -> IO Binding
resolveBinding cache dir b = do
  body' <- resolveExpr cache dir (bindBody b)
  pure b { bindBody = body' }

resolveAlt :: IORef (Map.Map FilePath Expr) -> FilePath -> Alt -> IO Alt
resolveAlt cache dir (Alt p g body) = do
  body' <- resolveExpr cache dir body
  g' <- case g of
    Just ge -> Just <$> resolveExpr cache dir ge
    Nothing -> pure Nothing
  pure $ Alt p g' body'

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
