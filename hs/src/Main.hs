{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Data.List (nub, isPrefixOf)
import System.Exit (exitFailure, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout, hFlush, hSetBuffering, BufferMode(..))
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (removeFile, getCurrentDirectory, doesFileExist, getXdgDirectory, XdgDirectory(..), createDirectoryIfMissing)
import System.FilePath (replaceExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Milang.Syntax (prettyExpr, Expr(..), Binding(..), SrcPos(..))
import Milang.Parser (parseProgram, parseExpr, parseBinding)
import Text.Megaparsec (errorBundlePretty)
import Milang.Import (resolveImports, resolveAndPin, findURLImports, LinkInfo(..))
import Milang.Reduce (reduce, emptyEnv, warnings, Warning(..))
import Milang.Syntax (prettySrcPos)
import Milang.Codegen (codegen)
import Milang.Prelude (preludeBindings)
import Milang.TypeCheck (typeCheck, TypeError(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

-- | Names defined by the standard prelude (hidden from script output)
preludeNames :: Set.Set T.Text
preludeNames = Set.fromList (map bindName preludeBindings)

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
    ["repl"]                 -> cmdRepl
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
  hPutStrLn stderr "  repl                    Interactive REPL"
  exitFailure

-- | Parse a milang source file (raw, no import resolution)
loadAndParseRaw :: FilePath -> IO (Either String Milang.Syntax.Expr)
loadAndParseRaw file = do
  src <- TIO.readFile file
  case parseProgram file src of
    Left err  -> pure $ Left (errorBundlePretty err)
    Right ast -> pure $ Right ast

-- | Parse + resolve imports
loadAndParse :: FilePath -> IO (Either String (Milang.Syntax.Expr, LinkInfo))
loadAndParse file = do
  result <- loadAndParseRaw file
  case result of
    Left err  -> pure $ Left err
    Right ast -> resolveImports file ast

-- | Parse + resolve imports + inject prelude + reduce
loadAndReduce :: FilePath -> IO (Either String (Milang.Syntax.Expr, LinkInfo))
loadAndReduce file = do
  result <- loadAndParse file
  case result of
    Left err  -> pure $ Left err
    Right (ast, li) -> do
      let astWithPrelude = injectPrelude ast
      -- Type check pre-reduction (catches call-site argument mismatches)
      let preTypeErrs = case astWithPrelude of
            Namespace bs -> typeCheck bs
            _            -> []
      let reduced = reduce emptyEnv astWithPrelude
      let ws = warnings reduced
      mapM_ (printWarning file) ws
      -- Type check post-reduction (catches residual type issues)
      let postTypeErrs = case reduced of
            Namespace bs -> typeCheck bs
            _            -> []
      -- Deduplicate: combine both passes, keep unique error messages
      let allErrs = dedup (filterPrelude preTypeErrs ++ filterPrelude postTypeErrs)
      mapM_ (printTypeError file) allErrs
      if not (null allErrs)
        then do
          hPutStrLn stderr $ show (length allErrs) ++ " type error(s)"
          exitFailure
        else pure $ Right (reduced, li)

-- | Filter out type errors originating from prelude definitions
filterPrelude :: [TypeError] -> [TypeError]
filterPrelude = filter (not . isPrelude)
  where
    isPrelude te = case tePos te of
      Just pos -> "<prelude>" `isPrefixOf` srcFile pos
      Nothing  -> False

-- | Deduplicate type errors by (name, message) pair
dedup :: [TypeError] -> [TypeError]
dedup = go Set.empty
  where
    go _ [] = []
    go seen (te:tes) =
      let key = (teName te, teMessage te)
      in if Set.member key seen
         then go seen tes
         else te : go (Set.insert key seen) tes

-- | Prepend prelude bindings to the user's top-level namespace
injectPrelude :: Expr -> Expr
injectPrelude (Namespace bs) = Namespace (preludeBindings ++ bs)
injectPrelude other = other

printWarning :: FilePath -> Warning -> IO ()
printWarning _file (Warning mpos msg) =
  let loc = case mpos of
              Just pos -> prettySrcPos pos
              Nothing  -> _file
  in hPutStrLn stderr $ "warning: " ++ loc ++ ": " ++ msg

printTypeError :: FilePath -> TypeError -> IO ()
printTypeError _file te =
  let loc = case tePos te of
              Just pos -> prettySrcPos pos
              Nothing  -> _file
  in hPutStrLn stderr $ "type error: " ++ loc ++ ": " ++ T.unpack (teName te) ++ ": " ++ T.unpack (teMessage te)

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
        then codegen stdout preludeNames ast
        else withFile outf WriteMode (\h -> codegen h preludeNames ast)
      hPutStrLn stderr $ "Wrote " ++ outf

-- | run: compile to C, invoke gcc, run the binary
cmdRun :: FilePath -> IO ()
cmdRun file = do
  result <- loadAndReduce file
  case result of
    Left err       -> hPutStrLn stderr err >> exitFailure
    Right (ast, li) -> withSystemTempDirectory "milang" $ \tmpDir -> do
      cwd <- getCurrentDirectory
      let cFile  = tmpDir </> "out.c"
          binFile = tmpDir </> "out"
          extraFlags = nub (linkFlags li)
          extraSrcs  = nub (linkSources li)
          gccArgs = ["-O2", "-o", binFile, cFile, "-I" ++ cwd]
                    ++ extraSrcs ++ extraFlags ++ ["-lm"]
      withFile cFile WriteMode (\h -> codegen h preludeNames ast)
      (ec, _, cerr) <- readProcessWithExitCode "gcc" gccArgs ""
      case ec of
        ExitFailure _ -> do
          hPutStrLn stderr "gcc compilation failed:"
          hPutStrLn stderr cerr
          exitFailure
        ExitSuccess ->
          callProcess binFile []

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

-- | REPL: interactive read-eval-print loop with haskeline
cmdRepl :: IO ()
cmdRepl = do
  hSetBuffering stdout LineBuffering
  dataDir <- getXdgDirectory XdgData "milang"
  createDirectoryIfMissing True dataDir
  let histFile = dataDir </> "history"
  let settings = defaultSettings { historyFile = Just histFile }
  putStrLn "milang repl — type expressions or bindings, :q to quit"
  putStrLn "  End a line with \\ to continue on next line. Ctrl-D to exit."
  runInputT settings (replLoop preludeBindings)

replLoop :: [Binding] -> InputT IO ()
replLoop env = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> outputStrLn ""  -- EOF / Ctrl-D
    Just line -> do
      let txt = T.strip (T.pack line)
      if txt == ":q" || txt == ":quit" then pure ()
      else if T.null txt then replLoop env
      else do
        -- Check if input needs continuation (unclosed delimiters, trailing \ etc.)
        full <- readContinuation (T.pack line)
        evalAndLoop env full

-- | Read continuation lines for auto-detected incomplete input
readContinuation :: T.Text -> InputT IO T.Text
readContinuation soFar
  | T.isSuffixOf "\\" soFar = do
      let base = T.init soFar
      more <- getInputLine ".. "
      case more of
        Nothing -> pure (T.strip base)
        Just line -> readContinuation (base <> "\n" <> T.pack line)
  | isIncomplete soFar = do
      more <- getInputLine ".. "
      case more of
        Nothing -> pure (T.strip soFar)
        Just line ->
          let full = soFar <> "\n" <> T.pack line
          in if T.null (T.strip (T.pack line))
             then pure (T.strip full)  -- blank line ends block
             else readContinuation full
  | otherwise = pure (T.strip soFar)

-- | Evaluate input and continue the loop
evalAndLoop :: [Binding] -> T.Text -> InputT IO ()
evalAndLoop env txt
  | T.null txt = replLoop env
  | otherwise = do
      (env', printNames) <- liftIO $ replEval env txt
      case printNames of
        Nothing -> replLoop env
        Just names -> do
          liftIO $ replPrint env' names
          replLoop env'

-- | Check if text has unclosed delimiters or ends with continuation tokens
isIncomplete :: T.Text -> Bool
isIncomplete t =
  let stripped = T.stripEnd t
  in hasUnclosedDelims t ||
     T.isSuffixOf "->" stripped ||
     T.isSuffixOf "," stripped

hasUnclosedDelims :: T.Text -> Bool
hasUnclosedDelims t = go 0 0 0 (T.unpack t)
  where
    go p b c [] = p > 0 || b > 0 || c > 0
    go p b c ('"':xs) = go p b c (skipStr xs)
    go p b c ('(':xs) = go (p+1) b c xs
    go p b c (')':xs) = go (max 0 (p-1)) b c xs
    go p b c ('[':xs) = go p (b+1) c xs
    go p b c (']':xs) = go p (max 0 (b-1)) c xs
    go p b c ('{':xs) = go p b (c+1) xs
    go p b c ('}':xs) = go p b (max 0 (c-1)) xs
    go p b c (_:xs)   = go p b c xs
    skipStr [] = []
    skipStr ('\\':_:xs) = skipStr xs
    skipStr ('"':xs) = xs
    skipStr (_:xs) = skipStr xs

-- | Try parsing input as binding(s), then as expression
replEval :: [Binding] -> T.Text -> IO ([Binding], Maybe [T.Text])
replEval env input =
  case parseProgram "<repl>" input of
    Right (Namespace [b]) | isTypeOnly b -> do
      -- Type-only binding (x :: Num): store it, don't print
      let envClean = filter (\x -> bindName x /= bindName b) env
      let newEnv = envClean ++ [b]
      putStrLn $ "  " ++ T.unpack (bindName b) ++ " :: <type>"
      pure (newEnv, Nothing)
    Right (Namespace [b]) | isNamedBinding b -> do
      -- Single named binding: merge with pending type annotation if present
      let merged = mergeWithPendingType env b
      let envClean = filter (\x -> bindName x /= bindName merged) env
      let newEnv = envClean ++ [merged]
      let ast = Namespace newEnv
      let reduced = reduce emptyEnv ast
      let ws = warnings reduced
      mapM_ (printWarning "<repl>") ws
      case reduced of
        Namespace bs -> pure (bs, Just [bindName merged])
        _ -> pure (newEnv, Just [bindName merged])
    Right (Namespace bs) | length bs > 1, all isNamedBinding bs -> do
      -- Multiple bindings
      let names = map bindName bs
      let envClean = filter (\x -> bindName x `notElem` names) env
      let newEnv = envClean ++ bs
      let ast = Namespace newEnv
      let reduced = reduce emptyEnv ast
      let ws = warnings reduced
      mapM_ (printWarning "<repl>") ws
      case reduced of
        Namespace bs' -> pure (bs', Just names)
        _ -> pure (newEnv, Just names)
    _ ->
      -- Try as expression
      case parseExpr "<repl>" input of
        Right expr -> do
          let b = Binding "_it" False [] expr Nothing Nothing Nothing
          let envNoIt = filter (\x -> bindName x /= "_it") env
          let newEnv = envNoIt ++ [b]
          let ast = Namespace newEnv
          let reduced = reduce emptyEnv ast
          let ws = warnings reduced
          mapM_ (printWarning "<repl>") ws
          case reduced of
            Namespace bs -> pure (bs, Just ["_it"])
            _ -> pure (newEnv, Just ["_it"])
        Left err -> do
          hPutStrLn stderr (errorBundlePretty err)
          pure (env, Nothing)
  where
    isNamedBinding b = not ("_stmt" `T.isPrefixOf` bindName b)
    isTypeOnly b = case (bindType b, bindBody b) of
      (Just _, IntLit 0) | null (bindParams b) -> True
      _                                        -> False
    -- Merge a value binding with a pending type-only binding in env
    mergeWithPendingType envBindings valBind =
      case filter (\x -> bindName x == bindName valBind && isTypeOnly x) envBindings of
        (typeBind:_) -> valBind { bindType = bindType typeBind }
        []           -> valBind

-- | Print specific bindings by compiling and running
replPrint :: [Binding] -> [T.Text] -> IO ()
replPrint env names = withSystemTempDirectory "milang-repl" $ \tmpDir -> do
  let ast = Namespace env
  let hidden = Set.fromList (map bindName env) `Set.difference` Set.fromList names
  let cFile = tmpDir </> "repl.c"
      binFile = tmpDir </> "repl"
  withFile cFile WriteMode (\h -> codegen h hidden ast)
  (ec, _out, cerr) <- readProcessWithExitCode "gcc" ["-O2", "-o", binFile, cFile, "-lm"] ""
  case ec of
    ExitFailure _ -> hPutStrLn stderr $ "compile error: " ++ cerr
    ExitSuccess -> do
      (ec2, out2, err2) <- readProcessWithExitCode binFile [] ""
      case ec2 of
        ExitFailure _ -> hPutStrLn stderr $ "runtime error: " ++ err2
        ExitSuccess -> do
          let output = out2
          case names of
            ["_it"] -> case stripPrefix "_it = " output of
                         Just rest -> putStr rest
                         Nothing   -> putStr output
            _ -> putStr output
  where
    stripPrefix prefix str =
      if prefix `isPrefixOf` str then Just (drop (length prefix) str)
      else Nothing
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
