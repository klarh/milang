{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import Data.List (nub)
import System.Exit (exitFailure, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout, hFlush, hSetBuffering, BufferMode(..), isEOF)
import System.Process (callProcess, readProcessWithExitCode)
import System.Directory (removeFile, getCurrentDirectory, doesFileExist)
import System.FilePath (replaceExtension)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Milang.Syntax (prettyExpr, Expr(..), Binding(..))
import Milang.Parser (parseProgram, parseExpr, parseBinding)
import Milang.Import (resolveImports, resolveAndPin, findURLImports, LinkInfo(..))
import Milang.Reduce (reduce, emptyEnv, warnings, Warning(..))
import Milang.Syntax (prettySrcPos)
import Milang.Codegen (codegen)
import Milang.Prelude (preludeBindings)
import Milang.TypeCheck (typeCheck, TypeError(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

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
    Left err  -> pure $ Left (show err)
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
      let reduced = reduce emptyEnv astWithPrelude
      let ws = warnings reduced
      mapM_ (printWarning file) ws
      -- Type check: report errors as warnings
      let typeErrs = case reduced of
            Namespace bs -> typeCheck bs
            _            -> []
      mapM_ (printTypeError file) typeErrs
      pure $ Right (reduced, li)

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
    Right (ast, li) -> do
      cwd <- getCurrentDirectory
      let cFile  = "/tmp/milang_out.c"
          binFile = "/tmp/milang_out"
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

-- | REPL: interactive read-eval-print loop
cmdRepl :: IO ()
cmdRepl = do
  hSetBuffering stdout LineBuffering
  putStrLn "milang repl — type expressions or bindings, :q to quit"
  replLoop preludeBindings

replLoop :: [Binding] -> IO ()
replLoop env = do
  input <- readInput "λ> "
  case input of
    Nothing -> putStrLn ""  -- EOF
    Just txt
      | txt == ":q" || txt == ":quit" -> pure ()
      | T.null txt -> replLoop env
      | otherwise -> do
          (env', printNames) <- replEval env txt
          case printNames of
            Nothing -> replLoop env
            Just names -> do
              replPrint env' names
              replLoop env'

-- | Read possibly multi-line input. Continue reading if line ends with \
--   or has unclosed delimiters, or ends with -> or ,
readInput :: String -> IO (Maybe T.Text)
readInput prompt = readLines prompt T.empty False

readLines :: String -> T.Text -> Bool -> IO (Maybe T.Text)
readLines prompt acc inBlock = do
  putStr prompt
  hFlush stdout
  eof <- isEOF
  if eof then pure (if T.null acc then Nothing else Just (T.strip acc))
  else do
    line <- getLine
    let txt = T.pack line
    -- Empty line ends a multi-line block
    if T.null (T.strip txt) && not (T.null acc)
      then pure (Just (T.strip acc))
      else if T.isSuffixOf "\\" txt
        then do
          let base = T.init txt
          let soFar = if T.null acc then base else acc <> "\n" <> base
          readLines ".. " soFar True
        else do
          let soFar = if T.null acc then txt else acc <> "\n" <> txt
          if isIncomplete soFar
            then readLines ".. " soFar True
            else if inBlock && isContinuationLine txt
              then readLines ".. " soFar True
              else pure (Just (T.strip soFar))

-- | Check if a line looks like a continuation (starts with | for guards, or is indented)
isContinuationLine :: T.Text -> Bool
isContinuationLine t = case T.uncons t of
  Just (' ', _) -> True
  Just ('|', _) -> True
  _ -> False

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
    Right (Namespace [b]) | bindName b /= "_stmt" && not ("_stmt_" `T.isPrefixOf` bindName b) -> do
      -- Single named binding
      let envClean = filter (\x -> bindName x /= bindName b) env
      let newEnv = envClean ++ [b]
      let ast = Namespace newEnv
      let reduced = reduce emptyEnv ast
      let ws = warnings reduced
      mapM_ (printWarning "<repl>") ws
      case reduced of
        Namespace bs -> pure (bs, Just [bindName b])
        _ -> pure (newEnv, Just [bindName b])
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
          hPutStrLn stderr "parse error"
          pure (env, Nothing)
  where
    isNamedBinding b = not ("_stmt" `T.isPrefixOf` bindName b)

-- | Print specific bindings by compiling and running
replPrint :: [Binding] -> [T.Text] -> IO ()
replPrint env names = do
  let ast = Namespace env
  let hidden = Set.fromList (map bindName env) `Set.difference` Set.fromList names
  let cFile = "/tmp/milang_repl.c"
      binFile = "/tmp/milang_repl"
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
          -- Strip "name = " prefix for single expressions
          case names of
            ["_it"] -> case stripPrefix "_it = " output of
                         Just rest -> putStr rest
                         Nothing   -> putStr output
            _ -> putStr output
  mapM_ safeRemove [cFile, binFile]
  where
    safeRemove f = do
      exists <- doesFileExist f
      if exists then removeFile f else pure ()
    stripPrefix prefix str =
      if prefix `isPrefixOf` str then Just (drop (length prefix) str)
      else Nothing
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
