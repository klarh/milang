{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout, hFlush, hSetBuffering, BufferMode(..))
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, doesFileExist, getCurrentDirectory, findExecutable)
import System.Info (os, arch)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeBaseName, (</>), normalise)
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.List (nub, isPrefixOf, sort)
import Control.Exception (catch, IOException)
import Data.Aeson.Encode.Pretty (encodePretty)
import Options.Applicative

import Core.Syntax
import Core.Parser (parseProgram, parseExpr)
import Text.Megaparsec (errorBundlePretty)
import Core.Reduce (reduce, reduceWithEnv, emptyEnv, builtinEnv, Env, Warning(..), envMap, nativeBuiltinNames)
import Core.Codegen (codegen)
import Core.Optimize (optimize)
import Core.NativeCodegen (nativeCodegen)
import Core.Prelude (preludeBindings)
import Core.CHeader (parseCHeader, parseCHeaderInclude, CFunSig(..), CEnumConst(..))
import Core.Remote (fetchRemote, hashFile, hashBytes, isURL, urlDirName, resolveURL)
import Core.Version (version)
import Core.IR (exprToJSON)

-- | Link info accumulated during import resolution
data LinkInfo = LinkInfo
  { linkFlags    :: [String]    -- extra gcc flags (-lm, -lpthread, etc.)
  , linkSources  :: [FilePath]  -- extra .c files to compile
  , linkIncludes :: [FilePath]  -- extra -I include directories
  , linkCCFlags  :: [String]    -- extra flags for gcc only (not preprocessor)
  }

emptyLinkInfo :: LinkInfo
emptyLinkInfo = LinkInfo [] [] [] []

mergeLinkInfo :: LinkInfo -> LinkInfo -> LinkInfo
mergeLinkInfo a b = LinkInfo
  (linkFlags a ++ linkFlags b)
  (nub $ map normalise (linkSources a ++ linkSources b))
  (nub $ map normalise (linkIncludes a ++ linkIncludes b))
  (linkCCFlags a ++ linkCCFlags b)

-- | Resolution context for import resolution
data ResCtx = ResCtx
  { rcCache      :: IORef (Map.Map String Expr)     -- resolved imports by path/URL
  , rcInProgress :: IORef (Set.Set String)           -- circular import detection
  , rcCircRefs   :: IORef (Set.Set String)           -- detected circular refs
  , rcLinkRef    :: IORef LinkInfo                   -- accumulated link info
  , rcMerkle     :: IORef (Map.Map String String)    -- URL -> Merkle hash
  , rcHashErrs   :: IORef [(String, String, String)] -- (path, expected, actual)
  , rcNoCache    :: Bool                             -- disable disk cache for remote imports
  , rcCC         :: String                           -- C compiler for header parsing and compilation
  }

-- ── CLI types ─────────────────────────────────────────────────────

data Command
  = CmdRun RunOpts
  | CmdCompile CompileOpts
  | CmdReduce ReduceOpts
  | CmdPin PinOpts
  | CmdRepl

data GlobalOpts = GlobalOpts
  { optNoCache :: Bool
  , optCC      :: Maybe String
  , optCommand :: Command
  }

data RunOpts = RunOpts
  { runFile    :: FilePath
  , runKeepC   :: Bool
  , runInterpret :: Bool
  , runArgs    :: [String]
  }

data CompileOpts = CompileOpts
  { compFile    :: FilePath
  , compOut     :: Maybe FilePath
  , compInterpret :: Bool
  }

data ReduceOpts = ReduceOpts
  { reduceFile      :: FilePath
  , reduceJSON      :: Bool
  , reduceNoPrelude :: Bool
  , reduceNoReduce  :: Bool
  , reduceOut       :: Maybe FilePath
  }

data PinOpts = PinOpts
  { pinFile :: FilePath }

-- ── CLI parsers ──────────────────────────────────────────────────

cliParser :: ParserInfo GlobalOpts
cliParser = info (globalParser <**> helper <**> versionOpt)
  (  fullDesc
  <> header "milang — the milang compiler"
  <> progDesc "A minimalist functional programming language with zero keywords"
  )

versionOpt :: Parser (a -> a)
versionOpt = infoOption ("milang " ++ version)
  (  long "version"
  <> short 'v'
  <> help "Show version"
  )

globalParser :: Parser GlobalOpts
globalParser = GlobalOpts
  <$> switch (long "no-cache" <> help "Disable disk cache for remote URL imports")
  <*> optional (strOption (long "cc" <> metavar "COMPILER"
        <> help "C compiler for header parsing and compilation"))
  <*> commandParser

commandParser :: Parser Command
commandParser = hsubparser
  (  command "run" (info runParser
       (progDesc "Compile and run a .mi file (native codegen; use --interpret for interpreter)"))
  <> command "compile" (info compileParser
       (progDesc "Compile a .mi file to C or a binary (native codegen; use --interpret for interpreter)"))
  <> command "reduce" (info reduceParser
       (progDesc "Show AST (parsed, reduced, or as JSON IR)"))
  <> command "dump" (info dumpParser
       (progDesc "Show parsed AST (alias for reduce --no-reduce)"))
  <> command "raw-reduce" (info rawReduceParser
       (progDesc "Reduce without prelude (alias for reduce --no-prelude)"))
  <> command "pin" (info pinParser
       (progDesc "Add sha256 hashes to URL imports"))
  <> command "repl" (info (pure CmdRepl <**> helper)
       (progDesc "Interactive REPL"))
  )

runParser :: Parser Command
runParser = fmap CmdRun $ RunOpts
  <$> argument str (metavar "FILE" <> help "Milang source file")
  <*> switch (long "keep-c" <> help "Keep generated C file after execution")
  <*> switch (long "interpret" <> help "Use interpreter-based codegen instead of native")
  <*> many (argument str (metavar "ARGS..." <> help "Arguments passed to the compiled program"))

compileParser :: Parser Command
compileParser = fmap CmdCompile $ CompileOpts
  <$> argument str (metavar "FILE" <> help "Milang source file")
  <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE"
        <> help "Output file (.c for C source, other for binary; default: <input>.c)"))
  <*> switch (long "interpret" <> help "Use interpreter-based codegen instead of native")

reduceParser :: Parser Command
reduceParser = fmap CmdReduce $ ReduceOpts
  <$> argument str (metavar "FILE" <> help "Milang source file")
  <*> switch (long "json" <> help "Output structured JSON IR")
  <*> switch (long "no-prelude" <> help "Skip prelude injection")
  <*> switch (long "no-reduce" <> help "Show parsed AST without reduction")
  <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE"
        <> help "Write output to file (default: stdout)"))

-- Hidden aliases for backward compatibility
dumpParser :: Parser Command
dumpParser = fmap CmdReduce $ ReduceOpts
  <$> argument str (metavar "FILE" <> help "Milang source file")
  <*> switch (long "json" <> help "Output structured JSON IR")
  <*> pure False
  <*> pure True  -- --no-reduce is always on for dump
  <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE"
        <> help "Write output to file (default: stdout)"))

rawReduceParser :: Parser Command
rawReduceParser = fmap CmdReduce $ ReduceOpts
  <$> argument str (metavar "FILE" <> help "Milang source file")
  <*> switch (long "json" <> help "Output structured JSON IR")
  <*> pure True  -- --no-prelude is always on for raw-reduce
  <*> pure False
  <*> optional (strOption (short 'o' <> long "output" <> metavar "FILE"
        <> help "Write output to file (default: stdout)"))

pinParser :: Parser Command
pinParser = fmap CmdPin $ PinOpts
  <$> argument str (metavar "FILE" <> help "Milang source file")

-- | Resolve the C compiler. If explicitly specified via --cc, error if not
-- found. Otherwise auto-detect from a prioritized list.
resolveCC :: Maybe String -> IO String
resolveCC (Just specified) = do
  found <- findExecutable specified
  case found of
    Just _  -> return specified
    Nothing -> do
      hPutStrLn stderr $ "error: C compiler '" ++ specified ++ "' not found on PATH"
      exitFailure
resolveCC Nothing = tryFallbacks ["gcc", "clang", "cc"]
  where
    tryFallbacks [] = do
      hPutStrLn stderr "error: no C compiler found on PATH (tried gcc, clang, cc)"
      exitFailure
    tryFallbacks (c:cs) = do
      f <- findExecutable c
      case f of
        Just _  -> return c
        Nothing -> tryFallbacks cs

main :: IO ()
main = do
  gopts <- execParser cliParser
  let nc = optNoCache gopts
  cc <- resolveCC (optCC gopts)
  case optCommand gopts of
    CmdRun opts
      | runInterpret opts -> cmdRun cc nc opts
      | otherwise         -> cmdRunNative cc nc opts
    CmdCompile opts
      | compInterpret opts -> cmdCompile cc nc opts
      | otherwise          -> cmdCompileNative cc nc opts
    CmdReduce opts  -> cmdReduce cc nc opts
    CmdPin opts     -> cmdPin cc nc opts
    CmdRepl         -> cmdRepl

-- | Load and parse a file
loadAndParse :: String -> IO Expr
loadAndParse file = do
  exists <- doesFileExist file
  unless exists $ do
    hPutStrLn stderr $ "error: file not found: " ++ file
    exitFailure
  src <- TIO.readFile file
  case parseProgram file src of
    Right expr -> pure expr
    Left err -> do
      hPutStrLn stderr $ errorBundlePretty err
      exitFailure

-- | Inject prelude bindings before user bindings
injectPrelude :: Bool -> Expr -> Expr
injectPrelude True (Namespace bs) =
  Namespace (preludeBindings ++ [buildBinding] ++ bs)
injectPrelude True e =
  Namespace (preludeBindings ++ [buildBinding] ++ [mkBind "_main" e])
injectPrelude False e = e

-- | Compile-time build info record (target, os, arch)
buildBinding :: Binding
buildBinding = mkBind "build" $ Record ""
  [ mkBind "target" (StringLit "c")
  , mkBind "os"     (StringLit (T.pack os))
  , mkBind "arch"   (StringLit (T.pack arch))
  ]

-- | Special import "prelude": all prelude bindings + C builtin wrappers
specialPrelude :: Expr
specialPrelude =
  let builtinBs = [mkBind n (Builtin n)
                    | n <- Set.toList nativeBuiltinNames
                    , n /= "if", n /= "truthy"
                    , n /= "__sized_int", n /= "__sized_uint" ]
  in Namespace (preludeBindings ++ builtinBs)

-- | Special import "ast": metaprogramming AST constructors
specialAst :: Expr
specialAst = Namespace
  [ mkBind "App" $ Lam "fn" (Lam "arg" (Record "App"
      [ mkBind "fn" (Name "fn"), mkBind "arg" (Name "arg") ]))
  , mkBind "Var" $ Lam "name" (Record "Var"
      [ mkBind "name" (Name "name") ])
  , mkBind "Lam" $ Lam "param" (Lam "body" (Record "Lam"
      [ mkBind "param" (Name "param"), mkBind "body" (Name "body") ]))
  ]

-- | Load, parse, inject prelude, and reduce
loadAndReduce :: String -> Bool -> String -> IO (Expr, LinkInfo)
loadAndReduce cc noCache file = do
  ast <- loadAndParse file
  (resolved, li) <- resolveImports cc noCache file ast
  let withPrelude = injectPrelude True resolved
      (reduced, ws) = reduce builtinEnv withPrelude
      -- Filter out prelude-origin errors
      userWarns = filter (not . isPreludeWarning) ws
      -- Dedup and format errors
      fmtWarn (TypeWarning pos name msg) =
        (pos, T.unpack name <> ": " <> T.unpack msg)
      fmtWarn (TraitWarning pos name msg) =
        (pos, T.unpack name <> ": " <> T.unpack msg)
      fmtWarn (GeneralWarning pos msg) = (pos, T.unpack msg)
      msgs = dedupByLine (map fmtWarn userWarns)
  mapM_ (\(pos, msg) -> hPutStrLn stderr $ "error: " ++ fmtLoc file pos ++ ": " ++ msg) msgs
  if not (null msgs)
    then do
      hPutStrLn stderr $ show (length msgs) ++ " error(s)"
      exitFailure
    else pure (reduced, li)

-- | Check if a warning originates from the prelude
isPreludeWarning :: Warning -> Bool
isPreludeWarning (TypeWarning pos _ _) = isPosPrelude pos
isPreludeWarning (TraitWarning pos _ _) = isPosPrelude pos
isPreludeWarning (GeneralWarning pos _) = isPosPrelude pos

isPosPrelude :: Maybe SrcPos -> Bool
isPosPrelude (Just pos) = "<prelude>" `isPrefixOf` srcFile pos
isPosPrelude Nothing    = False

-- | Format a source location for error messages
fmtLoc :: FilePath -> Maybe SrcPos -> String
fmtLoc file Nothing    = file
fmtLoc _    (Just pos) = prettySrcPos pos

-- | Deduplicate errors: merge same-line errors into one message
dedupByLine :: [(Maybe SrcPos, String)] -> [(Maybe SrcPos, String)]
dedupByLine msgs =
  let lineKey (Just p)  = Just (srcFile p, srcLine p)
      lineKey Nothing   = Nothing
      -- Group by source line, dedup messages within each group
      grouped = Map.fromListWith (\new old -> old ++ "; " ++ new)
                  [(lineKey pos, msg) | (pos, msg) <- msgs]
      -- Reconstruct with original positions
      posMap = Map.fromList [(lineKey pos, pos) | (pos, _) <- msgs]
  in [(Map.findWithDefault Nothing k posMap, msg) | (k, msg) <- Map.toList grouped]

-- | Strip deeply nested Namespace content from circular imports for codegen.
-- Only strips Namespace nesting that contains circular module references
-- (detected by __mod_ Name prefixes). Non-circular module hierarchies are
-- left intact.
stripDeepModules :: Int -> Expr -> Expr
stripDeepModules maxD = go False 0
  where
    go _ d (Namespace bs)
      | d >= maxD && circular = StringLit "<closure>"
      | otherwise = Namespace (map (goB circular' (d + 1)) bs)
      where circular = hasCircularRef (Namespace bs)
            circular' = circular
    go c d (Record t bs)  = Record t (map (goB c d) bs)
    go c d (App f x)      = App (go c d f) (go c d x)
    go c d (BinOp op l r) = BinOp op (go c d l) (go c d r)
    go c d (Lam p b)      = Lam p (go c d b)
    go c d (FieldAccess e f) = FieldAccess (go c d e) f
    go c d (Case s alts)  = Case (go c d s)
      (map (\(Alt p g body) -> Alt p (fmap (go c d) g) (go c d body)) alts)
    go c d (With e bs)    = With (go c d e) (map (goB c d) bs)
    go _ _ e              = e
    -- If in a circular context at boundary depth, strip non-simple bodies
    goB circ d b =
      let body' = go circ d (bindBody b)
          origBody = bindBody b
      in if circ && d >= 2 && isCircularBackRef origBody
         then b { bindBody = StringLit "<closure>" }
         else if d >= maxD && circ && not (isSimpleVal body')
         then b { bindBody = StringLit "<closure>" }
         else b { bindBody = body' }
    -- A circular back-ref has __mod_ Names within 2 levels of nesting
    isCircularBackRef e = case e of
      Namespace _ -> minModDepth 0 e <= 2
      _           -> False
    minModDepth d (Name n)
      | "__mod_" `T.isPrefixOf` n && "__" `T.isSuffixOf` n = d
    minModDepth d (Namespace bs) = minimum ((maxBound :: Int) : map (minModDepth (d+1) . bindBody) bs)
    minModDepth d (App f x)      = min (minModDepth d f) (minModDepth d x)
    minModDepth d (BinOp _ l r)  = min (minModDepth d l) (minModDepth d r)
    minModDepth _ _              = maxBound
    isSimpleVal (IntLit _)    = True
    isSimpleVal (FloatLit _)  = True
    isSimpleVal (StringLit _) = True
    isSimpleVal (Record _ bs) = all (isSimpleVal . bindBody) bs
    isSimpleVal _             = False

-- | Check if an expression tree contains circular module references (__mod_ names)
hasCircularRef :: Expr -> Bool
hasCircularRef (Name n)       = "__mod_" `T.isPrefixOf` n && "__" `T.isSuffixOf` n
hasCircularRef (Namespace bs) = any (hasCircularRef . bindBody) bs
hasCircularRef (Record _ bs)  = any (hasCircularRef . bindBody) bs
hasCircularRef (App f x)      = hasCircularRef f || hasCircularRef x
hasCircularRef (BinOp _ l r)  = hasCircularRef l || hasCircularRef r
hasCircularRef (Lam _ b)      = hasCircularRef b
hasCircularRef (FieldAccess e _) = hasCircularRef e
hasCircularRef (Case s alts)  = hasCircularRef s || any (\(Alt _ g b) -> hasCircularRef b || maybe False hasCircularRef g) alts
hasCircularRef (With e bs)    = hasCircularRef e || any (hasCircularRef . bindBody) bs
hasCircularRef _              = False

-- | Resolve all imports in an AST, replacing Import nodes with parsed content.
-- Circular imports are replaced with Name references to top-level module bindings.
resolveImports :: String -> Bool -> String -> Expr -> IO (Expr, LinkInfo)
resolveImports cc noCache file expr = do
  result <- resolveAndPin cc noCache file expr
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right (ast, li, _) -> pure (ast, li)

-- | Like resolveImports but also returns the Merkle hash map (URL → hash).
resolveAndPin :: String -> Bool -> String -> Expr -> IO (Either String (Expr, LinkInfo, Map.Map String String))
resolveAndPin cc noCache file expr = do
  cache      <- newIORef Map.empty
  inProgress <- newIORef Set.empty
  circRefs   <- newIORef Set.empty
  linkRef    <- newIORef emptyLinkInfo
  merkle     <- newIORef Map.empty
  hashErrs   <- newIORef []
  let ctx = ResCtx cache inProgress circRefs linkRef merkle hashErrs noCache cc
  resolved <- resolveExpr ctx (takeDirectory file) expr
  -- Check for hash verification errors
  errs <- readIORef hashErrs
  case errs of
    (_:_) -> do
      mapM_ (\(path, expected, actual) -> do
        hPutStrLn stderr $ "Hash mismatch for " ++ path
        hPutStrLn stderr $ "  expected: " ++ expected
        hPutStrLn stderr $ "  actual:   " ++ actual
        ) (reverse errs)
      pure $ Left $ "sha256 verification failed for " ++ show (length errs) ++ " import(s)"
    [] -> do
      -- Lift circular-referenced modules as top-level bindings
      circSet  <- readIORef circRefs
      cacheMap <- readIORef cache
      li       <- readIORef linkRef
      mh       <- readIORef merkle
      let result = if Set.null circSet
            then resolved
            else
              let modBinds = [ Binding { bindDomain = Value
                                       , bindName   = moduleRefName p
                                       , bindParams = []
                                       , bindBody   = content
                                       , bindPos    = Nothing
                                       }
                             | p <- Set.toList circSet
                             , Just content <- [Map.lookup p cacheMap]
                             ]
              in case resolved of
                   Namespace bs -> Namespace (modBinds ++ bs)
                   _ -> resolved
      pure $ Right (result, li, mh)

-- | Generate a stable reference name for a module file path
moduleRefName :: FilePath -> T.Text
moduleRefName path = "__mod_" <> T.pack (map sanitize path) <> "__"
  where sanitize '/' = '_'; sanitize '.' = '_'; sanitize '-' = '_'; sanitize c = c

resolveExpr :: ResCtx -> String -> Expr -> IO Expr
-- Handle import' "path" ({opts}) — 2-arg import with options
resolveExpr ctx dir (App (App (Name "import'") (StringLit path)) (Record _ opts)) = do
  let pathStr = T.unpack path
      sha256  = extractSha256 opts
      standardImport = extractStandardImport opts
      funFilter = extractFilter opts
  -- For local imports, extract link info
  unless (isURL pathStr) $ do
    importOpts <- extractImportOpts dir opts
    modifyIORef (rcLinkRef ctx) (mergeLinkInfo importOpts)
  result <- resolveImport ctx dir pathStr sha256 standardImport funFilter
  -- If annotate option is present, wrap with __ffi_apply
  case extractAnnotate opts of
    Just f -> do
      f' <- resolveExpr ctx dir f
      pure $ App (App (Name "__ffi_apply") f') result
    Nothing -> pure result
-- Handle regular imports
resolveExpr ctx dir (Import path) =
  resolveImport ctx dir (T.unpack path) Nothing False Nothing
resolveExpr ctx dir (Namespace bs) =
  Namespace <$> mapM (resolveBinding ctx dir) bs
resolveExpr ctx dir (App f x) =
  App <$> resolveExpr ctx dir f <*> resolveExpr ctx dir x
resolveExpr ctx dir (BinOp op l r) =
  BinOp op <$> resolveExpr ctx dir l <*> resolveExpr ctx dir r
resolveExpr ctx dir (Lam p b) =
  Lam p <$> resolveExpr ctx dir b
resolveExpr ctx dir (Record t bs) =
  Record t <$> mapM (resolveBinding ctx dir) bs
resolveExpr ctx dir (FieldAccess e f) =
  (\e' -> FieldAccess e' f) <$> resolveExpr ctx dir e
resolveExpr ctx dir (Case s alts) =
  Case <$> resolveExpr ctx dir s <*> mapM (resolveAlt ctx dir) alts
resolveExpr ctx dir (Thunk e) = Thunk <$> resolveExpr ctx dir e
resolveExpr ctx dir (ListLit es) = ListLit <$> mapM (resolveExpr ctx dir) es
resolveExpr ctx dir (With e bs) =
  With <$> resolveExpr ctx dir e <*> mapM (resolveBinding ctx dir) bs
resolveExpr ctx dir (Quote e) = Quote <$> resolveExpr ctx dir e
resolveExpr ctx dir (Splice e) = Splice <$> resolveExpr ctx dir e
resolveExpr _ _ e = pure e  -- literals, names, errors

-- | Unified import resolution: handles local files and URLs
resolveImport :: ResCtx -> String -> String -> Maybe String -> Bool -> Maybe [T.Text] -> IO Expr
resolveImport ctx dir pathStr expectedHash standardImport funFilter
  | isURL pathStr = resolveURLImport ctx pathStr expectedHash standardImport
  | isURL dir     = resolveURLImport ctx (resolveURL dir pathStr) expectedHash standardImport
  | otherwise     = resolveLocalImport ctx dir pathStr expectedHash standardImport funFilter

-- | Resolve a local file import
resolveLocalImport :: ResCtx -> String -> String -> Maybe String -> Bool -> Maybe [T.Text] -> IO Expr
resolveLocalImport _ctx _dir pathStr _expectedHash _standardImport _funFilter
  | pathStr == "prelude" = pure specialPrelude
  | pathStr == "build"   = pure (bindBody buildBinding)
  | pathStr == "ast"     = pure specialAst
resolveLocalImport ctx dir pathStr _expectedHash standardImport funFilter = do
  let relPath = dir </> pathStr
  cached <- readIORef (rcCache ctx)
  case Map.lookup relPath cached of
    Just e  -> pure e
    Nothing -> case Map.lookup pathStr cached of
      Just e -> pure e
      Nothing -> do
        if takeExtension pathStr == ".h"
          then do
            let fullPath = if not (null pathStr) && head pathStr == '/' then pathStr else relPath
            autoInfo <- autoLinkInfo fullPath
            modifyIORef (rcLinkRef ctx) (mergeLinkInfo autoInfo)
            result <- loadCHeader (rcCC ctx) fullPath pathStr standardImport funFilter
            case result of
              Left err -> pure $ Error (T.pack err)
              Right ns -> do
                modifyIORef (rcCache ctx) (Map.insert relPath ns)
                pure ns
          else do
            progress <- readIORef (rcInProgress ctx)
            if Set.member relPath progress
              then do
                modifyIORef (rcCircRefs ctx) (Set.insert relPath)
                pure $ Name (moduleRefName relPath)
              else do
                exists <- doesFileExist relPath
                if not exists
                  then pure $ Error ("import not found: " <> T.pack pathStr)
                  else do
                    src <- TIO.readFile relPath
                    case parseProgram relPath src of
                      Left err -> pure $ Error (T.pack (errorBundlePretty err))
                      Right ast -> do
                        modifyIORef (rcInProgress ctx) (Set.insert relPath)
                        resolved <- resolveExpr ctx (takeDirectory relPath) ast
                        modifyIORef (rcCache ctx) (Map.insert relPath resolved)
                        modifyIORef (rcInProgress ctx) (Set.delete relPath)
                        pure resolved

-- | Resolve a URL import with Merkle hash computation
resolveURLImport :: ResCtx -> String -> Maybe String -> Bool -> IO Expr
resolveURLImport ctx url expectedHash _standardImport = do
  -- Check Merkle cache first (skip when --no-cache)
  merkleMap <- readIORef (rcMerkle ctx)
  case if rcNoCache ctx then Nothing else Map.lookup url merkleMap of
    Just _ -> do
      cached <- Map.lookup url <$> readIORef (rcCache ctx)
      case cached of
        Just expr -> pure expr
        Nothing   -> pure $ Error (T.pack $ "internal error: Merkle cached but not import cached: " ++ url)
    Nothing -> do
      result <- fetchRemote (rcNoCache ctx) url
      case result of
        Left err -> pure $ Error (T.pack err)
        Right localPath -> do
          src <- TIO.readFile localPath
          case parseProgram localPath src of
            Left err -> pure $ Error (T.pack (errorBundlePretty err))
            Right parsedAST -> do
              let baseDir = urlDirName url
                  subURLs = collectImportURLs baseDir parsedAST
              -- Resolve recursively
              resolved <- resolveExpr ctx baseDir parsedAST
              -- Compute Merkle hash
              contentHash <- hashFile localPath
              merkleMap' <- readIORef (rcMerkle ctx)
              let subHashes = sort [ h | u <- subURLs
                                       , Just h <- [Map.lookup u merkleMap'] ]
                  merkleInput = contentHash ++ concat subHashes
                  merkleHash = hashBytes (BS8.pack merkleInput)
              modifyIORef (rcMerkle ctx) (Map.insert url merkleHash)
              modifyIORef (rcCache ctx) (Map.insert url resolved)
              -- Verify against expected hash
              case expectedHash of
                Nothing -> do
                  hPutStrLn stderr $ "WARNING: no sha256 for import \"" ++ url ++ "\""
                  hPutStrLn stderr $ "  sha256 = \"" ++ merkleHash ++ "\""
                Just expected
                  | expected == merkleHash -> pure ()
                  | otherwise ->
                      modifyIORef (rcHashErrs ctx) ((url, expected, merkleHash) :)
              pure resolved

-- | Collect import URLs from a parsed AST (for Merkle hash computation)
collectImportURLs :: String -> Expr -> [String]
collectImportURLs base = go
  where
    resolve p = if isURL p then p else resolveURL base p
    go (App (App (Name "import'") (StringLit path)) (Record _ _)) =
      let p = T.unpack path
      in if isURL p || not ("/" `isPrefixOf` p) then [resolve p] else []
    go (Import path) =
      let p = T.unpack path
      in if isURL p || not ("/" `isPrefixOf` p) then [resolve p] else []
    go (BinOp _ l r)      = go l ++ go r
    go (App f x)          = go f ++ go x
    go (Lam _ b)          = go b
    go (With e bs)        = go e ++ concatMap (go . bindBody) bs
    go (Record _ bs)      = concatMap (go . bindBody) bs
    go (FieldAccess e _)  = go e
    go (Namespace bs)     = concatMap (go . bindBody) bs
    go (Case s as)        = go s ++ concatMap (\a -> maybe [] go (altGuard a) ++ go (altBody a)) as
    go (Thunk b)          = go b
    go (ListLit es)       = concatMap go es
    go (Quote b)          = go b
    go (Splice b)         = go b
    go _                  = []

-- | Extract sha256 from import options
extractSha256 :: [Binding] -> Maybe String
extractSha256 bs = case [T.unpack v | Binding { bindName = "sha256", bindBody = StringLit v } <- bs] of
  (h:_) -> Just h
  []    -> Nothing

-- | Extract standard_import option from import options (nonzero Int = True)
extractStandardImport :: [Binding] -> Bool
extractStandardImport bs = case [x | Binding { bindName = "standard_import", bindBody = IntLit x } <- bs] of
  (n:_) -> n /= 0
  []    -> False

-- | Extract filter list from import options
extractFilter :: [Binding] -> Maybe [T.Text]
extractFilter bs = case [es | Binding { bindName = "filter", bindBody = ListLit es } <- bs] of
  (es:_) -> Just [v | StringLit v <- es]
  []     -> Nothing

-- | Extract cc_flags option from import options (flags passed only to gcc, not preprocessor)
extractCCFlags :: [Binding] -> [String]
extractCCFlags bs = concatMap (words . T.unpack . textVal) [ v | Binding { bindName = "cc_flags", bindBody = v } <- bs, isTextVal v ]
  where
    isTextVal (StringLit _) = True
    isTextVal _ = False
    textVal (StringLit t) = t
    textVal _ = ""
-- | Extract annotate function from import options
extractAnnotate :: [Binding] -> Maybe Expr
extractAnnotate bs = case [bindBody b | b <- bs, bindName b == "annotate"] of
  (f:_) -> Just f
  []    -> Nothing

resolveBinding :: ResCtx -> String -> Binding -> IO Binding
resolveBinding ctx dir b = do
  body' <- resolveExpr ctx dir (bindBody b)
  pure b { bindBody = body' }

resolveAlt :: ResCtx -> String -> Alt -> IO Alt
resolveAlt ctx dir (Alt p g body) = do
  body' <- resolveExpr ctx dir body
  g' <- case g of
    Just ge -> Just <$> resolveExpr ctx dir ge
    Nothing -> pure Nothing
  pure $ Alt p g' body'

-- | Parse a C header file, returning a Namespace of CFunction bindings
loadCHeader :: String -> FilePath -> String -> Bool -> Maybe [T.Text] -> IO (Either String Expr)
loadCHeader cc path hdrName standardImport funFilter = do
  result <- if standardImport
              then parseCHeaderInclude cc hdrName
              else parseCHeader cc path
  case result of
    Left err -> pure $ Left err
    Right (sigs, enums) -> do
      let hdr = T.pack hdrName
          filteredSigs = case funFilter of
            Nothing    -> sigs
            Just names -> let nameSet = Set.fromList names
                          in filter (\(CFunSig n _ _) -> Set.member n nameSet) sigs
          filteredEnums = case funFilter of
            Nothing    -> enums
            Just names -> let nameSet = Set.fromList names
                          in filter (\(CEnumConst n _) -> Set.member n nameSet) enums
          funBindings = concatMap (sigToBinding hdr) filteredSigs
          enumBindings = map enumToBinding filteredEnums
      pure $ Right (Namespace (funBindings ++ enumBindings))
  where
    sigToBinding hdr (CFunSig n r p) =
      let valBind = Binding { bindDomain = Value
                            , bindName   = n
                            , bindParams = []
                            , bindBody   = CFunction hdr n r p standardImport
                            , bindPos    = Nothing
                            }
          typeBind = Binding { bindDomain = Type
                             , bindName   = n
                             , bindParams = []
                             , bindBody   = ctypeToTypeExpr r p
                             , bindPos    = Nothing
                             }
      in [valBind, typeBind]
    enumToBinding (CEnumConst n v) =
      Binding { bindDomain = Value
              , bindName   = n
              , bindParams = []
              , bindBody   = IntLit (fromIntegral v)
              , bindPos    = Nothing
              }

-- | Convert a CFunction's return type and parameter types to a type annotation expression.
-- E.g., CInt 32 → App (Name "Int'") (IntLit 32), and builds a chain: param1 : param2 : ... : ret
ctypeToTypeExpr :: CType -> [CType] -> Expr
ctypeToTypeExpr ret params =
  let paramExprs = map ctypeToExpr (filter (not . isOut) params)
      retExpr = ctypeToExpr ret
  in foldr (\p r -> BinOp ":" p r) retExpr paramExprs
  where
    isOut (COut _) = True
    isOut _        = False
    ctypeToExpr :: CType -> Expr
    ctypeToExpr (CInt 64)   = Name "Int"
    ctypeToExpr (CInt w)    = App (Name "Int'") (IntLit (fromIntegral w))
    ctypeToExpr (CUInt 64)  = Name "UInt"
    ctypeToExpr (CUInt 8)   = Name "Byte"
    ctypeToExpr (CUInt w)   = App (Name "UInt'") (IntLit (fromIntegral w))
    ctypeToExpr CLong       = Name "Int"
    ctypeToExpr CULong      = Name "UInt"
    ctypeToExpr CFloat      = Name "Float"
    ctypeToExpr CFloat32    = App (Name "Float'") (IntLit 32)
    ctypeToExpr CString     = Name "Str"
    ctypeToExpr CVoid       = Name "Int"  -- void returns 0
    ctypeToExpr (CPtr _)    = Name "ptr"  -- opaque pointer (type variable)
    ctypeToExpr (COut ct)   = ctypeToExpr ct
    ctypeToExpr (CStruct name _) = Name name
    ctypeToExpr (CStructPtr name _) = Name name
    ctypeToExpr (CCallback _ _)  = Name "fn"  -- callback (type variable)

-- | Auto-detect link flags for a C header
autoLinkInfo :: FilePath -> IO LinkInfo
autoLinkInfo hdrPath = do
  let sysFlags = systemHeaderFlags hdrPath
      hdrDir = takeDirectory hdrPath
      includeDir = if null hdrDir || hdrDir == "." then [] else [normalise hdrDir]
      baseName = takeBaseName hdrPath
      cFile = normalise (takeDirectory hdrPath </> baseName ++ ".c")
  hasCFile <- doesFileExist cFile
  pure $ LinkInfo sysFlags (if hasCFile then [cFile] else []) includeDir []

-- | Built-in map of system headers to link flags
systemHeaderFlags :: FilePath -> [String]
systemHeaderFlags path = case takeBaseName path of
  "math"    -> ["-lm"]
  "pthread" -> ["-lpthread"]
  "dl"      -> ["-ldl"]
  "rt"      -> ["-lrt"]
  _         -> []

-- | Extract import options from import' opts record
extractImportOpts :: FilePath -> [Binding] -> IO LinkInfo
extractImportOpts dir bs = do
  let srcs = [ normalise (dir </> T.unpack (textVal v)) | Binding { bindName = "src", bindBody = v } <- bs, isTextVal v ]
      flags = concatMap (words . T.unpack . textVal) [ v | Binding { bindName = "flags", bindBody = v } <- bs, isTextVal v ]
      incls = [ normalise (dir </> T.unpack (textVal v)) | Binding { bindName = "include", bindBody = v } <- bs, isTextVal v ]
      ccFlags = extractCCFlags bs
  pure $ LinkInfo flags srcs incls ccFlags
  where
    isTextVal (StringLit _) = True
    isTextVal _ = False
    textVal (StringLit t) = t
    textVal _ = ""

-- | Set of prelude names to hide in script mode output
preludeNames :: Set.Set T.Text
preludeNames = Set.fromList names
  where
    names = concatMap expandName preludeBindings
    expandName b = bindName b : case bindBody b of
      Namespace ctors -> map bindName ctors
      _ -> []

-- | raw-reduce: reduce without prelude (kept for internal use)
cmdRawReduce :: String -> IO ()
cmdRawReduce file = do
  ast <- loadAndParse file
  let (reduced, _) = reduce emptyEnv ast
  putStrLn (prettyExpr 0 reduced)

-- | pin: find unpinned URL imports, fetch them, compute Merkle hashes, rewrite source
cmdPin :: String -> Bool -> PinOpts -> IO ()
cmdPin cc noCache opts = do
  let file = pinFile opts
  ast <- loadAndParse file
  -- Find URL imports that are unpinned
  let urlImports = findURLImports ast
  if null urlImports
    then putStrLn "No URL imports found."
    else do
      -- Resolve all imports to compute Merkle hashes
      result <- resolveAndPin cc noCache file (injectPrelude True ast)
      case result of
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right (_, _, merkleMap) -> do
          -- Rewrite the source file, adding sha256 hashes
          src <- TIO.readFile file
          let updates = [ (url, hash)
                        | (url, currentHash) <- urlImports
                        , Nothing <- [currentHash]
                        , Just hash <- [Map.lookup url merkleMap]
                        ]
          if null updates
            then putStrLn "All URL imports are already pinned."
            else do
              let src' = foldl (\s (url, hash) -> pinImport s url hash) src updates
              TIO.writeFile file src'
              mapM_ (\(url, hash) -> putStrLn $ "Pinned: " ++ url ++ " sha256=\"" ++ hash ++ "\"") updates

-- | Rewrite a source text to add sha256 to an import
pinImport :: T.Text -> String -> String -> T.Text
pinImport src url hash =
  let urlT = T.pack url
      hashT = T.pack hash
      -- Pattern: import "url" → import' "url" ({sha256 = "hash"})
      importPat = "import \"" <> urlT <> "\""
      replacement = "import' \"" <> urlT <> "\" ({sha256 = \"" <> hashT <> "\"})"
  in T.replace importPat replacement src

-- | Find URL imports in a parsed AST, returning (url, Maybe existing_hash)
findURLImports :: Expr -> [(String, Maybe String)]
findURLImports = go
  where
    go (App (App (Name "import'") (StringLit path)) (Record _ opts)) =
      let p = T.unpack path
      in if isURL p then [(p, extractSha256 opts)] else concatMap (go . bindBody) opts
    go (Import path) =
      let p = T.unpack path
      in if isURL p then [(p, Nothing)] else []
    go (Namespace bs)     = concatMap (go . bindBody) bs
    go (App f x)          = go f ++ go x
    go (BinOp _ l r)      = go l ++ go r
    go (Lam _ b)          = go b
    go (Record _ bs)      = concatMap (go . bindBody) bs
    go (FieldAccess e _)  = go e
    go (Case s as)        = go s ++ concatMap (\a -> maybe [] go (altGuard a) ++ go (altBody a)) as
    go (With e bs)        = go e ++ concatMap (go . bindBody) bs
    go (Thunk b)          = go b
    go (ListLit es)       = concatMap go es
    go (Quote b)          = go b
    go (Splice b)         = go b
    go _                  = []

-- | Unified reduce command: handles dump, reduce, raw-reduce, and JSON IR
cmdReduce :: String -> Bool -> ReduceOpts -> IO ()
cmdReduce cc noCache opts = do
  let file = reduceFile opts
  ast <- loadAndParse file
  result <- if reduceNoReduce opts
    then do
      -- Parse only (dump mode) — still resolve imports unless --no-prelude
      if reduceNoPrelude opts
        then pure ast
        else do
          (resolved, _) <- resolveImports cc noCache file ast
          pure resolved
    else if reduceNoPrelude opts
      then do
        -- Reduce without prelude (raw-reduce mode)
        let (reduced, _) = reduce emptyEnv ast
        pure reduced
      else do
        -- Full reduce with prelude and imports
        (reduced, _) <- loadAndReduce cc noCache file
        pure reduced
  let output = if reduceJSON opts
        then BSL.unpack (encodePretty (exprToJSON result))
        else prettyExpr 0 result
  case reduceOut opts of
    Nothing -> putStrLn output
    Just f  -> writeFile f (output ++ "\n")

-- | compile: emit C
cmdCompile :: String -> Bool -> CompileOpts -> IO ()
cmdCompile cc noCache opts = do
  let file = compFile opts
  cwd <- getCurrentDirectory
  (reduced, li) <- loadAndReduce cc noCache file
  let stripped = optimize (stripDeepModules 3 reduced)
      extraFlags = nub ("-lm" : linkFlags li)
      extraSrcs  = nub (linkSources li)
      extraIncls = nub (map ("-I" ++) (linkIncludes li))
      extraCCFlags = nub (linkCCFlags li)
  case compOut opts of
    Just "-" -> codegen stdout preludeNames stripped
    Just f | not (isCOutput f) -> do
      -- Binary output: generate C to temp file next to source, compile with gcc
      let cFile = dropExtension file ++ "_core.c"
          gccArgs = ["-O2", "-o", f, cFile, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags
      withFile cFile WriteMode $ \h -> do
        emitCompileComment h cc cFile f extraIncls extraSrcs extraFlags
        codegen h preludeNames stripped
      (gccExit, _, gccErr) <- readProcessWithExitCode cc gccArgs ""
      case gccExit of
        ExitSuccess -> removeFile cFile
        ExitFailure code -> do
          hPutStrLn stderr $ "error: C compilation failed (" ++ cc ++ " exit " ++ show code ++ ")"
          unless (null gccErr) $ hPutStrLn stderr gccErr
          exitFailure
    outSpec -> do
      -- C output (default)
      let f = case outSpec of
                Just name -> name
                Nothing   -> dropExtension file ++ ".c"
          compileCmd = unwords ([cc, "-O2", "-o", dropExtension f, f, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags)
      withFile f WriteMode $ \h -> do
        hPutStrLn h $ "// Compile: " ++ compileCmd
        hPutStrLn h ""
        codegen h preludeNames stripped
  where
    isCOutput f = takeExtension f == ".c"
    emitCompileComment h cc' cFile binFile incls srcs flags' = do
      let cmd = unwords ([cc', "-O2", "-o", binFile, cFile] ++ incls ++ srcs ++ flags')
      hPutStrLn h $ "// Compile: " ++ cmd
      hPutStrLn h ""

-- | run: compile to C, invoke gcc, execute
cmdRun :: String -> Bool -> RunOpts -> IO ()
cmdRun cc noCache opts = do
  let file = runFile opts
  cwd <- getCurrentDirectory
  (reduced, li) <- loadAndReduce cc noCache file
  let stripped = optimize (stripDeepModules 3 reduced)
      cFile = dropExtension file ++ "_core.c"
      binBase = takeDirectory file </> (takeBaseName file ++ "_core")
      extraFlags = nub ("-lm" : linkFlags li)
      extraSrcs  = nub (linkSources li)
      extraIncls = nub (map ("-I" ++) (linkIncludes li))
      extraCCFlags = nub (linkCCFlags li)
      gccArgs = ["-O2", "-o", binBase, cFile, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags
      isWin = any (`isPrefixOf` os) ["mingw", "cygwin", "windows"]
      exeExt = if isWin then ".exe" else ""
      candidates = if exeExt /= "" then [binBase ++ exeExt, binBase] else [binBase]
      findExisting [] = return (head candidates)
      findExisting (p:ps) = do
        e <- doesFileExist p
        if e then return p else findExisting ps
  withFile cFile WriteMode $ \h -> codegen h preludeNames stripped
  (gccExit, _, gccErr) <- readProcessWithExitCode cc gccArgs ""
  case gccExit of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      hPutStrLn stderr $ "error: C compilation failed (" ++ cc ++ " exit " ++ show code ++ ")"
      unless (null gccErr) $ hPutStrLn stderr gccErr
      existsC <- doesFileExist cFile
      when existsC $ removeFile cFile
      exitFailure
  exeToRun <- findExisting candidates
  (runExit, runOut, runErr) <- readProcessWithExitCode exeToRun (runArgs opts) ""
  -- cleanup
  unless (runKeepC opts) $ do
    existsC2 <- doesFileExist cFile
    when existsC2 $ removeFile cFile
  mapM_ (\p -> do ex <- doesFileExist p; when ex $ removeFile p) candidates
  putStr runOut
  if not (null runErr) then hPutStrLn stderr runErr else pure ()
  exitWith runExit

-- | Emit C using native function codegen
cmdCompileNative :: String -> Bool -> CompileOpts -> IO ()
cmdCompileNative cc noCache opts = do
  let file = compFile opts
  cwd <- getCurrentDirectory
  (reduced, li) <- loadAndReduce cc noCache file
  let stripped = optimize (stripDeepModules 3 reduced)
      extraFlags = nub ("-lm" : linkFlags li)
      extraSrcs  = nub (linkSources li)
      extraIncls = nub (map ("-I" ++) (linkIncludes li))
      extraCCFlags = nub (linkCCFlags li)
  case compOut opts of
    Just "-" -> nativeCodegen stdout preludeNames stripped
    Just f | not (isCOutput f) -> do
      let cFile = dropExtension file ++ "_native.c"
          gccArgs = ["-O2", "-o", f, cFile, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags
      withFile cFile WriteMode $ \h -> do
        hPutStrLn h $ "// Compile: " ++ unwords ([cc, "-O2", "-o", f, cFile, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags)
        hPutStrLn h ""
        nativeCodegen h preludeNames stripped
      (gccExit, _, gccErr) <- readProcessWithExitCode cc gccArgs ""
      case gccExit of
        ExitSuccess -> removeFile cFile
        ExitFailure code -> do
          hPutStrLn stderr $ "error: C compilation failed (" ++ cc ++ " exit " ++ show code ++ ")"
          unless (null gccErr) $ hPutStrLn stderr gccErr
          exitFailure
    outSpec -> do
      let f = case outSpec of
                Just name -> name
                Nothing   -> dropExtension file ++ ".c"
          compileCmd = unwords ([cc, "-O2", "-o", dropExtension f, f, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags)
      withFile f WriteMode $ \h -> do
        hPutStrLn h $ "// Compile: " ++ compileCmd
        hPutStrLn h ""
        nativeCodegen h preludeNames stripped
  where
    isCOutput f = takeExtension f == ".c"

-- | Compile to C using native codegen, invoke gcc, execute
cmdRunNative :: String -> Bool -> RunOpts -> IO ()
cmdRunNative cc noCache opts = do
  let file = runFile opts
  cwd <- getCurrentDirectory
  (reduced, li) <- loadAndReduce cc noCache file
  let stripped = optimize (stripDeepModules 3 reduced)
      cFile = dropExtension file ++ "_native.c"
      binBase = takeDirectory file </> (takeBaseName file ++ "_native")
      extraFlags = nub ("-lm" : linkFlags li)
      extraSrcs  = nub (linkSources li)
      extraIncls = nub (map ("-I" ++) (linkIncludes li))
      extraCCFlags = nub (linkCCFlags li)
      gccArgs = ["-O2", "-o", binBase, cFile, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraCCFlags ++ extraFlags
      isWin = any (`isPrefixOf` os) ["mingw", "cygwin", "windows"]
      exeExt = if isWin then ".exe" else ""
      candidates = if exeExt /= "" then [binBase ++ exeExt, binBase] else [binBase]
      findExisting [] = return (head candidates)
      findExisting (p:ps) = do
        e <- doesFileExist p
        if e then return p else findExisting ps
  withFile cFile WriteMode $ \h -> nativeCodegen h preludeNames stripped
  (gccExit, _, gccErr) <- readProcessWithExitCode cc gccArgs ""
  case gccExit of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      hPutStrLn stderr $ "error: C compilation failed (" ++ cc ++ " exit " ++ show code ++ ")"
      unless (null gccErr) $ hPutStrLn stderr gccErr
      existsC <- doesFileExist cFile
      when existsC $ removeFile cFile
      exitFailure
  exeToRun <- findExisting candidates
  (runExit, runOut, runErr) <- readProcessWithExitCode exeToRun (runArgs opts) ""
  unless (runKeepC opts) $ do
    existsC2 <- doesFileExist cFile
    when existsC2 $ removeFile cFile
  mapM_ (\p -> do ex <- doesFileExist p; when ex $ removeFile p) candidates
  putStr runOut
  if not (null runErr) then hPutStrLn stderr runErr else pure ()
  exitWith runExit

-- ── REPL ─────────────────────────────────────────────────────────

cmdRepl :: IO ()
cmdRepl = do
  hSetBuffering stdout LineBuffering
  putStrLn "milang REPL (type :q to quit, :env to show bindings)"
  -- Bootstrap with prelude
  let preludeNS = Namespace preludeBindings
      (preludeEnv, _, _) = reduceWithEnv builtinEnv preludeNS
  replLoop preludeEnv

replLoop :: Env -> IO ()
replLoop env = do
  putStr "λ> "
  hFlush stdout
  mLine <- (Just <$> getLine) `catch` (\(_ :: IOException) -> pure Nothing)
  case mLine of
    Nothing -> putStrLn "" -- EOF
    Just line
      | line == ":q" || line == ":quit" -> pure ()
      | line == ":env" -> do
          let binds = Map.toAscList (envMap env)
              userBinds = filter (\(k,_) -> not (Set.member k preludeNames)) binds
          mapM_ (\(k,v) -> putStrLn $ T.unpack k ++ " = " ++ prettyExpr 0 v) userBinds
          replLoop env
      | T.null (T.strip (T.pack line)) -> replLoop env
      | otherwise -> do
          let src = T.pack line
          -- Try as bindings (namespace) first, then as expression
          case parseProgram "<repl>" src of
            Right ns@(Namespace _) -> do
              let (env', reduced, ws) = reduceWithEnv env ns
              printWarnings "<repl>" ws
              -- Print any value bindings that were just defined
              case reduced of
                Namespace bs -> mapM_ (\b ->
                  when (bindDomain b == Value && bindName b /= "_") $
                    putStrLn $ T.unpack (bindName b) ++ " = " ++ prettyExpr 0 (bindBody b)
                  ) bs
                _ -> pure ()
              replLoop env'
            _ -> case parseExpr "<repl>" src of
              Right expr -> do
                let (_, result, ws) = reduceWithEnv env expr
                printWarnings "<repl>" ws
                putStrLn (prettyExpr 0 result)
                replLoop env
              Left err -> do
                hPutStrLn stderr $ errorBundlePretty err
                replLoop env

printWarnings :: String -> [Warning] -> IO ()
printWarnings file ws = do
  let userWarns = filter (not . isPreludeWarning) ws
      fmtWarn (TypeWarning pos name msg) =
        (pos, T.unpack name ++ ": " ++ T.unpack msg)
      fmtWarn (TraitWarning pos name msg) =
        (pos, T.unpack name ++ ": " ++ T.unpack msg)
      fmtWarn (GeneralWarning pos msg) = (pos, T.unpack msg)
      msgs = dedupByLine (map fmtWarn userWarns)
  mapM_ (\(pos, msg) -> hPutStrLn stderr $ "warning: " ++ fmtLoc file pos ++ ": " ++ msg) msgs
