{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout, hFlush, hSetBuffering, BufferMode(..))
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, doesFileExist, getCurrentDirectory)
import System.Info (os)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeBaseName, (</>))
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.List (nub, isPrefixOf, sort)
import Control.Exception (catch, IOException)

import Core.Syntax
import Core.Parser (parseProgram, parseExpr)
import Text.Megaparsec (errorBundlePretty)
import Core.Reduce (reduce, reduceWithEnv, emptyEnv, Env, Warning(..), envMap)
import Core.Codegen (codegen)
import Core.Prelude (preludeBindings)
import Core.CHeader (parseCHeader, parseCHeaderInclude, CFunSig(..))
import Core.Remote (fetchRemote, hashFile, hashBytes, isURL, urlDirName, resolveURL)
import Core.Version (version)

-- | Link info accumulated during import resolution
data LinkInfo = LinkInfo
  { linkFlags    :: [String]    -- extra gcc flags (-lm, -lpthread, etc.)
  , linkSources  :: [FilePath]  -- extra .c files to compile
  , linkIncludes :: [FilePath]  -- extra -I include directories
  }

emptyLinkInfo :: LinkInfo
emptyLinkInfo = LinkInfo [] [] []

mergeLinkInfo :: LinkInfo -> LinkInfo -> LinkInfo
mergeLinkInfo a b = LinkInfo
  (linkFlags a ++ linkFlags b)
  (linkSources a ++ linkSources b)
  (linkIncludes a ++ linkIncludes b)

-- | Resolution context for import resolution
data ResCtx = ResCtx
  { rcCache      :: IORef (Map.Map String Expr)     -- resolved imports by path/URL
  , rcInProgress :: IORef (Set.Set String)           -- circular import detection
  , rcCircRefs   :: IORef (Set.Set String)           -- detected circular refs
  , rcLinkRef    :: IORef LinkInfo                   -- accumulated link info
  , rcMerkle     :: IORef (Map.Map String String)    -- URL -> Merkle hash
  , rcHashErrs   :: IORef [(String, String, String)] -- (path, expected, actual)
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("run" : file : runArgs)     -> cmdRun file runArgs
    ["compile", file]            -> cmdCompile file Nothing
    ["compile", file, out]       -> cmdCompile file (Just out)
    ["compile", file, "-o", out] -> cmdCompile file (Just out)
    ["dump", file]               -> cmdDump file
    ["reduce", file]             -> cmdReduce file
    ["raw-reduce", file]         -> cmdRawReduce file
    ["pin", file]                -> cmdPin file
    ["repl"]                     -> cmdRepl
    ["--version"]                -> putStrLn $ "milang " ++ version
    ["-v"]                       -> putStrLn $ "milang " ++ version
    ["--help"]                   -> printHelp
    ["-h"]                       -> printHelp
    []                           -> printHelp
    _ -> do
      hPutStrLn stderr $ "milang: unknown command '" ++ unwords args ++ "'"
      hPutStrLn stderr "Run 'milang --help' for usage information."
      exitFailure

printHelp :: IO ()
printHelp = do
  putStrLn "milang — the milang compiler"
  putStrLn ""
  putStrLn "Usage: milang <command> [options]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  run <file> [args]       Compile and run a .mi file"
  putStrLn "  compile <file> [-o out] Compile to C (default: <file>.c)"
  putStrLn "  reduce <file>           Show reduced AST (with prelude)"
  putStrLn "  dump <file>             Show parsed AST (no reduction)"
  putStrLn "  raw-reduce <file>       Show reduced AST (no prelude)"
  putStrLn "  pin <file>              Add sha256 hashes to URL imports"
  putStrLn "  repl                    Interactive REPL"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --help, -h              Show this help message"
  putStrLn "  --version, -v           Show version"

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
injectPrelude True (Namespace bs) = Namespace (preludeBindings ++ bs)
injectPrelude True e = Namespace (preludeBindings ++ [mkBind "_main" e])
injectPrelude False e = e

-- | Load, parse, inject prelude, and reduce
loadAndReduce :: String -> IO (Expr, LinkInfo)
loadAndReduce file = do
  ast <- loadAndParse file
  (resolved, li) <- resolveImports file ast
  let withPrelude = injectPrelude True resolved
      (reduced, ws) = reduce emptyEnv withPrelude
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
resolveImports :: String -> Expr -> IO (Expr, LinkInfo)
resolveImports file expr = do
  result <- resolveAndPin file expr
  case result of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right (ast, li, _) -> pure (ast, li)

-- | Like resolveImports but also returns the Merkle hash map (URL → hash).
resolveAndPin :: String -> Expr -> IO (Either String (Expr, LinkInfo, Map.Map String String))
resolveAndPin file expr = do
  cache      <- newIORef Map.empty
  inProgress <- newIORef Set.empty
  circRefs   <- newIORef Set.empty
  linkRef    <- newIORef emptyLinkInfo
  merkle     <- newIORef Map.empty
  hashErrs   <- newIORef []
  let ctx = ResCtx cache inProgress circRefs linkRef merkle hashErrs
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
  -- For local imports, extract link info
  unless (isURL pathStr) $ do
    importOpts <- extractImportOpts dir opts
    modifyIORef (rcLinkRef ctx) (mergeLinkInfo importOpts)
  resolveImport ctx dir pathStr sha256 standardImport
-- Handle regular imports
resolveExpr ctx dir (Import path) =
  resolveImport ctx dir (T.unpack path) Nothing False
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
resolveImport :: ResCtx -> String -> String -> Maybe String -> Bool -> IO Expr
resolveImport ctx dir pathStr expectedHash standardImport
  | isURL pathStr = resolveURLImport ctx pathStr expectedHash standardImport
  | isURL dir     = resolveURLImport ctx (resolveURL dir pathStr) expectedHash standardImport
  | otherwise     = resolveLocalImport ctx dir pathStr expectedHash standardImport

-- | Resolve a local file import
resolveLocalImport :: ResCtx -> String -> String -> Maybe String -> Bool -> IO Expr
resolveLocalImport ctx dir pathStr _expectedHash standardImport = do
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
            result <- loadCHeader fullPath pathStr standardImport
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
  -- Check Merkle cache first
  merkleMap <- readIORef (rcMerkle ctx)
  case Map.lookup url merkleMap of
    Just _ -> do
      cached <- Map.lookup url <$> readIORef (rcCache ctx)
      case cached of
        Just expr -> pure expr
        Nothing   -> pure $ Error (T.pack $ "internal error: Merkle cached but not import cached: " ++ url)
    Nothing -> do
      result <- fetchRemote url
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
loadCHeader :: FilePath -> String -> Bool -> IO (Either String Expr)
loadCHeader path hdrName standardImport = do
  result <- if standardImport
              then parseCHeaderInclude hdrName
              else parseCHeader path
  case result of
    Left err -> pure $ Left err
    Right sigs -> do
      let hdr = T.pack hdrName
          bindings = map (sigToBinding hdr) sigs
      pure $ Right (Namespace bindings)
  where
    sigToBinding hdr (CFunSig n r p) =
      Binding { bindDomain = Value
              , bindName   = n
              , bindParams = []
              , bindBody   = CFunction hdr n r p standardImport
              , bindPos    = Nothing
              }

-- | Auto-detect link flags for a C header
autoLinkInfo :: FilePath -> IO LinkInfo
autoLinkInfo hdrPath = do
  let sysFlags = systemHeaderFlags hdrPath
      hdrDir = takeDirectory hdrPath
      includeDir = if null hdrDir || hdrDir == "." then [] else [hdrDir]
      baseName = takeBaseName hdrPath
      cFile = takeDirectory hdrPath </> baseName ++ ".c"
  hasCFile <- doesFileExist cFile
  pure $ LinkInfo sysFlags (if hasCFile then [cFile] else []) includeDir

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
  let srcs = [ dir </> T.unpack (textVal v) | Binding { bindName = "src", bindBody = v } <- bs, isTextVal v ]
      flags = [ T.unpack (textVal v) | Binding { bindName = "flags", bindBody = v } <- bs, isTextVal v ]
      incls = [ dir </> T.unpack (textVal v) | Binding { bindName = "include", bindBody = v } <- bs, isTextVal v ]
  pure $ LinkInfo flags srcs incls
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

-- | raw-reduce: reduce without prelude
cmdRawReduce :: String -> IO ()
cmdRawReduce file = do
  ast <- loadAndParse file
  let (reduced, _) = reduce emptyEnv ast
  putStrLn (prettyExpr 0 reduced)

-- | pin: find unpinned URL imports, fetch them, compute Merkle hashes, rewrite source
cmdPin :: String -> IO ()
cmdPin file = do
  ast <- loadAndParse file
  -- Find URL imports that are unpinned
  let urlImports = findURLImports ast
  if null urlImports
    then putStrLn "No URL imports found."
    else do
      -- Resolve all imports to compute Merkle hashes
      result <- resolveAndPin file (injectPrelude True ast)
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
  let stripped = stripDeepModules 3 reduced
      outFile = case mOut of
        Just "-" -> Nothing  -- stdout
        Just f   -> Just f
        Nothing  -> Just (dropExtension file ++ ".c")
  case outFile of
    Nothing -> codegen stdout preludeNames stripped
    Just f  -> withFile f WriteMode $ \h -> codegen h preludeNames stripped

-- | run: compile to C, invoke gcc, execute
cmdRun :: String -> [String] -> IO ()
cmdRun file runArgs = do
  cwd <- getCurrentDirectory
  (reduced, li) <- loadAndReduce file
  let stripped = stripDeepModules 3 reduced
      cFile = dropExtension file ++ "_core.c"
      binBase = dropExtension file ++ "_core"
      extraFlags = nub (linkFlags li)
      extraSrcs  = nub (linkSources li)
      extraIncls = nub (map ("-I" ++) (linkIncludes li))
      gccArgs = ["-O2", "-o", binBase, cFile, "-I" ++ cwd] ++ extraIncls ++ extraSrcs ++ extraFlags
      isWin = any (`isPrefixOf` os) ["mingw", "cygwin", "windows"]
      exeExt = if isWin then ".exe" else ""
      candidates = if exeExt /= "" then [binBase ++ exeExt, binBase] else [binBase]
      -- helper to find first existing path
      findExisting [] = return (head candidates)
      findExisting (p:ps) = do
        e <- doesFileExist p
        if e then return p else findExisting ps
  withFile cFile WriteMode $ \h -> codegen h preludeNames stripped
  (gccExit, _, gccErr) <- readProcessWithExitCode "gcc" gccArgs ""
  case gccExit of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      hPutStrLn stderr $ "error: C compilation failed (gcc exit " ++ show code ++ ")"
      unless (null gccErr) $ hPutStrLn stderr gccErr
      existsC <- doesFileExist cFile
      when existsC $ removeFile cFile
      exitFailure
  exeToRun <- findExisting candidates
  (runExit, runOut, runErr) <- readProcessWithExitCode exeToRun runArgs ""
  -- cleanup
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
      (preludeEnv, _, _) = reduceWithEnv emptyEnv preludeNS
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
  where
    when True a = a
    when False _ = pure ()

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
