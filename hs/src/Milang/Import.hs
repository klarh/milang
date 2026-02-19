{-# LANGUAGE OverloadedStrings #-}
module Milang.Import (resolveImports, resolveAndPin, findURLImports, LinkInfo(..)) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS8
import Data.IORef
import Data.List (sort, isPrefixOf)
import System.FilePath (takeDirectory, (</>), normalise, takeExtension, takeBaseName)
import System.Directory (doesFileExist)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO (hPutStrLn, stderr)

import Milang.Syntax
import Milang.Parser (parseProgram)
import Text.Megaparsec (errorBundlePretty)
import Milang.CHeader (parseCHeader, CFunSig(..))
import Milang.Remote (fetchRemote, hashFile, hashBytes, isURL, urlDirName, resolveURL)

-- | Link info accumulated during import resolution
data LinkInfo = LinkInfo
  { linkFlags   :: [String]    -- extra gcc flags (-lm, -lpthread, etc.)
  , linkSources :: [FilePath]  -- extra .c files to compile
  } deriving (Show)

emptyLinkInfo :: LinkInfo
emptyLinkInfo = LinkInfo [] []

mergeLinkInfo :: LinkInfo -> LinkInfo -> LinkInfo
mergeLinkInfo a b = LinkInfo
  { linkFlags   = linkFlags a ++ linkFlags b
  , linkSources = linkSources a ++ linkSources b
  }

-- | Cache of already-resolved imports (path -> resolved AST)
type ImportCache = IORef (Map.Map FilePath Expr)

-- | Set of files currently being resolved (for cycle detection)
type InProgress = IORef (Set.Set FilePath)

-- | Accumulated link info
type LinkRef = IORef LinkInfo

-- | Merkle hash map: URL/path -> merkle hash
-- Populated during resolution; used for hash verification
type MerkleRef = IORef (Map.Map String String)

-- | Resolution context passed through all resolve functions
data ResCtx = ResCtx
  { rcCache      :: ImportCache
  , rcInProgress :: InProgress
  , rcLinkRef    :: LinkRef
  , rcMerkle     :: MerkleRef
  }

-- | Resolve all `import "path"` calls in an AST by loading and parsing files.
-- Returns the resolved AST and accumulated link info for gcc.
resolveImports :: FilePath -> Expr -> IO (Either String (Expr, LinkInfo))
resolveImports basePath expr = do
  result <- resolveAndPin basePath expr
  case result of
    Left err -> pure $ Left err
    Right (ast, li, _) -> pure $ Right (ast, li)

-- | Like resolveImports but also returns the Merkle hash map (URL → hash).
-- Used by `milang pin` to get Merkle hashes for URL imports.
resolveAndPin :: FilePath -> Expr -> IO (Either String (Expr, LinkInfo, Map.Map String String))
resolveAndPin basePath expr = do
  cache <- newIORef Map.empty
  inProg <- newIORef Set.empty
  linkRef <- newIORef emptyLinkInfo
  merkle <- newIORef Map.empty
  let ctx = ResCtx cache inProg linkRef merkle
      baseDir = takeDirectory basePath
  result <- resolveExpr ctx baseDir expr
  case result of
    Left err -> pure $ Left err
    Right ast -> do
      li <- readIORef linkRef
      mh <- readIORef merkle
      pure $ Right (ast, li, mh)

addLinkInfo :: LinkRef -> LinkInfo -> IO ()
addLinkInfo ref li = modifyIORef ref (mergeLinkInfo li)

resolveExpr :: ResCtx -> String -> Expr -> IO (Either String Expr)
resolveExpr _ _ e@(IntLit _)    = pure $ Right e
resolveExpr _ _ e@(FloatLit _)  = pure $ Right e
resolveExpr _ _ e@(StringLit _) = pure $ Right e
resolveExpr _ _ e@(Name _)      = pure $ Right e
resolveExpr _ _ e@(CFunction {}) = pure $ Right e

resolveExpr ctx dir (BinOp op l r) = do
  l' <- resolveExpr ctx dir l
  r' <- resolveExpr ctx dir r
  pure $ BinOp op <$> l' <*> r'

-- Intercept `import "path" { opts }`
resolveExpr ctx dir (With (App (Name "import") (StringLit path)) opts) = do
  let pathStr = T.unpack path
  -- Extract link/hash opts (only makes sense for local imports)
  unless (isURL pathStr) $ do
    importOpts <- extractImportOpts dir opts
    addLinkInfo (rcLinkRef ctx) importOpts
  let sha256 = extractSha256 opts
  resolveImport ctx dir pathStr sha256

-- Intercept `import "path"`
resolveExpr ctx dir (App (Name "import") (StringLit path)) = do
  let pathStr = T.unpack path
  resolveImport ctx dir pathStr Nothing

resolveExpr ctx dir (App f x) = do
  f' <- resolveExpr ctx dir f
  x' <- resolveExpr ctx dir x
  pure $ App <$> f' <*> x'

resolveExpr ctx dir (Lam p b) = do
  b' <- resolveExpr ctx dir b
  pure $ Lam p <$> b'

resolveExpr ctx dir (With body bs) = do
  body' <- resolveExpr ctx dir body
  bs' <- resolveBindings ctx dir bs
  pure $ With <$> body' <*> bs'

resolveExpr ctx dir (Record tag bs) = do
  bs' <- resolveBindings ctx dir bs
  pure $ Record tag <$> bs'

resolveExpr ctx dir (FieldAccess e field) = do
  e' <- resolveExpr ctx dir e
  pure $ FieldAccess <$> e' <*> pure field

resolveExpr ctx dir (Namespace bs) = do
  bs' <- resolveBindings ctx dir bs
  pure $ Namespace <$> bs'

resolveExpr ctx dir (Case scrut alts) = do
  scrut' <- resolveExpr ctx dir scrut
  alts' <- resolveAlts ctx dir alts
  pure $ Case <$> scrut' <*> alts'

resolveExpr ctx dir (Thunk body) = do
  body' <- resolveExpr ctx dir body
  pure $ Thunk <$> body'

resolveExpr ctx dir (ListLit es) = do
  es' <- mapM (resolveExpr ctx dir) es
  pure $ ListLit <$> sequence es'

resolveExpr ctx dir (Quote body) = do
  body' <- resolveExpr ctx dir body
  pure $ Quote <$> body'

resolveExpr ctx dir (Splice body) = do
  body' <- resolveExpr ctx dir body
  pure $ Splice <$> body'

-- | Unified import resolution. Handles both local and URL imports.
-- `dir` is the base directory (filesystem path or URL base).
-- `pathStr` is the import target (relative path or absolute URL).
-- `expectedHash` is the optional Merkle hash for verification.
resolveImport :: ResCtx -> String -> String -> Maybe String -> IO (Either String Expr)
resolveImport ctx dir pathStr expectedHash
  | isURL pathStr = resolveURLImport ctx pathStr expectedHash
  | isURL dir     = resolveURLImport ctx (resolveURL dir pathStr) expectedHash
  | otherwise     = do
      let fullPath = normalise (dir </> pathStr)
      when (takeExtension pathStr == ".h") $ do
        autoInfo <- autoLinkInfo fullPath
        addLinkInfo (rcLinkRef ctx) autoInfo
      fst <$> resolveImportPath ctx fullPath pathStr Nothing

-- | Resolve an import from a URL
resolveURLImport :: ResCtx -> String -> Maybe String -> IO (Either String Expr)
resolveURLImport ctx url expectedHash = do
  -- Check Merkle cache first (already resolved this URL)
  merkleMap <- readIORef (rcMerkle ctx)
  case Map.lookup url merkleMap of
    Just _ -> do
      -- Already resolved — use the import cache
      cached <- Map.lookup url <$> readIORef (rcCache ctx)
      case cached of
        Just expr -> pure $ Right expr
        Nothing   -> pure $ Left $ "Internal error: Merkle cached but not import cached: " ++ url
    Nothing -> do
      result <- fetchRemote url Nothing
      case result of
        Left err -> pure $ Left err
        Right localPath -> do
          -- Parse file to find sub-import URLs (before resolution replaces them)
          src <- TIO.readFile localPath
          case parseProgram localPath src of
            Left err -> pure $ Left (errorBundlePretty err)
            Right parsedAST -> do
              let baseDir = urlDirName url
                  subURLs = collectImportURLs baseDir parsedAST
              -- Resolve the file (this recursively resolves sub-imports)
              let originDir = urlDirName url
              (exprResult, _circular) <- resolveImportPath ctx localPath url (Just originDir)
              case exprResult of
                Left err -> pure $ Left err
                Right expr -> do
                  -- Compute Merkle hash: content hash + sorted sub-import Merkle hashes
                  contentHash <- hashFile localPath
                  merkleMap' <- readIORef (rcMerkle ctx)
                  let subHashes = sort [h | u <- subURLs
                                          , Just h <- [Map.lookup u merkleMap']]
                      merkleInput = contentHash ++ concat subHashes
                      merkleHash = hashBytes (BS8.pack merkleInput)
                  modifyIORef (rcMerkle ctx) (Map.insert url merkleHash)
                  -- Also cache by URL for future lookups
                  modifyIORef (rcCache ctx) (Map.insert url expr)
                  -- Verify against expected hash
                  case expectedHash of
                    Nothing -> do
                      hPutStrLn stderr $ "WARNING: no sha256 for import \"" ++ url ++ "\""
                      hPutStrLn stderr $ "  sha256 = \"" ++ merkleHash ++ "\""
                    Just expected
                      | expected == merkleHash -> pure ()
                      | otherwise -> do
                          hPutStrLn stderr $ "Hash mismatch for " ++ url
                          hPutStrLn stderr $ "  expected: " ++ expected
                          hPutStrLn stderr $ "  actual:   " ++ merkleHash
                  pure $ Right expr

-- | Collect import URLs from a parsed (pre-resolution) AST.
-- Relative paths are resolved against the given URL base.
collectImportURLs :: String -> Expr -> [String]
collectImportURLs base = go
  where
    resolve p = if isURL p then p else resolveURL base p
    go (App (Name "import") (StringLit path)) =
      let p = T.unpack path
      in if isURL p || not ("/" `isPrefixOf` p) then [resolve p] else []
    go (With (App (Name "import") (StringLit path)) _) =
      let p = T.unpack path
      in if isURL p || not ("/" `isPrefixOf` p) then [resolve p] else []
    go (BinOp _ l r)    = go l ++ go r
    go (App f x)        = go f ++ go x
    go (Lam _ b)        = go b
    go (With e bs)      = go e ++ concatMap (go . bindBody) bs
    go (Record _ bs)    = concatMap (go . bindBody) bs
    go (FieldAccess e _)= go e
    go (Namespace bs)   = concatMap (go . bindBody) bs
    go (Case s as)      = go s ++ concatMap (\a -> maybe [] go (altGuard a) ++ go (altBody a)) as
    go (Thunk b)        = go b
    go (ListLit es)     = concatMap go es
    go (Quote b)        = go b
    go (Splice b)       = go b
    go _                = []

-- | Resolve an import path. Returns (Either String Expr, Bool) where the Bool
-- indicates whether this was a circular import (True = cycle detected).
-- `originDir` is Just url-base for remote files, Nothing for local files.
resolveImportPath :: ResCtx -> FilePath -> String -> Maybe String -> IO (Either String Expr, Bool)
resolveImportPath ctx fullPath pathStr originDir = do
  -- Check cache first (fully resolved from a prior import)
  cached <- Map.lookup fullPath <$> readIORef (rcCache ctx)
  case cached of
    Just expr -> pure (Right expr, False)
    Nothing -> do
      -- Check if this file is currently being resolved (circular import)
      inProg <- readIORef (rcInProgress ctx)
      if Set.member fullPath inProg
        then do
          -- Circular import detected. Parse file, return only non-import
          -- bindings. The caller marks this binding lazy so the codegen
          -- emits a thunk — by the time the thunk is forced at runtime,
          -- the full module is available in the environment.
          result <- parseOwnBindings fullPath
          pure (result, True)
        else do
          result <- case takeExtension pathStr of
            ".h" -> loadCHeader fullPath
            _    -> loadFile ctx fullPath originDir
          case result of
            Left err -> pure (Left err, False)
            Right expr -> do
              modifyIORef (rcCache ctx) (Map.insert fullPath expr)
              pure (Right expr, False)

-- | Parse a file and return only its non-import bindings (for circular refs)
parseOwnBindings :: FilePath -> IO (Either String Expr)
parseOwnBindings path = do
  src <- TIO.readFile path
  case parseProgram path src of
    Left err -> pure $ Left (errorBundlePretty err)
    Right ast -> pure $ Right (extractOwnBindings ast)

-- | Extract non-import bindings from a Namespace
extractOwnBindings :: Expr -> Expr
extractOwnBindings (Namespace bs) = Namespace (filter (not . isImportBinding) bs)
extractOwnBindings e = e

-- | Check if a binding's body is an import expression
isImportBinding :: Binding -> Bool
isImportBinding b = case bindBody b of
  App (Name "import") (StringLit _)   -> True
  With (App (Name "import") (StringLit _)) _ -> True
  _ -> False

-- | Extract link options from import { ... } bindings
extractImportOpts :: FilePath -> [Binding] -> IO LinkInfo
extractImportOpts dir bs = do
  let flags = concatMap getFlag bs
      srcs  = concatMap (getSrc dir) bs
  pkgFlags <- concat <$> mapM getPkgConfig bs
  pure $ LinkInfo (flags ++ pkgFlags) srcs
  where
    getFlag (Binding "flags" _ _ (StringLit s) _ _ _) = words (T.unpack s)
    getFlag _ = []
    getSrc d (Binding "src" _ _ (StringLit s) _ _ _) = [normalise (d </> T.unpack s)]
    getSrc _ _ = []
    getPkgConfig (Binding "pkg" _ _ (StringLit pkg) _ _ _) = do
      let pkgStr = T.unpack pkg
      (ec1, cflags, _) <- readProcessWithExitCode
        "pkg-config" ["--cflags", pkgStr] ""
      (ec2, libs, _) <- readProcessWithExitCode
        "pkg-config" ["--libs", pkgStr] ""
      pure $ case (ec1, ec2) of
        (ExitSuccess, ExitSuccess) -> words cflags ++ words libs
        _ -> []
    getPkgConfig _ = pure []

-- | Extract sha256 hash from import options, if present
extractSha256 :: [Binding] -> Maybe String
extractSha256 [] = Nothing
extractSha256 (Binding "sha256" _ _ (StringLit s) _ _ _ : _) = Just (T.unpack s)
extractSha256 (_ : bs) = extractSha256 bs

-- | Auto-detect link info for .h imports
autoLinkInfo :: FilePath -> IO LinkInfo
autoLinkInfo hdrPath = do
  let sysFlags = systemHeaderFlags hdrPath
  let hdrDir = takeDirectory hdrPath
      includeFlag = if null hdrDir || hdrDir == "." then [] else ["-I" ++ hdrDir]
  let baseName = takeBaseName hdrPath
      cFile = takeDirectory hdrPath </> baseName ++ ".c"
  hasCFile <- doesFileExist cFile
  pure $ LinkInfo (sysFlags ++ includeFlag) (if hasCFile then [cFile] else [])

-- | Built-in map of system headers to link flags
systemHeaderFlags :: FilePath -> [String]
systemHeaderFlags path = case takeBaseName path of
  "math"    -> ["-lm"]
  "pthread" -> ["-lpthread"]
  "dl"      -> ["-ldl"]
  "rt"      -> ["-lrt"]
  _         -> []

-- Reusable 'when' without importing Control.Monad
when :: Bool -> IO () -> IO ()
when True  a = a
when False _ = pure ()

unless :: Bool -> IO () -> IO ()
unless b = when (not b)

-- | Load and resolve a milang file.
-- Marks the file as in-progress to detect circular imports.
-- `originDir` overrides the directory for relative import resolution
-- (used when the file was fetched from a URL — resolves relative to URL base).
loadFile :: ResCtx -> FilePath -> Maybe String -> IO (Either String Expr)
loadFile ctx path originDir = do
  src <- TIO.readFile path
  case parseProgram path src of
    Left err -> pure $ Left (errorBundlePretty err)
    Right ast -> do
      modifyIORef (rcInProgress ctx) (Set.insert path)
      let dir = case originDir of
                  Just urlBase -> urlBase
                  Nothing      -> takeDirectory path
      result <- resolveExpr ctx dir ast
      modifyIORef (rcInProgress ctx) (Set.delete path)
      pure result

-- | Parse a C header file, returning a Namespace of CFunction bindings
loadCHeader :: FilePath -> IO (Either String Expr)
loadCHeader path = do
  result <- parseCHeader path
  case result of
    Left err -> pure $ Left err
    Right sigs -> do
      let hdr = T.pack path
          bindings = map (sigToBinding hdr) sigs
      pure $ Right (Namespace bindings)
  where
    sigToBinding hdr (CFunSig n r p) =
      Binding
        { bindName   = n
        , bindLazy   = False
        , bindParams = []
        , bindBody   = CFunction hdr n r p
        , bindPos    = Nothing
        , bindType   = Nothing
        , bindSource = Nothing
        }

-- | Resolve bindings, detecting circular imports and marking them lazy.
resolveBindings :: ResCtx -> String -> [Binding] -> IO (Either String [Binding])
resolveBindings ctx dir = go
  where
    go [] = pure $ Right []
    go (b:bs) = do
      (body', circular) <- resolveBindingBody ctx dir b
      rest <- go bs
      pure $ do
        b' <- body'
        bs' <- rest
        -- Circular imports become lazy bindings (thunks at runtime)
        let b'' = if circular then b { bindBody = b', bindLazy = True }
                              else b { bindBody = b' }
        Right (b'' : bs')

-- | Resolve a binding body, detecting circular imports.
resolveBindingBody :: ResCtx -> String -> Binding -> IO (Either String Expr, Bool)
resolveBindingBody ctx dir b = case bindBody b of
  App (Name "import") (StringLit path) -> do
    let pathStr = T.unpack path
    resolveBindingImport ctx dir pathStr Nothing

  With (App (Name "import") (StringLit path)) opts -> do
    let pathStr = T.unpack path
        sha256 = extractSha256 opts
    unless (isURL pathStr || isURL dir) $ do
      importOpts <- extractImportOpts dir opts
      addLinkInfo (rcLinkRef ctx) importOpts
    resolveBindingImport ctx dir pathStr sha256

  _ -> do
    result <- resolveExpr ctx dir (bindBody b)
    pure (result, False)

-- | Resolve an import in a binding context, returning circularity flag.
resolveBindingImport :: ResCtx -> String -> String -> Maybe String -> IO (Either String Expr, Bool)
resolveBindingImport ctx dir pathStr sha256
  | isURL pathStr || isURL dir = do
      result <- resolveImport ctx dir pathStr sha256
      -- URL imports are never "circular" in the lazy-binding sense
      -- (cycle detection handles them differently)
      pure (result, False)
  | otherwise = do
      let fullPath = normalise (dir </> pathStr)
      when (takeExtension pathStr == ".h") $ do
        autoInfo <- autoLinkInfo fullPath
        addLinkInfo (rcLinkRef ctx) autoInfo
      resolveImportPath ctx fullPath pathStr Nothing

resolveAlts :: ResCtx -> String -> [Alt] -> IO (Either String [Alt])
resolveAlts ctx dir = go
  where
    go [] = pure $ Right []
    go (a:as) = do
      body' <- resolveExpr ctx dir (altBody a)
      guard' <- case altGuard a of
        Nothing -> pure $ Right Nothing
        Just g  -> fmap (fmap Just) (resolveExpr ctx dir g)
      rest <- go as
      pure $ do
        b' <- body'
        g' <- guard'
        as' <- rest
        Right (a { altBody = b', altGuard = g' } : as')

-- | Find all URL imports in an AST.  Returns (url, Maybe sha256) pairs.
-- Used by `milang pin` to discover which imports need hashes.
findURLImports :: Expr -> [(String, Maybe String)]
findURLImports = go
  where
    go (App (Name "import") (StringLit path))
      | isURL (T.unpack path) = [(T.unpack path, Nothing)]
    go (With (App (Name "import") (StringLit path)) opts)
      | isURL (T.unpack path) = [(T.unpack path, extractSha256 opts)]
    go (BinOp _ l r)      = go l ++ go r
    go (App f x)           = go f ++ go x
    go (Lam _ b)           = go b
    go (With e bs)         = go e ++ concatMap (go . bindBody) bs
    go (Record _ bs)       = concatMap (go . bindBody) bs
    go (FieldAccess e _)   = go e
    go (Namespace bs)      = concatMap (go . bindBody) bs
    go (Case s as)         = go s ++ concatMap (\a -> maybe [] go (altGuard a) ++ go (altBody a)) as
    go (Thunk b)           = go b
    go (ListLit es)        = concatMap go es
    go (Quote b)           = go b
    go (Splice b)          = go b
    go _                   = []
