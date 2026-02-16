{-# LANGUAGE OverloadedStrings #-}
module Milang.Import (resolveImports, LinkInfo(..)) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.IORef
import System.FilePath (takeDirectory, (</>), normalise, takeExtension, takeBaseName)
import System.Directory (doesFileExist)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Milang.Syntax
import Milang.Parser (parseProgram)
import Milang.CHeader (parseCHeader, CFunSig(..))

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

-- | Cache of already-resolved imports (path → resolved AST)
type ImportCache = IORef (Map.Map FilePath Expr)

-- | Accumulated link info
type LinkRef = IORef LinkInfo

-- | Resolve all `import "path"` calls in an AST by loading and parsing files.
-- Returns the resolved AST and accumulated link info for gcc.
resolveImports :: FilePath -> Expr -> IO (Either String (Expr, LinkInfo))
resolveImports basePath expr = do
  cache <- newIORef Map.empty
  linkRef <- newIORef emptyLinkInfo
  let baseDir = takeDirectory basePath
  result <- resolveExpr cache linkRef baseDir expr
  case result of
    Left err -> pure $ Left err
    Right ast -> do
      li <- readIORef linkRef
      pure $ Right (ast, li)

addLinkInfo :: LinkRef -> LinkInfo -> IO ()
addLinkInfo ref li = modifyIORef ref (mergeLinkInfo li)

resolveExpr :: ImportCache -> LinkRef -> FilePath -> Expr -> IO (Either String Expr)
resolveExpr _ _ _ e@(IntLit _)    = pure $ Right e
resolveExpr _ _ _ e@(FloatLit _)  = pure $ Right e
resolveExpr _ _ _ e@(StringLit _) = pure $ Right e
resolveExpr _ _ _ e@(Name _)      = pure $ Right e
resolveExpr _ _ _ e@(CFunction {}) = pure $ Right e

resolveExpr cache lr dir (BinOp op l r) = do
  l' <- resolveExpr cache lr dir l
  r' <- resolveExpr cache lr dir r
  pure $ BinOp op <$> l' <*> r'

-- Intercept `import "path" { opts }` — With (App (Name "import") (StringLit path)) bindings
resolveExpr cache lr dir (With (App (Name "import") (StringLit path)) opts) = do
  let pathStr = T.unpack path
      fullPath = normalise (dir </> pathStr)
  -- Extract link options from the With bindings
  importOpts <- extractImportOpts dir opts
  addLinkInfo lr importOpts
  -- Also auto-detect for .h imports
  when (takeExtension pathStr == ".h") $ do
    autoInfo <- autoLinkInfo fullPath
    addLinkInfo lr autoInfo
  resolveImportPath cache lr dir fullPath pathStr

-- Intercept `import "path"` — App (Name "import") (StringLit path)
resolveExpr cache lr dir (App (Name "import") (StringLit path)) = do
  let pathStr = T.unpack path
      fullPath = normalise (dir </> pathStr)
  -- Auto-detect link info for .h imports
  when (takeExtension pathStr == ".h") $ do
    autoInfo <- autoLinkInfo fullPath
    addLinkInfo lr autoInfo
  resolveImportPath cache lr dir fullPath pathStr

resolveExpr cache lr dir (App f x) = do
  f' <- resolveExpr cache lr dir f
  x' <- resolveExpr cache lr dir x
  pure $ App <$> f' <*> x'

resolveExpr cache lr dir (Lam p b) = do
  b' <- resolveExpr cache lr dir b
  pure $ Lam p <$> b'

resolveExpr cache lr dir (With body bs) = do
  body' <- resolveExpr cache lr dir body
  bs' <- resolveBindings cache lr dir bs
  pure $ With <$> body' <*> bs'

resolveExpr cache lr dir (Record tag bs) = do
  bs' <- resolveBindings cache lr dir bs
  pure $ Record tag <$> bs'

resolveExpr cache lr dir (FieldAccess e field) = do
  e' <- resolveExpr cache lr dir e
  pure $ FieldAccess <$> e' <*> pure field

resolveExpr cache lr dir (Namespace bs) = do
  bs' <- resolveBindings cache lr dir bs
  pure $ Namespace <$> bs'

resolveExpr cache lr dir (Case scrut alts) = do
  scrut' <- resolveExpr cache lr dir scrut
  alts' <- resolveAlts cache lr dir alts
  pure $ Case <$> scrut' <*> alts'

-- | Common import resolution logic
resolveImportPath :: ImportCache -> LinkRef -> FilePath -> FilePath -> String -> IO (Either String Expr)
resolveImportPath cache lr dir fullPath pathStr = do
  cached <- Map.lookup fullPath <$> readIORef cache
  case cached of
    Just expr -> pure $ Right expr
    Nothing -> do
      result <- case takeExtension pathStr of
        ".h" -> loadCHeader fullPath
        _    -> loadFile cache lr fullPath
      case result of
        Left err -> pure $ Left err
        Right expr -> do
          modifyIORef cache (Map.insert fullPath expr)
          pure $ Right expr

-- | Extract link options from import { ... } bindings
extractImportOpts :: FilePath -> [Binding] -> IO LinkInfo
extractImportOpts dir bs = do
  let flags = concatMap getFlag bs
      srcs  = concatMap (getSrc dir) bs
  pkgFlags <- concat <$> mapM getPkgConfig bs
  pure $ LinkInfo (flags ++ pkgFlags) srcs
  where
    getFlag (Binding "flags" _ _ (StringLit s)) = words (T.unpack s)
    getFlag _ = []
    getSrc d (Binding "src" _ _ (StringLit s)) = [normalise (d </> T.unpack s)]
    getSrc _ _ = []
    getPkgConfig (Binding "pkg" _ _ (StringLit pkg)) = do
      let pkgStr = T.unpack pkg
      (ec1, cflags, _) <- readProcessWithExitCode
        "pkg-config" ["--cflags", pkgStr] ""
      (ec2, libs, _) <- readProcessWithExitCode
        "pkg-config" ["--libs", pkgStr] ""
      pure $ case (ec1, ec2) of
        (ExitSuccess, ExitSuccess) -> words cflags ++ words libs
        _ -> []  -- pkg-config failed, silently skip
    getPkgConfig _ = pure []

-- | Auto-detect link info for .h imports
autoLinkInfo :: FilePath -> IO LinkInfo
autoLinkInfo hdrPath = do
  -- Check built-in system header map
  let sysFlags = systemHeaderFlags hdrPath
  -- Add -I for the header's directory so gcc can find it
  let hdrDir = takeDirectory hdrPath
      includeFlag = if null hdrDir || hdrDir == "." then [] else ["-I" ++ hdrDir]
  -- Check for companion .c file
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

-- | Load and resolve a milang file, returning its top-level Namespace
loadFile :: ImportCache -> LinkRef -> FilePath -> IO (Either String Expr)
loadFile cache lr path = do
  src <- TIO.readFile path
  case parseProgram path src of
    Left err -> pure $ Left (show err)
    Right ast -> resolveExpr cache lr (takeDirectory path) ast

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
        }

resolveBindings :: ImportCache -> LinkRef -> FilePath -> [Binding] -> IO (Either String [Binding])
resolveBindings cache lr dir = go
  where
    go [] = pure $ Right []
    go (b:bs) = do
      body' <- resolveExpr cache lr dir (bindBody b)
      rest <- go bs
      pure $ do
        b' <- body'
        bs' <- rest
        Right (b { bindBody = b' } : bs')

resolveAlts :: ImportCache -> LinkRef -> FilePath -> [Alt] -> IO (Either String [Alt])
resolveAlts cache lr dir = go
  where
    go [] = pure $ Right []
    go (a:as) = do
      body' <- resolveExpr cache lr dir (altBody a)
      rest <- go as
      pure $ do
        b' <- body'
        as' <- rest
        Right (a { altBody = b' } : as')
