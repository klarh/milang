{-# LANGUAGE OverloadedStrings #-}
module Milang.Import (resolveImports, LinkInfo(..)) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
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

-- | Cache of already-resolved imports (path -> resolved AST)
type ImportCache = IORef (Map.Map FilePath Expr)

-- | Set of files currently being resolved (for cycle detection)
type InProgress = IORef (Set.Set FilePath)

-- | Accumulated link info
type LinkRef = IORef LinkInfo

-- | Resolution context passed through all resolve functions
data ResCtx = ResCtx
  { rcCache      :: ImportCache
  , rcInProgress :: InProgress
  , rcLinkRef    :: LinkRef
  }

-- | Resolve all `import "path"` calls in an AST by loading and parsing files.
-- Returns the resolved AST and accumulated link info for gcc.
resolveImports :: FilePath -> Expr -> IO (Either String (Expr, LinkInfo))
resolveImports basePath expr = do
  cache <- newIORef Map.empty
  inProg <- newIORef Set.empty
  linkRef <- newIORef emptyLinkInfo
  let ctx = ResCtx cache inProg linkRef
      baseDir = takeDirectory basePath
  result <- resolveExpr ctx baseDir expr
  case result of
    Left err -> pure $ Left err
    Right ast -> do
      li <- readIORef linkRef
      pure $ Right (ast, li)

addLinkInfo :: LinkRef -> LinkInfo -> IO ()
addLinkInfo ref li = modifyIORef ref (mergeLinkInfo li)

resolveExpr :: ResCtx -> FilePath -> Expr -> IO (Either String Expr)
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
      fullPath = normalise (dir </> pathStr)
  importOpts <- extractImportOpts dir opts
  addLinkInfo (rcLinkRef ctx) importOpts
  when (takeExtension pathStr == ".h") $ do
    autoInfo <- autoLinkInfo fullPath
    addLinkInfo (rcLinkRef ctx) autoInfo
  fst <$> resolveImportPath ctx fullPath pathStr

-- Intercept `import "path"`
resolveExpr ctx dir (App (Name "import") (StringLit path)) = do
  let pathStr = T.unpack path
      fullPath = normalise (dir </> pathStr)
  when (takeExtension pathStr == ".h") $ do
    autoInfo <- autoLinkInfo fullPath
    addLinkInfo (rcLinkRef ctx) autoInfo
  fst <$> resolveImportPath ctx fullPath pathStr

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

-- | Resolve an import path. Returns (Either String Expr, Bool) where the Bool
-- indicates whether this was a circular import (True = cycle detected).
resolveImportPath :: ResCtx -> FilePath -> String -> IO (Either String Expr, Bool)
resolveImportPath ctx fullPath pathStr = do
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
            _    -> loadFile ctx fullPath
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
    Left err -> pure $ Left (show err)
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
        _ -> []
    getPkgConfig _ = pure []

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

-- | Load and resolve a milang file.
-- Marks the file as in-progress to detect circular imports.
loadFile :: ResCtx -> FilePath -> IO (Either String Expr)
loadFile ctx path = do
  src <- TIO.readFile path
  case parseProgram path src of
    Left err -> pure $ Left (show err)
    Right ast -> do
      modifyIORef (rcInProgress ctx) (Set.insert path)
      result <- resolveExpr ctx (takeDirectory path) ast
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
        }

-- | Resolve bindings, detecting circular imports and marking them lazy.
resolveBindings :: ResCtx -> FilePath -> [Binding] -> IO (Either String [Binding])
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
resolveBindingBody :: ResCtx -> FilePath -> Binding -> IO (Either String Expr, Bool)
resolveBindingBody ctx dir b = case bindBody b of
  App (Name "import") (StringLit path) -> do
    let pathStr = T.unpack path
        fullPath = normalise (dir </> pathStr)
    when (takeExtension pathStr == ".h") $ do
      autoInfo <- autoLinkInfo fullPath
      addLinkInfo (rcLinkRef ctx) autoInfo
    resolveImportPath ctx fullPath pathStr

  With (App (Name "import") (StringLit path)) opts -> do
    let pathStr = T.unpack path
        fullPath = normalise (dir </> pathStr)
    importOpts <- extractImportOpts dir opts
    addLinkInfo (rcLinkRef ctx) importOpts
    when (takeExtension pathStr == ".h") $ do
      autoInfo <- autoLinkInfo fullPath
      addLinkInfo (rcLinkRef ctx) autoInfo
    resolveImportPath ctx fullPath pathStr

  _ -> do
    result <- resolveExpr ctx dir (bindBody b)
    pure (result, False)

resolveAlts :: ResCtx -> FilePath -> [Alt] -> IO (Either String [Alt])
resolveAlts ctx dir = go
  where
    go [] = pure $ Right []
    go (a:as) = do
      body' <- resolveExpr ctx dir (altBody a)
      rest <- go as
      pure $ do
        b' <- body'
        as' <- rest
        Right (a { altBody = b' } : as')
