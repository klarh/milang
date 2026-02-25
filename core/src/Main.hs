{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr, withFile, IOMode(..), stdout)
import System.Process (readProcessWithExitCode)
import System.Directory (removeFile, doesFileExist, getCurrentDirectory)
import System.FilePath (dropExtension, takeDirectory, takeExtension, takeBaseName, (</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.List (nub, isPrefixOf)

import Core.Syntax
import Core.Parser (parseProgram)
import Core.Reduce (reduce, emptyEnv, Warning(..))
import Core.Codegen (codegen)
import Core.Prelude (preludeBindings)
import Core.CHeader (parseCHeader, CFunSig(..))

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
  cache      <- newIORef (Map.empty :: Map.Map FilePath Expr)
  inProgress <- newIORef (Set.empty :: Set.Set FilePath)
  circRefs   <- newIORef (Set.empty :: Set.Set FilePath)
  linkRef    <- newIORef emptyLinkInfo
  resolved <- resolveExpr cache inProgress circRefs linkRef (takeDirectory file) expr
  -- Lift circular-referenced modules as top-level bindings
  circSet  <- readIORef circRefs
  cacheMap <- readIORef cache
  li       <- readIORef linkRef
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
  pure (result, li)

-- | Generate a stable reference name for a module file path
moduleRefName :: FilePath -> T.Text
moduleRefName path = "__mod_" <> T.pack (map sanitize path) <> "__"
  where sanitize '/' = '_'; sanitize '.' = '_'; sanitize '-' = '_'; sanitize c = c

resolveExpr :: IORef (Map.Map FilePath Expr) -> IORef (Set.Set FilePath) -> IORef (Set.Set FilePath) -> IORef LinkInfo -> FilePath -> Expr -> IO Expr
-- Handle import' "path" ({opts}) — 2-arg import with options
resolveExpr cache inProg circRefs linkRef dir (App (App (Name "import'") (StringLit path)) (Record _ opts)) = do
  importOpts <- extractImportOpts dir opts
  modifyIORef linkRef (mergeLinkInfo importOpts)
  resolveExpr cache inProg circRefs linkRef dir (Import path)
-- Handle regular imports
resolveExpr cache inProg circRefs linkRef dir (Import path) = do
  let pathStr = T.unpack path
      relPath = dir </> pathStr
  cached <- readIORef cache
  case Map.lookup relPath cached of
    Just e  -> pure e
    Nothing -> case Map.lookup pathStr cached of
      Just e -> pure e
      Nothing -> do
        -- Check for C header import
        if takeExtension pathStr == ".h"
          then do
            let fullPath = if head pathStr == '/' then pathStr else relPath
            autoInfo <- autoLinkInfo fullPath
            modifyIORef linkRef (mergeLinkInfo autoInfo)
            result <- loadCHeader fullPath
            case result of
              Left err -> pure $ Error (T.pack err)
              Right ns -> do
                modifyIORef cache (Map.insert relPath ns)
                pure ns
          else do
            progress <- readIORef inProg
            if Set.member relPath progress
              then do
                modifyIORef circRefs (Set.insert relPath)
                pure $ Name (moduleRefName relPath)
              else do
                exists <- doesFileExist relPath
                if not exists
                  then pure $ Error ("import not found: " <> path)
                  else do
                    src <- TIO.readFile relPath
                    case parseProgram relPath src of
                      Left err -> pure $ Error (T.pack (show err))
                      Right ast -> do
                        modifyIORef inProg (Set.insert relPath)
                        resolved <- resolveExpr cache inProg circRefs linkRef (takeDirectory relPath) ast
                        modifyIORef cache (Map.insert relPath resolved)
                        modifyIORef inProg (Set.delete relPath)
                        pure resolved
resolveExpr cache ip cr lr dir (Namespace bs) =
  Namespace <$> mapM (resolveBinding cache ip cr lr dir) bs
resolveExpr cache ip cr lr dir (App f x) =
  App <$> resolveExpr cache ip cr lr dir f <*> resolveExpr cache ip cr lr dir x
resolveExpr cache ip cr lr dir (BinOp op l r) =
  BinOp op <$> resolveExpr cache ip cr lr dir l <*> resolveExpr cache ip cr lr dir r
resolveExpr cache ip cr lr dir (Lam p b) =
  Lam p <$> resolveExpr cache ip cr lr dir b
resolveExpr cache ip cr lr dir (Record t bs) =
  Record t <$> mapM (resolveBinding cache ip cr lr dir) bs
resolveExpr cache ip cr lr dir (FieldAccess e f) =
  (\e' -> FieldAccess e' f) <$> resolveExpr cache ip cr lr dir e
resolveExpr cache ip cr lr dir (Case s alts) =
  Case <$> resolveExpr cache ip cr lr dir s <*> mapM (resolveAlt cache ip cr lr dir) alts
resolveExpr cache ip cr lr dir (Thunk e) = Thunk <$> resolveExpr cache ip cr lr dir e
resolveExpr cache ip cr lr dir (ListLit es) = ListLit <$> mapM (resolveExpr cache ip cr lr dir) es
resolveExpr cache ip cr lr dir (With e bs) =
  With <$> resolveExpr cache ip cr lr dir e <*> mapM (resolveBinding cache ip cr lr dir) bs
resolveExpr cache ip cr lr dir (Quote e) = Quote <$> resolveExpr cache ip cr lr dir e
resolveExpr cache ip cr lr dir (Splice e) = Splice <$> resolveExpr cache ip cr lr dir e
resolveExpr _ _ _ _ _ e = pure e  -- literals, names, errors

resolveBinding :: IORef (Map.Map FilePath Expr) -> IORef (Set.Set FilePath) -> IORef (Set.Set FilePath) -> IORef LinkInfo -> FilePath -> Binding -> IO Binding
resolveBinding cache ip cr lr dir b = do
  body' <- resolveExpr cache ip cr lr dir (bindBody b)
  pure b { bindBody = body' }

resolveAlt :: IORef (Map.Map FilePath Expr) -> IORef (Set.Set FilePath) -> IORef (Set.Set FilePath) -> IORef LinkInfo -> FilePath -> Alt -> IO Alt
resolveAlt cache ip cr lr dir (Alt p g body) = do
  body' <- resolveExpr cache ip cr lr dir body
  g' <- case g of
    Just ge -> Just <$> resolveExpr cache ip cr lr dir ge
    Nothing -> pure Nothing
  pure $ Alt p g' body'

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
      Binding { bindDomain = Value
              , bindName   = n
              , bindParams = []
              , bindBody   = CFunction hdr n r p
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
      binFile = dropExtension file ++ "_core"
      extraFlags = nub (linkFlags li)
      extraSrcs  = nub (linkSources li)
      extraIncls = nub (map ("-I" ++) (linkIncludes li))
      gccArgs = ["-O2", "-o", binFile, cFile, "-I" ++ cwd]
                ++ extraIncls ++ extraSrcs ++ extraFlags
  withFile cFile WriteMode $ \h -> codegen h preludeNames stripped
  (gccExit, _, gccErr) <- readProcessWithExitCode "gcc" gccArgs ""
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
