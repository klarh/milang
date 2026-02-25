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

-- | Strip deeply nested Namespace content from circular imports for codegen.
-- Only strips Namespace nesting that contains circular module references
-- (detected by __mod_ Name prefixes). Non-circular module hierarchies are
-- left intact.
stripDeepModules :: Int -> Expr -> Expr
stripDeepModules maxD = go False 0
  where
    go _ d (Namespace bs)
      | d >= maxD && circular = StringLit "<circular>"
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
      in if d >= maxD && circ && not (isSimpleVal body')
         then b { bindBody = StringLit "<circular>" }
         else b { bindBody = body' }
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
resolveImports :: String -> Expr -> IO Expr
resolveImports file expr = do
  cache      <- newIORef (Map.empty :: Map.Map FilePath Expr)
  inProgress <- newIORef (Set.empty :: Set.Set FilePath)
  circRefs   <- newIORef (Set.empty :: Set.Set FilePath)
  resolved <- resolveExpr cache inProgress circRefs (takeDirectory file) expr
  -- Lift circular-referenced modules as top-level bindings
  circSet  <- readIORef circRefs
  cacheMap <- readIORef cache
  if Set.null circSet
    then pure resolved
    else do
      let modBinds = [ Binding { bindDomain = Value
                               , bindName   = moduleRefName p
                               , bindParams = []
                               , bindBody   = content
                               , bindPos    = Nothing
                               }
                     | p <- Set.toList circSet
                     , Just content <- [Map.lookup p cacheMap]
                     ]
      case resolved of
        Namespace bs -> pure $ Namespace (modBinds ++ bs)
        _ -> pure resolved

-- | Generate a stable reference name for a module file path
moduleRefName :: FilePath -> T.Text
moduleRefName path = "__mod_" <> T.pack (map sanitize path) <> "__"
  where sanitize '/' = '_'; sanitize '.' = '_'; sanitize '-' = '_'; sanitize c = c

resolveExpr :: IORef (Map.Map FilePath Expr) -> IORef (Set.Set FilePath) -> IORef (Set.Set FilePath) -> FilePath -> Expr -> IO Expr
resolveExpr cache inProg circRefs dir (Import path) = do
  let pathStr = T.unpack path
      relPath = dir </> pathStr
  cached <- readIORef cache
  case Map.lookup relPath cached of
    Just e  -> pure e
    Nothing -> do
      -- Also check by raw path (for pre-resolved absolute paths)
      case Map.lookup pathStr cached of
        Just e -> pure e
        Nothing -> do
          progress <- readIORef inProg
          if Set.member relPath progress
            then do
              -- Circular import: return a Name reference
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
                      resolved <- resolveExpr cache inProg circRefs (takeDirectory relPath) ast
                      modifyIORef cache (Map.insert relPath resolved)
                      modifyIORef inProg (Set.delete relPath)
                      pure resolved
resolveExpr cache ip cr dir (Namespace bs) =
  Namespace <$> mapM (resolveBinding cache ip cr dir) bs
resolveExpr cache ip cr dir (App f x) =
  App <$> resolveExpr cache ip cr dir f <*> resolveExpr cache ip cr dir x
resolveExpr cache ip cr dir (BinOp op l r) =
  BinOp op <$> resolveExpr cache ip cr dir l <*> resolveExpr cache ip cr dir r
resolveExpr cache ip cr dir (Lam p b) =
  Lam p <$> resolveExpr cache ip cr dir b
resolveExpr cache ip cr dir (Record t bs) =
  Record t <$> mapM (resolveBinding cache ip cr dir) bs
resolveExpr cache ip cr dir (FieldAccess e f) =
  (\e' -> FieldAccess e' f) <$> resolveExpr cache ip cr dir e
resolveExpr cache ip cr dir (Case s alts) =
  Case <$> resolveExpr cache ip cr dir s <*> mapM (resolveAlt cache ip cr dir) alts
resolveExpr cache ip cr dir (Thunk e) = Thunk <$> resolveExpr cache ip cr dir e
resolveExpr cache ip cr dir (ListLit es) = ListLit <$> mapM (resolveExpr cache ip cr dir) es
resolveExpr cache ip cr dir (With e bs) =
  With <$> resolveExpr cache ip cr dir e <*> mapM (resolveBinding cache ip cr dir) bs
resolveExpr cache ip cr dir (Quote e) = Quote <$> resolveExpr cache ip cr dir e
resolveExpr cache ip cr dir (Splice e) = Splice <$> resolveExpr cache ip cr dir e
resolveExpr _ _ _ _ e = pure e  -- literals, names, errors

resolveBinding :: IORef (Map.Map FilePath Expr) -> IORef (Set.Set FilePath) -> IORef (Set.Set FilePath) -> FilePath -> Binding -> IO Binding
resolveBinding cache ip cr dir b = do
  body' <- resolveExpr cache ip cr dir (bindBody b)
  pure b { bindBody = body' }

resolveAlt :: IORef (Map.Map FilePath Expr) -> IORef (Set.Set FilePath) -> IORef (Set.Set FilePath) -> FilePath -> Alt -> IO Alt
resolveAlt cache ip cr dir (Alt p g body) = do
  body' <- resolveExpr cache ip cr dir body
  g' <- case g of
    Just ge -> Just <$> resolveExpr cache ip cr dir ge
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
  (reduced, _) <- loadAndReduce file
  let stripped = stripDeepModules 3 reduced
      cFile = dropExtension file ++ "_core.c"
      binFile = dropExtension file ++ "_core"
  withFile cFile WriteMode $ \h -> codegen h preludeNames stripped
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
