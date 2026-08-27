{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Kip.Playground
Description : Reusable one-shot execution API for playground invocations.

This module is the /single-run boundary/ for playground execution.

It intentionally provides a request/response API that can be called repeatedly
by different hosts:

- the classic command-style executable (`app/Playground.hs`)
- the reactor-style exported function (`app/PlaygroundReactor.hs`)

Each call to 'runPlaygroundRequest' allocates fresh parser/type/eval state.
That is the key invariant required by the playground runtime:
definitions created in one run must not leak into the next run unless they
come from files loaded again in that run.
-}
module Kip.Playground
  ( PlaygroundMode(..)
  , PlaygroundRequest(..)
  , PlaygroundOutput(..)
  , runPlaygroundRequest
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class
import Control.Monad.Reader (runReaderT)
import Data.Char (toLower)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Exit (die)
import System.FilePath (takeDirectory, takeFileName)
import System.IO (stderr)

import Language.Foma
import Kip.AST
import Kip.Cache
import Kip.Codegen.JS (codegenProgram, pruneProgramTaggedStmts)
import Kip.Parser
import Kip.Render
import Kip.Runner
import Kip.TypeCheck

-- | Supported execution modes for a single playground request.
data PlaygroundMode
  = PlaygroundExec
  -- ^ Parse, type-check, and evaluate the input files.
  | PlaygroundBuild
  -- ^ Parse and type-check only, without evaluating expression statements.
  | PlaygroundCodegen Text
  -- ^ Emit code for the named target. Currently supports @\"js\"@.
  deriving (Eq, Show)

{- |
Input for one isolated playground invocation.

The host is expected to supply @prFiles@ per run. In the WASI playground this
is usually a virtual file path like @/main.kip@.
-}
data PlaygroundRequest =
  PlaygroundRequest
    { -- | Requested run behavior.
      prMode :: PlaygroundMode
      -- | Input file(s) for the invocation.
    , prFiles :: [FilePath]
      -- | Additional module search directories.
    , prIncludeDirs :: [FilePath]
      -- | Diagnostic language.
    , prLang :: Lang
      -- | Whether to skip implicit prelude loading.
    , prNoPrelude :: Bool
    }

-- | Structured output of a single playground execution.
data PlaygroundOutput
  -- | No text payload (e.g. normal @--exec@ / @--build@ flow).
  = PlaygroundNoOutput
  -- | Text payload (currently for code generation mode).
  | PlaygroundTextOutput Text
  deriving (Eq, Show)

-- | Accumulated code generation state: typed statements tagged with the file
-- they came from, parser and typechecker state, and the set of loaded files.
type CodegenState = ([(FilePath, Stmt Ann)], ParserState, TCState, Set FilePath)

-- | Optional callbacks invoked when a module starts and finishes compiling,
-- used to report progress from long code generation runs.
type CodegenProgressHooks = Maybe (FilePath -> IO (), FilePath -> IO ())

{- |
Execute one request with fresh compiler and evaluator state.

Execution steps:

1. Resolve data files (@trmorph@ and stdlib root).
2. Build fresh morphology/render caches for this invocation.
3. Load prelude state once for this invocation.
4. Execute requested mode.

This function does /not/ persist user definitions across invocations by design.
-}
runPlaygroundRequest :: PlaygroundRequest -- ^ Request describing the mode, inputs, and options.
                     -> IO PlaygroundOutput -- ^ Text payload for code generation, otherwise no output.
runPlaygroundRequest req = do
  progressEnabled <- isPlaygroundProgressEnabled
  let reportProgress pct label =
        when (progressEnabled && isCodegenMode (prMode req)) $
          emitPlaygroundProgress pct label
      trLabel tr en = if prLang req == LangTr then tr else en
  reportProgress 5 "prepare-data"
  trmorphPath <- locateTrmorph (prLang req)
  libDir <- locateLibDir (prLang req)
  reportProgress 12 "load-morphology"
  fsm <- fsmReadBinaryFile trmorphPath
  reportProgress 20 "init-caches"
  renderCache <- newRenderCache
  let moduleDirs = uniquePreserve (libDir : prIncludeDirs req)
      renderCtx = RenderCtx (prLang req) renderCache fsm
  reportProgress 30 "load-prelude"
  (preludePst, preludeTC, preludeEval, preludeLoaded) <-
    runReaderT (loadPreludeState (prNoPrelude req) moduleDirs renderCache fsm) renderCtx
  reportProgress 45 "prelude-ready"
  case prMode req of
    PlaygroundExec -> do
      when (null (prFiles req)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFile) renderCtx
      _ <- runReaderT (runFiles False preludePst preludeTC preludeEval moduleDirs preludeLoaded (prFiles req)) renderCtx
      return PlaygroundNoOutput
    PlaygroundBuild -> do
      when (null (prFiles req)) $
        die . T.unpack =<< runReaderT (renderMsg MsgNeedFileOrDir) renderCtx
      buildTargets <- resolveBuildTargets (prFiles req)
      let extraDirs = uniquePreserve (map takeDirectory buildTargets)
          buildModuleDirs = uniquePreserve (moduleDirs ++ extraDirs)
      (preludeBuildPst, preludeBuildTC, preludeBuildEval, preludeBuildLoaded) <-
        runReaderT (loadPreludeState (prNoPrelude req) buildModuleDirs renderCache fsm) renderCtx
      _ <- runReaderT (runFiles True preludeBuildPst preludeBuildTC preludeBuildEval buildModuleDirs preludeBuildLoaded buildTargets) renderCtx
      return PlaygroundNoOutput
    PlaygroundCodegen target ->
      case target of
        "js" -> do
          discoveredRef <- newIORef Set.empty
          completedRef <- newIORef (0 :: Int)
          maxPctRef <- newIORef (55 :: Int)
          let reportCodegenProgress pct label = do
                cur <- readIORef maxPctRef
                let bounded = max 0 (min 100 pct)
                    monotonic = max cur bounded
                modifyIORef' maxPctRef (const monotonic)
                reportProgress monotonic label
              reportModulePhase path done = do
                discovered <- readIORef discoveredRef
                completed <- readIORef completedRef
                let d = max 1 (Set.size discovered)
                    c = max 0 (min d completed)
                    ratio = fromIntegral c / fromIntegral d :: Double
                    base = if done then 56 else 55
                    spanPct = 37 :: Int
                    pct = base + floor (ratio * fromIntegral spanPct)
                    label
                      | done =
                          trLabel
                            ("modül tamamlandı: " <> T.pack (takeFileName path))
                            ("module done: " <> T.pack (takeFileName path))
                      | otherwise =
                          trLabel
                            ("modül işleniyor: " <> T.pack (takeFileName path))
                            ("processing module: " <> T.pack (takeFileName path))
                reportCodegenProgress pct label
              onModuleStart path = do
                modifyIORef' discoveredRef (Set.insert path)
                reportModulePhase path False
              onModuleDone path = do
                modifyIORef' completedRef (+ 1)
                reportModulePhase path True
          reportProgress 52 (trLabel "kod üretimi başlatılıyor" "starting code generation")
          reportProgress 55 "resolve-modules"
          js <- runReaderT (emitJsFilesWithDeps moduleDirs preludePst preludeTC (prFiles req) (Just (onModuleStart, onModuleDone))) renderCtx
          reportProgress 98 (trLabel "çıktı yazılıyor" "writing output")
          return (PlaygroundTextOutput js)
        _ ->
          die . T.unpack =<< runReaderT (renderMsg (MsgUnknownCodegenTarget target)) renderCtx

-- | Check whether a mode requests code generation.
isCodegenMode :: PlaygroundMode -- ^ Mode to classify.
              -> Bool -- ^ 'True' for 'PlaygroundCodegen'.
isCodegenMode (PlaygroundCodegen _) = True
isCodegenMode _ = False

-- | Read @KIP_PLAYGROUND_PROGRESS@ to decide whether to emit progress lines.
isPlaygroundProgressEnabled :: IO Bool -- ^ 'True' unless the variable is unset or a falsey value.
isPlaygroundProgressEnabled = do
  mVal <- lookupEnv "KIP_PLAYGROUND_PROGRESS"
  return $
    case fmap (map toLower) mVal of
      Nothing -> False
      Just val -> val `notElem` ["", "0", "false", "no"]

-- | Write one machine-readable progress line to standard error.
emitPlaygroundProgress :: Int -- ^ Completion percentage.
                       -> Text -- ^ Label naming the current phase.
                       -> IO () -- ^ Writes a @KIP_PROGRESS:@ line to standard error.
emitPlaygroundProgress pct label =
  TIO.hPutStrLn stderr ("KIP_PROGRESS:" <> T.pack (show pct) <> ":" <> label)

-- | Locate @trmorph.fst@ path with data-dir fallback behavior.
locateTrmorph :: Lang -- ^ Diagnostic language used if the file is missing.
              -> IO FilePath -- ^ Path to the transducer; exits the process when absent.
locateTrmorph lang = do
  path <- locateDataFile "vendor/trmorph.fst"
  exists <- doesFileExist path
  if exists
    then return path
    else die . T.unpack $ missingDataFileMessage lang "vendor/trmorph.fst"

-- | Locate stdlib root (@lib/temel.kip@) with data-dir fallback behavior.
locateLibDir :: Lang -- ^ Diagnostic language used if the library is missing.
             -> IO FilePath -- ^ Directory holding the standard library; exits the process when absent.
locateLibDir lang = do
  path <- locateDataFile "lib/temel.kip"
  exists <- doesFileExist path
  if exists
    then return (takeDirectory path)
    else die . T.unpack $ missingDataFileMessage lang "lib/temel.kip"

{- |
Generate one JS program text from entry files and transitive dependencies.

This intentionally starts from @giriş@ to mirror the runtime prelude that
normal execution uses.
-}
emitJsFilesWithDeps :: [FilePath] -- ^ Directories searched when resolving imports.
                    -> ParserState -- ^ Initial parser state.
                    -> TCState -- ^ Initial typechecker state.
                    -> [FilePath] -- ^ Entry files to compile.
                    -> CodegenProgressHooks -- ^ Optional per-module progress callbacks.
                    -> RenderM Text -- ^ Complete JavaScript program text.
emitJsFilesWithDeps moduleDirs basePst baseTC files progressHooks = do
  preludePath <- resolveModulePath moduleDirs [] ([], T.pack "giriş")
  preludeAbs <- liftIO (canonicalizePathCached preludePath)
  let codegenTC = setTCOutputMode TCOutputCodegen baseTC
  (preludeStmts, pst', tcSt', loaded') <- emitJsFileWithDeps moduleDirs progressHooks ([], basePst, codegenTC, Set.empty) preludeAbs
  (taggedStmts, _, finalTC, _) <- foldM' (emitJsFileWithDeps moduleDirs progressHooks) (preludeStmts, pst', tcSt', loaded') files
  entryAbs <- liftIO (mapM canonicalizePathCached files)
  let resolvMap = Map.fromList (tcResolvedSigs finalTC)
      rootFiles = Set.fromList entryAbs
      prunedTagged = pruneProgramTaggedStmts resolvMap (`Set.member` rootFiles) taggedStmts
  return (codegenProgram resolvMap (map snd prunedTagged))

{- |
Parse/typecheck one file and recursively include dependencies for codegen.

Unlike runtime execution, this path accumulates typed statements and then emits
a single JS program.
-}
emitJsFileWithDeps :: [FilePath] -- ^ Directories searched when resolving imports.
                   -> CodegenProgressHooks -- ^ Optional per-module progress callbacks.
                   -> CodegenState -- ^ State accumulated from files compiled so far.
                   -> FilePath -- ^ File to compile.
                   -> RenderM CodegenState -- ^ State including this file and its dependencies.
emitJsFileWithDeps moduleDirs progressHooks (acc, pst, tcSt, loaded) path = do
  exists <- liftIO (doesFileExist path)
  unless exists $ do
    msg <- renderMsg (MsgFileNotFound path)
    liftIO (die (T.unpack msg))
  absPath <- liftIO (canonicalizePathCached path)
  if Set.member absPath loaded
    then return (acc, pst, tcSt, loaded)
    else do
      liftIO $ forM_ progressHooks $ \(onStart, _) -> onStart absPath
      input <- liftIO (TIO.readFile path)
      liftIO (parseFromFile pst input) >>= \case
        Left err -> do
          msg <- renderMsg (MsgParseError err)
          liftIO (die (T.unpack msg))
        Right (fileStmts, pst') -> do
          let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
              tyMods = parserTyMods pst'
              loaded' = Set.insert absPath loaded
          let loadStmts = [(dirPath, name) | Load dirPath name <- fileStmts]
          (depStmts, pst'', tcSt', loaded'') <- foldM' (emitJsLoad moduleDirs progressHooks) ([], pst', tcSt, loaded') loadStmts
          liftIO (runTCM (registerForwardDecls fileStmts) tcSt') >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (_, tcStWithDecls) ->
              liftIO (runTCM (mapM tcStmt fileStmts) tcStWithDecls) >>= \case
                Left tcErr -> do
                  msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons tyMods)
                  liftIO (die (T.unpack msg))
                Right (typedStmts, tcSt'') ->
                  let filteredStmts = filter (not . isLoadStmt) typedStmts
                      taggedStmts = [(absPath, stmt) | stmt <- filteredStmts]
                  in do
                    liftIO $ forM_ progressHooks $ \(_, onDone) -> onDone absPath
                    return (acc ++ depStmts ++ taggedStmts, pst'', tcSt'', loaded'')

-- | Check whether a statement is @Load@.
isLoadStmt :: Stmt Ann -- ^ Statement to classify.
           -> Bool -- ^ 'True' for a module import.
isLoadStmt (Load _ _) = True
isLoadStmt _ = False

-- | Load one dependency module for JS code generation.
emitJsLoad :: [FilePath] -- ^ Directories searched when resolving imports.
           -> CodegenProgressHooks -- ^ Optional per-module progress callbacks.
           -> CodegenState -- ^ State accumulated from files compiled so far.
           -> ([Text], Identifier) -- ^ Directory segments and name of the imported module.
           -> RenderM CodegenState -- ^ State including the loaded module.
emitJsLoad moduleDirs progressHooks (acc, pst, tcSt, loaded) (dirPath, name) = do
  path <- resolveModulePath moduleDirs dirPath name
  absPath <- liftIO (canonicalizePathCached path)
  emitJsFileWithDeps moduleDirs progressHooks (acc, pst, tcSt, loaded) absPath
