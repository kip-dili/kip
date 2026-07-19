{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bytecode cache support for compiled modules.
module Kip.Cache where

import GHC.Generics (Generic)
import Data.Binary
import Data.Binary.Get (runGetOrFail)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Version (showVersion)
import System.FilePath
import System.Directory
import System.Environment (getExecutablePath)
import Control.Exception (try, SomeException)
import Control.Monad (when, foldM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Crypto.Hash.SHA256 (hash, hashlazy)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashTable.IO as HT

import Kip.AST
import Kip.Parser (ParserState(..), MorphCache, newParserStateWithCtxAndCaches)
import Kip.TypeCheck (TCState(..), buildFuncSigsByArity, buildFuncRetByName)
import Kip.Eval (EvalState(..), runEvalM, evalStmtInFile)
import Language.Foma (FSM)
import Kip.Render (RenderCache, renderExpValue)
import Paths_kip (version)

-- | Memoized file hash cache for the current process.
-- Stores (mtime in microseconds, hash) so we can skip re-hashing unchanged files.
{-# NOINLINE hashCache #-}
hashCache :: IORef (Map.Map FilePath (Integer, ByteString))
hashCache = unsafePerformIO (newIORef Map.empty)

-- | Memoized canonical-path cache for the current process.
--
-- ==== Performance note (Optimization: canonical path interning)
-- Many startup code paths repeatedly canonicalize the same module and cache
-- paths. Interning canonicalization results avoids redundant filesystem calls.
{-# NOINLINE canonicalPathCache #-}
canonicalPathCache :: IORef (Map.Map FilePath FilePath)
canonicalPathCache = unsafePerformIO (newIORef Map.empty)

-- | Memoized compiler hash for the current process.
-- The executable hash is stable per process, so compute once.
{-# NOINLINE compilerHashCache #-}
compilerHashCache :: IORef (Maybe ByteString)
compilerHashCache = unsafePerformIO (newIORef Nothing)

-- | Memoized dependency validation results for the current process.
--
-- ==== Performance note (Optimization: cache validation short-circuit)
-- Multiple module caches often depend on the same library files. During a
-- warm startup we may validate the same @(path,size,mtime,hash)@ tuple many
-- times. Caching positive validations avoids repeated filesystem metadata reads
-- and hash checks across those modules.
{-# NOINLINE verifyPathCache #-}
verifyPathCache :: IORef (Map.Map (FilePath, Integer, Integer, ByteString) Bool)
verifyPathCache = unsafePerformIO (newIORef Map.empty)

-- | Reverse dependency graph (dep source file -> direct dependent source files).
--
-- ==== Performance note (Optimization: dependency invalidation propagation)
-- When one source changes, we propagate a dirty marker through this graph so
-- subsequent cache validations can fail fast without re-checking all metadata.
{-# NOINLINE reverseDepGraphCache #-}
reverseDepGraphCache :: IORef (Map.Map FilePath (Set.Set FilePath))
reverseDepGraphCache = unsafePerformIO (newIORef Map.empty)

-- | Dirty source files known to invalidate dependent module caches.
{-# NOINLINE dirtySourcesCache #-}
dirtySourcesCache :: IORef (Set.Set FilePath)
dirtySourcesCache = unsafePerformIO (newIORef Set.empty)

-- | Metadata stored alongside cached modules for validation.
data CacheMetadata = CacheMetadata
  { compilerHash :: !ByteString          -- ^ SHA256 of the compiler executable.
  , sourceHash   :: !ByteString          -- ^ SHA256 of the source file.
  , sourceSize   :: !Integer             -- ^ Source file size in bytes.
  , sourceMTime  :: !Integer             -- ^ Source file mtime in microseconds.
  , dependencies :: ![(FilePath, ByteString, Integer, Integer)]  -- ^ (path, hash, size, mtime) for deps.
  } deriving (Generic)

instance Binary CacheMetadata

-- | Fully cached module payload.
data CachedModule = CachedModule
  { metadata      :: !CacheMetadata -- ^ Cache validation metadata.
  , cachedStmts   :: ![Stmt Ann] -- ^ Parsed statements.
  , cachedTypedStmts :: ![Stmt Ann] -- ^ Type-checked statements.
  , cachedParser  :: !CachedParserState -- ^ Parser state snapshot.
  , cachedTC      :: !CachedTCState -- ^ Type checker state snapshot.
  , cachedEval    :: !CachedEvalState -- ^ Evaluator state snapshot.
  } deriving (Generic)

instance Binary CachedModule

-- | Cached prelude snapshot payload used for cold-start acceleration.
--
-- The snapshot stores fully merged parser/typechecker/evaluator states after
-- loading @lib/giriş.kip@ and transitive dependencies.
data CachedPrelude = CachedPrelude
  { preludeCompilerHash :: !ByteString
  , preludeFiles :: ![(FilePath, ByteString, Integer, Integer)] -- ^ (path, hash, size, mtime)
  , preludeLoaded :: ![FilePath]
  , preludeParser :: !CachedParserState
  , preludeTC :: !CachedTCState
  , preludeEval :: !CachedEvalState
  , preludePrimStmts :: ![(FilePath, Stmt Ann)]
    -- ^ Source path and 'PrimFunc' statement pairs needed to rebuild
    -- 'evalPrimFuncs' host callbacks (see 'Kip.Eval.evalPrimFuncs').
    --
    -- ==== Performance note (Optimization: prelude prim-func snapshot)
    -- Previously these were recovered at __every__ load by re-decoding the
    -- full @.iz@ cache of each prelude module (~25 MB across ~11 files) just
    -- to filter out a handful of 'PrimFunc' statements. We now do that
    -- decode-and-filter work once, when the snapshot is (re)written, and
    -- persist the small result directly. Loading the prelude snapshot then
    -- only replays these statements, with no extra file I/O or decoding.
  } deriving (Generic)

instance Binary CachedPrelude

-- | Serialized parser state subset needed to restore a module.
--
-- ==== Performance note (Optimization 10)
-- Stores serialized snapshots of parser morphology caches
-- ('pupsCache'/'pdownsCache') so repeated runs can avoid cold-start Foma
-- calls after loading @.iz@ files.
data CachedParserState = CachedParserState
  { pctx :: [Identifier] -- ^ Parser context identifiers.
  , pctors :: [Identifier] -- ^ Constructor identifiers.
  , ptyParams :: [Identifier] -- ^ Type parameter identifiers.
  , ptyCons :: [(Identifier, Int)] -- ^ Type constructor arities.
  , ptyMods :: [(Identifier, [Identifier])] -- ^ Type modifier expansions.
  , pprimTypes :: [Identifier] -- ^ Primitive type identifiers.
  , pfuncArities :: Map.Map Identifier (Set.Set Int) -- ^ Known function arities.
  , pdefSpans :: Map.Map Identifier [Span] -- ^ Definition spans.
  , pupsCache :: [(T.Text, [T.Text])] -- ^ Persisted morphology analysis cache.
  , pdownsCache :: [(T.Text, [T.Text])] -- ^ Persisted morphology generation cache.
  } deriving (Generic)

instance Binary CachedParserState

-- | Convert a parser state into a cached representation, including a full
-- snapshot of the shared morphology caches.
--
-- ==== Performance note (Optimization 10)
-- Materializes parser hash-table morphology caches into lists so they can be
-- persisted in the module cache.
--
-- Only the prelude snapshot ('saveCachedPrelude') should use this: it is the
-- single on-disk copy of the accumulated morphology cache that all other
-- module caches rely on being pre-populated at load time. Per-module
-- @.iz@ caches should use 'toCachedParserStateNoMorph' instead (see there
-- for why).
toCachedParserState ::
  ParserState -- ^ Parser state to serialize.
  -> IO CachedParserState -- ^ Compact cached payload.
toCachedParserState ps = do
  upsEntries <- HT.toList (parserUpsCache ps)
  downsEntries <- HT.toList (parserDownsCache ps)
  return
    CachedParserState
      { pctx = Set.toList (parserCtx ps)
      , pctors = parserCtors ps
      , ptyParams = parserTyParams ps
      , ptyCons = parserTyCons ps
      , ptyMods = parserTyMods ps
      , pprimTypes = parserPrimTypes ps
      , pfuncArities = parserFuncArities ps
      , pdefSpans = parserDefSpans ps
      , pupsCache = upsEntries
      , pdownsCache = downsEntries
      }

-- | Convert a parser state into a cached representation, omitting the
-- shared morphology caches.
--
-- ==== Performance note (Optimization: de-duplicate morphology cache)
-- The ups/downs morphology caches ('parserUpsCache'/'parserDownsCache') are
-- __shared__ across all modules loaded in a process and accumulate
-- monotonically (prelude entries plus whatever each subsequently loaded
-- file needed). Because 'toCachedParserState' snapshotted the caches in
-- full, every per-module @.iz@ file redundantly embedded a duplicate of the
-- entire accumulated cache -- multi-megabyte files consisting almost
-- entirely of prelude vocabulary already captured once in the prelude
-- snapshot ('CachedPrelude'). That bloat was paid twice: once encoding it
-- into every @.iz@ written, and again decoding it out of every @.iz@ read
-- back on a warm run.
--
-- Per-module caches only need the non-morphology parser state; any
-- morphology lookups specific to that file's own vocabulary are cheap to
-- redo against the FFI (a handful of words per file, batched) and get
-- merged back into the shared in-process cache as they happen.
toCachedParserStateNoMorph ::
  ParserState -- ^ Parser state to serialize.
  -> IO CachedParserState -- ^ Compact cached payload without morphology entries.
toCachedParserStateNoMorph ps =
  return
    CachedParserState
      { pctx = Set.toList (parserCtx ps)
      , pctors = parserCtors ps
      , ptyParams = parserTyParams ps
      , ptyCons = parserTyCons ps
      , ptyMods = parserTyMods ps
      , pprimTypes = parserPrimTypes ps
      , pfuncArities = parserFuncArities ps
      , pdefSpans = parserDefSpans ps
      , pupsCache = []
      , pdownsCache = []
      }

-- | Restore a parser state from its cached representation.
--
-- ==== Performance note (Optimization 10)
-- Rehydrates persisted morphology cache entries into the shared parser cache
-- tables before returning the parser state.
fromCachedParserState ::
  FSM -- ^ Morphology FSM handle.
  -> Maybe FilePath -- ^ Path to the cached module (for validation).
  -> MorphCache -- ^ Shared ups cache.
  -> MorphCache -- ^ Shared downs cache.
  -> CachedParserState -- ^ Cached parser snapshot.
  -> IO ParserState -- ^ Rehydrated parser state.
fromCachedParserState fsm cachePath upsCache downsCache CachedParserState{..} = do
  mapM_ (uncurry (HT.insert upsCache)) pupsCache
  mapM_ (uncurry (HT.insert downsCache)) pdownsCache
  -- Use the shared constructor so derived parser indices (for overload
  -- lookup) are rebuilt consistently after cache restore.
  return (newParserStateWithCtxAndCaches fsm (Set.fromList pctx) pctors ptyParams ptyCons ptyMods pprimTypes pfuncArities pdefSpans cachePath upsCache downsCache)

-- | Cached wrapper for the type checker state.
newtype CachedTCState = CachedTCState TCState
  deriving (Generic)

instance Binary CachedTCState

-- | Wrap a type checker state for caching.
toCachedTCState ::
  TCState -- ^ Type checker state to wrap.
  -> CachedTCState -- ^ Wrapped cached state.
toCachedTCState = CachedTCState

-- | Unwrap a cached type checker state.
--
-- ==== Performance note (A1)
-- 'TCState' stores overloadable entries as @Map k [v]@ encoded by flattening
-- each list entry; decoding via @Map.fromListWith (++)@ reverses per-key order.
-- We reverse those lists here to preserve declaration order expected by REPL
-- signature rendering and cached/non-cached parity.
fromCachedTCState ::
  CachedTCState -- ^ Cached state wrapper.
  -> TCState -- ^ Restored type checker state.
fromCachedTCState (CachedTCState s) =
  let funcs' = fmap reverse (tcFuncs s)
      sigs' = fmap reverse (tcFuncSigs s)
      sigRets' = tcFuncSigRets s
  in s
      { tcFuncs = funcs'
      , tcFuncSigs = sigs'
      , tcFuncSigsByArity = buildFuncSigsByArity sigs'
      , tcFuncRetByName = buildFuncRetByName sigRets'
      }

-- | Serialized evaluator state without closures.
data CachedEvalState = CachedEvalState
  { evals :: [(Identifier, Exp Ann)] -- ^ Cached value bindings.
  , efuncs :: [(Identifier, ([Arg Ann], [Clause Ann]))] -- ^ Cached function clauses.
  , eselectors :: [(Identifier, Int)] -- ^ Cached record selectors.
  , ectors :: [(Identifier, ([Ty Ann], Ty Ann))] -- ^ Cached constructors.
  , etyCons :: [(Identifier, Int)] -- ^ Cached type constructors.
  } deriving (Generic)

instance Binary CachedEvalState

-- | Convert an evaluator state into a cached representation.
toCachedEvalState ::
  EvalState -- ^ Evaluator state to serialize.
  -> CachedEvalState -- ^ Cached evaluator payload.
toCachedEvalState es =
  CachedEvalState
    { evals = Map.toList (evalVals es)
    , efuncs = [(k, v) | (k, vs) <- Map.toList (evalFuncs es), v <- vs]
    , eselectors = [(k, v) | (k, vs) <- Map.toList (evalSelectors es), v <- vs]
    , ectors = Map.toList (evalCtors es)
    , etyCons = Map.toList (evalTyCons es)
    }

-- | Restore an evaluator state from a cached representation.
fromCachedEvalState ::
  RenderCache -- ^ Render cache for value printing.
  -> FSM -- ^ Morphology FSM handle.
  -> CachedEvalState -- ^ Cached evaluator snapshot.
  -> EvalState -- ^ Rehydrated evaluator state.
fromCachedEvalState cache fsm CachedEvalState{..} =
  MkEvalState
    { evalVals = Map.fromList evals
    , evalFuncs = Map.fromListWith (++) [(k, [v]) | (k, v) <- efuncs]
    , evalPrimFuncs = Map.empty -- Rebuilt at load time
    , evalSelectors = Map.fromListWith (++) [(k, [v]) | (k, v) <- eselectors]
    , evalCtors = Map.fromList ectors
    , evalTyCons = Map.fromList etyCons
    , evalCurrentFile = Nothing
    , evalArgs = []
    , evalRender = renderExpValue cache fsm
    , evalRandState = Nothing
    }

-- | Map a source file to its cache file path.
cacheFilePath ::
  FilePath -- ^ Source path to a `.kip` file.
  -> FilePath -- ^ Corresponding `.iz` cache path.
cacheFilePath path = replaceExtension path ".iz"

-- | Default on-disk location for the prelude snapshot.
--
-- We keep this in @~/.kip/cache@ so all executables (`kip`, `kip-lsp`,
-- `kip-playground`) can reuse the same startup artifact.
preludeSnapshotPath :: IO FilePath
preludeSnapshotPath = do
  home <- getHomeDirectory
  let dir = home </> ".kip" </> "cache"
  createDirectoryIfMissing True dir
  return (dir </> "prelude.izp")

-- | Canonicalize a path with process-local memoization.
canonicalizePathCached ::
  FilePath -- ^ Path to canonicalize.
  -> IO FilePath -- ^ Canonical absolute path.
canonicalizePathCached path = do
  cached <- readIORef canonicalPathCache
  case Map.lookup path cached of
    Just absPath -> return absPath
    Nothing -> do
      absPath <- canonicalizePath path
      modifyIORef' canonicalPathCache (Map.insert path absPath)
      return absPath

-- | Load a cached module from disk if it is valid.
--
-- ==== Performance note (Optimization: header-first cache validation)
-- 'CachedModule' encodes its 'metadata' field first, followed by the much
-- larger AST/parser/typechecker/evaluator payload. Previously we fully
-- decoded the entire payload before checking whether the cache was even
-- valid for the current compiler/sources, wasting the decode on every
-- invalidated cache (e.g. after editing a dependency). We now decode only
-- the 'CacheMetadata' prefix first (binary's derived product encoding is a
-- plain field-by-field concatenation with no reordering, so this reads
-- exactly the same bytes the full decode would read for that field) and
-- only pay for decoding the rest of the payload once metadata validation
-- passes.
loadCachedModule ::
  FilePath -- ^ Cache file path.
  -> IO (Maybe CachedModule) -- ^ Cached module when valid.
loadCachedModule path = do
  absCachePath <- canonicalizePathCached path
  exists <- doesFileExist absCachePath
  if not exists
    then return Nothing
    else do
      res <- try (BS.readFile absCachePath)
      case res of
        Left (_ :: SomeException) -> return Nothing
        Right bytes ->
          case runGetOrFail (get :: Get CacheMetadata) (fromStrict bytes) of
            Left _ -> return Nothing
            Right (_, _, meta) -> do
              valid <- isCacheValidMeta absCachePath meta
              if not valid
                then return Nothing
                else case decodeOrFail (fromStrict bytes) of
                  Left _ -> return Nothing
                  Right (_, _, m) -> return (Just m)

-- | Persist a cached module to disk.
saveCachedModule ::
  FilePath -- ^ Cache file path.
  -> CachedModule -- ^ Module payload to write.
  -> IO () -- ^ Writes the cache file.
saveCachedModule path m = do
  absPath <- canonicalizePathCached path
  let bytes = toStrict (encode m)
      newSize = fromIntegral (BS.length bytes)
  mCurrentMeta <- getFileMeta absPath
  shouldWrite <-
    case mCurrentMeta of
      Just (oldSize, _) | oldSize == newSize -> do
        oldRes <- try (BS.readFile absPath)
        case oldRes of
          Left (_ :: SomeException) -> return True
          Right oldBytes -> return (oldBytes /= bytes)
      _ -> return True
  when shouldWrite (BS.writeFile absPath bytes)

-- | Check whether a cached module is valid for the current compiler and sources.
isCacheValid ::
  FilePath -- ^ Cache file path.
  -> CachedModule -- ^ Cached module to validate.
  -> IO Bool -- ^ True when metadata matches current sources/compiler.
isCacheValid path m = isCacheValidMeta path (metadata m)

-- | Check whether cache metadata is valid for the current compiler and
-- sources, without requiring the full decoded 'CachedModule'.
isCacheValidMeta ::
  FilePath -- ^ Cache file path.
  -> CacheMetadata -- ^ Cache metadata to validate.
  -> IO Bool -- ^ True when metadata matches current sources/compiler.
isCacheValidMeta path meta = do
  absCachePath <- canonicalizePathCached path
  let sourcePathRaw = replaceExtension absCachePath ".kip"
  mCompilerHash <- getCompilerHash
  case mCompilerHash of
    Nothing -> return False
    Just currentCompilerHash ->
      if compilerHash meta /= currentCompilerHash
        then return False
        else do
          sourcePath <- canonicalizePathCached sourcePathRaw
          registerDependencyEdges sourcePath (map (\(p, _, _, _) -> p) (dependencies meta))
          dirty <- readIORef dirtySourcesCache
          if Set.member sourcePath dirty
            then return False
            else validateSources sourcePath meta
  where
    -- | Validate the source file and dependency hashes.
    validateSources :: FilePath -- ^ Canonical source path.
                    -> CacheMetadata -- ^ Cached metadata.
                    -> IO Bool -- ^ True when sources match.
    validateSources sourcePath meta = do
      sourceOk <- verifyPath sourcePath (sourceHash meta) (sourceSize meta) (sourceMTime meta)
      if not sourceOk
        then markDirtySource sourcePath >> return False
        else do
          depsValid <- mapM (\(depPathRaw, depHash, depSize, depMTime) -> do
            depPath <- canonicalizePathCached depPathRaw
            verifyPath depPath depHash depSize depMTime) (dependencies meta)
          return (and depsValid)

    -- | Verify a file by fast metadata check and fallback hashing.
    -- This avoids hashing unchanged dependencies on hot build paths.
    verifyPath :: FilePath -- ^ Dependency path.
               -> ByteString -- ^ Expected hash.
               -> Integer -- ^ Expected size.
               -> Integer -- ^ Expected mtime.
               -> IO Bool -- ^ True when dependency matches.
    verifyPath depPath depHash depSize depMTime = do
      dirty <- readIORef dirtySourcesCache
      if Set.member depPath dirty
        then return False
        else do
          let cacheKey = (depPath, depSize, depMTime, depHash)
          validationCache <- readIORef verifyPathCache
          case Map.lookup cacheKey validationCache of
            Just ok -> return ok
            Nothing -> do
              mMeta <- getFileMeta depPath
              ok <-
                case mMeta of
                  Just (size, mtime)
                    | size == depSize && mtime == depMTime -> return True
                  _ -> do
                    mDepHash <- hashFile depPath
                    return (mDepHash == Just depHash)
              if ok
                then modifyIORef' verifyPathCache (Map.insert cacheKey True)
                else markDirtySource depPath
              return ok

-- | Register reverse edges for one source file to its direct dependencies.
registerDependencyEdges :: FilePath -> [FilePath] -> IO ()
registerDependencyEdges source deps = do
  deps' <- mapM canonicalizePathCached deps
  modifyIORef'
    reverseDepGraphCache
    (\g ->
      foldl
        (\acc dep -> Map.insertWith Set.union dep (Set.singleton source) acc)
        g
        deps')

-- | Mark one source file dirty and propagate to transitive dependents.
markDirtySource :: FilePath -> IO ()
markDirtySource sourceRaw = do
  source <- canonicalizePathCached sourceRaw
  graph <- readIORef reverseDepGraphCache
  dirty <- readIORef dirtySourcesCache
  let go [] seen = seen
      go (x:xs) seen
        | Set.member x seen = go xs seen
        | otherwise =
            let parents = Set.toList (Map.findWithDefault Set.empty x graph)
            in go (parents ++ xs) (Set.insert x seen)
      newlyDirty = go [source] Set.empty
  writeIORef dirtySourcesCache (Set.union dirty newlyDirty)
  
-- | Load a prelude snapshot if present and valid.
loadCachedPrelude ::
  FilePath -- ^ Snapshot file path.
  -> RenderCache -- ^ Render cache for evaluator restore.
  -> FSM -- ^ Morphology FSM handle.
  -> MorphCache -- ^ Shared parser ups cache.
  -> MorphCache -- ^ Shared parser downs cache.
  -> IO (Maybe (ParserState, TCState, EvalState, Set.Set FilePath))
loadCachedPrelude snapshotPath cache fsm upsCache downsCache = do
  absSnapshotPath <- canonicalizePathCached snapshotPath
  exists <- doesFileExist absSnapshotPath
  if not exists
    then return Nothing
    else do
      res <- try (BS.readFile absSnapshotPath)
      case res of
        Left (_ :: SomeException) -> return Nothing
        Right bytes ->
          case decodeOrFail (fromStrict bytes) of
            Left _ -> return Nothing
            Right (_, _, preludeSnap) -> do
              valid <- isCachedPreludeValid preludeSnap
              if not valid
                then return Nothing
                else do
                  pst <- fromCachedParserState fsm Nothing upsCache downsCache (preludeParser preludeSnap)
                  let tcSt = fromCachedTCState (preludeTC preludeSnap)
                      evalBase = fromCachedEvalState cache fsm (preludeEval preludeSnap)
                  evalSt <- replayPreludePrimStmts (preludePrimStmts preludeSnap) evalBase
                  loadedSet <- Set.fromList <$> mapM canonicalizePathCached (preludeLoaded preludeSnap)
                  return (Just (pst, tcSt, evalSt, loadedSet))

-- | Persist a fully merged prelude snapshot.
saveCachedPrelude ::
  FilePath -- ^ Snapshot file path.
  -> ParserState -- ^ Parser state after prelude load.
  -> TCState -- ^ Typechecker state after prelude load.
  -> EvalState -- ^ Evaluator state after prelude load.
  -> Set.Set FilePath -- ^ Loaded source files in prelude graph.
  -> IO ()
saveCachedPrelude snapshotPath pst tcSt evalSt loaded = do
  mCompilerHash <- getCompilerHash
  case mCompilerHash of
    Nothing -> return ()
    Just compHash -> do
      canonicalLoaded <- mapM canonicalizePathCached (Set.toList loaded)
      files <- mapM fileFingerprint canonicalLoaded
      case sequence files of
        Nothing -> return ()
        Just fps -> do
          cachedParser <- toCachedParserState pst
          primStmts <- collectPreludePrimStmts canonicalLoaded
          let snap =
                CachedPrelude
                  { preludeCompilerHash = compHash
                  , preludeFiles = fps
                  , preludeLoaded = canonicalLoaded
                  , preludeParser = cachedParser
                  , preludeTC = toCachedTCState tcSt
                  , preludeEval = toCachedEvalState evalSt
                  , preludePrimStmts = primStmts
                  }
          absSnapshotPath <- canonicalizePathCached snapshotPath
          let bytes = toStrict (encode snap)
          -- Same write dedup shortcut as module caches.
          mCurrentMeta <- getFileMeta absSnapshotPath
          shouldWrite <-
            case mCurrentMeta of
              Just (oldSize, _) | oldSize == fromIntegral (BS.length bytes) -> do
                oldRes <- try (BS.readFile absSnapshotPath)
                case oldRes of
                  Left (_ :: SomeException) -> return True
                  Right oldBytes -> return (oldBytes /= bytes)
              _ -> return True
          when shouldWrite (BS.writeFile absSnapshotPath bytes)

-- | Validate a prelude snapshot against current compiler and source metadata.
isCachedPreludeValid :: CachedPrelude -> IO Bool
isCachedPreludeValid snap = do
  mCompilerHash <- getCompilerHash
  case mCompilerHash of
    Nothing -> return False
    Just compHash
      | compHash /= preludeCompilerHash snap -> return False
      | otherwise -> do
          allOk <- mapM verify (preludeFiles snap)
          return (and allOk)
  where
    verify (pathRaw, expectedHash, expectedSize, expectedMTime) = do
      path <- canonicalizePathCached pathRaw
      mMeta <- getFileMeta path
      case mMeta of
        Just (size, mtime)
          | size == expectedSize && mtime == expectedMTime -> return True
        _ -> do
          mDigest <- hashFile path
          return (mDigest == Just expectedHash)

-- | Compute stable fingerprint tuple for one source file.
fileFingerprint :: FilePath -> IO (Maybe (FilePath, ByteString, Integer, Integer))
fileFingerprint pathRaw = do
  path <- canonicalizePathCached pathRaw
  mMeta <- getFileMeta path
  case mMeta of
    Nothing -> return Nothing
    Just (size, mtime) -> do
      mDigest <- hashFile path
      case mDigest of
        Nothing -> return Nothing
        Just digest -> return (Just (path, digest, size, mtime))

-- | Collect the @(source path, 'PrimFunc' statement)@ pairs needed to
-- rebuild 'evalPrimFuncs' host callbacks for a set of loaded modules.
--
-- `evalPrimFuncs` contains host callbacks and therefore cannot be
-- serialized directly. This decodes each module's @.iz@ cache once to
-- extract its 'PrimFunc' statements; the result is small and is persisted
-- directly in 'CachedPrelude' so future loads never need to touch the
-- module caches again (see 'replayPreludePrimStmts').
collectPreludePrimStmts :: [FilePath] -> IO [(FilePath, Stmt Ann)]
collectPreludePrimStmts loaded = do
  canonicalLoaded <- mapM canonicalizePathCached loaded
  concat <$> mapM collectOne canonicalLoaded
  where
    collectOne srcPath = do
      let cachePath = cacheFilePath srcPath
      mMod <- loadCachedModule cachePath
      case mMod of
        Nothing -> return []
        Just cm ->
          return
            [ (srcPath, stmt)
            | stmt@PrimFunc {} <- cachedTypedStmts cm
            ]

-- | Rebuild primitive-function bindings for a restored prelude evaluator by
-- replaying pre-collected 'PrimFunc' statements. No file I/O or cache
-- decoding is needed here; see 'collectPreludePrimStmts' for where the
-- statements come from.
replayPreludePrimStmts :: [(FilePath, Stmt Ann)] -> EvalState -> IO EvalState
replayPreludePrimStmts primStmts evalBase =
  foldM
    (\acc (srcPath, stmt) -> do
      res <- runEvalM (evalStmtInFile (Just srcPath) stmt) acc
      case res of
        Left _ -> return acc
        Right (_, st') -> return st')
    evalBase
    primStmts

-- | Get file size and modification time (microseconds since epoch).
getFileMeta ::
  FilePath -- ^ File path to inspect.
  -> IO (Maybe (Integer, Integer)) -- ^ (size, mtime) or Nothing on failure.
getFileMeta path = do
  mSize <- try (getFileSize path) :: IO (Either SomeException Integer)
  mTime <- try (getModificationTime path) :: IO (Either SomeException UTCTime)
  case (mSize, mTime) of
    (Right size, Right time) ->
      let micros = round (utcTimeToPOSIXSeconds time * 1000000)
      in return (Just (size, micros))
    _ -> return Nothing

-- | Compute a SHA256 digest for a file.
hashFile ::
  FilePath -- ^ Path to hash.
  -> IO (Maybe ByteString) -- ^ Digest, or Nothing on error.
hashFile path = do
  cached <- readIORef hashCache
  mMeta <- getFileMeta path
  case (Map.lookup path cached, mMeta) of
    -- Cache hit and mtime unchanged: skip re-hashing.
    (Just (cachedMtime, digest), Just (_, currentMtime))
      | cachedMtime == currentMtime -> return (Just digest)
    -- Otherwise re-hash.
    (_, Just (_, currentMtime)) -> do
      res <- try (BL.readFile path)
      case res of
        Left (_ :: SomeException) -> return Nothing
        Right bytes -> do
          let digest = hashlazy bytes
          modifyIORef' hashCache (Map.insert path (currentMtime, digest))
          return (Just digest)
    -- Can't get metadata, try hashing anyway.
    _ -> do
      res <- try (BL.readFile path)
      case res of
        Left (_ :: SomeException) -> return Nothing
        Right bytes -> do
          let digest = hashlazy bytes
          return (Just digest)

-- | Fingerprint the currently running compiler executable.
--
-- ==== Performance note (Optimization: cheap executable fingerprint)
-- Previously this SHA256-hashed the entire executable (tens of MB) on
-- __every__ process invocation to validate module caches, which showed up
-- as ~30% of sampled main-thread time for short-lived runs. Since the
-- executable is rewritten (not patched in place) on every build, its
-- @(size, mtime)@ pair changes whenever the binary changes and is
-- effectively as reliable an invalidation key as a content hash, at the
-- cost of a single 'getFileSize'/'getModificationTime' pair instead of a
-- full read-and-hash. We keep the field named/typed as a 'ByteString'
-- ("hash") to avoid touching the on-disk cache format or other call sites.
getCompilerHash ::
  IO (Maybe ByteString) -- ^ Cached fingerprint of the executable.
getCompilerHash = do
  cached <- readIORef compilerHashCache
  case cached of
    Just digest -> return (Just digest)
    Nothing -> do
      res <- try getExecutablePath
      case res of
        Left (_ :: SomeException) -> do
          let digest = hash (BS8.pack ("kip-" ++ showVersion version))
          writeIORef compilerHashCache (Just digest)
          return (Just digest)
        Right exePath -> do
          mMeta <- try (getFileMeta exePath) :: IO (Either SomeException (Maybe (Integer, Integer)))
          case mMeta of
            Right (Just (size, mtime)) -> do
              let fingerprint = BS8.pack ("kip-exe-" ++ show size ++ "-" ++ show mtime)
              writeIORef compilerHashCache (Just fingerprint)
              return (Just fingerprint)
            _ -> do
              let fallback = hash (BS8.pack ("kip-" ++ showVersion version))
              writeIORef compilerHashCache (Just fallback)
              return (Just fallback)
