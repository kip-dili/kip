{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bytecode cache support for compiled modules.
module Kip.Cache where

import GHC.Generics (Generic)
import Data.Binary
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Version (showVersion)
import System.FilePath
import System.Directory
import System.Environment (getExecutablePath)
import Control.Exception (try, SomeException)
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
import Kip.TypeCheck (TCState(..))
import Kip.Eval (EvalState(..))
import Language.Foma (FSM)
import Kip.Render (RenderCache, renderExpValue)
import Paths_kip (version)

-- | Memoized file hash cache for the current process.
-- Stores (mtime in microseconds, hash) so we can skip re-hashing unchanged files.
{-# NOINLINE hashCache #-}
hashCache :: IORef (Map.Map FilePath (Integer, ByteString))
hashCache = unsafePerformIO (newIORef Map.empty)

-- | Memoized compiler hash for the current process.
-- The executable hash is stable per process, so compute once.
{-# NOINLINE compilerHashCache #-}
compilerHashCache :: IORef (Maybe ByteString)
compilerHashCache = unsafePerformIO (newIORef Nothing)

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

-- | Convert a parser state into a cached representation.
--
-- ==== Performance note (Optimization 10)
-- Materializes parser hash-table morphology caches into lists so they can be
-- persisted in the module cache.
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
  s
    { tcFuncs = fmap reverse (tcFuncs s)
    , tcFuncSigs = fmap reverse (tcFuncSigs s)
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

-- | Load a cached module from disk if it is valid.
loadCachedModule ::
  FilePath -- ^ Cache file path.
  -> IO (Maybe CachedModule) -- ^ Cached module when valid.
loadCachedModule path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      res <- try (BS.readFile path)
      case res of
        Left (_ :: SomeException) -> return Nothing
        Right bytes ->
          case decodeOrFail (fromStrict bytes) of
            Left _ -> return Nothing
            Right (_, _, m) -> do
              valid <- isCacheValid path m
              if valid
                then return (Just m)
                else return Nothing

-- | Persist a cached module to disk.
saveCachedModule ::
  FilePath -- ^ Cache file path.
  -> CachedModule -- ^ Module payload to write.
  -> IO () -- ^ Writes the cache file.
saveCachedModule path m = do
  let bytes = toStrict (encode m)
  BS.writeFile path bytes

-- | Check whether a cached module is valid for the current compiler and sources.
isCacheValid ::
  FilePath -- ^ Cache file path.
  -> CachedModule -- ^ Cached module to validate.
  -> IO Bool -- ^ True when metadata matches current sources/compiler.
isCacheValid path m = do
  let meta = metadata m
  mCompilerHash <- getCompilerHash
  case mCompilerHash of
    Nothing -> return False
    Just currentCompilerHash ->
      if compilerHash meta /= currentCompilerHash
        then return False
        else validateSources meta
  where
    -- | Validate the source file and dependency hashes.
    validateSources :: CacheMetadata -- ^ Cached metadata.
                    -> IO Bool -- ^ True when sources match.
    validateSources meta = do
      let sourcePath = replaceExtension path ".kip"
      sourceOk <- verifyPath sourcePath (sourceHash meta) (sourceSize meta) (sourceMTime meta)
      if not sourceOk
        then return False
        else do
          depsValid <- mapM (\(depPath, depHash, depSize, depMTime) ->
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
      mMeta <- getFileMeta depPath
      case mMeta of
        Just (size, mtime)
          | size == depSize && mtime == depMTime -> return True
        _ -> do
          mDepHash <- hashFile depPath
          return (mDepHash == Just depHash)

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

-- | Hash the currently running compiler executable.
getCompilerHash ::
  IO (Maybe ByteString) -- ^ Cached hash of the executable.
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
          digest <- hashFile exePath
          case digest of
            Just _ -> do
              writeIORef compilerHashCache digest
              return digest
            Nothing -> do
              let fallback = hash (BS8.pack ("kip-" ++ showVersion version))
              writeIORef compilerHashCache (Just fallback)
              return (Just fallback)
