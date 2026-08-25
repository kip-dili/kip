{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- | Bytecode cache support for compiled modules.
module Kip.Cache where

import GHC.Generics (Generic)
import Data.Binary
import Data.Binary.Get (runGetOrFail, getWord8, getWord16be, getWord32be, getWord64be, getByteString)
import Data.Binary.Put (putWord8, putWord16be, putWord32be, putWord64be, putByteString)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Version (showVersion)
import System.FilePath
import System.Directory
import System.Environment (getExecutablePath)
import Control.Exception (try, SomeException)
import Control.Monad (when, foldM, replicateM)
import Data.List (delete, foldl', isPrefixOf, sort)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import System.IO.Unsafe (unsafePerformIO)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
#if !defined(mingw32_HOST_OS) && !defined(wasi_HOST_OS)
import qualified System.Posix.Files as Posix
#endif
import Crypto.Hash.SHA256 (hash, hashlazy)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy as BL

import Kip.AST
import Kip.Parser (ParserState(..), newParserStateWithCtxAndCaches)
import Kip.TypeCheck (TCOutputMode(..), TCState(..), buildFuncSigsByArity, buildFuncRetByName, emptyTCState)
import Kip.Eval (EvalState(..), runEvalM, evalStmtInFile)
import Language.Foma (FSM)
import qualified Kip.MorphCache as MC
import Kip.MorphCache (MorphDelta(..))
import Kip.Render (RenderCache, renderExpValue)
import Paths_kip (getLibDir, version)

-- | Process-local memo state shared by cache operations.
data CacheMemo = CacheMemo
  { memoHashes :: !(IORef (Map.Map FilePath (Integer, ByteString)))
  , memoCanonicalPaths :: !(IORef (Map.Map FilePath FilePath))
  , memoCompilerHash :: !(IORef (Maybe ByteString))
  , memoVerifiedPaths :: !(IORef (Map.Map (FilePath, Integer, Integer, ByteString) Bool))
  , memoReverseDependencies :: !(IORef (Map.Map FilePath (Set.Set FilePath)))
  , memoDirtySources :: !(IORef (Set.Set FilePath))
  , memoValidatedRoots :: !(IORef (Set.Set ByteString))
  }

{-# NOINLINE cacheMemo #-}
cacheMemo :: CacheMemo
cacheMemo = unsafePerformIO $
  CacheMemo
    <$> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Nothing
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Set.empty
    <*> newIORef Set.empty

-- | Stable identity and filesystem metadata for one source file.
data FileFingerprint = FileFingerprint
  { fingerprintPath :: !FilePath
  , fingerprintDigest :: !ByteString
  , fingerprintSize :: !Integer
  , fingerprintMTime :: !Integer
  }

-- Keep the existing tuple-compatible prelude-cache representation.
instance Binary FileFingerprint where
  put FileFingerprint{..} = do
    put fingerprintPath
    put fingerprintDigest
    put fingerprintSize
    put fingerprintMTime
  get = FileFingerprint <$> get <*> get <*> get <*> get

-- | Metadata stored alongside cached modules for validation.
data CacheMetadata = CacheMetadata
  { compilerHash :: !ByteString          -- ^ SHA256 of the compiler executable.
  , sourceHash   :: !ByteString          -- ^ SHA256 of the source file.
  , sourceSize   :: !Integer             -- ^ Source file size in bytes.
  , sourceMTime  :: !Integer             -- ^ Source file mtime in microseconds.
  , dependencyRootHash :: !ByteString    -- ^ Merkle root of direct dependency fingerprints.
  , dependencies :: ![FileFingerprint]
  } deriving (Generic)

-- | Build cache metadata for one source and its direct dependencies.
buildCacheMetadata ::
  FilePath ->
  T.Text ->
  [FileFingerprint] ->
  IO (Maybe CacheMetadata)
buildCacheMetadata sourcePath sourceText dependencyFingerprints = do
  mCompilerHash <- getCompilerHash
  case mCompilerHash of
    Nothing -> return Nothing
    Just currentCompilerHash -> do
      mSourceMeta <- getFileMeta sourcePath
      let sourceBytes = encodeUtf8 sourceText
          fallbackSourceSize = fromIntegral (BS.length sourceBytes)
          (currentSourceSize, currentSourceMTime) =
            maybe (fallbackSourceSize, 0) id mSourceMeta
      return . Just $
        CacheMetadata
          { compilerHash = currentCompilerHash
          , sourceHash = hash sourceBytes
          , sourceSize = currentSourceSize
          , sourceMTime = currentSourceMTime
          , dependencyRootHash = dependencyMerkleRoot dependencyFingerprints
          , dependencies = dependencyFingerprints
          }

-- | Explicit, fixed-width metadata encoding for cache format stability.
instance Binary CacheMetadata where
  put CacheMetadata{..} = do
    putHash compilerHash
    putHash sourceHash
    putWord64be (fromIntegral sourceSize)
    putWord64be (fromIntegral sourceMTime)
    putHash dependencyRootHash
    putWord32be (fromIntegral (length dependencies))
    mapM_ putDependency dependencies
    where
      putDependency fingerprint = do
        put (fingerprintPath fingerprint)
        putHash (fingerprintDigest fingerprint)
        putWord64be (fromIntegral (fingerprintSize fingerprint))
        putWord64be (fromIntegral (fingerprintMTime fingerprint))

  get = do
    compilerHash <- getHash
    sourceHash <- getHash
    sourceSize <- fromIntegral <$> getWord64be
    sourceMTime <- fromIntegral <$> getWord64be
    dependencyRootHash <- getHash
    dependencyCount <- getWord32be
    dependencies <- replicateM (fromIntegral dependencyCount) getDependency
    return CacheMetadata{..}
    where
      getDependency = do
        path <- get
        digest <- getHash
        size <- fromIntegral <$> getWord64be
        mtime <- fromIntegral <$> getWord64be
        return (FileFingerprint path digest size mtime)

putHash :: ByteString -> Put
putHash digest = do
  putWord8 (fromIntegral (BS.length digest))
  putByteString digest

getHash :: Get ByteString
getHash = getWord8 >>= getByteString . fromIntegral

cacheMagic :: Word32
cacheMagic = 0x4b49505a -- "KIPZ"

cacheFormatVersion :: Word16
cacheFormatVersion = 3

-- | Stable Merkle root for a module's direct dependency leaves.
dependencyMerkleRoot :: [FileFingerprint] -> ByteString
dependencyMerkleRoot deps =
  hashlazy
    (encode
      [ (fingerprintPath fingerprint, fingerprintDigest fingerprint)
      | fingerprint <- deps
      ])

putCacheHeader :: CacheMetadata -> Put
putCacheHeader meta = do
  putWord32be cacheMagic
  putWord16be cacheFormatVersion
  put meta

getCacheHeader :: Get CacheMetadata
getCacheHeader = do
  magic <- getWord32be
  when (magic /= cacheMagic) (fail "Invalid Kip module-cache magic")
  version <- getWord16be
  when (version /= cacheFormatVersion) (fail "Unsupported Kip module-cache version")
  get

-- | Fully cached module payload.
data CachedModule = CachedModule
  { metadata      :: !CacheMetadata -- ^ Cache validation metadata.
  , cachedTypedStmts :: ![Stmt Ann] -- ^ Type-checked statements, retaining source spans for indexing.
  , cachedParser  :: !CachedParserState -- ^ Parser state snapshot.
  , cachedTC      :: !CachedTCState -- ^ Type checker state snapshot.
  } deriving (Generic)

-- | Sectioned module-cache encoding.
--
-- Each consumer-facing payload has its own length-prefixed byte section.  A
-- full load remains transparent, while lightweight consumers can decode only
-- the AST or AST/parser prefix and skip the typechecker payload entirely.
instance Binary CachedModule where
  put CachedModule{..} = do
    putCacheHeader metadata
    putSection 1 cachedTypedStmts
    putSection 2 cachedParser
    putSection 3 cachedTC
  get = do
    metadata <- getCacheHeader
    stmtsBytes <- getSection 1
    parserBytes <- getSection 2
    tcBytes <- getSection 3
    cachedTypedStmts <- decodeSection stmtsBytes
    cachedParser <- decodeSection parserBytes
    cachedTC <- decodeSection tcBytes
    return CachedModule{..}

encodeSection :: Binary a => a -> ByteString
encodeSection = toStrict . encode

putSection :: Binary a => Word8 -> a -> Put
putSection tag value = do
  let bytes = encodeSection value
  putWord8 tag
  putWord32be (fromIntegral (BS.length bytes))
  putByteString bytes

getSection :: Word8 -> Get ByteString
getSection expectedTag = do
  tag <- getWord8
  when (tag /= expectedTag) (fail "Unexpected module-cache section tag")
  sectionLength <- getWord32be
  getByteString (fromIntegral sectionLength)

decodeSection :: Binary a => ByteString -> Get a
decodeSection bytes =
  case decodeOrFail (fromStrict bytes) of
    Left (_, _, err) -> fail err
    Right (remaining, _, value)
      | BL.null remaining -> return value
      | otherwise -> fail "Trailing bytes in cache section"

-- | Cached prelude snapshot payload used for cold-start acceleration.
--
-- The snapshot stores fully merged parser/typechecker/evaluator states after
-- loading @lib/giriş.kip@ and transitive dependencies.
data CachedPrelude = CachedPrelude
  { preludeCompilerHash :: !ByteString
  , preludeFiles :: ![FileFingerprint]
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

preludeMagic :: Word32
preludeMagic = 0x4b495050 -- "KIPP"

preludeFormatVersion :: Word16
preludeFormatVersion = 2

-- | Versioned prelude-image encoding.  Unlike the original generic instance,
-- this fails immediately and predictably when the on-disk representation
-- changes instead of attempting to interpret an older product encoding.
instance Binary CachedPrelude where
  put CachedPrelude{..} = do
    putWord32be preludeMagic
    putWord16be preludeFormatVersion
    put preludeCompilerHash
    put preludeFiles
    put preludeLoaded
    put preludeParser
    put preludeTC
    put preludeEval
    put preludePrimStmts
  get = do
    magic <- getWord32be
    when (magic /= preludeMagic) (fail "Invalid Kip prelude-cache magic")
    version <- getWord16be
    when (version /= preludeFormatVersion) (fail "Unsupported Kip prelude-cache version")
    preludeCompilerHash <- get
    preludeFiles <- get
    preludeLoaded <- get
    preludeParser <- get
    preludeTC <- get
    preludeEval <- get
    preludePrimStmts <- get
    return CachedPrelude{..}

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

-- | Compact parser-state encoding.
--
-- Prelude morphology tables contain many repeated analyses: the current
-- standard-library image has roughly 137,000 text occurrences but only
-- 16,000 distinct values.  The generic list encoding stored every UTF-8
-- payload independently.  Large tables now share one sorted text dictionary
-- and encode keys/results as fixed-width dictionary indices.  Small module
-- deltas retain the ordinary representation to avoid dictionary overhead.
instance Binary CachedParserState where
  put CachedParserState{..} = do
    put pctx
    put pctors
    put ptyParams
    put ptyCons
    put ptyMods
    put pprimTypes
    put pfuncArities
    put pdefSpans
    putMorphCaches pupsCache pdownsCache
  get = do
    pctx <- get
    pctors <- get
    ptyParams <- get
    ptyCons <- get
    ptyMods <- get
    pprimTypes <- get
    pfuncArities <- get
    pdefSpans <- get
    (pupsCache, pdownsCache) <- getMorphCaches
    return CachedParserState{..}

morphDictionaryThreshold :: Int
morphDictionaryThreshold = 64

putMorphCaches :: [(T.Text, [T.Text])] -> [(T.Text, [T.Text])] -> Put
putMorphCaches upsEntries downsEntries
  | textCount < morphDictionaryThreshold = do
      putWord8 0
      put upsEntries
      put downsEntries
  | otherwise = do
      putWord8 1
      putWord32be (fromIntegral (length dictionary))
      mapM_ put dictionary
      putMorphEntries dictionaryIds upsEntries
      putMorphEntries dictionaryIds downsEntries
  where
    allEntries = upsEntries ++ downsEntries
    textCount = sum [1 + length values | (_, values) <- allEntries]
    dictionary = Set.toAscList (Set.fromList [txt | (key, values) <- allEntries, txt <- key : values])
    dictionaryIds = Map.fromDistinctAscList (zip dictionary [0..])

putMorphEntries :: Map.Map T.Text Word32 -> [(T.Text, [T.Text])] -> Put
putMorphEntries dictionaryIds entries = do
  putWord32be (fromIntegral (length entries))
  mapM_ putEntry entries
  where
    putEntry (key, values) = do
      putWord32be (dictionaryIds Map.! key)
      putWord32be (fromIntegral (length values))
      mapM_ (putWord32be . (dictionaryIds Map.!)) values

getMorphCaches :: Get ([(T.Text, [T.Text])], [(T.Text, [T.Text])])
getMorphCaches = do
  encoding <- getWord8
  case encoding of
    0 -> (,) <$> get <*> get
    1 -> do
      dictionaryLength <- getWord32be
      dictionary <- V.replicateM (fromIntegral dictionaryLength) get
      upsEntries <- getMorphEntries dictionary
      downsEntries <- getMorphEntries dictionary
      return (upsEntries, downsEntries)
    _ -> fail "Unsupported morphology-cache encoding"

getMorphEntries :: V.Vector T.Text -> Get [(T.Text, [T.Text])]
getMorphEntries dictionary = do
  entryCount <- getWord32be
  replicateM (fromIntegral entryCount) $ do
    key <- getMorphText dictionary
    valueCount <- getWord32be
    values <- replicateM (fromIntegral valueCount) (getMorphText dictionary)
    return (key, values)

getMorphText :: V.Vector T.Text -> Get T.Text
getMorphText dictionary = do
  idx <- getWord32be
  case dictionary V.!? fromIntegral idx of
    Just txt -> return txt
    Nothing -> fail "Invalid morphology-cache dictionary index"

-- | Convert a parser state into a cached representation, including a full
-- snapshot of the shared morphology caches.
--
-- ==== Performance note (Optimization 10)
-- Materializes parser hash-table morphology caches into lists so they can be
-- persisted in the module cache.
--
-- Only the prelude snapshot ('saveCachedPrelude') should use this: it is the
-- single on-disk copy of the accumulated morphology cache that all other
-- module caches rely on being pre-populated at load time. Per-module caches
-- persist only their incrementally tracked morphology entries.
toCachedParserState ::
  ParserState -- ^ Parser state to serialize.
  -> IO CachedParserState -- ^ Compact cached payload.
toCachedParserState ps = do
  upsEntries <- MC.morphCacheToList (MC.morphUpsCache (parserMorphCaches ps))
  downsEntries <- MC.morphCacheToList (MC.morphDownsCache (parserMorphCaches ps))
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

-- | Cache only parser declarations introduced since the input state.
toCachedParserStateDelta ::
  ParserState -- ^ State before parsing the module.
  -> ParserState -- ^ State immediately after parsing the module.
  -> IO CachedParserState
toCachedParserStateDelta base current =
  return
    CachedParserState
      { pctx = Set.toList (parserCtx current `Set.difference` parserCtx base)
      , pctors = listDelta (parserCtors current) (parserCtors base)
      , ptyParams = listDelta (parserTyParams current) (parserTyParams base)
      , ptyCons = listDelta (parserTyCons current) (parserTyCons base)
      , ptyMods = listDelta (parserTyMods current) (parserTyMods base)
      , pprimTypes = listDelta (parserPrimTypes current) (parserPrimTypes base)
      , pfuncArities =
          Map.mapMaybeWithKey
            (\name arities ->
              let added = arities `Set.difference` Map.findWithDefault Set.empty name (parserFuncArities base)
              in if Set.null added then Nothing else Just added)
            (parserFuncArities current)
      , pdefSpans =
          Map.mapMaybeWithKey
            (\name spans ->
              let previous = Map.findWithDefault [] name (parserDefSpans base)
                  added = drop (length previous) spans
              in if null added then Nothing else Just added)
            (parserDefSpans current)
      , pupsCache = []
      , pdownsCache = []
      }

-- | Attach the O(number-of-genuine-misses) morphology delta collected while
-- compiling one module.
attachMorphDelta :: MorphDelta -> CachedParserState -> CachedParserState
attachMorphDelta MorphDelta{..} cached =
  cached
    { pupsCache = morphUpsDelta
    , pdownsCache = morphDownsDelta
    }

-- | Restore a parser state from its cached representation.
--
-- ==== Performance note (Optimization 10)
-- Rehydrates persisted morphology cache entries into the shared parser cache
-- tables before returning the parser state.
fromCachedParserState ::
  FSM -- ^ Morphology FSM handle.
  -> Maybe FilePath -- ^ Path to the cached module (for validation).
  -> MC.MorphCaches -- ^ Shared morphology caches.
  -> CachedParserState -- ^ Cached parser snapshot.
  -> IO ParserState -- ^ Rehydrated parser state.
fromCachedParserState fsm cachePath morphCaches CachedParserState{..} = do
  let upsCache = MC.morphUpsCache morphCaches
      downsCache = MC.morphDownsCache morphCaches
  mapM_ (uncurry (MC.insertMorphCache upsCache)) pupsCache
  mapM_ (uncurry (MC.insertMorphCache downsCache)) pdownsCache
  -- Use the shared constructor so derived parser indices (for overload
  -- lookup) are rebuilt consistently after cache restore.
  return (newParserStateWithCtxAndCaches fsm (Set.fromList pctx) pctors ptyParams ptyCons ptyMods pprimTypes pfuncArities pdefSpans cachePath morphCaches)

-- | Apply a cached parser delta to the state accumulated by earlier modules.
fromCachedParserStateDelta ::
  FSM
  -> Maybe FilePath
  -> MC.MorphCaches
  -> ParserState
  -> CachedParserState
  -> IO ParserState
fromCachedParserStateDelta fsm cachePath morphCaches base cached = do
  delta <- fromCachedParserState fsm cachePath morphCaches cached
  return
    (newParserStateWithCtxAndCaches
      fsm
      (Set.union (parserCtx delta) (parserCtx base))
      (parserCtors delta ++ parserCtors base)
      (parserTyParams delta ++ parserTyParams base)
      (parserTyCons delta ++ parserTyCons base)
      (parserTyMods delta ++ parserTyMods base)
      (parserPrimTypes delta ++ parserPrimTypes base)
      (Map.unionWith Set.union (parserFuncArities delta) (parserFuncArities base))
      (Map.unionWith (++) (parserDefSpans base) (parserDefSpans delta))
      cachePath
      morphCaches)

-- | Cached wrapper for the type checker state.
newtype CachedTCState = CachedTCState TCState
  deriving (Generic)

instance Binary CachedTCState

-- | Wrap a type checker state for caching.
toCachedTCState ::
  TCState -- ^ Type checker state to wrap.
  -> CachedTCState -- ^ Wrapped cached state.
toCachedTCState = CachedTCState

-- | Cache only typechecker entries introduced or changed by one module load.
toCachedTCStateDelta :: TCState -> TCState -> CachedTCState
toCachedTCStateDelta base current =
  CachedTCState
    (emptyTCState
      { tcCtx = tcCtx current `Set.difference` tcCtx base
      , tcFuncs = multiMapDelta (tcFuncs base) (tcFuncs current)
      , tcFuncSigs = multiMapDelta (tcFuncSigs base) (tcFuncSigs current)
      , tcFuncSigRets = mapDelta (tcFuncSigRets base) (tcFuncSigRets current)
      , tcFuncEffectsByArity =
          HM.filterWithKey
            (\key value -> HM.lookup key (tcFuncEffectsByArity base) /= Just value)
            (tcFuncEffectsByArity current)
      , tcVarTys = listDelta (tcVarTys current) (tcVarTys base)
      , tcVals = mapDelta (tcVals base) (tcVals current)
      , tcCtors = mapDelta (tcCtors base) (tcCtors current)
      , tcTyCons = mapDelta (tcTyCons base) (tcTyCons current)
      , tcInfinitives = tcInfinitives current `Set.difference` tcInfinitives base
      , tcOutputMode = tcOutputMode current
      , tcResolvedNames = listDelta (tcResolvedNames current) (tcResolvedNames base)
      , tcResolvedSigs = listDelta (tcResolvedSigs current) (tcResolvedSigs base)
      , tcResolvedTypes = listDelta (tcResolvedTypes current) (tcResolvedTypes base)
      , tcDefLocations = mapDelta (tcDefLocations base) (tcDefLocations current)
      , tcFuncSigLocs = mapDelta (tcFuncSigLocs base) (tcFuncSigLocs current)
      })

-- | Remove one occurrence of every baseline element while preserving the
-- order of entries introduced by the current module.
listDelta :: Eq a => [a] -> [a] -> [a]
listDelta = foldl' (flip delete)

multiMapDelta :: (Ord key, Eq value) => Map.Map key [value] -> Map.Map key [value] -> Map.Map key [value]
multiMapDelta base =
  Map.mapMaybeWithKey $ \key values ->
    let added = listDelta values (Map.findWithDefault [] key base)
    in if null added then Nothing else Just added

mapDelta :: (Ord key, Eq value) => Map.Map key value -> Map.Map key value -> Map.Map key value
mapDelta base =
  Map.filterWithKey $ \key value -> Map.lookup key base /= Just value

-- | Merge one cached typechecker delta into the accumulated state.
mergeCachedTCState :: TCState -> TCState -> TCState
mergeCachedTCState current delta =
  let mergedFuncs = Map.unionWith (++) (tcFuncs delta) (tcFuncs current)
      mergedSigs = Map.unionWith (++) (tcFuncSigs delta) (tcFuncSigs current)
      mergedRets = Map.union (tcFuncSigRets delta) (tcFuncSigRets current)
      mergedNamesByArity = HM.unionWith Set.union (tcFuncNamesByArity delta) (tcFuncNamesByArity current)
      mergedSigsByArity = HM.unionWith (++) (tcFuncSigsByArity delta) (tcFuncSigsByArity current)
      mergedRetByName = HM.unionWith Map.union (tcFuncRetByName delta) (tcFuncRetByName current)
      outputMode = tcOutputMode current
  in emptyTCState
      { tcCtx = Set.union (tcCtx current) (tcCtx delta)
      , tcFuncs = mergedFuncs
      , tcFuncNamesByArity = mergedNamesByArity
      , tcFuncSigs = mergedSigs
      , tcFuncSigsByArity = mergedSigsByArity
      , tcFuncSigRets = mergedRets
      , tcFuncRetByName = mergedRetByName
      , tcFuncEffectsByArity = HM.union (tcFuncEffectsByArity delta) (tcFuncEffectsByArity current)
      , tcVarTys = tcVarTys delta ++ tcVarTys current
      , tcVals = Map.union (tcVals delta) (tcVals current)
      , tcCtors = Map.union (tcCtors delta) (tcCtors current)
      , tcTyCons = Map.union (tcTyCons delta) (tcTyCons current)
      , tcInfinitives = Set.union (tcInfinitives current) (tcInfinitives delta)
      , tcOutputMode = outputMode
      , tcResolvedNames = if outputMode >= TCOutputLsp then tcResolvedNames delta ++ tcResolvedNames current else []
      , tcResolvedSigs = tcResolvedSigs delta ++ tcResolvedSigs current
      , tcResolvedTypes = if outputMode >= TCOutputLsp then tcResolvedTypes delta ++ tcResolvedTypes current else []
      , tcDefLocations = if outputMode >= TCOutputLsp then Map.union (tcDefLocations delta) (tcDefLocations current) else Map.empty
      , tcFuncSigLocs = if outputMode >= TCOutputLsp then Map.union (tcFuncSigLocs delta) (tcFuncSigLocs current) else Map.empty
      }

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
  cached <- readIORef (memoCanonicalPaths cacheMemo)
  case Map.lookup path cached of
    Just absPath -> return absPath
    Nothing -> do
      absPath <- canonicalizePath path
      modifyIORef' (memoCanonicalPaths cacheMemo) (Map.insert path absPath)
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
  mCacheFile <- readCacheBytes path
  case mCacheFile of
    Nothing -> return Nothing
    Just (absCachePath, bytes) ->
      case runGetOrFail getCacheHeader (fromStrict bytes) of
        Left _ -> return Nothing
        Right (_, _, meta) -> do
          valid <- isCacheValidMeta absCachePath meta
          if not valid
            then return Nothing
            else case decodeOrFail (fromStrict bytes) of
              Left _ -> return Nothing
              Right (_, _, m) -> return (Just m)

-- | Load only the typed-statement section from a valid module cache.
loadCachedTypedStmts :: FilePath -> IO (Maybe [Stmt Ann])
loadCachedTypedStmts path =
  loadCachedSections path getStmtsPrefix fst (\(_, stmtsBytes) -> decodeSectionValue stmtsBytes)
  where
    getStmtsPrefix :: Get (CacheMetadata, ByteString)
    getStmtsPrefix = (,) <$> getCacheHeader <*> getSection 1

-- | Load the typed-statement and parser sections without decoding typechecker
-- state.  This is the exact prefix needed by LSP workspace definition scans.
loadCachedAstParser :: FilePath -> IO (Maybe ([Stmt Ann], CachedParserState))
loadCachedAstParser path =
  loadCachedSections path getAstParserPrefix (\(meta, _, _) -> meta) decodePrefix
  where
    getAstParserPrefix :: Get (CacheMetadata, ByteString, ByteString)
    getAstParserPrefix = (,,) <$> getCacheHeader <*> getSection 1 <*> getSection 2

    decodePrefix (_, stmtsBytes, parserBytes) = do
      stmts <- decodeSectionValue stmtsBytes
      parserState <- decodeSectionValue parserBytes
      return (stmts, parserState)

-- | Read, validate, and decode a selected cache prefix.
loadCachedSections :: FilePath -> Get prefix -> (prefix -> CacheMetadata) -> (prefix -> Either String value) -> IO (Maybe value)
loadCachedSections path getPrefix prefixMetadata decodePrefix = do
  mCacheFile <- readCacheBytes path
  case mCacheFile of
    Nothing -> return Nothing
    Just (absCachePath, bytes) ->
      case runGetOrFail getPrefix (fromStrict bytes) of
        Left _ -> return Nothing
        Right (_, _, prefix) -> do
          let meta = prefixMetadata prefix
          valid <- isCacheValidMeta absCachePath meta
          if not valid
            then return Nothing
            else return (either (const Nothing) Just (decodePrefix prefix))

-- | Read a cache file, swallowing missing-file and I/O failures.
readCacheBytes :: FilePath -> IO (Maybe (FilePath, ByteString))
readCacheBytes path = do
  absPath <- canonicalizePathCached path
  exists <- doesFileExist absPath
  if not exists
    then return Nothing
    else do
      result <- try (BS.readFile absPath) :: IO (Either SomeException ByteString)
      return (either (const Nothing) (\bytes -> Just (absPath, bytes)) result)

decodeSectionValue :: Binary a => ByteString -> Either String a
decodeSectionValue bytes =
  case decodeOrFail (fromStrict bytes) of
    Left (_, _, err) -> Left err
    Right (remaining, _, value)
      | BL.null remaining -> Right value
      | otherwise -> Left "Trailing bytes in cache section"

-- | Persist a cached module to disk.
saveCachedModule ::
  FilePath -- ^ Cache file path.
  -> CachedModule -- ^ Module payload to write.
  -> IO () -- ^ Writes the cache file.
saveCachedModule path = writeCacheBytesIfChanged path . toStrict . encode

-- | Write cache bytes only when their contents differ from the existing file.
writeCacheBytesIfChanged :: FilePath -> ByteString -> IO ()
writeCacheBytesIfChanged path bytes = do
  absPath <- canonicalizePathCached path
  mCurrentMeta <- getFileMeta absPath
  shouldWrite <-
    case mCurrentMeta of
      Just (oldSize, _) | oldSize == fromIntegral (BS.length bytes) -> do
        oldRes <- try (BS.readFile absPath)
        case oldRes of
          Left (_ :: SomeException) -> return True
          Right oldBytes -> return (oldBytes /= bytes)
      _ -> return True
  when shouldWrite (BS.writeFile absPath bytes)

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
          registerDependencyEdges sourcePath (map fingerprintPath (dependencies meta))
          dirty <- readIORef (memoDirtySources cacheMemo)
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
          rootValidated <- Set.member (dependencyRootHash meta) <$> readIORef (memoValidatedRoots cacheMemo)
          if rootValidated
            then return True
            else do
              depsValid <- mapM (\fingerprint -> do
                depPath <- canonicalizePathCached (fingerprintPath fingerprint)
                verifyPath depPath
                  (fingerprintDigest fingerprint)
                  (fingerprintSize fingerprint)
                  (fingerprintMTime fingerprint)) (dependencies meta)
              let allValid = and depsValid
              when allValid
                (modifyIORef' (memoValidatedRoots cacheMemo) (Set.insert (dependencyRootHash meta)))
              return allValid

    -- | Verify a file by fast metadata check and fallback hashing.
    -- This avoids hashing unchanged dependencies on hot build paths.
    verifyPath :: FilePath -- ^ Dependency path.
               -> ByteString -- ^ Expected hash.
               -> Integer -- ^ Expected size.
               -> Integer -- ^ Expected mtime.
               -> IO Bool -- ^ True when dependency matches.
    verifyPath depPath depHash depSize depMTime = do
      dirty <- readIORef (memoDirtySources cacheMemo)
      if Set.member depPath dirty
        then return False
        else do
          let cacheKey = (depPath, depSize, depMTime, depHash)
          validationCache <- readIORef (memoVerifiedPaths cacheMemo)
          case Map.lookup cacheKey validationCache of
            Just ok -> return ok
            Nothing -> do
              ok <- fileMatchesFingerprint depPath depHash depSize depMTime
              if ok
                then modifyIORef' (memoVerifiedPaths cacheMemo) (Map.insert cacheKey True)
                else markDirtySource depPath
              return ok

-- | Register reverse edges for one source file to its direct dependencies.
registerDependencyEdges :: FilePath -> [FilePath] -> IO ()
registerDependencyEdges source deps = do
  deps' <- mapM canonicalizePathCached deps
  modifyIORef'
    (memoReverseDependencies cacheMemo)
    (\g ->
      foldl
        (\acc dep -> Map.insertWith Set.union dep (Set.singleton source) acc)
        g
        deps')

-- | Mark one source file dirty and propagate to transitive dependents.
markDirtySource :: FilePath -> IO ()
markDirtySource sourceRaw = do
  source <- canonicalizePathCached sourceRaw
  graph <- readIORef (memoReverseDependencies cacheMemo)
  dirty <- readIORef (memoDirtySources cacheMemo)
  let go [] seen = seen
      go (x:xs) seen
        | Set.member x seen = go xs seen
        | otherwise =
            let parents = Set.toList (Map.findWithDefault Set.empty x graph)
            in go (parents ++ xs) (Set.insert x seen)
      newlyDirty = go [source] Set.empty
  writeIORef (memoDirtySources cacheMemo) (Set.union dirty newlyDirty)
  
-- | Load a prelude snapshot if present and valid.
loadCachedPrelude ::
  FilePath -- ^ Snapshot file path.
  -> RenderCache -- ^ Render cache for evaluator restore.
  -> FSM -- ^ Morphology FSM handle.
  -> IO (Maybe (ParserState, TCState, EvalState, Set.Set FilePath))
loadCachedPrelude snapshotPath cache fsm = do
  mCacheFile <- readCacheBytes snapshotPath
  case mCacheFile of
    Nothing -> return Nothing
    Just (_, bytes) ->
      case decodeOrFail (fromStrict bytes) of
        Left _ -> return Nothing
        Right (_, _, preludeSnap) -> do
          valid <- isCachedPreludeValid preludeSnap
          if not valid
            then return Nothing
            else do
              let cachedParser = preludeParser preludeSnap
                  upsCache = MC.morphUpsCache cache
                  downsCache = MC.morphDownsCache cache
              MC.installFrozenMorphCache upsCache (pupsCache cachedParser)
              MC.installFrozenMorphCache downsCache (pdownsCache cachedParser)
              pst <- fromCachedParserState fsm Nothing cache
                cachedParser { pupsCache = [], pdownsCache = [] }
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
          writeCacheBytesIfChanged snapshotPath (toStrict (encode snap))

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
    verify fingerprint = do
      path <- canonicalizePathCached (fingerprintPath fingerprint)
      fileMatchesFingerprint path
        (fingerprintDigest fingerprint)
        (fingerprintSize fingerprint)
        (fingerprintMTime fingerprint)

-- | Compare a file with an expected fingerprint, hashing only when metadata
-- does not match.
fileMatchesFingerprint ::
  FilePath -> ByteString -> Integer -> Integer -> IO Bool
fileMatchesFingerprint path expectedDigest expectedSize expectedMTime = do
  mMeta <- getFileMeta path
  case mMeta of
    Just (size, mtime)
      | size == expectedSize && mtime == expectedMTime -> return True
    _ -> do
      mDigest <- hashFile path
      return (mDigest == Just expectedDigest)

-- | Compute a stable fingerprint for one source file.
fileFingerprint :: FilePath -> IO (Maybe FileFingerprint)
fileFingerprint pathRaw = do
  path <- canonicalizePathCached pathRaw
  mMeta <- getFileMeta path
  case mMeta of
    Nothing -> return Nothing
    Just (size, mtime) -> do
      mDigest <- hashFile path
      case mDigest of
        Nothing -> return Nothing
        Just digest -> return (Just (FileFingerprint path digest size mtime))

-- | Fingerprint a file, falling back to a content hash when metadata fails.
fileFingerprintOrHash :: FilePath -> IO FileFingerprint
fileFingerprintOrHash path = do
  mFingerprint <- fileFingerprint path
  case mFingerprint of
    Just fingerprint -> return fingerprint
    Nothing -> do
      digest <- hash <$> BS.readFile path
      return (FileFingerprint path digest 0 0)

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
      mStmts <- loadCachedTypedStmts cachePath
      case mStmts of
        Nothing -> return []
        Just stmts ->
          return
            [ (srcPath, stmt)
            | stmt@PrimFunc {} <- stmts
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
#if !defined(mingw32_HOST_OS) && !defined(wasi_HOST_OS)
getFileMeta path = do
  result <- try (Posix.getFileStatus path) :: IO (Either SomeException Posix.FileStatus)
  case result of
    Left _ -> return Nothing
    Right status ->
      return
        (Just
          ( fromIntegral (Posix.fileSize status)
          , round (Posix.modificationTimeHiRes status * 1000000)
          ))
#else
getFileMeta path = do
  mSize <- try (getFileSize path) :: IO (Either SomeException Integer)
  mTime <- try (getModificationTime path) :: IO (Either SomeException UTCTime)
  case (mSize, mTime) of
    (Right size, Right time) ->
      let micros = round (utcTimeToPOSIXSeconds time * 1000000)
      in return (Just (size, micros))
    _ -> return Nothing
#endif

-- | Compute a SHA256 digest for a file.
hashFile ::
  FilePath -- ^ Path to hash.
  -> IO (Maybe ByteString) -- ^ Digest, or Nothing on error.
hashFile path = do
  cached <- readIORef (memoHashes cacheMemo)
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
          modifyIORef' (memoHashes cacheMemo) (Map.insert path (currentMtime, digest))
          return (Just digest)
    -- Can't get metadata, try hashing anyway.
    _ -> do
      res <- try (BL.readFile path)
      case res of
        Left (_ :: SomeException) -> return Nothing
        Right bytes -> do
          let digest = hashlazy bytes
          return (Just digest)

-- | Fingerprint the shared Kip compiler library.
--
-- Module and prelude caches are shared by @kip@, @kip-lsp@, and
-- @kip-playground@. Fingerprinting the current executable made those programs
-- invalidate one another's otherwise-compatible caches because their binary
-- sizes and mtimes necessarily differ. Cabal installs the single library
-- archive linked by all three programs in the library component's 'getLibDir';
-- its metadata changes whenever the shared compiler implementation is rebuilt
-- while remaining identical for every frontend.
getCompilerHash ::
  IO (Maybe ByteString) -- ^ Cached fingerprint of the shared compiler implementation.
getCompilerHash = do
  cached <- readIORef (memoCompilerHash cacheMemo)
  case cached of
    Just digest -> return (Just digest)
    Nothing -> do
      mSharedArtifact <- findSharedCompilerArtifact
      mMeta <- case mSharedArtifact of
        Just artifact -> getFileMeta artifact
        Nothing -> return Nothing
      fingerprint <- case mMeta of
        Just (size, mtime) ->
          return (BS8.pack ("kip-lib-" ++ show size ++ "-" ++ show mtime))
        Nothing -> executableFingerprint
      writeIORef (memoCompilerHash cacheMemo) (Just fingerprint)
      return (Just fingerprint)
  where
    executableFingerprint = do
      res <- try getExecutablePath
      case res of
        Left (_ :: SomeException) ->
          return (hash (BS8.pack ("kip-" ++ showVersion version)))
        Right exePath -> do
          mMeta <- getFileMeta exePath
          case mMeta of
            Just (size, mtime) ->
              return (BS8.pack ("kip-exe-" ++ show size ++ "-" ++ show mtime))
            Nothing ->
              return (hash (BS8.pack ("kip-" ++ showVersion version)))

-- | Locate the Cabal-installed static archive for the shared Kip library.
findSharedCompilerArtifact :: IO (Maybe FilePath)
findSharedCompilerArtifact = do
  res <- try $ do
    libDir <- getLibDir
    entries <- listDirectory libDir
    return . sort $
      [ libDir </> entry
      | entry <- entries
      , "libHSkip-" `isPrefixOf` entry
      , takeExtension entry == ".a"
      ]
  case res of
    Left (_ :: SomeException) -> return Nothing
    Right (artifact : _) -> return (Just artifact)
    Right [] -> return Nothing
