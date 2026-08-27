{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Mutable morphology misses layered over a compact immutable snapshot.
module Kip.MorphCache
  ( MorphCache
  , MorphCaches
  , newMorphCache
  , newMorphCaches
  , morphUpsCache
  , morphDownsCache
  , lookupMorphCache
  , insertMorphCache
  , morphCacheToList
  , installFrozenMorphCache
  , upsCached
  , upsCachedBatch
  , downsCached
  , downsCachedBatch
  , MorphDelta(..)
  , MorphTrackingToken
  , beginMorphTracking
  , finishMorphTracking
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashTable.IO as HT
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Vector as V
import Language.Foma (FSM)
import qualified Language.Foma as Foma
import Kip.Util (stableNub)
import System.IO.Unsafe (unsafePerformIO)

-- | A small mutable overlay plus a sorted, directly queryable base image.
data MorphCache = MorphCache
  { mutableEntries :: !(HT.BasicHashTable Text [Text])
    -- ^ Entries learned since the immutable snapshot was installed.
  , frozenEntries :: !(IORef (V.Vector (Text, [Text])))
    -- ^ Sorted snapshot searched without rebuilding a hash table.
  }

-- | The analysis and generation caches owned by one runtime.
data MorphCaches = MorphCaches
  { morphUpsCache :: !MorphCache
    -- ^ Cache from surface forms to morphological analyses.
  , morphDownsCache :: !MorphCache
    -- ^ Cache from morphological analyses to generated surface forms.
  }

-- | Selects whether a tracked lookup performs analysis or generation.
data MorphDirection = MorphUps | MorphDowns

-- | Entries inserted while one module was active.
data MorphDelta = MorphDelta
  { morphUpsDelta :: [(Text, [Text])]
    -- ^ Analysis entries inserted during the tracking scope.
  , morphDownsDelta :: [(Text, [Text])]
    -- ^ Generation entries inserted during the tracking scope.
  }

-- | Opaque identity for a nested morphology-cache tracking scope.
newtype MorphTrackingToken = MorphTrackingToken Int
  -- ^ Monotonically increasing scope identifier.

-- | Mutable accumulator for one active cache-tracking scope.
data TrackingFrame = TrackingFrame
  { frameToken :: !Int
    -- ^ Identifier matched by 'finishMorphTracking'.
  , frameUpsRev :: [(Text, [Text])]
    -- ^ Analysis inserts accumulated in reverse insertion order.
  , frameDownsRev :: [(Text, [Text])]
    -- ^ Generation inserts accumulated in reverse insertion order.
  }

{-# NOINLINE trackingState #-}
-- | Process-wide token counter and stack of active tracking frames.
trackingState :: IORef (Int, [TrackingFrame])
trackingState = unsafePerformIO (newIORef (0, []))

-- | Begin a possibly nested module-tracking scope.
beginMorphTracking :: IO MorphTrackingToken
beginMorphTracking =
  atomicModifyIORef' trackingState $ \(nextToken, frames) ->
    let frame = TrackingFrame nextToken [] []
    in ((nextToken + 1, frame : frames), MorphTrackingToken nextToken)

-- | Finish a module scope and return its inserts in insertion order.
finishMorphTracking :: MorphTrackingToken -- ^ Token returned by 'beginMorphTracking'.
                    -> IO MorphDelta -- ^ Entries recorded by that scope.
finishMorphTracking (MorphTrackingToken token) =
  atomicModifyIORef' trackingState $ \(nextToken, frames) ->
    case frames of
      TrackingFrame{frameToken, frameUpsRev, frameDownsRev} : rest
        | frameToken == token ->
            ( (nextToken, rest)
            , MorphDelta (reverse frameUpsRev) (reverse frameDownsRev)
            )
      _ -> ((nextToken, frames), MorphDelta [] [])

-- | Record one genuine cache miss in the innermost active module scope.
recordMorphInsert :: MorphDirection -- ^ Cache direction receiving the entry.
                  -> Text -- ^ Lookup key that missed the cache.
                  -> [Text] -- ^ Value computed for the key.
                  -> IO () -- ^ Completion after updating the active frame.
recordMorphInsert direction key value =
  atomicModifyIORef' trackingState $ \(nextToken, frames) ->
    case frames of
      [] -> ((nextToken, frames), ())
      frame : rest ->
        let frame' =
              case direction of
                MorphUps -> frame { frameUpsRev = (key, value) : frameUpsRev frame }
                MorphDowns -> frame { frameDownsRev = (key, value) : frameDownsRev frame }
        in ((nextToken, frame' : rest), ())

-- | Create an empty morphology cache.
newMorphCache :: IO MorphCache
newMorphCache = MorphCache <$> HT.new <*> newIORef V.empty

-- | Create the cache pair used by parsing and rendering.
newMorphCaches :: IO MorphCaches
newMorphCaches = do
  upsCache <- newMorphCache
  populateDemonstrativeCache upsCache
  MorphCaches upsCache <$> newMorphCache

-- | Seed analyses for demonstrative pronouns that TRmorph may miss.
populateDemonstrativeCache :: MorphCache -- ^ Analysis cache to seed.
                            -> IO () -- ^ Completion after inserting built-ins.
populateDemonstrativeCache cache =
  mapM_ (uncurry (insertMorphCache cache)) entries
  where
    entries =
      [ ("bu", ["bu<nom>"])
      , ("bunu", ["bu<acc>"])
      , ("buna", ["bu<dat>"])
      , ("bunda", ["bu<loc>"])
      , ("bundan", ["bu<abl>"])
      , ("bunun", ["bu<gen>"])
      , ("bunla", ["bu<ins>"])
      , ("bununla", ["bu<ins>"])
      , ("şu", ["şu<nom>"])
      , ("şunu", ["şu<acc>"])
      , ("şuna", ["şu<dat>"])
      , ("şunda", ["şu<loc>"])
      , ("şundan", ["şu<abl>"])
      , ("şunun", ["şu<gen>"])
      , ("şunla", ["şu<ins>"])
      , ("şununla", ["şu<ins>"])
      , ("o", ["o<nom>"])
      , ("onu", ["o<acc>"])
      , ("ona", ["o<dat>"])
      , ("onda", ["o<loc>"])
      , ("ondan", ["o<abl>"])
      , ("onun", ["o<gen>"])
      , ("onunla", ["o<ins>"])
      ]

-- | Look up the mutable overlay first, then binary-search the frozen image.
lookupMorphCache :: MorphCache -- ^ Cache to query.
                 -> Text -- ^ Surface form or analysis used as the key.
                 -> IO (Maybe [Text]) -- ^ Cached values, when present.
lookupMorphCache MorphCache{mutableEntries, frozenEntries} key = do
  mutable <- HT.lookup mutableEntries key
  case mutable of
    Just value -> return (Just value)
    Nothing -> binaryLookup key <$> readIORef frozenEntries

-- | Insert a newly computed entry into the mutable overlay.
insertMorphCache :: MorphCache -- ^ Cache whose mutable overlay is updated.
                 -> Text -- ^ Lookup key.
                 -> [Text] -- ^ Morphology results stored for the key.
                 -> IO () -- ^ Completion after insertion.
insertMorphCache MorphCache{mutableEntries} = HT.insert mutableEntries

-- | Materialize all entries in sorted-key order, with mutable values winning.
morphCacheToList :: MorphCache -- ^ Cache to materialize.
                 -> IO [(Text, [Text])] -- ^ Entries sorted by key.
morphCacheToList MorphCache{mutableEntries, frozenEntries} = do
  frozen <- V.toList <$> readIORef frozenEntries
  mutable <- HT.toList mutableEntries
  return (Map.toAscList (Map.fromList (frozen ++ mutable)))

-- | Install entries already serialized in ascending key order as the base.
installFrozenMorphCache :: MorphCache -- ^ Cache whose base image is replaced.
                        -> [(Text, [Text])] -- ^ Entries already sorted by key.
                        -> IO () -- ^ Completion after installing the snapshot.
installFrozenMorphCache MorphCache{frozenEntries} =
  writeIORef frozenEntries . V.fromList

-- | Analyze one surface form, caching and tracking genuine misses.
upsCached :: MorphCache -- ^ Analysis cache.
          -> FSM -- ^ Morphology machine used on a miss.
          -> Text -- ^ Surface form to analyze.
          -> IO [Text] -- ^ Cached or freshly computed analyses.
upsCached = cachedLookup MorphUps Foma.ups
{-# INLINE upsCached #-}

-- | Analyze surface forms in one batch, caching and tracking genuine misses.
upsCachedBatch :: MorphCache -- ^ Analysis cache.
               -> FSM -- ^ Morphology machine used for misses.
               -> [Text] -- ^ Surface forms to analyze.
               -> IO [[Text]] -- ^ Analyses corresponding to each input.
upsCachedBatch = cachedLookupBatch MorphUps Foma.upsBatch
{-# INLINE upsCachedBatch #-}

-- | Generate one surface form, caching and tracking genuine misses.
downsCached :: MorphCache -- ^ Generation cache.
            -> FSM -- ^ Morphology machine used on a miss.
            -> Text -- ^ Analysis string to realize.
            -> IO [Text] -- ^ Cached or freshly generated surface forms.
downsCached = cachedLookup MorphDowns Foma.downs
{-# INLINE downsCached #-}

-- | Generate surface forms in one batch, caching and tracking genuine misses.
downsCachedBatch :: MorphCache -- ^ Generation cache.
                 -> FSM -- ^ Morphology machine used for misses.
                 -> [Text] -- ^ Analysis strings to realize.
                 -> IO [[Text]] -- ^ Surface forms corresponding to each input.
downsCachedBatch = cachedLookupBatch MorphDowns Foma.downsBatch
{-# INLINE downsCachedBatch #-}

-- | Implement a single cached morphology lookup and record genuine misses.
cachedLookup :: MorphDirection -- ^ Direction recorded for a cache miss.
             -> (FSM -> Text -> IO [Text]) -- ^ Underlying morphology operation.
             -> MorphCache -- ^ Cache to consult and update.
             -> FSM -- ^ Morphology machine passed to the operation.
             -> Text -- ^ Lookup key.
             -> IO [Text] -- ^ Cached or newly fetched values.
cachedLookup direction fetch cache fsm key = do
  cached <- lookupMorphCache cache key
  case cached of
    Just result -> return result
    Nothing -> do
      result <- fetch fsm key
      insertMorphCache cache key result
      recordMorphInsert direction key result
      return result
{-# INLINE cachedLookup #-}

-- | Resolve a batch through a cache while fetching each distinct miss once.
cachedLookupBatch :: MorphDirection -- ^ Direction recorded for cache misses.
                  -> (FSM -> [Text] -> IO [[Text]]) -- ^ Batched morphology operation.
                  -> MorphCache -- ^ Cache to consult and update.
                  -> FSM -- ^ Morphology machine passed to the operation.
                  -> [Text] -- ^ Lookup keys, including possible duplicates.
                  -> IO [[Text]] -- ^ Values in the same order as the keys.
cachedLookupBatch _ _ _ _ [] = return []
cachedLookupBatch direction fetch cache fsm keys = do
  cached <- mapM (lookupMorphCache cache) keys
  let missing = stableNub [key | (key, Nothing) <- zip keys cached]
  fetched <- if null missing then return [] else fetch fsm missing
  let fetchedMap = Map.fromList (zip missing fetched)
  mapM_ insertTracked (zip missing fetched)
  let resolve key = fromMaybe (fromMaybe [] (Map.lookup key fetchedMap))
  return (zipWith resolve keys cached)
  where
    insertTracked (key, value) = do
      insertMorphCache cache key value
      recordMorphInsert direction key value
{-# INLINE cachedLookupBatch #-}

-- | Binary search over the sorted base vector.
binaryLookup :: Text -- ^ Key to locate.
             -> V.Vector (Text, [Text]) -- ^ Entries sorted by key.
             -> Maybe [Text] -- ^ Value associated with the key, if present.
binaryLookup key entries = go 0 (V.length entries - 1)
  where
    go low high
      | low > high = Nothing
      | otherwise =
          let mid = low + (high - low) `quot` 2
              (candidate, value) = entries V.! mid
          in case compare key candidate of
               LT -> go low (mid - 1)
               GT -> go (mid + 1) high
               EQ -> Just value
