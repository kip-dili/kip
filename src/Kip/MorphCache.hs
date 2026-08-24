{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Mutable morphology misses layered over a compact immutable snapshot.
module Kip.MorphCache
  ( MorphCache
  , MorphCaches
  , newMorphCache
  , newMorphCaches
  , mkMorphCaches
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
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as V
import Language.Foma (FSM)
import qualified Language.Foma as Foma
import System.IO.Unsafe (unsafePerformIO)

-- | A small mutable overlay plus a sorted, directly queryable base image.
data MorphCache = MorphCache
  { mutableEntries :: !(HT.BasicHashTable Text [Text])
  , frozenEntries :: !(IORef (V.Vector (Text, [Text])))
  }

-- | The analysis and generation caches owned by one runtime.
data MorphCaches = MorphCaches
  { morphUpsCache :: !MorphCache
  , morphDownsCache :: !MorphCache
  }

data MorphDirection = MorphUps | MorphDowns

-- | Entries inserted while one module was active.
data MorphDelta = MorphDelta
  { morphUpsDelta :: [(Text, [Text])]
  , morphDownsDelta :: [(Text, [Text])]
  }

newtype MorphTrackingToken = MorphTrackingToken Int

data TrackingFrame = TrackingFrame
  { frameToken :: !Int
  , frameUpsRev :: [(Text, [Text])]
  , frameDownsRev :: [(Text, [Text])]
  }

{-# NOINLINE trackingState #-}
trackingState :: IORef (Int, [TrackingFrame])
trackingState = unsafePerformIO (newIORef (0, []))

-- | Begin a possibly nested module-tracking scope.
beginMorphTracking :: IO MorphTrackingToken
beginMorphTracking =
  atomicModifyIORef' trackingState $ \(nextToken, frames) ->
    let frame = TrackingFrame nextToken [] []
    in ((nextToken + 1, frame : frames), MorphTrackingToken nextToken)

-- | Finish a module scope and return its inserts in insertion order.
finishMorphTracking :: MorphTrackingToken -> IO MorphDelta
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
recordMorphInsert :: MorphDirection -> Text -> [Text] -> IO ()
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

-- | Group existing analysis and generation caches under one owner.
mkMorphCaches :: MorphCache -> MorphCache -> MorphCaches
mkMorphCaches = MorphCaches
{-# INLINE mkMorphCaches #-}

-- | Seed analyses for demonstrative pronouns that TRmorph may miss.
populateDemonstrativeCache :: MorphCache -> IO ()
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
lookupMorphCache :: MorphCache -> Text -> IO (Maybe [Text])
lookupMorphCache MorphCache{mutableEntries, frozenEntries} key = do
  mutable <- HT.lookup mutableEntries key
  case mutable of
    Just value -> return (Just value)
    Nothing -> binaryLookup key <$> readIORef frozenEntries

-- | Insert a newly computed entry into the mutable overlay.
insertMorphCache :: MorphCache -> Text -> [Text] -> IO ()
insertMorphCache MorphCache{mutableEntries} = HT.insert mutableEntries

-- | Materialize all entries in sorted-key order, with mutable values winning.
morphCacheToList :: MorphCache -> IO [(Text, [Text])]
morphCacheToList MorphCache{mutableEntries, frozenEntries} = do
  frozen <- V.toList <$> readIORef frozenEntries
  mutable <- HT.toList mutableEntries
  return (Map.toAscList (Map.fromList (frozen ++ mutable)))

-- | Install entries already serialized in ascending key order as the base.
installFrozenMorphCache :: MorphCache -> [(Text, [Text])] -> IO ()
installFrozenMorphCache MorphCache{frozenEntries} =
  writeIORef frozenEntries . V.fromList

-- | Analyze one surface form, caching and tracking genuine misses.
upsCached :: MorphCache -> FSM -> Text -> IO [Text]
upsCached = cachedLookup MorphUps Foma.ups
{-# INLINE upsCached #-}

-- | Analyze surface forms in one batch, caching and tracking genuine misses.
upsCachedBatch :: MorphCache -> FSM -> [Text] -> IO [[Text]]
upsCachedBatch = cachedLookupBatch MorphUps Foma.upsBatch
{-# INLINE upsCachedBatch #-}

-- | Generate one surface form, caching and tracking genuine misses.
downsCached :: MorphCache -> FSM -> Text -> IO [Text]
downsCached = cachedLookup MorphDowns Foma.downs
{-# INLINE downsCached #-}

-- | Generate surface forms in one batch, caching and tracking genuine misses.
downsCachedBatch :: MorphCache -> FSM -> [Text] -> IO [[Text]]
downsCachedBatch = cachedLookupBatch MorphDowns Foma.downsBatch
{-# INLINE downsCachedBatch #-}

cachedLookup :: MorphDirection
             -> (FSM -> Text -> IO [Text])
             -> MorphCache
             -> FSM
             -> Text
             -> IO [Text]
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

cachedLookupBatch :: MorphDirection
                  -> (FSM -> [Text] -> IO [[Text]])
                  -> MorphCache
                  -> FSM
                  -> [Text]
                  -> IO [[Text]]
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

-- | Remove duplicate text values while retaining their first occurrence.
stableNub :: [Text] -> [Text]
stableNub values = reverse uniquesRev
  where
    (_, uniquesRev) = foldl' step (Set.empty, []) values
    step (!seen, acc) value
      | value `Set.member` seen = (seen, acc)
      | otherwise =
          let !seen' = Set.insert value seen
          in (seen', value : acc)

-- | Binary search over the sorted base vector.
binaryLookup :: Text -> V.Vector (Text, [Text]) -> Maybe [Text]
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
