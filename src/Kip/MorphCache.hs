{-# LANGUAGE NamedFieldPuns #-}
-- | Mutable morphology misses layered over a compact immutable snapshot.
module Kip.MorphCache
  ( MorphCache
  , newMorphCache
  , lookupMorphCache
  , insertMorphCache
  , morphCacheToList
  , installFrozenMorphCache
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.HashTable.IO as HT
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Vector as V

-- | A small mutable overlay plus a sorted, directly queryable base image.
data MorphCache = MorphCache
  { mutableEntries :: !(HT.BasicHashTable Text [Text])
  , frozenEntries :: !(IORef (V.Vector (Text, [Text])))
  }

-- | Create an empty morphology cache.
newMorphCache :: IO MorphCache
newMorphCache = MorphCache <$> HT.new <*> newIORef V.empty

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
