{-# LANGUAGE BangPatterns #-}
-- | Small utilities shared across compiler layers.
module Kip.Util (stableNub) where

import Data.List (foldl')
import qualified Data.Set as Set

-- | Remove duplicates while preserving first-occurrence order.
stableNub :: Ord a => [a] -> [a]
stableNub values = reverse uniquesRev
  where
    (_, uniquesRev) = foldl' step (Set.empty, []) values
    step (!seen, acc) value
      | Set.member value seen = (seen, acc)
      | otherwise = (Set.insert value seen, value : acc)
