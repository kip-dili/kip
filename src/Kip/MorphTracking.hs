{-# LANGUAGE NamedFieldPuns #-}

-- | Incremental tracking for morphology-cache misses.
module Kip.MorphTracking
  ( MorphDirection(..)
  , MorphDelta(..)
  , MorphTrackingToken
  , beginMorphTracking
  , finishMorphTracking
  , recordMorphInsert
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)

-- | Which shared morphology cache received an entry.
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

-- | Record one genuine cache miss in O(1). Only the innermost active module
-- owns the entry, so recursively loaded modules get independent deltas.
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
