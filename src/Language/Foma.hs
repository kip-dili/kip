{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level FFI bindings to the Foma morphology library.
module Language.Foma where

import Control.Monad
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO)
import Data.List (find, partition, sort)
import Foreign.C
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.Marshal
import Foreign.Marshal.Array (withArray, peekArray, peekArray0)
import Foreign.Marshal.Utils (withMany)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set

-- | Opaque handle for a Foma finite state machine.
newtype FSM = FSM (Ptr ())

-- | Opaque handle for a Foma apply handle.
data ApplyHandle

-- | Raw FFI binding for reading a binary FSM file.
foreign import ccall unsafe "fomalib.h fsm_read_binary_file"
  fsmReadBinaryFile' :: CString -> IO FSM

-- | Read an FSM from a binary file on disk.
--
-- ==== Performance note (Optimization: lazy FSM load)
-- @trmorph.fst@ is gzip-compressed on disk; @fsm_read_binary_file@
-- decompresses and parses it (measured ~34 ms standalone) on __every__
-- process start, even for runs whose morphology needs are fully satisfied
-- by the persisted parser cache (see @Kip.Cache@) and never touch the FSM
-- at all. We defer the actual read via 'unsafeInterleaveIO' so the cost is
-- only paid the first time some caller actually forces the returned 'FSM'
-- (i.e. the first real 'ups'/'downs' call, or never, if the process's morph
-- lookups are all cache hits).
--
-- Safety: the deferred action only reads a file and builds a self-contained
-- FSM value with no shared mutable state; if two threads race to force the
-- same thunk (via 'unsafeInterleaveIO's use of 'unsafeDupablePerformIO'),
-- at worst one redundant read/parse happens and one result is discarded --
-- never corruption.
fsmReadBinaryFile ::
  FilePath -- ^ Path to the compiled Foma binary file.
  -> IO FSM -- ^ Loaded FSM handle (lazily populated on first real use).
fsmReadBinaryFile path = do
  cached <- modifyMVar fsmCache $ \m ->
    case Map.lookup path m of
      Just fsm -> return (m, Just fsm)
      Nothing -> return (m, Nothing)
  case cached of
    Just fsm -> return fsm
    Nothing -> do
      fsm <- unsafeInterleaveIO (newCString path >>= fsmReadBinaryFile')
      modifyMVar fsmCache $ \m -> return (Map.insert path fsm m, ())
      return fsm

{-# NOINLINE fsmCache #-}
fsmCache :: MVar (Map.Map FilePath FSM)
fsmCache = unsafePerformIO (newMVar Map.empty)

-- | Release batch-allocated results.
foreign import ccall unsafe "morphology.h free_batch"
  freeBatch_ffi :: Ptr (Ptr CString) -> CInt -> IO ()

-- | Initialize a reusable apply handle for analysis.
foreign import ccall unsafe "morphology.h ups_handle_init"
  upsHandleInit_ffi :: FSM -> IO (Ptr ApplyHandle)
-- | Initialize a reusable apply handle for generation.
foreign import ccall unsafe "morphology.h downs_handle_init"
  downsHandleInit_ffi :: FSM -> IO (Ptr ApplyHandle)
-- | Free an apply handle.
foreign import ccall unsafe "&apply_handle_free"
  applyHandleFree_ffi :: FunPtr (Ptr ApplyHandle -> IO ())

-- | Batch FFI binding using a pre-initialized handle for analysis.
foreign import ccall unsafe "morphology.h ups_batch_handle"
  upsBatchHandle_ffi :: Ptr ApplyHandle -> Ptr CString -> CInt -> IO (Ptr (Ptr CString))
-- | Batch FFI binding using a pre-initialized handle for generation.
foreign import ccall unsafe "morphology.h downs_batch_handle"
  downsBatchHandle_ffi :: Ptr ApplyHandle -> Ptr CString -> CInt -> IO (Ptr (Ptr CString))

data ApplyHandleCache = ApplyHandleCache
  { ahUps :: !(MVar (Maybe (ForeignPtr ApplyHandle)))
  , ahDowns :: !(MVar (Maybe (ForeignPtr ApplyHandle)))
  }

newApplyHandleCache :: IO ApplyHandleCache
newApplyHandleCache = do
  upsVar <- newMVar Nothing
  downsVar <- newMVar Nothing
  return (ApplyHandleCache upsVar downsVar)

{-# NOINLINE applyHandleCache #-}
applyHandleCache :: MVar (Map.Map (Ptr ()) ApplyHandleCache)
applyHandleCache = unsafePerformIO (newMVar Map.empty)

getApplyHandleCache :: FSM -> IO ApplyHandleCache
getApplyHandleCache (FSM key) =
  modifyMVar applyHandleCache $ \m ->
    case Map.lookup key m of
      Just cache -> return (m, cache)
      Nothing -> do
        cache <- newApplyHandleCache
        return (Map.insert key cache m, cache)

withApplyHandle ::
  FSM
  -> (ApplyHandleCache -> MVar (Maybe (ForeignPtr ApplyHandle)))
  -> (FSM -> IO (Ptr ApplyHandle))
  -> (Ptr ApplyHandle -> IO a)
  -> IO a
withApplyHandle fsm pickHandle initHandle action = do
  cache <- getApplyHandleCache fsm
  modifyMVar (pickHandle cache) $ \mHandle -> do
    handle <- case mHandle of
      Just h -> return h
      Nothing -> do
        ptr <- initHandle fsm
        newForeignPtr applyHandleFree_ffi ptr
    res <- withForeignPtr handle action
    return (Just handle, res)

-- | Morphological analysis (surface form to analyses).
-- Uses 'Text' to match the parser and avoid extra conversions.
--
-- ==== Performance note (Optimization: reuse cached apply handle)
-- Previously this called the C @ups()@ entry point, which runs
-- @apply_init@ (building the FST's apply indices) on __every single call__.
-- Sampling showed this dominating warm-run time once the process-startup
-- costs were fixed, since callers still hit this single-word path for any
-- vocabulary not already covered by the persisted morphology cache. We now
-- route through 'upsBatch' (a one-element batch), which reuses the same
-- cached 'ApplyHandle' machinery already used by batch analysis, so
-- @apply_init@ only runs once per process per FSM.
ups ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Surface form to analyze.
  -> IO [Text] -- ^ Analyses returned by TRmorph.
ups fsm t = do
  results <- upsBatch fsm [t]
  case results of
    (r:_) -> return r
    [] -> return []

-- | Morphological generation (analysis to surface forms).
-- Uses 'Text' to match the parser and avoid extra conversions.
--
-- ==== Performance note (Optimization: reuse cached apply handle)
-- See 'ups': routes through 'downsBatch' to reuse the cached 'ApplyHandle'
-- instead of paying a fresh @apply_init@ per call.
downs ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Analysis string to realize.
  -> IO [Text] -- ^ Surface forms returned by TRmorph.
downs fsm t = do
  results <- downsBatch fsm [t]
  case results of
    (r:_) -> return r
    [] -> return []

-- | Batch morphological analysis (surface forms to analyses).
-- Reuses a single Foma apply handle to amortize setup costs across inputs.
upsBatch ::
  FSM -- ^ Morphology finite state machine.
  -> [Text] -- ^ Surface forms to analyze.
  -> IO [[Text]] -- ^ Analyses returned by TRmorph, per input.
upsBatch fsm = batchCallWithHandle fsm ahUps upsHandleInit_ffi upsBatchHandle_ffi

-- | Batch morphological generation (analysis strings to surface forms).
-- Reuses a single Foma apply handle to amortize setup costs across inputs.
downsBatch ::
  FSM -- ^ Morphology finite state machine.
  -> [Text] -- ^ Analysis strings to realize.
  -> IO [[Text]] -- ^ Surface forms returned by TRmorph, per input.
downsBatch fsm = batchCallWithHandle fsm ahDowns downsHandleInit_ffi downsBatchHandle_ffi

-- | Shared batch call helper to amortize Foma handle setup.
-- Marshals inputs to C, invokes a batch FFI entry point, and converts the
-- returned C strings back to 'Text' while ensuring allocations are freed.
batchCallWithHandle ::
  FSM -- ^ Morphology finite state machine.
  -> (ApplyHandleCache -> MVar (Maybe (ForeignPtr ApplyHandle))) -- ^ Handle selector.
  -> (FSM -> IO (Ptr ApplyHandle)) -- ^ Handle initializer.
  -> (Ptr ApplyHandle -> Ptr CString -> CInt -> IO (Ptr (Ptr CString))) -- ^ Batch FFI function.
  -> [Text] -- ^ Inputs to process.
  -> IO [[Text]] -- ^ Outputs per input.
batchCallWithHandle _ _ _ _ [] = return []
batchCallWithHandle fsm pickHandle initHandle ffi inputs = do
  let bss = map TE.encodeUtf8 inputs
      count = length bss
  withMany BS.useAsCString bss $ \cstrs ->
    withArray cstrs $ \carr -> do
      withApplyHandle fsm pickHandle initHandle $ \handle -> do
        res <- ffi handle carr (fromIntegral count)
        if res == nullPtr
          then return (replicate count [])
          else do
            rows <- peekArray count res
            results <- forM rows $ \row ->
              if row == nullPtr
                then return []
                else do
                  strs <- peekArray0 nullPtr row
                  forM strs $ \cstr -> do
                    bytes <- BSU.unsafePackCString cstr
                    let !txt = TE.decodeUtf8 bytes
                    return txt
            freeBatch_ffi res (fromIntegral count)
            return results

-- | Suggest dictionary words that are exactly one edit away from the input.
-- Candidate surfaces are generated locally and validated through TRmorph.
suggestEditDistance1 ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Input word.
  -> IO [Text] -- ^ Dictionary words at edit distance 1.
suggestEditDistance1 = suggestFromEditDistanceOnly

-- | Suggest usage-site hints by stripping repeated case-like suffixes.
-- This favors context-like fixes such as @tersininin -> tersi@.
suggestContextLike ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Input word.
  -> IO [Text] -- ^ TRmorph-valid stripped candidates.
suggestContextLike _ word | T.null word = return []
suggestContextLike fsm word = do
  let strippedCandidates = generateSuffixStrips word
  strippedAnalyses <- upsBatch fsm strippedCandidates
  let strippedValid =
        [ cand
        | (cand, as) <- zip strippedCandidates strippedAnalyses
        , not (null as)
        ]
  return (take 50 (sort (dedupStable strippedValid)))

-- | Generate one-edit surface candidates and validate them with TRmorph analysis.
suggestFromEditDistanceOnly ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Misspelled word.
  -> IO [Text] -- ^ Valid dictionary surface forms.
suggestFromEditDistanceOnly _ word | T.null word = return []
suggestFromEditDistanceOnly fsm word = do
  let candidates = generateEditDistance1 word
  analyses <- upsBatch fsm candidates
  let valid =
        [ cand
        | (cand, as) <- zip candidates analyses
        , not (null as)
        ]
      (sameLength, differentLength) = partition (\cand -> T.length cand == T.length word) valid
      ordered = sameLength ++ differentLength
  return (take 50 (sort (dedupStable ordered)))

-- | Generate unique candidates at Levenshtein distance 1.
generateEditDistance1 ::
  Text -- ^ Source word.
  -> [Text] -- ^ Unique candidates, in stable order.
generateEditDistance1 word =
  dedupStable (deletes ++ transposes ++ replaces ++ inserts)
  where
    turkishAlphabet :: [Char]
    turkishAlphabet = "abcçdefgğhıijklmnoöprsştuüvyzqwx"

    -- Share each prefix and suffix slice across all edit families. In
    -- particular, replacements no longer re-slice and re-index the word for
    -- every alphabet character.
    chars = T.unpack word
    prefixes = T.inits word
    suffixes = T.tails word
    positions = zip3 prefixes chars (drop 1 suffixes)
    boundaries = zip prefixes suffixes

    deletes =
      [ T.concat [prefix, suffix]
      | (prefix, _, suffix) <- positions
      ]

    transposes =
      [ T.concat [prefix, T.singleton next, T.singleton current, suffix]
      | (prefix, current, afterCurrent) <- positions
      , Just (next, suffix) <- [T.uncons afterCurrent]
      ]

    replaces =
      [ T.concat [prefix, T.singleton c, suffix]
      | (prefix, current, suffix) <- positions
      , c <- turkishAlphabet
      , c /= current
      ]

    inserts =
      [ T.concat [prefix, T.singleton c, suffix]
      | (prefix, suffix) <- boundaries
      , c <- turkishAlphabet
      ]

-- | Generate candidates by repeatedly stripping common case-like suffixes.
-- Candidates are validated via TRmorph before use.
generateSuffixStrips :: Text -> [Text]
generateSuffixStrips = go 0 []
  where
    -- Ordered longest-first to prefer removing the most specific suffix.
    suffixes =
      map T.pack
        [ "nının", "ninin", "nunun", "nünün"
        , "ının", "inin", "unun", "ünün"
        , "nın", "nin", "nun", "nün"
        , "den", "dan", "ten", "tan"
        , "ın", "in", "un", "ün"
        , "de", "da", "te", "ta"
        ]
    maxDepth = 4 :: Int

    go depth acc txt
      | depth >= maxDepth = reverse acc
      | otherwise =
          case firstStrip txt of
            Nothing -> reverse acc
            Just next
              | T.length next < 2 -> reverse acc
              | next `elem` acc -> reverse acc
              | otherwise -> go (depth + 1) (next : acc) next

    firstStrip txt = do
      suff <- find (\suff -> T.isSuffixOf suff txt && T.length txt > T.length suff) suffixes
      return (T.dropEnd (T.length suff) txt)

-- | Keep first occurrence of each item while preserving order.
dedupStable :: Ord a => [a] -> [a]
dedupStable = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs
