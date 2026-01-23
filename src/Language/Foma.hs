{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level FFI bindings to the Foma morphology library.
module Language.Foma where

import Control.Monad
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.Marshal
import Foreign.Marshal.Array (withArray, peekArray, peekArray0)
import Foreign.Marshal.Utils (withMany)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Opaque handle for a Foma finite state machine.
newtype FSM = FSM (Ptr ())

-- | Opaque handle for a Foma apply handle.
data ApplyHandle

-- | Raw FFI binding for reading a binary FSM file.
foreign import ccall unsafe "fomalib.h fsm_read_binary_file"
  fsmReadBinaryFile' :: CString -> IO FSM

-- | Read an FSM from a binary file on disk.
fsmReadBinaryFile ::
  FilePath -- ^ Path to the compiled Foma binary file.
  -> IO FSM -- ^ Loaded FSM handle.
fsmReadBinaryFile path = do
  cached <- modifyMVar fsmCache $ \m ->
    case Map.lookup path m of
      Just fsm -> return (m, Just fsm)
      Nothing -> return (m, Nothing)
  case cached of
    Just fsm -> return fsm
    Nothing -> do
      fsm <- newCString path >>= fsmReadBinaryFile'
      modifyMVar fsmCache $ \m -> return (Map.insert path fsm m, ())
      return fsm

{-# NOINLINE fsmCache #-}
fsmCache :: MVar (Map.Map FilePath FSM)
fsmCache = unsafePerformIO (newMVar Map.empty)

-- | FFI binding for morphological analysis; safe to call as it never calls back into Haskell.
foreign import ccall unsafe "morphology.h ups"
  ups_ffi :: FSM -> CString -> IO (Ptr CString)
-- | FFI binding for morphological generation; safe to call as it never calls back into Haskell.
foreign import ccall unsafe "morphology.h downs"
  downs_ffi :: FSM -> CString -> IO (Ptr CString)
-- | Batch FFI binding for morphological analysis (amortizes apply_init).
foreign import ccall unsafe "morphology.h ups_batch"
  upsBatch_ffi :: FSM -> Ptr CString -> CInt -> IO (Ptr (Ptr CString))
-- | Batch FFI binding for morphological generation (amortizes apply_init).
foreign import ccall unsafe "morphology.h downs_batch"
  downsBatch_ffi :: FSM -> Ptr CString -> CInt -> IO (Ptr (Ptr CString))
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
ups ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Surface form to analyze.
  -> IO [Text] -- ^ Analyses returned by TRmorph.
ups fsm t = do
  let bs = TE.encodeUtf8 t
  -- useAsCString adds a null terminator and avoids allocation for short strings.
  BS.useAsCString bs $ \cs -> do
    res <- ups_ffi fsm cs
    arr <- peekArray0 nullPtr res
    -- Convert each CString to Text without an extra copy, then free.
    results <- forM arr $ \cstr -> do
      bytes <- BSU.unsafePackCString cstr
      let !txt = TE.decodeUtf8 bytes
      free cstr
      return txt
    free res
    return results

-- | Morphological generation (analysis to surface forms).
-- Uses 'Text' to match the parser and avoid extra conversions.
downs ::
  FSM -- ^ Morphology finite state machine.
  -> Text -- ^ Analysis string to realize.
  -> IO [Text] -- ^ Surface forms returned by TRmorph.
downs fsm t = do
  let bs = TE.encodeUtf8 t
  BS.useAsCString bs $ \cs -> do
    res <- downs_ffi fsm cs
    arr <- peekArray0 nullPtr res
    results <- forM arr $ \cstr -> do
      bytes <- BSU.unsafePackCString cstr
      let !txt = TE.decodeUtf8 bytes
      free cstr
      return txt
    free res
    return results

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
