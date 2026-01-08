{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Low-level FFI bindings to the Foma morphology library.
module Language.Foma where

import Control.Monad
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Marshal.Array (withArray, peekArray, peekArray0)
import Foreign.Marshal.Utils (withMany)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- | Opaque handle for a Foma finite state machine.
newtype FSM = FSM (Ptr ())

-- | Raw FFI binding for reading a binary FSM file.
foreign import ccall unsafe "fomalib.h fsm_read_binary_file"
  fsmReadBinaryFile' :: CString -> IO FSM

-- | Read an FSM from a binary file on disk.
fsmReadBinaryFile ::
  FilePath -- ^ Path to the compiled Foma binary file.
  -> IO FSM -- ^ Loaded FSM handle.
fsmReadBinaryFile = newCString >=> fsmReadBinaryFile'

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
upsBatch = batchCall upsBatch_ffi

-- | Batch morphological generation (analysis strings to surface forms).
-- Reuses a single Foma apply handle to amortize setup costs across inputs.
downsBatch ::
  FSM -- ^ Morphology finite state machine.
  -> [Text] -- ^ Analysis strings to realize.
  -> IO [[Text]] -- ^ Surface forms returned by TRmorph, per input.
downsBatch = batchCall downsBatch_ffi

-- | Shared batch call helper to amortize Foma handle setup.
-- Marshals inputs to C, invokes a batch FFI entry point, and converts the
-- returned C strings back to 'Text' while ensuring allocations are freed.
batchCall ::
  (FSM -> Ptr CString -> CInt -> IO (Ptr (Ptr CString))) -- ^ Batch FFI function.
  -> FSM -- ^ Morphology finite state machine.
  -> [Text] -- ^ Inputs to process.
  -> IO [[Text]] -- ^ Outputs per input.
batchCall _ _ [] = return []
batchCall ffi fsm inputs = do
  let bss = map TE.encodeUtf8 inputs
      count = length bss
  withMany BS.useAsCString bss $ \cstrs ->
    withArray cstrs $ \carr -> do
      res <- ffi fsm carr (fromIntegral count)
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
