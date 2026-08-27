{-# LANGUAGE OverloadedStrings #-}
{- |
Module      : Main
Description : Reactor entrypoint exported to the browser worker.

This executable is built in WASM reactor mode and exports @kip_run@ so the
worker can call into the same WASM instance multiple times.

ABI contract for @kip_run@:

- arg0: mode (@0 = exec@, @1 = codegen-js@)
- arg1: language (@0 = tr@, @1 = en@)
- return: status (@0 = ok@, non-zero = error)

The source file is always expected at @/main.kip@ inside the WASI filesystem
prepared by the worker.
-}
module Main where

import Control.Exception (SomeException, displayException, fromException, try)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Foreign.C.Types (CInt(..))
import System.Exit (ExitCode(..))
import System.IO (BufferMode(..), hFlush, hPutStrLn, hSetBuffering, stderr, stdout)

import Kip.Playground
import Kip.Runner (Lang(..))

-- | Unused in reactor mode; required so Cabal can compile a Main module.
main :: IO ()
main = return ()

-- | Reactor run mode decoded from host-provided integers.
data ReactorMode
  = ReactorExec
  -- ^ Run @\/main.kip@ and report its exit status.
  | ReactorCodegenJs
  -- ^ Compile @\/main.kip@ to JavaScript and write it to standard output.
  deriving (Eq, Show)

-- | Structured reactor-level failures.
data ReactorError
  = UnknownReactorMode CInt
  -- ^ The host passed a mode integer that does not decode.
  | ReactorRuntimeFailure Text
  -- ^ The request threw an exception, described by this message.

-- | Render reactor failures in the selected language.
renderReactorError :: Lang -- ^ Diagnostic language.
                   -> ReactorError -- ^ Failure to describe.
                   -> Text -- ^ Localized message for standard error.
renderReactorError lang err =
  case (lang, err) of
    (LangTr, UnknownReactorMode n) -> "Bilinmeyen reaktör kipi: " <> T.pack (show n)
    (LangEn, UnknownReactorMode n) -> "Unknown reactor mode: " <> T.pack (show n)
    (LangTr, ReactorRuntimeFailure msg) -> "Reaktör çalışma hatası: " <> msg
    (LangEn, ReactorRuntimeFailure msg) -> "Reactor runtime failure: " <> msg

-- | Decode host mode integer into 'ReactorMode'.
decodeMode :: CInt -- ^ Mode integer supplied by the host.
           -> Either ReactorError ReactorMode -- ^ Decoded mode, or a failure for unknown values.
decodeMode n =
  case n of
    0 -> Right ReactorExec
    1 -> Right ReactorCodegenJs
    _ -> Left (UnknownReactorMode n)

-- | Decode host language integer into runner language.
decodeLang :: CInt -- ^ Language integer supplied by the host; 1 selects English.
           -> Lang -- ^ Decoded language, defaulting to Turkish.
decodeLang n =
  case n of
    1 -> LangEn
    _ -> LangTr

-- | Build a single isolated playground request for one reactor call.
mkRequest :: ReactorMode -- ^ What this call should do.
          -> Lang -- ^ Language diagnostics are reported in.
          -> PlaygroundRequest -- ^ Request over the fixed virtual entry file.
mkRequest mode lang =
  PlaygroundRequest
    { prMode =
        case mode of
          ReactorExec -> PlaygroundExec
          ReactorCodegenJs -> PlaygroundCodegen "js"
    , prFiles = ["/main.kip"]
    , prIncludeDirs = []
    , prLang = lang
    , prNoPrelude = False
    }

{- |
Execute one call from the host.

Each call delegates to 'runPlaygroundRequest', which guarantees fresh runtime
state and therefore prevents user-definition leakage between consecutive calls.
-}
kipRun :: CInt -- ^ Mode integer supplied by the host.
       -> CInt -- ^ Language integer supplied by the host.
       -> IO CInt -- ^ Process-style status: 0 on success, 2 for an unknown mode.
kipRun modeRaw langRaw =
  let lang = decodeLang langRaw
      emitReactorError :: ReactorError -> IO ()
      emitReactorError e = hPutStrLn stderr (T.unpack (renderReactorError lang e))
  in
  do
    -- Reactor mode reuses one long-lived WASM instance, so process-exit flushes
    -- do not happen between requests. Keep stdio unbuffered to avoid truncated
    -- stdout/stderr payloads (e.g. large codegen output).
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering
    case decodeMode modeRaw of
      Left err -> do
        emitReactorError err
        return 2
      Right mode -> do
        result <- try (runPlaygroundRequest (mkRequest mode lang)) :: IO (Either SomeException PlaygroundOutput)
        case result of
          Left err -> do
            case fromException err of
              Just ExitSuccess -> return 0
              Just (ExitFailure n) -> return (fromIntegral n)
              Nothing -> do
                emitReactorError (ReactorRuntimeFailure (T.pack (displayException err)))
                return 1
          Right PlaygroundNoOutput ->
            return 0
          Right (PlaygroundTextOutput txt) -> do
            TIO.putStrLn txt
            hFlush stdout
            return 0

foreign export ccall kip_run :: CInt -> CInt -> IO CInt

-- | C entry point exported to the WASM host; see 'kipRun'.
kip_run :: CInt -- ^ Mode integer supplied by the host.
        -> CInt -- ^ Language integer supplied by the host.
        -> IO CInt -- ^ Process-style status code.
kip_run = kipRun
