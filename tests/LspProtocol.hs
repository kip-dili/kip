{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | JSON-RPC transport and message builders for LSP integration tests.
module LspProtocol
  ( LspProcess
  , startLsp
  , cleanupLsp
  , sendMessage
  , awaitResponseId
  , awaitMessage
  , pathToUri
  , initializeRequest
  , initializedNotification
  , didOpenNotification
  , didChangeAppendNotification
  , didChangeWholeNotification
  , sendCharwiseAppend
  , didSaveNotification
  , formattingRequest
  , hoverRequest
  , completionRequest
  , positionRequest
  , shutdownRequest
  , exitNotification
  , lookupKey
  ) where

import Control.Monad (unless)
import Data.Char (isAsciiUpper)
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.IO (BufferMode(..), Handle, hClose, hFlush, hSetBuffering)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )

-- | Handles and process state for a running LSP server.
type LspProcess = (Handle, Handle, Handle, ProcessHandle)

-- | Launch the LSP server and return its stdio handles.
startLsp :: FilePath -- ^ Path to the @kip-lsp@ executable.
         -> IO LspProcess -- ^ The running server with its stdio handles.
startLsp lspPath = do
  let cp = (proc lspPath [])
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }
  (mIn, mOut, mErr, ph) <- createProcess cp
  let inH = mustHandle "stdin" mIn
      outH = mustHandle "stdout" mOut
      errH = mustHandle "stderr" mErr
  hSetBuffering inH NoBuffering
  hSetBuffering outH NoBuffering
  hSetBuffering errH NoBuffering
  return (inH, outH, errH, ph)

-- | Shut down an LSP process and close its handles.
cleanupLsp :: LspProcess -- ^ Server to shut down.
           -> IO () -- ^ Closes the handles and waits for the process to exit.
cleanupLsp (inH, outH, errH, ph) = do
  hClose inH
  hClose outH
  hClose errH
  terminateProcess ph
  _ <- waitForProcess ph
  return ()

-- | Require a handle from a process launch.
mustHandle :: String -- ^ Name of the stream, used in the failure message.
           -> Maybe Handle -- ^ Handle produced by the process launch.
           -> Handle -- ^ The handle; throws when it is absent.
mustHandle name = fromMaybe (error ("missing handle for " ++ name))

-- | Send one JSON-RPC message.
sendMessage :: Handle -- ^ Server's standard input.
            -> A.Value -- ^ Message to send.
            -> IO () -- ^ Writes the message with its @Content-Length@ header.
sendMessage h val = do
  let body = A.encode val
      header = "Content-Length: " ++ show (BL.length body) ++ "\r\n\r\n"
  B8.hPut h (B8.pack header)
  BL.hPut h body
  hFlush h

-- | Wait for a response with a matching request id.
awaitResponseId :: Handle -- ^ Server's standard output.
                -> Int -- ^ Request id to wait for.
                -> IO A.Object -- ^ The response object with that id.
awaitResponseId h target =
  awaitMessage h $ \case
    A.Object obj ->
      case lookupKey "id" obj of
        Just (A.Number n)
          | toBoundedInteger n == Just target -> Just obj
        _ -> Nothing
    _ -> Nothing

-- | Read messages until one matches a predicate.
awaitMessage :: Handle -- ^ Server's standard output.
             -> (A.Value -> Maybe a) -- ^ Selector returning a value for the message being awaited.
             -> IO a -- ^ The first selected value.
awaitMessage h match = do
  val <- recvMessage h
  case match val of
    Just res -> return res
    Nothing -> awaitMessage h match

-- | Receive one JSON-RPC message.
recvMessage :: Handle -- ^ Server's standard output.
            -> IO A.Value -- ^ The next JSON-RPC message.
recvMessage h = do
  len <- readContentLength h
  body <- B8.hGet h len
  case A.decodeStrict body of
    Just val -> return val
    Nothing -> recvMessage h

-- | Read the Content-Length header for the next message.
readContentLength :: Handle -- ^ Server's standard output.
                  -> IO Int -- ^ Byte length of the message body that follows.
readContentLength h = go
  where
    go = do
      line <- B8.hGetLine h
      let trimmed = B8.takeWhile (/= '\r') line
      if B8.null trimmed
        then go
        else
          case B8.break (== ':') trimmed of
            (key, rest)
              | B8.map toLowerAscii key == "content-length" ->
                  case readMaybeInt (B8.dropWhile (== ' ') (B8.drop 1 rest)) of
                    Just n -> do
                      readHeaders h
                      return n
                    Nothing -> go
            _ -> go

-- | Consume message headers through the blank separator line.
readHeaders :: Handle -- ^ Server's standard output.
            -> IO () -- ^ Consumes header lines through the blank separator.
readHeaders h = do
  line <- B8.hGetLine h
  let trimmed = B8.takeWhile (/= '\r') line
  unless (B8.null trimmed) (readHeaders h)

-- | Parse an integer from ASCII bytes.
readMaybeInt :: B8.ByteString -- ^ ASCII digits to parse.
             -> Maybe Int -- ^ The integer, when the whole input is numeric.
readMaybeInt bs =
  case reads (B8.unpack bs) of
    [(n, "")] -> Just n
    _ -> Nothing

-- | Lowercase an ASCII character without locale-dependent behavior.
toLowerAscii :: Char -- ^ Character to fold.
             -> Char -- ^ Lowercase form for ASCII letters, unchanged otherwise.
toLowerAscii c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c

-- | Convert a file path to a file URI.
pathToUri :: FilePath -- ^ Absolute file path.
          -> T.Text -- ^ Equivalent @file:@ URI.
pathToUri path =
  let fixed = if take 1 path == "/" then path else "/" ++ path
  in T.pack ("file://" ++ fixed)

-- | Build a JSON-RPC request envelope.
request :: Int -- ^ Request id.
        -> T.Text -- ^ Method name.
        -> A.Value -- ^ Parameters object.
        -> A.Value -- ^ The JSON-RPC request.
request reqId method params =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= method
    , "params" A..= params
    ]

-- | Build a JSON-RPC notification envelope.
notification :: T.Text -- ^ Method name.
             -> Maybe A.Value -- ^ Parameters object, when the method takes one.
             -> A.Value -- ^ The JSON-RPC notification.
notification method params =
  A.object
    ( [ "jsonrpc" A..= ("2.0" :: String)
      , "method" A..= method
      ]
      ++ maybe [] (\value -> ["params" A..= value]) params
    )

-- | Build the common text-document identifier object.
textDocument :: T.Text -- ^ Document URI.
             -> A.Value -- ^ A @TextDocumentIdentifier@ object.
textDocument uri = A.object ["uri" A..= uri]

-- | Build a versioned text-document identifier object.
versionedTextDocument :: T.Text -- ^ Document URI.
                      -> Int -- ^ Document version.
                      -> A.Value -- ^ A @VersionedTextDocumentIdentifier@ object.
versionedTextDocument uri version =
  A.object ["uri" A..= uri, "version" A..= version]

-- | Build an initialize request.
initializeRequest :: Int -- ^ Request id.
                  -> Maybe T.Text -- ^ Workspace root URI, when the session has one.
                  -> A.Value -- ^ The initialize request.
initializeRequest reqId rootUri =
  request reqId "initialize" $ A.object
    [ "processId" A..= A.Null
    , "rootUri" A..= maybe A.Null A.String rootUri
    , "capabilities" A..= A.object []
    , "workspaceFolders" A..= A.Null
    ]

-- | Build an initialized notification.
initializedNotification :: A.Value
initializedNotification =
  notification "initialized" (Just (A.object []))

-- | Build a didOpen notification.
didOpenNotification :: T.Text -- ^ Document URI.
                    -> T.Text -- ^ Initial document text.
                    -> A.Value -- ^ The didOpen notification.
didOpenNotification uri content =
  notification "textDocument/didOpen" . Just $ A.object
    [ "textDocument" A..= A.object
        [ "uri" A..= uri
        , "languageId" A..= ("kip" :: String)
        , "version" A..= (1 :: Int)
        , "text" A..= content
        ]
    ]

-- | Build a didChange notification that appends a ranged edit.
didChangeAppendNotification :: T.Text -- ^ Document URI.
                            -> Int -- ^ New document version.
                            -> T.Text -- ^ Current document text, used to locate the end position.
                            -> T.Text -- ^ Text to append.
                            -> A.Value -- ^ The didChange notification carrying a ranged edit.
didChangeAppendNotification uri version oldText suffix =
  let (line, character) = endPosition oldText
  in notification "textDocument/didChange" . Just $ A.object
      [ "textDocument" A..= versionedTextDocument uri version
      , "contentChanges" A..=
          [ A.object
              [ "range" A..= A.object
                  [ "start" A..= A.object ["line" A..= line, "character" A..= character]
                  , "end" A..= A.object ["line" A..= line, "character" A..= character]
                  ]
              , "rangeLength" A..= (0 :: Int)
              , "text" A..= suffix
              ]
          ]
      ]

-- | Build a didChange notification containing a full document replacement.
didChangeWholeNotification :: T.Text -- ^ Document URI.
                           -> Int -- ^ New document version.
                           -> T.Text -- ^ Replacement text for the whole document.
                           -> A.Value -- ^ The didChange notification carrying a full replacement.
didChangeWholeNotification uri version text =
  notification "textDocument/didChange" . Just $ A.object
    [ "textDocument" A..= versionedTextDocument uri version
    , "contentChanges" A..= [A.object ["text" A..= text]]
    ]

-- | Send append edits one character at a time with increasing versions.
sendCharwiseAppend :: Handle -- ^ Server's standard input.
                   -> T.Text -- ^ Document URI.
                   -> Int -- ^ Version to use for the first edit.
                   -> T.Text -- ^ Current document text.
                   -> T.Text -- ^ Text to append, one character per notification.
                   -> IO () -- ^ Sends one didChange per character.
sendCharwiseAppend h uri versionStart oldText suffix =
  go versionStart oldText (T.unpack suffix)
  where
    go _ _ [] = return ()
    go version current (c:rest) = do
      let chunk = T.singleton c
      sendMessage h (didChangeAppendNotification uri version current chunk)
      go (version + 1) (current <> chunk) rest

-- | Build a didSave notification.
didSaveNotification :: T.Text -- ^ Document URI.
                    -> A.Value -- ^ The didSave notification.
didSaveNotification uri =
  notification "textDocument/didSave" (Just (A.object ["textDocument" A..= textDocument uri]))

-- | Build a formatting request.
formattingRequest :: Int -- ^ Request id.
                  -> T.Text -- ^ Document URI.
                  -> A.Value -- ^ The formatting request.
formattingRequest reqId uri =
  request reqId "textDocument/formatting" $ A.object
    [ "textDocument" A..= textDocument uri
    , "options" A..= A.object
        [ "tabSize" A..= (2 :: Int)
        , "insertSpaces" A..= True
        ]
    ]

-- | Build a position-based text document request.
positionRequest :: T.Text -- ^ Method name.
                -> Int -- ^ Request id.
                -> T.Text -- ^ Document URI.
                -> Int -- ^ Zero-based line number.
                -> Int -- ^ Zero-based character offset.
                -> A.Value -- ^ The request for that method and position.
positionRequest method reqId uri line col =
  request reqId method $ A.object
    [ "textDocument" A..= textDocument uri
    , "position" A..= A.object ["line" A..= line, "character" A..= col]
    ]

-- | Build a hover request.
hoverRequest :: Int -- ^ Request id.
             -> T.Text -- ^ Document URI.
             -> Int -- ^ Zero-based line number.
             -> Int -- ^ Zero-based character offset.
             -> A.Value -- ^ The hover request.
hoverRequest = positionRequest "textDocument/hover"

-- | Build a completion request.
completionRequest :: Int -- ^ Request id.
                  -> T.Text -- ^ Document URI.
                  -> Int -- ^ Zero-based line number.
                  -> Int -- ^ Zero-based character offset.
                  -> A.Value -- ^ The completion request.
completionRequest = positionRequest "textDocument/completion"

-- | Build a shutdown request.
shutdownRequest :: Int -- ^ Request id.
                -> A.Value -- ^ The shutdown request.
shutdownRequest reqId = request reqId "shutdown" A.Null

-- | Build an exit notification.
exitNotification :: A.Value
exitNotification = notification "exit" Nothing

-- | Compute the final LSP position of a text buffer.
endPosition :: T.Text -- ^ Document text.
            -> (Int, Int) -- ^ Zero-based line and character just past the last character.
endPosition txt =
  let ls = T.splitOn "\n" txt
  in case reverse ls of
       [] -> (0, 0)
       lastLine:_ -> (length ls - 1, T.length lastLine)

-- | Look up a key in a JSON object.
lookupKey :: T.Text -- ^ Key to look up.
          -> A.Object -- ^ JSON object to search.
          -> Maybe A.Value -- ^ Value stored under that key.
lookupKey key = AKM.lookup (AK.fromText key)
