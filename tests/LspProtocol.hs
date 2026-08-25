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
startLsp :: FilePath -> IO LspProcess
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
cleanupLsp :: LspProcess -> IO ()
cleanupLsp (inH, outH, errH, ph) = do
  hClose inH
  hClose outH
  hClose errH
  terminateProcess ph
  _ <- waitForProcess ph
  return ()

-- | Require a handle from a process launch.
mustHandle :: String -> Maybe Handle -> Handle
mustHandle name = fromMaybe (error ("missing handle for " ++ name))

-- | Send one JSON-RPC message.
sendMessage :: Handle -> A.Value -> IO ()
sendMessage h val = do
  let body = A.encode val
      header = "Content-Length: " ++ show (BL.length body) ++ "\r\n\r\n"
  B8.hPut h (B8.pack header)
  BL.hPut h body
  hFlush h

-- | Wait for a response with a matching request id.
awaitResponseId :: Handle -> Int -> IO A.Object
awaitResponseId h target =
  awaitMessage h $ \case
    A.Object obj ->
      case lookupKey "id" obj of
        Just (A.Number n)
          | toBoundedInteger n == Just target -> Just obj
        _ -> Nothing
    _ -> Nothing

-- | Read messages until one matches a predicate.
awaitMessage :: Handle -> (A.Value -> Maybe a) -> IO a
awaitMessage h match = do
  val <- recvMessage h
  case match val of
    Just res -> return res
    Nothing -> awaitMessage h match

-- | Receive one JSON-RPC message.
recvMessage :: Handle -> IO A.Value
recvMessage h = do
  len <- readContentLength h
  body <- B8.hGet h len
  case A.decodeStrict body of
    Just val -> return val
    Nothing -> recvMessage h

-- | Read the Content-Length header for the next message.
readContentLength :: Handle -> IO Int
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
readHeaders :: Handle -> IO ()
readHeaders h = do
  line <- B8.hGetLine h
  let trimmed = B8.takeWhile (/= '\r') line
  unless (B8.null trimmed) (readHeaders h)

-- | Parse an integer from ASCII bytes.
readMaybeInt :: B8.ByteString -> Maybe Int
readMaybeInt bs =
  case reads (B8.unpack bs) of
    [(n, "")] -> Just n
    _ -> Nothing

-- | Lowercase an ASCII character without locale-dependent behavior.
toLowerAscii :: Char -> Char
toLowerAscii c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c

-- | Convert a file path to a file URI.
pathToUri :: FilePath -> T.Text
pathToUri path =
  let fixed = if take 1 path == "/" then path else "/" ++ path
  in T.pack ("file://" ++ fixed)

-- | Build an initialize request.
initializeRequest :: Int -> Maybe T.Text -> A.Value
initializeRequest reqId rootUri =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("initialize" :: String)
    , "params" A..= A.object
        [ "processId" A..= A.Null
        , "rootUri" A..= maybe A.Null A.String rootUri
        , "capabilities" A..= A.object []
        , "workspaceFolders" A..= A.Null
        ]
    ]

-- | Build an initialized notification.
initializedNotification :: A.Value
initializedNotification =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("initialized" :: String)
    , "params" A..= A.object []
    ]

-- | Build a didOpen notification.
didOpenNotification :: T.Text -> T.Text -> A.Value
didOpenNotification uri content =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("textDocument/didOpen" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object
            [ "uri" A..= uri
            , "languageId" A..= ("kip" :: String)
            , "version" A..= (1 :: Int)
            , "text" A..= content
            ]
        ]
    ]

-- | Build a didChange notification that appends a ranged edit.
didChangeAppendNotification :: T.Text -> Int -> T.Text -> T.Text -> A.Value
didChangeAppendNotification uri version oldText suffix =
  let (line, character) = endPosition oldText
  in A.object
      [ "jsonrpc" A..= ("2.0" :: String)
      , "method" A..= ("textDocument/didChange" :: String)
      , "params" A..= A.object
          [ "textDocument" A..= A.object
              [ "uri" A..= uri
              , "version" A..= version
              ]
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
      ]

-- | Build a didChange notification containing a full document replacement.
didChangeWholeNotification :: T.Text -> Int -> T.Text -> A.Value
didChangeWholeNotification uri version text =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("textDocument/didChange" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object
            [ "uri" A..= uri
            , "version" A..= version
            ]
        , "contentChanges" A..= [A.object ["text" A..= text]]
        ]
    ]

-- | Send append edits one character at a time with increasing versions.
sendCharwiseAppend :: Handle -> T.Text -> Int -> T.Text -> T.Text -> IO ()
sendCharwiseAppend h uri versionStart oldText suffix =
  go versionStart oldText (T.unpack suffix)
  where
    go _ _ [] = return ()
    go version current (c:rest) = do
      let chunk = T.singleton c
      sendMessage h (didChangeAppendNotification uri version current chunk)
      go (version + 1) (current <> chunk) rest

-- | Build a didSave notification.
didSaveNotification :: T.Text -> A.Value
didSaveNotification uri =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("textDocument/didSave" :: String)
    , "params" A..= A.object ["textDocument" A..= A.object ["uri" A..= uri]]
    ]

-- | Build a formatting request.
formattingRequest :: Int -> T.Text -> A.Value
formattingRequest reqId uri =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("textDocument/formatting" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object ["uri" A..= uri]
        , "options" A..= A.object
            [ "tabSize" A..= (2 :: Int)
            , "insertSpaces" A..= True
            ]
        ]
    ]

-- | Build a position-based text document request.
positionRequest :: T.Text -> Int -> T.Text -> Int -> Int -> A.Value
positionRequest method reqId uri line col =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= method
    , "params" A..= A.object
        [ "textDocument" A..= A.object ["uri" A..= uri]
        , "position" A..= A.object ["line" A..= line, "character" A..= col]
        ]
    ]

-- | Build a hover request.
hoverRequest :: Int -> T.Text -> Int -> Int -> A.Value
hoverRequest = positionRequest "textDocument/hover"

-- | Build a completion request.
completionRequest :: Int -> T.Text -> Int -> Int -> A.Value
completionRequest = positionRequest "textDocument/completion"

-- | Build a shutdown request.
shutdownRequest :: Int -> A.Value
shutdownRequest reqId =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("shutdown" :: String)
    , "params" A..= A.Null
    ]

-- | Build an exit notification.
exitNotification :: A.Value
exitNotification =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("exit" :: String)
    ]

-- | Compute the final LSP position of a text buffer.
endPosition :: T.Text -> (Int, Int)
endPosition txt =
  let ls = T.splitOn "\n" txt
  in case reverse ls of
       [] -> (0, 0)
       lastLine:_ -> (length ls - 1, T.length lastLine)

-- | Look up a key in a JSON object.
lookupKey :: T.Text -> A.Object -> Maybe A.Value
lookupKey key = AKM.lookup (AK.fromText key)
