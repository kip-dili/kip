{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LSP fixture tests for kip-lsp.
module LspTest (lspTestsFor) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Char (isAsciiUpper)
import System.Directory (doesFileExist, getModificationTime, getTemporaryDirectory, listDirectory)
import System.FilePath (replaceExtension, takeExtension, takeFileName, (</>))
import System.IO (BufferMode(..), Handle, hClose, hFlush, hSetBuffering)
import System.Process (CreateProcess(..), ProcessHandle, StdStream(..), createProcess, proc, terminateProcess, waitForProcess)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Data.Time.Clock (UTCTime)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Concurrent (threadDelay)
import Data.Vector (toList)

-- | Expected behaviors for a single LSP fixture.
data LspSpec = LspSpec
  { specDiagnosticsAtLeast :: Int
  , specDiagnosticMessageContains :: [T.Text]
  , specFormattingEdits :: Bool
  , specFormattingNoop :: Bool
  , specHover :: Bool
  , specHoverAt :: Maybe PositionQuery
  , specHoverContains :: [T.Text]
  , specCompletionIncludes :: [T.Text]
  , specCompletionAt :: Maybe PositionQuery
  , specCache :: Bool
  , specCacheReuse :: Bool
  , specDefinitionAt :: Maybe DefinitionQuery
  }

-- | Decode fixture expectations from JSON.
instance A.FromJSON LspSpec where
  parseJSON = A.withObject "LspSpec" $ \obj -> do
    specDiagnosticsAtLeast <- obj A..:? "diagnosticsAtLeast" A..!= 0
    specDiagnosticMessageContains <- obj A..:? "diagnosticMessageContains" A..!= []
    specFormattingEdits <- obj A..:? "formattingEdits" A..!= False
    specFormattingNoop <- obj A..:? "formattingNoop" A..!= False
    specHover <- obj A..:? "hover" A..!= False
    specHoverAt <- obj A..:? "hoverAt"
    specHoverContains <- obj A..:? "hoverContains" A..!= []
    specCompletionIncludes <- obj A..:? "completionIncludes" A..!= []
    specCompletionAt <- obj A..:? "completionAt"
    specCache <- obj A..:? "cache" A..!= False
    specCacheReuse <- obj A..:? "cacheReuse" A..!= False
    specDefinitionAt <- obj A..:? "definitionAt"
    return LspSpec
      { specDiagnosticsAtLeast = specDiagnosticsAtLeast
      , specDiagnosticMessageContains = specDiagnosticMessageContains
      , specFormattingEdits = specFormattingEdits
      , specFormattingNoop = specFormattingNoop
      , specHover = specHover
      , specHoverAt = specHoverAt
      , specHoverContains = specHoverContains
      , specCompletionIncludes = specCompletionIncludes
      , specCompletionAt = specCompletionAt
      , specCache = specCache
      , specCacheReuse = specCacheReuse
      , specDefinitionAt = specDefinitionAt
      }

-- | Definition query position and minimum result count.
data DefinitionQuery = DefinitionQuery
  { defLine :: Int
  , defCharacter :: Int
  , defAtLeast :: Int
  , defExpectedLine :: Maybe Int
  , defExpectedCharacter :: Maybe Int
  , defUriContains :: Maybe T.Text
  }

-- | Decode definition query positions from JSON.
instance A.FromJSON DefinitionQuery where
  parseJSON = A.withObject "DefinitionQuery" $ \obj -> do
    defLine <- obj A..: "line"
    defCharacter <- obj A..: "character"
    defAtLeast <- obj A..:? "atLeast" A..!= 1
    defExpectedLine <- obj A..:? "expectedLine"
    defExpectedCharacter <- obj A..:? "expectedCharacter"
    defUriContains <- obj A..:? "uriContains"
    return DefinitionQuery
      { defLine = defLine
      , defCharacter = defCharacter
      , defAtLeast = defAtLeast
      , defExpectedLine = defExpectedLine
      , defExpectedCharacter = defExpectedCharacter
      , defUriContains = defUriContains
      }

-- | Hover/completion position for LSP requests.
data PositionQuery = PositionQuery
  { posLine :: Int
  , posCharacter :: Int
  }

-- | Decode hover/completion positions from JSON.
instance A.FromJSON PositionQuery where
  parseJSON = A.withObject "PositionQuery" $ \obj -> do
    posLine <- obj A..: "line"
    posCharacter <- obj A..: "character"
    return PositionQuery{posLine = posLine, posCharacter = posCharacter}

-- | Resolved fixture input and expectations.
data LspFixture = LspFixture
  { fixtureName :: String
  , fixturePath :: FilePath
  , fixtureContent :: T.Text
  , fixtureSpec :: LspSpec
  }

-- | Build all LSP tests for the given kip-lsp executable.
lspTestsFor :: FilePath -> IO [TestTree]
lspTestsFor lspPath = map (mkFixtureTest lspPath) <$> loadFixtures

-- | Load all fixtures under tests/lsp.
loadFixtures :: IO [LspFixture]
loadFixtures = do
  let dir = "tests" </> "lsp"
  entries <- listDirectory dir
  let kipFiles = sort [dir </> f | f <- entries, takeExtension f == ".kip"]
  mapM loadFixture kipFiles

-- | Load a single fixture from disk.
loadFixture :: FilePath -> IO LspFixture
loadFixture path = do
  content <- TIO.readFile path
  let specPath = replaceExtension path "json"
  spec <- loadSpec specPath
  let fixtureName = takeFileName path
  return LspFixture
    { fixtureName = fixtureName
    , fixturePath = path
    , fixtureContent = content
    , fixtureSpec = spec
    }

-- | Load the JSON spec for a fixture, or defaults when missing.
loadSpec :: FilePath -> IO LspSpec
loadSpec path = do
  exists <- doesFileExist path
  if not exists
    then return LspSpec
      { specDiagnosticsAtLeast = 0
      , specDiagnosticMessageContains = []
      , specFormattingEdits = False
      , specFormattingNoop = False
      , specHover = False
      , specHoverAt = Nothing
      , specHoverContains = []
      , specCompletionIncludes = []
      , specCompletionAt = Nothing
      , specCache = False
      , specCacheReuse = False
      , specDefinitionAt = Nothing
      }
    else do
      bytes <- BL.readFile path
      case A.eitherDecode bytes of
        Left err -> fail ("invalid LSP spec " ++ path ++ ": " ++ err)
        Right spec -> return spec

-- | Create a test for an individual fixture file.
mkFixtureTest :: FilePath -> LspFixture -> TestTree
mkFixtureTest lspPath fixture =
  testCase (fixtureName fixture) $ do
    tempDir <- getTemporaryDirectory
    let filePath = tempDir </> takeFileName (fixturePath fixture)
        content = fixtureContent fixture
        spec = fixtureSpec fixture
    TIO.writeFile filePath content
    let uri = pathToUri filePath
    if specCacheReuse spec
      then do
        runSession lspPath uri content spec True
        mtime1 <- cacheMTime filePath
        runSession lspPath uri content spec False
        mtime2 <- cacheMTime filePath
        case (mtime1, mtime2) of
          (Just a, Just b) -> assertEqual "cache mtime changed" a b
          _ -> assertFailure "cache was not written"
      else runSession lspPath uri content spec (specCache spec)

-- | Run an LSP session for a fixture and validate expectations.
runSession :: FilePath -> T.Text -> T.Text -> LspSpec -> Bool -> IO ()
runSession lspPath uri content spec doSave = do
  (inH, outH, errH, ph) <- startLsp lspPath
  (do
      sendMessage inH (initializeRequest 1 (Just uri))
      _ <- awaitResponseId outH 1
      sendMessage inH initializedNotification
      sendMessage inH (didOpenNotification uri content)
      diags <- expectDiagnosticsAtLeast outH uri (specDiagnosticsAtLeast spec)
      mapM_ (expectDiagnosticContains diags) (specDiagnosticMessageContains spec)
      when (specFormattingEdits spec) $ do
        sendMessage inH (formattingRequest 2 uri)
        expectNonEmptyEdits outH 2
      when (specFormattingNoop spec) $ do
        sendMessage inH (formattingRequest 2 uri)
        expectEmptyEdits outH 2
      when (specHover spec) $ do
        let (line, col) = positionOrDefault (specHoverAt spec)
        sendMessage inH (hoverRequest 3 uri line col)
        expectHover outH 3 (specHoverContains spec)
      unless (null (specCompletionIncludes spec)) $ do
        let (line, col) = positionOrDefault (specCompletionAt spec)
        sendMessage inH (completionRequest 4 uri line col)
        expectCompletion outH 4 (specCompletionIncludes spec)
      case specDefinitionAt spec of
        Nothing -> return ()
        Just defQuery -> do
          sendMessage inH (definitionRequest 6 uri defQuery)
          expectDefinition outH 6 uri defQuery
      when doSave $ do
        sendMessage inH (didSaveNotification uri)
        waitForCache uri
      sendMessage inH (shutdownRequest 5)
      _ <- awaitResponseId outH 5
      sendMessage inH exitNotification)
    `finally` cleanupLsp (inH, outH, errH, ph)

-- | Launch the LSP server and return stdio handles.
startLsp :: FilePath -> IO (Handle, Handle, Handle, ProcessHandle)
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

-- | Shut down the LSP process and close handles.
cleanupLsp :: (Handle, Handle, Handle, ProcessHandle) -> IO ()
cleanupLsp (inH, outH, errH, ph) = do
  hClose inH
  hClose outH
  hClose errH
  terminateProcess ph
  _ <- waitForProcess ph
  return ()

-- | Require a handle from a process launch.
mustHandle :: String -> Maybe Handle -> Handle
mustHandle name mh =
  case mh of
    Just h -> h
    Nothing -> error ("missing handle for " ++ name)

-- | Send a JSON-RPC message to the LSP server.
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

-- | Wait for diagnostics and ensure a minimum count.
expectDiagnosticsAtLeast :: Handle -> T.Text -> Int -> IO [A.Value]
expectDiagnosticsAtLeast h uri expected = do
  diags <- awaitMessage h (matchDiagnostics uri)
  let count = length diags
  assertBool "diagnostics count too small" (count >= expected)
  return diags

-- | Ensure diagnostic messages contain a substring.
expectDiagnosticContains :: [A.Value] -> T.Text -> IO ()
expectDiagnosticContains diags needle =
  unless (any (diagHas needle) diags) $
    assertFailure ("missing diagnostic substring: " ++ T.unpack needle)

-- | Check whether a diagnostic contains a substring.
diagHas :: T.Text -> A.Value -> Bool
diagHas needle =
  \case
    A.Object obj ->
      case lookupKey "message" obj of
        Just (A.String msg) -> T.isInfixOf needle msg
        _ -> False
    _ -> False

-- | Extract all diagnostic messages from a list of diagnostics.
debugDiagMessages :: [A.Value] -> [T.Text]
debugDiagMessages = mapMaybe diagMessage

-- | Extract a diagnostic message from a value.
diagMessage :: A.Value -> Maybe T.Text
diagMessage =
  \case
    A.Object obj ->
      case lookupKey "message" obj of
        Just (A.String msg) -> Just msg
        _ -> Nothing
    _ -> Nothing

-- | Match publishDiagnostics messages for a specific URI.
matchDiagnostics :: T.Text -> A.Value -> Maybe [A.Value]
matchDiagnostics uri =
  \case
    A.Object obj -> do
      A.String method <- lookupKey "method" obj
      if method /= "textDocument/publishDiagnostics"
        then Nothing
        else do
          A.Object params <- lookupKey "params" obj
          A.String diagUri <- lookupKey "uri" params
          if diagUri /= uri
            then Nothing
            else do
              A.Array diags <- lookupKey "diagnostics" params
              return (toList diags)
    _ -> Nothing

-- | Ensure formatting returns at least one edit.
expectNonEmptyEdits :: Handle -> Int -> IO ()
expectNonEmptyEdits h target = do
  obj <- awaitResponseId h target
  case lookupKey "result" obj of
    Just (A.Array edits) ->
      when (null edits) (assertFailure "expected formatting edits")
    _ -> assertFailure "expected formatting edits"

-- | Ensure formatting returns no edits.
expectEmptyEdits :: Handle -> Int -> IO ()
expectEmptyEdits h target = do
  obj <- awaitResponseId h target
  case lookupKey "result" obj of
    Just (A.Array edits) ->
      unless (null edits) (assertFailure "expected no formatting edits")
    _ -> assertFailure "expected no formatting edits"

-- | Ensure hover returns content and matches substrings if provided.
expectHover :: Handle -> Int -> [T.Text] -> IO ()
expectHover h target contains = do
  obj <- awaitResponseId h target
  case lookupKey "result" obj of
    Just A.Null -> assertFailure "expected hover result"
    Just val ->
      unless (null contains) $
        case hoverText val of
          Nothing -> assertFailure "missing hover contents"
          Just text -> mapM_ (assertHoverContains text) contains
    Nothing -> assertFailure "missing hover result"

-- | Assert a hover string contains a substring.
assertHoverContains :: T.Text -> T.Text -> IO ()
assertHoverContains text needle =
  unless (T.isInfixOf needle text) (assertFailure "hover missing expected text")

-- | Extract hover contents text when present.
hoverText :: A.Value -> Maybe T.Text
hoverText =
  \case
    A.Object obj -> do
      A.Object contents <- lookupKey "contents" obj
      case lookupKey "value" contents of
        Just (A.String txt) -> Just txt
        _ -> Nothing
    _ -> Nothing

-- | Ensure completion results contain required labels.
expectCompletion :: Handle -> Int -> [T.Text] -> IO ()
expectCompletion h target needles = do
  obj <- awaitResponseId h target
  let items = completionItems obj
  mapM_ (assertCompletion items) needles

-- | Assert a completion list contains a label.
assertCompletion :: [A.Value] -> T.Text -> IO ()
assertCompletion items needle =
  unless (any (completionHas needle) items) (assertFailure "missing completion item")

-- | Extract completion items from a completion response.
completionItems :: A.Object -> [A.Value]
completionItems obj =
  case lookupKey "result" obj of
    Just (A.Array items) -> toList items
    Just (A.Object resObj) ->
      case lookupKey "items" resObj of
        Just (A.Array items) -> toList items
        _ -> []
    _ -> []

-- | Check whether a completion item matches a label.
completionHas :: T.Text -> A.Value -> Bool
completionHas needle =
  \case
    A.Object obj ->
      case lookupKey "label" obj of
        Just (A.String label) -> label == needle
        _ -> False
    _ -> False

-- | Ensure definition results include the current document.
expectDefinition :: Handle -> Int -> T.Text -> DefinitionQuery -> IO ()
expectDefinition h target expectedUri defQuery = do
  obj <- awaitResponseId h target
  let (count, locations) = definitionLocations obj
  assertBool "definition result too small" (count >= defAtLeast defQuery)
  let expectedUri' = case defUriContains defQuery of
        Just needle -> Just needle
        Nothing -> Just expectedUri
  case expectedUri' of
    Nothing -> return ()
    Just uriNeedle ->
      assertBool "definition uri mismatch" (any (uriMatches uriNeedle) locations)
  case (defExpectedLine defQuery, defExpectedCharacter defQuery) of
    (Nothing, Nothing) -> return ()
    _ ->
      assertBool "definition range mismatch" (any (locationMatches defQuery) locations)

-- | Extract definition count and URIs from a response.
definitionLocations :: A.Object -> (Int, [LocationInfo])
definitionLocations obj =
  case lookupKey "result" obj of
    Just A.Null -> (0, [])
    Just (A.Array items) -> foldLocations (toList items)
    Just (A.Object item) -> foldLocations [A.Object item]
    _ -> (0, [])

-- | Fold location-like responses into counts and URIs.
data LocationInfo = LocationInfo
  { locUri :: T.Text
  , locLine :: Maybe Int
  , locCharacter :: Maybe Int
  }

foldLocations :: [A.Value] -> (Int, [LocationInfo])
foldLocations vals =
  let locations = mapMaybe locationInfo vals
  in (length vals, locations)

-- | Extract a URI from a Location/DefinitionLink value.
locationInfo :: A.Value -> Maybe LocationInfo
locationInfo =
  \case
    A.Object obj ->
      case lookupKey "uri" obj of
        Just (A.String uri) -> Just (LocationInfo uri (rangeLine obj) (rangeCharacter obj))
        _ ->
          case lookupKey "targetUri" obj of
            Just (A.String uri) ->
              case lookupKey "targetRange" obj of
                Just (A.Object rangeObj) ->
                  Just (LocationInfo uri (rangeLine rangeObj) (rangeCharacter rangeObj))
                _ -> Just (LocationInfo uri Nothing Nothing)
            _ -> Nothing
    _ -> Nothing

rangeLine :: A.Object -> Maybe Int
rangeLine obj =
  case lookupKey "range" obj of
    Just (A.Object rangeObj) ->
      case lookupKey "start" rangeObj of
        Just (A.Object startObj) ->
          case lookupKey "line" startObj of
            Just (A.Number n) -> toBoundedInteger n
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

rangeCharacter :: A.Object -> Maybe Int
rangeCharacter obj =
  case lookupKey "range" obj of
    Just (A.Object rangeObj) ->
      case lookupKey "start" rangeObj of
        Just (A.Object startObj) ->
          case lookupKey "character" startObj of
            Just (A.Number n) -> toBoundedInteger n
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

uriMatches :: T.Text -> LocationInfo -> Bool
uriMatches needle loc = needle `T.isInfixOf` locUri loc

locationMatches :: DefinitionQuery -> LocationInfo -> Bool
locationMatches defQuery loc =
  let lineOk =
        case defExpectedLine defQuery of
          Nothing -> True
          Just line -> Just line == locLine loc
      charOk =
        case defExpectedCharacter defQuery of
          Nothing -> True
          Just ch -> Just ch == locCharacter loc
  in lineOk && charOk

-- | Resolve a position query, defaulting to (0, 0).
positionOrDefault :: Maybe PositionQuery -> (Int, Int)
positionOrDefault mPos =
  case mPos of
    Nothing -> (0, 0)
    Just pos -> (posLine pos, posCharacter pos)

-- | Read messages until a match is found.
awaitMessage :: Handle -> (A.Value -> Maybe a) -> IO a
awaitMessage h match = do
  val <- recvMessage h
  case match val of
    Just res -> return res
    Nothing -> awaitMessage h match

-- | Receive a single JSON-RPC message.
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

-- | Consume message headers until the blank line.
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

-- | Lowercase ASCII characters without locale changes.
toLowerAscii :: Char -> Char
toLowerAscii c
  | isAsciiUpper c = toEnum (fromEnum c + 32)
  | otherwise = c

-- | Convert a file path to a file:// URI.
pathToUri :: FilePath -> T.Text
pathToUri path =
  let fixed = if take 1 path == "/" then path else "/" ++ path
  in T.pack ("file://" ++ fixed)

-- | Build an initialize request payload.
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

-- | Build an initialized notification payload.
initializedNotification :: A.Value
initializedNotification =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("initialized" :: String)
    , "params" A..= A.object []
    ]

-- | Build a didOpen notification payload.
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

-- | Build a didSave notification payload.
didSaveNotification :: T.Text -> A.Value
didSaveNotification uri =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("textDocument/didSave" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object
            [ "uri" A..= uri
            ]
        ]
    ]

-- | Build a formatting request payload.
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

-- | Build a hover request payload.
hoverRequest :: Int -> T.Text -> Int -> Int -> A.Value
hoverRequest reqId uri line col =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("textDocument/hover" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object ["uri" A..= uri]
        , "position" A..= A.object ["line" A..= line, "character" A..= col]
        ]
    ]

-- | Build a completion request payload.
completionRequest :: Int -> T.Text -> Int -> Int -> A.Value
completionRequest reqId uri line col =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("textDocument/completion" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object ["uri" A..= uri]
        , "position" A..= A.object ["line" A..= line, "character" A..= col]
        ]
    ]

-- | Build a definition request payload.
definitionRequest :: Int -> T.Text -> DefinitionQuery -> A.Value
definitionRequest reqId uri defQuery =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("textDocument/definition" :: String)
    , "params" A..= A.object
        [ "textDocument" A..= A.object ["uri" A..= uri]
        , "position" A..= A.object
            [ "line" A..= defLine defQuery
            , "character" A..= defCharacter defQuery
            ]
        ]
    ]

-- | Build a shutdown request payload.
shutdownRequest :: Int -> A.Value
shutdownRequest reqId =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "id" A..= reqId
    , "method" A..= ("shutdown" :: String)
    , "params" A..= A.Null
    ]

-- | Build an exit notification payload.
exitNotification :: A.Value
exitNotification =
  A.object
    [ "jsonrpc" A..= ("2.0" :: String)
    , "method" A..= ("exit" :: String)
    ]

-- | Wait for a cache file to appear for the given URI.
waitForCache :: T.Text -> IO ()
waitForCache uri = go 10
  where
    cachePath = replaceExtension (T.unpack (uriToPath uri)) "iz"
    go 0 = assertFailure "cache was not written"
    go n = do
      exists <- doesFileExist cachePath
      if exists
        then return ()
        else do
          threadDelay 50000
          go (n - 1)

-- | Read the modification time of a cache file if it exists.
cacheMTime :: FilePath -> IO (Maybe UTCTime)
cacheMTime path = do
  let cachePath = replaceExtension path "iz"
  exists <- doesFileExist cachePath
  if exists
    then Just <$> getModificationTime cachePath
    else return Nothing

-- | Strip the file:// prefix from a URI.
uriToPath :: T.Text -> T.Text
uriToPath uri =
  fromMaybe uri (T.stripPrefix "file://" uri)

-- | Look up a key from a JSON object.
lookupKey :: T.Text -> A.Object -> Maybe A.Value
lookupKey key = AKM.lookup (AK.fromText key)
