{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | LSP fixture tests for kip-lsp.
module LspTest (lspTestsFor) where

import Control.Exception (finally)
import Control.Monad (unless, when)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import System.Directory (doesFileExist, getModificationTime, getTemporaryDirectory, listDirectory)
import System.FilePath (replaceExtension, takeExtension, takeFileName, (</>))
import System.IO (Handle)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, assertFailure, testCase)
import Data.Time.Clock (UTCTime)
import System.Timeout (timeout)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Concurrent (threadDelay)
import Data.Vector (toList)

import LspProtocol

-- | Expected behaviors for a single LSP fixture.
data LspSpec = LspSpec
  { specDiagnosticsAtLeast :: Int
  , specDiagnosticsAfterChangeAtMost :: Maybe Int
  , specDiagnosticsAfterAppendAtLeast :: Maybe Int
  , specDiagnosticsAfterRevertAtMost :: Maybe Int
  , specDiagnosticMessageContains :: [T.Text]
  , specFormattingEdits :: Bool
  , specFormattingNoop :: Bool
  , specHover :: Bool
  , specHoverAt :: Maybe PositionQuery
  , specHoverContains :: [T.Text]
  , specHoverEquals :: Maybe T.Text
  , specHoverNotContains :: [T.Text]
  , specHoverChecks :: [HoverCheck]
  , specCompletionIncludes :: [T.Text]
  , specCompletionAt :: Maybe PositionQuery
  , specCache :: Bool
  , specCacheReuse :: Bool
  , specDidChangeAppend :: Maybe T.Text
  , specDidChangeRevertToOriginal :: Bool
  , specDidChangeAppendCharwise :: Bool
  , specDefinitionAt :: Maybe DefinitionQuery
  , specTypeDefinitionAt :: Maybe DefinitionQuery
  , specDocumentHighlightAt :: Maybe HighlightQuery
  }

-- | Decode fixture expectations from JSON.
instance A.FromJSON LspSpec where
  parseJSON = A.withObject "LspSpec" $ \obj -> do
    specDiagnosticsAtLeast <- obj A..:? "diagnosticsAtLeast" A..!= 0
    specDiagnosticsAfterChangeAtMost <- obj A..:? "diagnosticsAfterChangeAtMost"
    specDiagnosticsAfterAppendAtLeast <- obj A..:? "diagnosticsAfterAppendAtLeast"
    specDiagnosticsAfterRevertAtMost <- obj A..:? "diagnosticsAfterRevertAtMost"
    specDiagnosticMessageContains <- obj A..:? "diagnosticMessageContains" A..!= []
    specFormattingEdits <- obj A..:? "formattingEdits" A..!= False
    specFormattingNoop <- obj A..:? "formattingNoop" A..!= False
    specHover <- obj A..:? "hover" A..!= False
    specHoverAt <- obj A..:? "hoverAt"
    specHoverContains <- obj A..:? "hoverContains" A..!= []
    specHoverEquals <- obj A..:? "hoverEquals"
    specHoverNotContains <- obj A..:? "hoverNotContains" A..!= []
    specHoverChecks <- obj A..:? "hoverChecks" A..!= []
    specCompletionIncludes <- obj A..:? "completionIncludes" A..!= []
    specCompletionAt <- obj A..:? "completionAt"
    specCache <- obj A..:? "cache" A..!= False
    specCacheReuse <- obj A..:? "cacheReuse" A..!= False
    specDidChangeAppend <- obj A..:? "didChangeAppend"
    specDidChangeRevertToOriginal <- obj A..:? "didChangeRevertToOriginal" A..!= False
    specDidChangeAppendCharwise <- obj A..:? "didChangeAppendCharwise" A..!= False
    specDefinitionAt <- obj A..:? "definitionAt"
    specTypeDefinitionAt <- obj A..:? "typeDefinitionAt"
    specDocumentHighlightAt <- obj A..:? "documentHighlightAt"
    return LspSpec
      { specDiagnosticsAtLeast = specDiagnosticsAtLeast
      , specDiagnosticsAfterChangeAtMost = specDiagnosticsAfterChangeAtMost
      , specDiagnosticsAfterAppendAtLeast = specDiagnosticsAfterAppendAtLeast
      , specDiagnosticsAfterRevertAtMost = specDiagnosticsAfterRevertAtMost
      , specDiagnosticMessageContains = specDiagnosticMessageContains
      , specFormattingEdits = specFormattingEdits
      , specFormattingNoop = specFormattingNoop
      , specHover = specHover
      , specHoverAt = specHoverAt
      , specHoverContains = specHoverContains
      , specHoverEquals = specHoverEquals
      , specHoverNotContains = specHoverNotContains
      , specHoverChecks = specHoverChecks
      , specCompletionIncludes = specCompletionIncludes
      , specCompletionAt = specCompletionAt
      , specCache = specCache
      , specCacheReuse = specCacheReuse
      , specDidChangeAppend = specDidChangeAppend
      , specDidChangeRevertToOriginal = specDidChangeRevertToOriginal
      , specDidChangeAppendCharwise = specDidChangeAppendCharwise
      , specDefinitionAt = specDefinitionAt
      , specTypeDefinitionAt = specTypeDefinitionAt
      , specDocumentHighlightAt = specDocumentHighlightAt
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

-- | Hover query with per-position assertions.
data HoverCheck = HoverCheck
  { hoverCheckAt :: PositionQuery
  , hoverCheckContains :: [T.Text]
  , hoverCheckEquals :: Maybe T.Text
  , hoverCheckNotContains :: [T.Text]
  }

-- | Document highlight query with expected highlight positions.
data HighlightQuery = HighlightQuery
  { hlLine :: Int
  , hlCharacter :: Int
  , hlExpectedRanges :: [[Int]]  -- List of [line, startChar, endChar]
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

instance A.FromJSON HoverCheck where
  parseJSON = A.withObject "HoverCheck" $ \obj -> do
    hoverCheckAt <- obj A..: "at"
    hoverCheckContains <- obj A..:? "contains" A..!= []
    hoverCheckEquals <- obj A..:? "equals"
    hoverCheckNotContains <- obj A..:? "notContains" A..!= []
    return HoverCheck
      { hoverCheckAt = hoverCheckAt
      , hoverCheckContains = hoverCheckContains
      , hoverCheckEquals = hoverCheckEquals
      , hoverCheckNotContains = hoverCheckNotContains
      }

-- | Decode highlight query from JSON.
instance A.FromJSON HighlightQuery where
  parseJSON = A.withObject "HighlightQuery" $ \obj -> do
    hlLine <- obj A..: "line"
    hlCharacter <- obj A..: "character"
    hlExpectedRanges <- obj A..: "expectedRanges"
    return HighlightQuery
      { hlLine = hlLine
      , hlCharacter = hlCharacter
      , hlExpectedRanges = hlExpectedRanges
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
      , specDiagnosticsAfterChangeAtMost = Nothing
      , specDiagnosticsAfterAppendAtLeast = Nothing
      , specDiagnosticsAfterRevertAtMost = Nothing
      , specDiagnosticMessageContains = []
      , specFormattingEdits = False
      , specFormattingNoop = False
      , specHover = False
      , specHoverAt = Nothing
      , specHoverContains = []
      , specHoverEquals = Nothing
      , specHoverNotContains = []
      , specHoverChecks = []
      , specCompletionIncludes = []
      , specCompletionAt = Nothing
      , specCache = False
      , specCacheReuse = False
      , specDidChangeAppend = Nothing
      , specDidChangeRevertToOriginal = False
      , specDidChangeAppendCharwise = False
      , specDefinitionAt = Nothing
      , specTypeDefinitionAt = Nothing
      , specDocumentHighlightAt = Nothing
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
      case specDidChangeAppend spec of
        Nothing -> return ()
        Just suffix -> do
          if specDidChangeAppendCharwise spec
            then sendCharwiseAppend inH uri 2 content suffix
            else sendMessage inH (didChangeAppendNotification uri 2 content suffix)
          case specDiagnosticsAfterAppendAtLeast spec of
            Nothing -> return ()
            Just minCount -> do
              _ <- expectDiagnosticsAtLeast outH uri minCount
              return ()
          case specDiagnosticsAfterChangeAtMost spec of
            Nothing -> return ()
            Just maxCount -> do
              _ <- expectDiagnosticsAtMost outH uri maxCount
              return ()
          when (specDidChangeRevertToOriginal spec) $ do
            sendMessage inH (didChangeWholeNotification uri 3 content)
            case specDiagnosticsAfterRevertAtMost spec of
              Nothing -> return ()
              Just maxCount -> do
                _ <- expectDiagnosticsAtMostStrict outH uri maxCount
                return ()
      when (specFormattingEdits spec) $ do
        sendMessage inH (formattingRequest 2 uri)
        expectNonEmptyEdits outH 2
      when (specFormattingNoop spec) $ do
        sendMessage inH (formattingRequest 2 uri)
        expectEmptyEdits outH 2
      when (specHover spec) $ do
        let (line, col) = positionOrDefault (specHoverAt spec)
        sendMessage inH (hoverRequest 3 uri line col)
        expectHover outH 3 (specHoverContains spec) (specHoverEquals spec) (specHoverNotContains spec)
      unless (null (specHoverChecks spec)) $
        mapM_ (runHoverCheck inH outH uri) (zip [30..] (specHoverChecks spec))
      unless (null (specCompletionIncludes spec)) $ do
        let (line, col) = positionOrDefault (specCompletionAt spec)
        sendMessage inH (completionRequest 4 uri line col)
        expectCompletion outH 4 (specCompletionIncludes spec)
      case specDefinitionAt spec of
        Nothing -> return ()
        Just defQuery -> do
          sendMessage inH (definitionRequest 6 uri defQuery)
          expectDefinition outH 6 uri defQuery
      case specTypeDefinitionAt spec of
        Nothing -> return ()
        Just defQuery -> do
          sendMessage inH (typeDefinitionRequest 8 uri defQuery)
          expectDefinition outH 8 uri defQuery
      case specDocumentHighlightAt spec of
        Nothing -> return ()
        Just hlQuery -> do
          sendMessage inH (documentHighlightRequest 7 uri hlQuery)
          expectDocumentHighlight outH 7 hlQuery
      when doSave $ do
        sendMessage inH (didSaveNotification uri)
        waitForCache uri
      sendMessage inH (shutdownRequest 5)
      _ <- awaitResponseId outH 5
      sendMessage inH exitNotification)
    `finally` cleanupLsp (inH, outH, errH, ph)

-- | Wait for diagnostics and ensure a minimum count.
expectDiagnosticsAtLeast :: Handle -> T.Text -> Int -> IO [A.Value]
expectDiagnosticsAtLeast h uri expected = do
  diags <- awaitMessage h (matchDiagnostics uri)
  let count = length diags
  assertBool "diagnostics count too small" (count >= expected)
  return diags

-- | Wait for diagnostics and ensure a maximum count.
expectDiagnosticsAtMost :: Handle -> T.Text -> Int -> IO [A.Value]
expectDiagnosticsAtMost = expectDiagnosticsAtMostWithin 2000000 False

-- | Wait for diagnostics (strict) and ensure a maximum count.
expectDiagnosticsAtMostStrict :: Handle -> T.Text -> Int -> IO [A.Value]
expectDiagnosticsAtMostStrict = expectDiagnosticsAtMostWithin 5000000 True

-- | Wait up to a timeout for diagnostics and enforce an upper bound.
expectDiagnosticsAtMostWithin :: Int -> Bool -> Handle -> T.Text -> Int -> IO [A.Value]
expectDiagnosticsAtMostWithin timeoutMicros required h uri maxExpected = do
  mDiags <- timeout timeoutMicros (awaitMessage h (matchDiagnostics uri))
  diags <- case (required, mDiags) of
    (_, Just ds) -> return ds
    (False, Nothing) -> return []
    (True, Nothing) -> assertFailure "expected diagnostics notification after change" >> return []
  let count = length diags
  assertBool "diagnostics count too large" (count <= maxExpected)
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
expectNonEmptyEdits = expectEdits False

-- | Ensure formatting returns no edits.
expectEmptyEdits :: Handle -> Int -> IO ()
expectEmptyEdits = expectEdits True

-- | Ensure formatting returns the expected edit-list emptiness.
expectEdits :: Bool -> Handle -> Int -> IO ()
expectEdits shouldBeEmpty h target = do
  obj <- awaitResponseId h target
  case lookupKey "result" obj of
    Just (A.Array edits) -> do
      let isEmpty = null edits
          message = if shouldBeEmpty then "expected no formatting edits" else "expected formatting edits"
      unless (isEmpty == shouldBeEmpty) (assertFailure message)
    _ -> assertFailure (if shouldBeEmpty then "expected no formatting edits" else "expected formatting edits")

-- | Ensure hover returns content and satisfies string assertions.
expectHover :: Handle -> Int -> [T.Text] -> Maybe T.Text -> [T.Text] -> IO ()
expectHover h target contains mEquals notContains = do
  obj <- awaitResponseId h target
  case lookupKey "result" obj of
    Just A.Null -> assertFailure "expected hover result"
    Just val ->
      case hoverText val of
        Nothing -> assertFailure "missing hover contents"
        Just text -> do
          mapM_ (assertHoverContains text) contains
          mapM_ (assertHoverNotContains text) notContains
          case mEquals of
            Nothing -> return ()
            Just expected -> assertEqual "hover text mismatch" expected text
    Nothing -> assertFailure "missing hover result"

-- | Assert a hover string contains a substring.
assertHoverContains :: T.Text -> T.Text -> IO ()
assertHoverContains text needle =
  unless (T.isInfixOf needle text) (assertFailure $ "hover missing expected text '" ++ T.unpack needle ++ "' in: " ++ T.unpack text)

-- | Assert a hover string does not contain a substring.
assertHoverNotContains :: T.Text -> T.Text -> IO ()
assertHoverNotContains text needle =
  when (T.isInfixOf needle text) (assertFailure $ "hover unexpectedly contains text '" ++ T.unpack needle ++ "' in: " ++ T.unpack text)

runHoverCheck :: Handle -> Handle -> T.Text -> (Int, HoverCheck) -> IO ()
runHoverCheck inH outH uri (reqId, hoverCheck) = do
  let (line, col) = positionOrDefault (Just (hoverCheckAt hoverCheck))
  sendMessage inH (hoverRequest reqId uri line col)
  expectHover outH reqId (hoverCheckContains hoverCheck) (hoverCheckEquals hoverCheck) (hoverCheckNotContains hoverCheck)

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

-- | Build a definition request payload.
definitionRequest :: Int -> T.Text -> DefinitionQuery -> A.Value
definitionRequest reqId uri defQuery =
  positionRequest "textDocument/definition" reqId uri
    (defLine defQuery) (defCharacter defQuery)

-- | Build a typeDefinition request payload.
typeDefinitionRequest :: Int -> T.Text -> DefinitionQuery -> A.Value
typeDefinitionRequest reqId uri defQuery =
  positionRequest "textDocument/typeDefinition" reqId uri
    (defLine defQuery) (defCharacter defQuery)

-- | Build a documentHighlight request payload.
documentHighlightRequest :: Int -> T.Text -> HighlightQuery -> A.Value
documentHighlightRequest reqId uri hlQuery =
  positionRequest "textDocument/documentHighlight" reqId uri
    (hlLine hlQuery) (hlCharacter hlQuery)

-- | Expect document highlight results matching the expected ranges.
expectDocumentHighlight :: Handle -> Int -> HighlightQuery -> IO ()
expectDocumentHighlight h target hlQuery = do
  obj <- awaitResponseId h target
  let highlights = extractHighlights obj
      expected = hlExpectedRanges hlQuery
  assertEqual "highlight count mismatch" (length expected) (length highlights)
  mapM_ (assertHighlightMatch highlights) expected

-- | Extract highlight ranges from a response.
extractHighlights :: A.Object -> [[Int]]
extractHighlights obj =
  case lookupKey "result" obj of
    Just (A.Array items) -> mapMaybe extractRange (toList items)
    _ -> []

-- | Extract [line, startChar, endChar] from a DocumentHighlight.
extractRange :: A.Value -> Maybe [Int]
extractRange =
  \case
    A.Object obj ->
      case lookupKey "range" obj of
        Just (A.Object rangeObj) ->
          case (lookupKey "start" rangeObj, lookupKey "end" rangeObj) of
            (Just (A.Object startObj), Just (A.Object endObj)) ->
              case (lookupKey "line" startObj, lookupKey "character" startObj, lookupKey "character" endObj) of
                (Just (A.Number line), Just (A.Number startChar), Just (A.Number endChar)) ->
                  case (toBoundedInteger line, toBoundedInteger startChar, toBoundedInteger endChar) of
                    (Just l, Just s, Just e) -> Just [l, s, e]
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- | Assert a highlight range exists in the results.
assertHighlightMatch :: [[Int]] -> [Int] -> IO ()
assertHighlightMatch highlights expected =
  unless (expected `elem` highlights) $
    assertFailure ("missing highlight range: " ++ show expected)

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
