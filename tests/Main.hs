-- | Golden tests for the Kip CLI.
module Main where

import Control.Exception (bracket)
import Data.List (isInfixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, doesDirectoryExist, findExecutable, getTemporaryDirectory, listDirectory, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), replaceExtension, takeExtension)
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import Test.Tasty
import Test.Tasty.HUnit

-- | Discover tests and run them through tasty.
main :: IO () -- ^ Test entry point.
main = do
  kipPath <- locateKip
  nodePath <- locateNode
  succeedFiles <- listKipFiles ("tests" </> "succeed")
  failFiles <- listKipFiles ("tests" </> "fail")
  replFiles <- listReplFiles ("tests" </> "repl")
  let succeedTests = map (mkTest kipPath True) succeedFiles
      failTests = map (mkTest kipPath False) failFiles
      replTests = map (mkReplTest kipPath) replFiles
      jsTests = case nodePath of
        Nothing -> []
        Just node -> map (mkJsTest kipPath node) succeedFiles
      jsGroupName = case nodePath of
        Nothing -> "js (skipped: node missing)"
        Just _ -> "js"
  defaultMain $
    testGroup
      "kip"
      [ testGroup "succeed" succeedTests
      , testGroup "fail" failTests
      , testGroup "repl" replTests
      , testGroup jsGroupName jsTests
      ]

-- | Resolve the Kip executable path from env or PATH.
locateKip :: IO FilePath -- ^ Resolved executable path.
locateKip = do
  envPath <- lookupEnv "KIP_BIN"
  case envPath of
    Just path -> return path
    Nothing -> do
      found <- findExecutable "kip"
      case found of
        Just path -> return path
        Nothing ->
          fail "kip executable not found. Run stack install or set KIP_BIN."

-- | Resolve the Node.js executable path from env or PATH.
locateNode :: IO (Maybe FilePath) -- ^ Resolved Node.js executable path.
locateNode = do
  envPath <- lookupEnv "NODE_BIN"
  case envPath of
    Just path -> return (Just path)
    Nothing -> findExecutable "node"

-- | List `.kip` files in a directory, sorted for stable test order.
listKipFiles :: FilePath -- ^ Directory to scan.
             -> IO [FilePath] -- ^ Sorted `.kip` files.
listKipFiles dir = do
  entries <- listDirectory dir
  let files = sort [dir </> f | f <- entries, takeExtension f == ".kip"]
  return files

-- | List `.repl` files in a directory, sorted for stable test order.
listReplFiles :: FilePath -- ^ Directory to scan.
              -> IO [FilePath] -- ^ Sorted `.repl` files.
listReplFiles dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      entries <- listDirectory dir
      let files = sort [dir </> f | f <- entries, takeExtension f == ".repl"]
      return files
    else return []

-- | Create a golden test for a single `.kip` file.
mkTest :: FilePath -- ^ Kip executable path.
       -> Bool -- ^ Whether the test should succeed.
       -> FilePath -- ^ Test file path.
       -> TestTree -- ^ Test case.
mkTest kipPath shouldSucceed path =
  testCase path $ do
    inputText <- readIfExists (replaceExtension path "in")
    let stdinText = fromMaybe "" inputText
    (exitCode, stdout, stderr) <- readProcessWithExitCode kipPath ["--test", path] stdinText
    expectedOut <- readIfExists (replaceExtension path "out")
    expectedErr <- readIfExists (replaceExtension path "err")
    case (shouldSucceed, exitCode) of
      (True, ExitSuccess) ->
        case expectedOut of
          Nothing -> return ()
          Just outText ->
            let expected = normalizeLines outText
                actual = normalizeLines stdout
            in assertEqual (renderOutputMismatch expected actual) expected actual
      (True, ExitFailure _) ->
        assertFailure (path ++ " failed:\n" ++ stdout ++ stderr)
      (False, ExitSuccess) ->
        assertFailure (path ++ " succeeded but should fail")
      (False, ExitFailure _) ->
        case expectedErr of
          Nothing -> return ()
          Just errText ->
            let needle = trimRight (stripCR errText)
                haystack = stdout ++ stderr
            in assertBool (renderErrorMismatch needle haystack) (needle `isInfixOf` haystack)

-- | Create a REPL test that feeds commands and compares output.
mkReplTest :: FilePath -- ^ Kip executable path.
           -> FilePath -- ^ REPL test file path.
           -> TestTree -- ^ Test case.
mkReplTest kipPath path =
  testCase path $ do
    input <- readFile path
    (exitCode, stdout, stderr) <- readProcessWithExitCode kipPath [] input
    case exitCode of
      ExitFailure _ ->
        assertFailure (path ++ " failed:\n" ++ stdout ++ stderr)
      ExitSuccess -> do
        expectedOut <- readIfExists (replaceExtension path "out")
        case expectedOut of
          Nothing -> return ()
          Just outText ->
            let expected = normalizeLines outText
                actual = normalizeReplLines stdout
            in assertEqual (renderOutputMismatch expected actual) expected actual

-- | Compare native execution output with generated JS output.
mkJsTest :: FilePath -- ^ Kip executable path.
         -> FilePath -- ^ Node.js executable path.
         -> FilePath -- ^ Test file path.
         -> TestTree -- ^ Test case.
mkJsTest kipPath nodePath path =
  testCase ("js:" ++ path) $ do
    inputText <- readIfExists (replaceExtension path "in")
    let stdinText = fromMaybe "" inputText
    (exitCode, kipOut, kipErr) <- readProcessWithExitCode kipPath ["--exec", path] stdinText
    case exitCode of
      ExitFailure _ ->
        assertFailure (path ++ " failed in kip --exec:\n" ++ kipOut ++ kipErr)
      ExitSuccess -> do
        (jsExit, jsSrc, jsErr) <- readProcessWithExitCode kipPath ["--codegen", "js", "--no-prelude", "lib/giriş.kip", path] ""
        case jsExit of
          ExitFailure _ ->
            assertFailure (path ++ " failed in kip --codegen js:\n" ++ jsSrc ++ jsErr)
          ExitSuccess -> do
            (nodeExit, nodeOut, nodeErr) <- runNodeOnJs nodePath jsSrc stdinText
            case nodeExit of
              ExitFailure _ ->
                assertFailure (path ++ " failed under node:\n" ++ nodeOut ++ nodeErr)
              ExitSuccess -> do
                let expected = normalizeLines kipOut
                    actual = normalizeLines nodeOut
                assertEqual (renderOutputMismatch expected actual) expected actual

-- | Write JS source to a temp file and execute it with Node.js.
runNodeOnJs :: FilePath -- ^ Node.js executable path.
            -> String -- ^ JavaScript source.
            -> String -- ^ stdin payload.
            -> IO (ExitCode, String, String) -- ^ Exit code, stdout, stderr.
runNodeOnJs nodePath jsSrc stdinText = do
  tempDir <- getTemporaryDirectory
  bracket
    (do
      (path, handle) <- openTempFile tempDir "kip-js-test-.mjs"
      hClose handle
      return path)
    removeFile
    (\path -> do
      writeFile path jsSrc
      readProcessWithExitCode nodePath [path] stdinText)

-- | Render a diff-friendly output mismatch message.
renderOutputMismatch :: [String] -- ^ Expected lines.
                     -> [String] -- ^ Actual lines.
                     -> String -- ^ Error message.
renderOutputMismatch expected actual =
  unlines
    [ "output mismatch"
    , "Expected:"
    , formatBlock expected
    , "Actual:"
    , formatBlock actual
    ]

-- | Render a diff-friendly error mismatch message.
renderErrorMismatch :: String -- ^ Expected substring.
                    -> String -- ^ Actual output.
                    -> String -- ^ Error message.
renderErrorMismatch expected actual =
  unlines
    [ "error mismatch"
    , "Expected substring:"
    , expected
    , "Actual:"
    , actual
    ]

-- | Indent blocks for readability in test failures.
formatBlock :: [String] -- ^ Lines to format.
            -> String -- ^ Formatted block.
formatBlock = unlines . map ("  " ++)

-- | Read a file if it exists.
readIfExists :: FilePath -- ^ File path.
             -> IO (Maybe String) -- ^ File contents when present.
readIfExists path = do
  exists <- doesFileExist path
  if exists
    then Just <$> readFile path
    else return Nothing

-- | Normalize line output for stable comparisons.
normalizeLines :: String -- ^ Raw output.
               -> [String] -- ^ Normalized lines.
normalizeLines = filter (not . null) . map stripCR . lines

-- | Normalize REPL output by stripping prompts.
normalizeReplLines :: String -- ^ Raw REPL output.
                   -> [String] -- ^ Normalized lines.
normalizeReplLines =
  filter (not . null) . filter (not . isBannerLine) . map (stripPrompt . stripAnsi . stripCR) . lines
  where
    stripPrompt :: String -- ^ Line to process.
                -> String -- ^ Line without prompt.
    stripPrompt s = fromMaybe s (stripPrefix "Kip> " s)
    stripAnsi :: String -- ^ Line to process.
              -> String -- ^ Line without ANSI escapes.
    stripAnsi [] = []
    stripAnsi ('\ESC':'[':xs) =
      let rest = dropWhile (/= 'm') xs
      in case rest of
           [] -> []
           (_:ys) -> stripAnsi ys
    stripAnsi (x:xs) = x : stripAnsi xs
    isBannerLine :: String -- ^ Line to inspect.
                 -> Bool -- ^ True when line is a banner.
    isBannerLine s =
      let stripped = filter (/= ' ') s
          boxChars = "┌┐└┘─│"
      in (not (null stripped) && all (`elem` boxChars) stripped)
         || ("Kip " `isInfixOf` s && any (`elem` boxChars) s)

-- | Strip Windows carriage returns from a line.
stripCR :: String -- ^ Line to normalize.
        -> String -- ^ Line without carriage returns.
stripCR s =
  case reverse s of
    '\r':rest -> reverse rest
    _ -> s

-- | Trim trailing whitespace for substring matching.
trimRight :: String -- ^ Input string.
          -> String -- ^ Trimmed string.
trimRight = reverse . dropWhile (`elem` ("\r\n \t" :: String)) . reverse
