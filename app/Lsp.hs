{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

{-|
Language Server Protocol implementation for Kip.

This module is intentionally monolithic because it couples several cross-cutting
concerns that need to share internal caches and typechecking results:

* Parsing and typechecking Kip source on document changes.
* Maintaining document-level indices that power hover/definition/highlight.
* Rendering types and signatures with Kip’s morphological rules.
* Bridging LSP protocol handlers to compiler/runtime services.

The implementation is optimized for editor responsiveness. The core strategy is:

1. Parse/typecheck once per document version.
2. Build multiple indices over the AST and resolved symbols.
3. Answer LSP queries via small, indexed lookups rather than re-traversing ASTs.
4. Cache expensive type rendering (morphology) per document version.

When reading the code, the main flow is:

* 'processDocument' -> 'analyzeDocument' to build 'DocState'.
* LSP handlers read from 'DocState' and avoid recomputing parse/TC work.
* Hover and definition resolution use a consistent, indexed lookup pipeline.
-}
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (void, when, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.List (foldl', nub, isPrefixOf, sortOn)
import Data.Char (isAlphaNum, isDigit, ord)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList, listToMaybe, isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Row.Records ((.!))
import System.Directory (doesFileExist, listDirectory, doesDirectoryExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, takeExtension, (</>), normalise, addTrailingPathSeparator)
import System.IO (hPutStrLn, stderr)
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (finally)
import Data.Int (Int32)

import Language.LSP.Server
import Language.LSP.Protocol.Types
import Language.LSP.Protocol.Message
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Diagnostics (partitionBySource)
import qualified Data.HashTable.IO as HT
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Crypto.Hash.SHA256 (hash)

import Kip.AST
import Kip.Cache
import Kip.Eval (EvalState)
import Kip.Parser
import Kip.Render
import qualified Kip.Render as Render
import Kip.Runner (RenderCtx(..), Lang(..), ParseErrorRenderTarget(..), renderParseErrorFor, renderTCError, tcErrSpan, loadPreludeState, locateDataFile)
import Kip.TypeCheck
import Language.Foma

import Control.Lens ((^.))
import Text.Megaparsec (ParseErrorBundle(..), ParseError(..), errorOffset)
import Text.Megaparsec.Error (ErrorFancy(..))
import Text.Megaparsec.Pos (SourcePos(..), unPos, sourceLine, sourceColumn)
import Data.List.NonEmpty (NonEmpty(..))

-- | Server configuration/state.
--
-- This is the long-lived state shared by all LSP handlers. It contains:
--
-- * Morphology caches used when rendering identifiers/types for hover/completion.
-- * Base parser/typechecker states that include the standard library.
-- * The open document map ('lsDocs') keyed by URI.
-- * A workspace definition index ('lsDefIndex') for cross-file go-to-definition.
data LspState = LspState
  { -- | Morphology-aware render cache shared across all documents.
    lsCache :: !RenderCache
    -- | Finite-state morphology transducer for Kip inflection.
  , lsFsm :: !FSM
    -- | Cached “ups” transformations for morphological rendering.
  , lsUpsCache :: !MorphCache
    -- | Cached “downs” transformations for morphological rendering.
  , lsDownsCache :: !MorphCache
    -- | Module roots for resolving and indexing definitions.
  , lsModuleDirs :: ![FilePath]
    -- | Parser state including the prelude (stdlib) context.
  , lsBaseParser :: !ParserState
    -- | Typechecker state including the prelude (stdlib) context.
  , lsBaseTC :: !TCState
    -- | Latest raw text per URI (authoritative for incremental edits).
  , lsLatestText :: Map.Map Uri Text
    -- | Per-URI document cache with parsed/typed state.
  , lsDocs :: Map.Map Uri DocState
    -- | Workspace definition index for cross-file go-to-definition.
  , lsDefIndex :: Map.Map Identifier Location
    -- | Versioned document-analysis workers, keyed by URI.
  , lsAnalysisJobs :: Map.Map Uri AnalysisJob
    -- | Monotonic identifier used to prevent stale-worker ABA races.
  , lsNextAnalysisJob :: !Int
  }

-- | A cancellable document-analysis worker.
data AnalysisJob = AnalysisJob
  { ajId :: !Int
  , ajThread :: !ThreadId
  }

-- | Information about a bound variable with its scope.
--
-- Each binder records:
--
-- * The identifier it binds.
-- * The range of the binder name itself (for go-to-definition).
-- * The span of the scope where the binding is valid (for resolution).
data BinderInfo = BinderInfo
  { -- | Identifier bound by this binder.
    biIdent :: !Identifier
    -- | LSP range of the binder name itself.
  , biRange :: !Range
    -- | Span where the binding is in scope.
  , biScope :: !Span
  }

-- | Per-document cached state.
--
-- This is the central cache for a single document version. It stores:
--
-- * Raw text, parser, and typechecker state.
-- * The AST and diagnostics for this version.
-- * Multiple symbol/type indices used by hover/definition/highlight.
-- * A per-document type rendering cache to avoid repeated morphology work.
--
-- The guiding rule: any computation that depends solely on the current text
-- should be done once here and then reused by all handlers.
data DocState = DocState
  { -- | Raw document text for this version.
    dsText :: !Text
    -- | Pre-split lines for fast line-indexed lookups.
  , dsLines :: !(V.Vector Text)
    -- | Start offsets of each line in the raw text.
  , dsLineStarts :: !(U.Vector Int)
    -- | Parser state used for this document.
  , dsParser :: !ParserState
    -- | Typechecker state used for this document.
  , dsTC :: !TCState
    -- | Parsed statements for this document.
  , dsStmts :: ![Stmt Ann]
    -- | Diagnostics generated for this document.
  , dsDiagnostics :: ![Diagnostic]
    -- | Definition name spans for local go-to-definition.
  , dsDefSpans :: Map.Map Identifier Range
    -- | Resolved identifier names keyed by span.
  , dsResolved :: Map.Map Span Identifier
    -- | Resolved overload signatures keyed by span.
  , dsResolvedSigs :: Map.Map Span (Identifier, [Ty Ann])
    -- | Resolved types keyed by span.
  , dsResolvedTypes :: Map.Map Span (Ty Ann)
    -- | Binder spans for local variable definitions.
  , dsBinderSpans :: [BinderInfo]
    -- | Unified span index for exact span/range lookups.
  , dsSpanIndex :: SpanIndex
    -- | Expression index for positional lookup.
  , dsExpIndex :: ExpIndex
    -- | Variable-use index for positional lookup.
  , dsVarIndex :: VarIndex
    -- | Pattern-variable index for positional lookup.
  , dsPatVarIndex :: PatVarIndex
    -- | Constructor-pattern index for positional lookup.
  , dsCtorIndex :: CtorIndex
    -- | Match clause index for positional lookup.
  , dsMatchClauseIndex :: MatchClauseIndex
    -- | Function clause index for positional lookup.
  , dsFuncClauseIndex :: FuncClauseIndex
    -- | Per-document cache of rendered types (hash table for O(1) lookups).
  , dsTyRenderCache :: HT.BasicHashTable Text Text
    -- | Per-document cache of token classifications keyed by (line, column).
  , dsTokenCache :: HT.BasicHashTable (UInt, UInt) (Maybe TokenAtPosition)
  }

-- | LSP server configuration wrapper.
--
-- The LSP library requires a config value; we store the mutable 'LspState' in it.
newtype Config = Config
  { -- | Mutable LSP server state.
    cfgVar :: MVar LspState
  }

-- | Entry point for the LSP server.
--
-- Initializes caches, loads the standard library, and registers handlers.
main :: IO ()
main = do
  initialState <- initState
  let config = Config initialState
  let serverDef = ServerDefinition
        { defaultConfig = config
#if MIN_VERSION_lsp_types(2,3,0)
        , configSection = "kip"
        , parseConfig = \cfg _ -> Right cfg
        , onConfigChange = \_ -> return ()
        , doInitialize = \env _ -> pure (Right env)
        , staticHandlers = \_ -> handlers
#else
        , onConfigurationChange = \cfg _ -> Right cfg
        , doInitialize = \env _ -> pure (Right env)
        , staticHandlers = handlers
#endif
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = lspOptions
        }
  void (runServer serverDef)

-- | LSP server options.
--
-- We request full-document sync and keep formatting simple to reduce latency.
lspOptions :: Options
lspOptions = defaultOptions
  { optTextDocumentSync = Just $ TextDocumentSyncOptions
      { _openClose = Just True
      , _change = Just TextDocumentSyncKind_Full
      , _willSave = Nothing
      , _willSaveWaitUntil = Nothing
      , _save = Just (InR (SaveOptions (Just False)))
      }
  , optCompletionTriggerCharacters = Just "-'"
  }

-- | Aggregate all LSP request/notification handlers.
handlers :: Handlers (LspM Config)
handlers = mconcat
  [ notificationHandler SMethod_Initialized onInitialized
  , notificationHandler SMethod_CancelRequest onCancelRequest
  , notificationHandler SMethod_SetTrace onSetTrace
  , notificationHandler SMethod_WorkspaceDidChangeWatchedFiles onDidChangeWatchedFiles
  , notificationHandler SMethod_TextDocumentDidOpen onDidOpen
  , notificationHandler SMethod_TextDocumentDidChange onDidChange
  , notificationHandler SMethod_TextDocumentDidClose onDidClose
  , notificationHandler SMethod_TextDocumentDidSave onDidSave
  , requestHandler SMethod_TextDocumentHover onHover
  , requestHandler SMethod_TextDocumentDefinition onDefinition
  , requestHandler SMethod_TextDocumentTypeDefinition onTypeDefinition
  , requestHandler SMethod_TextDocumentCompletion onCompletion
  , requestHandler SMethod_TextDocumentFormatting onFormatting
  , requestHandler SMethod_TextDocumentDocumentHighlight onDocumentHighlight
  ]

onInitialized :: TNotificationMessage 'Method_Initialized -> LspM Config ()
onInitialized _ = return ()

onSetTrace :: TNotificationMessage 'Method_SetTrace -> LspM Config ()
onSetTrace _ = return ()

onDidChangeWatchedFiles :: TNotificationMessage 'Method_WorkspaceDidChangeWatchedFiles -> LspM Config ()
onDidChangeWatchedFiles _ = return ()

onCancelRequest _ = return ()

-- | Atomically update the shared LSP state.
--
-- All mutations of 'LspState' should go through this helper to keep MVar
-- usage consistent.
withState :: (LspState -> IO (LspState, a)) -> LspM Config a
withState f = do
  Config var <- getConfig
  liftIO (modifyMVar var f)

-- | Read the current shared LSP state.
readState :: LspM Config LspState
readState = do
  Config var <- getConfig
  liftIO (readMVar var)

-- | Initialize the shared LSP state.
--
-- Loads the standard library, precomputes morphology caches, and builds the
-- base parser/typechecker state used for all documents.
initState :: IO (MVar LspState)
initState = do
  trmorphPath <- locateDataFile "vendor/trmorph.fst"
  libPath <- locateDataFile "lib/temel.kip"
  fsm <- fsmReadBinaryFile trmorphPath
  upsCache <- HT.new
  downsCache <- HT.new
  let renderCache = mkRenderCache upsCache downsCache
  let libDir = takeDirectory libPath
  cwd <- getExecutablePath
  let moduleDirs = [libDir, takeDirectory cwd]
  let ctx = RenderCtx LangEn renderCache fsm upsCache downsCache
  (baseParser, baseTC, _, _) <- runReaderT (loadPreludeState False moduleDirs renderCache fsm upsCache downsCache) ctx
  newMVar LspState
    { lsCache = renderCache
    , lsFsm = fsm
    , lsUpsCache = upsCache
    , lsDownsCache = downsCache
    , lsModuleDirs = moduleDirs
    , lsBaseParser = baseParser
    , lsBaseTC = baseTC
    , lsLatestText = Map.empty
    , lsDocs = Map.empty
    , lsDefIndex = Map.empty
    , lsAnalysisJobs = Map.empty
    , lsNextAnalysisJob = 0
    }

-- | Handlers
onDidOpen :: TNotificationMessage 'Method_TextDocumentDidOpen -> LspM Config ()
onDidOpen msg = do
  let doc = msg ^. L.params . L.textDocument
      uri = doc ^. L.uri
      version = doc ^. L.version
      text = doc ^. L.text
  withState $ \s ->
    return (s { lsLatestText = Map.insert uri text (lsLatestText s) }, ())
  scheduleDocumentAnalysis uri text (Just version) True False

onDidChange :: TNotificationMessage 'Method_TextDocumentDidChange -> LspM Config ()
onDidChange msg = do
  st <- readState
  let params = msg ^. L.params
      uri = params ^. L.textDocument . L.uri
      version = params ^. L.textDocument . L.version
      changes = params ^. L.contentChanges
      mOld = lookupLatestTextByUri uri st
  oldText <- case mOld of
    Just t -> return t
    Nothing -> liftIO (docTextForChange uri (lsDocs st))
  let newText = applyContentChanges oldText changes
  withState $ \s ->
    return (s { lsLatestText = Map.insert uri newText (lsLatestText s) }, ())
  scheduleDocumentAnalysis uri newText (Just version) True True

onDidClose :: TNotificationMessage 'Method_TextDocumentDidClose -> LspM Config ()
onDidClose msg = do
  let uri = msg ^. L.params . L.textDocument . L.uri
  mWorker <- withState $ \s ->
    return
      ( s
          { lsDocs = Map.delete uri (lsDocs s)
          , lsLatestText = Map.delete uri (lsLatestText s)
          , lsAnalysisJobs = Map.delete uri (lsAnalysisJobs s)
          }
      , ajThread <$> Map.lookup uri (lsAnalysisJobs s)
      )
  liftIO (mapM_ killThread mWorker)

onDidSave :: TNotificationMessage 'Method_TextDocumentDidSave -> LspM Config ()
onDidSave msg = do
  let uri = msg ^. L.params . L.textDocument . L.uri
  st <- readState
  case Map.lookup uri (lsDocs st) of
    Nothing -> return ()
    Just doc -> do
      _ <- liftIO (writeCacheForDoc st uri doc)
      return ()

-- | Render an identifier with spaces between parts.
renderIdentifier :: Identifier -> Text
renderIdentifier (parts, stem) =
  T.unwords (parts ++ [stem | not (T.null stem)])

stripTrailingVowel :: Identifier -> Maybe Identifier
stripTrailingVowel (mods, word) =
  case T.unsnoc word of
    Just (pref, c) | c `elem` ['i', 'ı', 'u', 'ü'] -> Just (mods, pref)
    _ -> Nothing

infinitiveCandidates :: [Identifier] -> [Identifier]
infinitiveCandidates candidates =
  nub (candidates ++ mapMaybe stripTrailingVowel candidates)

findInfinitiveDef :: [Identifier] -> [Stmt Ann] -> Maybe Identifier
findInfinitiveDef names stmts =
  listToMaybe [n | Function n _ _ _ True <- stmts, n `elem` names]
    <|> listToMaybe [n | PrimFunc n _ _ True <- stmts, n `elem` names]

-- | Find a function definition by name in statements.
-- Returns the function name, arguments (if any), return type, and infinitive flag.
findFunctionStmt :: Identifier -> [Stmt Ann] -> Maybe (Identifier, [Arg Ann], Ty Ann, Bool)
findFunctionStmt name stmts =
  listToMaybe $
    -- First try Function statements
    [(n, args, retTy, isInf) | Function n args retTy _ isInf <- stmts, n == name] ++
    -- Also try PrimFunc statements
    [(n, args, retTy, isInf) | PrimFunc n args retTy isInf <- stmts, n == name] ++
    -- Also try Defn statements (which might be nullary functions)
    [(n, [], ty, False) | Defn n ty _ <- stmts, n == name]

-- | Find a function definition by name in statements.
-- Returns the function name, arguments (if any), and return type.
findFunctionDef :: Identifier -> [Stmt Ann] -> Maybe (Identifier, [Arg Ann], Ty Ann)
findFunctionDef name stmts =
  (\(n, args, retTy, _) -> (n, args, retTy)) <$> findFunctionStmt name stmts

-- | Inflect the last word of a string with a given case.
inflectLastWord :: RenderCache -> FSM -> String -> IO String
inflectLastWord cache fsm s =
  case words s of
    [] -> return s
    ws ->
      case reverse ws of
        lastWord:revRest -> do
          inflected <- renderIdentWithCases cache fsm ([], T.pack lastWord) [P3s]
          return (unwords (reverse (inflected : revRest)))
        [] -> return s

-- | Render a function signature for hover with parameter names and return type.
--
-- This function:
--
-- * Chooses nominative or infinitive form for the function name.
-- * Normalizes return type for infinitive functions (e.g. “bitim”).
-- * Applies P3s inflection to the final return type word to match REPL output.
renderHoverSignature :: Identifier -> [Arg Ann] -> Ty Ann -> Bool -> Set.Set Identifier -> RenderCache -> FSM -> [Identifier] -> [(Identifier, [Identifier])] -> IO Text
renderHoverSignature fnName args retTy isInfinitive infinitives cache fsm paramTyCons tyMods = do
  -- Use renderFunctionSignatureParts if infinitive, otherwise regular renderFunctionSignature
  if isInfinitive
    then do
      -- For infinitives, use special rendering
      argsStrs <- mapM (renderArg cache fsm paramTyCons tyMods) args
      nameStr <- Render.renderInfinitiveName cache fsm fnName
      -- Render return type and inflect last word with P3s (following REPL approach)
      retTyStr <- renderHoverReturnType isInfinitive infinitives retTy cache fsm paramTyCons tyMods
      let paramTexts = argsStrs
          retText = "(" ++ nameStr ++ " " ++ retTyStr ++ ")"
      return $ T.pack $ unwords (paramTexts ++ [retText])
    else do
      -- Non-infinitive: use regular signature rendering
      (argsStrs, nameStr) <- renderFunctionSignature cache fsm paramTyCons tyMods fnName args
      -- Render return type and inflect last word with P3s (following REPL approach)
      retTyStr <- renderHoverReturnType isInfinitive infinitives retTy cache fsm paramTyCons tyMods
      let paramTexts = argsStrs
          retText = "(" ++ nameStr ++ " " ++ retTyStr ++ ")"
      return $ T.pack $ unwords (paramTexts ++ [retText])
  where
    normalizeInfRet isInf infs ty =
      let mkBitimTy = TyInd (mkAnn Nom NoSpan) ([], T.pack "bitim")
      in if not isInf
        then ty
        else case ty of
          TyVar _ name | Set.member name infs -> mkBitimTy
          TyInd _ name | Set.member name infs -> mkBitimTy
          _ -> ty
    renderHoverReturnType isInf infs ty cache' fsm' paramTyCons' tyMods' = do
      let ty' = normalizeInfRet isInf infs ty
      retTyBase <- renderTyNom cache' fsm' paramTyCons' tyMods' ty'
      if shouldInflectRet ty' then inflectLastWord cache' fsm' retTyBase else return retTyBase
    shouldInflectRet ty =
      case ty of
        TyInd {} -> True
        TyVar {} -> True
        TySkolem {} -> True
        TyInt {} -> True
        TyFloat {} -> True
        TyString {} -> True
        TyChar {} -> True
        TyApp _ (TyInd _ name) _ -> name `notElem` paramTyCons
        TyApp {} -> True
        Arr {} -> True

data ResolvedAt = ResolvedAt
  { -- | Expression under the cursor (if any).
    raExp :: Maybe (Exp Ann)
    -- | Variable occurrence and its morphological candidates.
  , raVar :: Maybe (Identifier, [(Identifier, Case)])
    -- | Definition identifier under the cursor (if any).
  , raDefIdent :: Maybe (Identifier, [(Identifier, Case)])
    -- | Constructor pattern information at the cursor.
  , raCtor :: Maybe (Identifier, Ann, [Pat Ann], Maybe (Exp Ann))
    -- | Pattern-bound variable at the cursor.
  , raPatVar :: Maybe Identifier
    -- | Unified span index info near the cursor.
  , raSpanInfo :: Maybe SpanInfo
    -- | Resolved type at the cursor (if any).
  , raResolvedType :: Maybe (Ty Ann)
  }

data TokenAtPosition
  = TokenKeyword Text
  | TokenIdent Text
  deriving (Eq, Show)

data SpanKey
  = SpanKey Span
  | RangeKey Range
  deriving (Eq, Ord, Show)

data SpanInfo = SpanInfo
  { -- | Span for the indexed entity (if span-backed).
    siSpan :: Maybe Span
    -- | Range for the indexed entity (if range-backed).
  , siRange :: Maybe Range
    -- | Resolved identifier for this span/range.
  , siIdent :: Maybe Identifier
    -- | Resolved overload signature for this span/range.
  , siSig :: Maybe (Identifier, [Ty Ann])
    -- | Resolved type for this span/range.
  , siType :: Maybe (Ty Ann)
    -- | Binder information if this span/range is a binder.
  , siBinder :: Maybe BinderInfo
  }

-- | Key for the unified span index.
--
-- Some entities (expressions, resolved symbols) are indexed by 'Span', while
-- binder locations are naturally represented as LSP 'Range's. We keep both
-- and allow position lookups to match either.
--
-- The index is hash-backed for fast exact lookups, while a separate list
-- enables the “smallest enclosing span” search needed for cursor queries.
data SpanIndex = SpanIndex
  { -- | Hash map for exact span/range lookups.
    siByKey :: HM.HashMap Text SpanInfo
    -- | Entry list used for containment-based lookups.
  , siEntries :: [(SpanKey, SpanInfo)]
  }

-- | Positional index backed by a hash map plus an entry list.
--
-- We use a hash map for exact lookups (O(1) average) and keep the original
-- entry list for containment queries (smallest enclosing span).
data PosIndex a = PosIndex
  { -- | Hash map for exact span lookups.
    piByKey :: HM.HashMap Text a
    -- | Entry list used for containment-based lookups.
  , piEntries :: [(Span, a)]
  }

-- | Map from expression span to the expression node.
type ExpIndex = PosIndex (Exp Ann)
-- | Map from variable-use span to its name and morphological candidates.
type VarIndex = PosIndex (Identifier, [(Identifier, Case)])
-- | Map from pattern variable span to the bound identifier.
type PatVarIndex = PosIndex Identifier
-- | Map from constructor-pattern span to its constructor metadata.
type CtorIndex = PosIndex (Identifier, Ann, [Pat Ann], Maybe (Exp Ann))
-- | Map from clause-body span to the enclosing match clause (scrutinee + pattern).
type MatchClauseIndex = PosIndex (Exp Ann, Pat Ann)
-- | Map from clause scope span to the enclosing function clause (args + pattern).
type FuncClauseIndex = PosIndex ([Arg Ann], Pat Ann)

data DocIndexLists = DocIndexLists
  { -- | Collected expression entries.
    expEntries :: [(Span, Exp Ann)]
    -- | Collected variable entries.
  , varEntries :: [(Span, (Identifier, [(Identifier, Case)]))]
    -- | Collected pattern variable entries.
  , patVarEntries :: [(Span, Identifier)]
    -- | Collected constructor-pattern entries.
  , ctorEntries :: [(Span, (Identifier, Ann, [Pat Ann], Maybe (Exp Ann)))]
    -- | Collected match clause entries.
  , matchClauseEntries :: [(Span, (Exp Ann, Pat Ann))]
    -- | Collected function clause entries.
  , funcClauseEntries :: [(Span, ([Arg Ann], Pat Ann))]
  }

emptyDocIndexLists :: DocIndexLists
emptyDocIndexLists =
  DocIndexLists [] [] [] [] [] []

-- | Stable textual key for a span, used in hash-based indices.
spanKeyText :: Span -> Text
spanKeyText =
  T.pack . show

-- | Stable textual key for an LSP range, used in hash-based indices.
rangeKeyText :: Range -> Text
rangeKeyText =
  T.pack . show

-- | Stable textual key for a span index entry.
spanKeyTextForSpanKey :: SpanKey -> Text
spanKeyTextForSpanKey key =
  case key of
    SpanKey sp -> "S:" <> spanKeyText sp
    RangeKey range -> "R:" <> rangeKeyText range

-- | Build a positional index from a list of span entries.
--
-- The hash map is used for exact span lookups; the list is used for
-- position-based queries that require span containment checks.
posIndexFromEntries :: [(Span, a)] -> PosIndex a
posIndexFromEntries entries =
  let byKey = HM.fromList [ (spanKeyText sp, value) | (sp, value) <- entries ]
  in PosIndex byKey entries

buildSpanIndex :: Map.Map Span Identifier -> Map.Map Span (Identifier, [Ty Ann]) -> Map.Map Span (Ty Ann) -> [BinderInfo] -> SpanIndex
buildSpanIndex resolved resolvedSigs resolvedTypes binders =
  SpanIndex (HM.map snd byKey) (HM.elems byKey)
  where
    entries =
      [ (SpanKey sp, SpanInfo (Just sp) Nothing (Just ident) Nothing Nothing Nothing)
      | (sp, ident) <- Map.toList resolved
      ] ++
      [ (SpanKey sp, SpanInfo (Just sp) Nothing Nothing (Just sig) Nothing Nothing)
      | (sp, sig) <- Map.toList resolvedSigs
      ] ++
      [ (SpanKey sp, SpanInfo (Just sp) Nothing Nothing Nothing (Just ty) Nothing)
      | (sp, ty) <- Map.toList resolvedTypes
      ] ++
      [ (RangeKey (biRange bi), SpanInfo Nothing (Just (biRange bi)) (Just (biIdent bi)) Nothing Nothing (Just bi))
      | bi <- binders
      ]
    byKey = foldl' insertEntry HM.empty entries
    insertEntry acc (key, info) =
      HM.insertWith mergeEntry (spanKeyTextForSpanKey key) (key, info) acc
    mergeEntry new old =
      let (oldKey, oldInfo) = old
          (_, newInfo) = new
      in (oldKey, mergeSpanInfo newInfo oldInfo)
    mergeSpanInfo new old =
      SpanInfo
        { siSpan = siSpan new <|> siSpan old
        , siRange = siRange new <|> siRange old
        , siIdent = siIdent new <|> siIdent old
        , siSig = siSig new <|> siSig old
        , siType = siType new <|> siType old
        , siBinder = siBinder new <|> siBinder old
        }

-- | Lookup span-index information by exact span key.
--
-- This is used when we already have a precise span from an AST node and
-- want to merge that with resolved symbol/type data.
spanInfoForSpan :: Span -> SpanIndex -> Maybe SpanInfo
spanInfoForSpan sp idx =
  HM.lookup ("S:" <> spanKeyText sp) (siByKey idx)

-- | Lookup span-index information at a cursor position.
--
-- Returns the smallest matching span/range (most specific) so that
-- nested constructs resolve to the innermost entity.
spanInfoAtPosition :: Position -> SpanIndex -> Maybe SpanInfo
spanInfoAtPosition pos idx =
  let matches =
        [ (spanKeySize key, info)
        | (key, info) <- siEntries idx
        , posInSpanKey pos key
        ]
  in fmap snd (listToMaybe (sortOn fst matches))
  where
    posInSpanKey p key =
      case key of
        SpanKey sp -> posInSpan p sp
        RangeKey range -> positionInRange p range
    spanKeySize key =
      case key of
        SpanKey sp -> spanSizeForSort sp
        RangeKey range -> rangeSizeForSort range

rangeSizeForSort :: Range -> (Int, Int)
rangeSizeForSort (Range (Position sl sc) (Position el ec)) =
  let lines = fromIntegral el - fromIntegral sl
      cols = if lines == 0 then fromIntegral ec - fromIntegral sc else maxBound :: Int
  in (lines, cols)

-- | Generic “smallest enclosing span” lookup for arbitrary indices.
--
-- Used by the per-document indices built from the AST to avoid repeated
-- tree traversals on hover/definition/highlight.
lookupByPosition :: Position -> PosIndex a -> Maybe a
lookupByPosition pos idx =
  let matches =
        [ (spanSizeForSort sp, value)
        | (sp, value) <- piEntries idx
        , posInSpan pos sp
        ]
  in fmap snd (listToMaybe (sortOn fst matches))

-- | Build all per-document indices in a single AST traversal.
--
-- These indices back the most frequent queries (hover/definition) and
-- allow near-O(log n) lookups instead of repeated tree walks.
buildDocIndices :: [Stmt Ann] -> (ExpIndex, VarIndex, PatVarIndex, CtorIndex, MatchClauseIndex, FuncClauseIndex)
buildDocIndices stmts =
  let lists = foldl' collectStmt emptyDocIndexLists stmts
  in ( posIndexFromEntries (expEntries lists)
     , posIndexFromEntries (varEntries lists)
     , posIndexFromEntries (patVarEntries lists)
     , posIndexFromEntries (ctorEntries lists)
     , posIndexFromEntries (matchClauseEntries lists)
     , posIndexFromEntries (funcClauseEntries lists)
     )
  where
    addSpanEntry sp val xs =
      case sp of
        NoSpan -> xs
        _ -> (sp, val) : xs
    collectStmt acc stt =
      case stt of
        Defn _ _ e -> collectExp Nothing acc e
        Function _ args _ clauses _ ->
          foldl' (collectFunctionClause args) acc clauses
        ExpStmt e -> collectExp Nothing acc e
        _ -> acc
    collectFunctionClause args acc (Clause pat body) =
      let bodySpan = annSpan (annExp body)
          scopeSpan = mergeSpanAll [bodySpan, patRootSpan pat]
          acc' = acc
            { funcClauseEntries = addSpanEntry scopeSpan (args, pat) (funcClauseEntries acc) }
          acc'' = collectPat Nothing acc' pat
      in collectExp Nothing acc'' body
    collectExp mScrutinee acc e =
      let acc' =
            acc { expEntries = addSpanEntry (annSpan (annExp e)) e (expEntries acc)
                , varEntries =
                    case e of
                      Var {annExp = ann, varName = name, varCandidates = candidates} ->
                        addSpanEntry (annSpan ann) (name, candidates) (varEntries acc)
                      _ -> varEntries acc
                }
      in case e of
          App _ f args -> foldl' (collectExp mScrutinee) (collectExp mScrutinee acc' f) args
          Bind _ _ _ b -> collectExp mScrutinee acc' b
          Seq _ a b -> collectExp mScrutinee (collectExp mScrutinee acc' a) b
          Match _ scr clauses ->
            let accScr = collectExp mScrutinee acc' scr
            in foldl' (collectMatchClause scr) accScr clauses
          Let _ _ body -> collectExp mScrutinee acc' body
          Ascribe _ _ exp' -> collectExp mScrutinee acc' exp'
          _ -> acc'
    collectMatchClause scr acc (Clause pat body) =
      let bodySpan = annSpan (annExp body)
          acc' = acc
            { matchClauseEntries = addSpanEntry bodySpan (scr, pat) (matchClauseEntries acc) }
          acc'' = collectPat (Just scr) acc' pat
      in collectExp (Just scr) acc'' body
    collectPat mScrutinee acc pat =
      case pat of
        PVar ident ann ->
          acc { patVarEntries = addSpanEntry (annSpan ann) ident (patVarEntries acc) }
        PCtor (ctor, ann) pats ->
          let ctorSpan = mergeSpanAll (annSpan ann : map patRootSpan pats)
              acc' = acc
                { ctorEntries = addSpanEntry ctorSpan (ctor, ann, pats, mScrutinee) (ctorEntries acc) }
          in foldl' (collectPat mScrutinee) acc' pats
        PListLit pats ->
          foldl' (collectPat mScrutinee) acc pats
        _ -> acc

-- | Resolve all symbol-related information for a cursor position.
--
-- This collects:
--
-- * The expression and variable under the cursor (via indices).
-- * The closest span-index info (resolved symbol/type).
-- * Pattern variables and constructor patterns.
--
-- The result is a compact bundle used by hover/definition handlers.
resolveAtPosition :: Position -> DocState -> ResolvedAt
resolveAtPosition pos doc =
  let mSpanInfo = spanInfoAtPosition pos (dsSpanIndex doc)
      mResolvedType = siType =<< mSpanInfo
  in
  ResolvedAt
    { raExp = findExpAt pos doc
    , raVar = findVarAt pos doc
    , raDefIdent = findDefinitionAt pos (dsDefSpans doc)
    , raCtor = findCtorInPattern pos doc
    , raPatVar = findPatVarAt pos doc
    , raSpanInfo = mSpanInfo
    , raResolvedType = mResolvedType
    }

-- | Render a constructor signature for hover.
--
-- The output mirrors the REPL signature style, preserving parameter
-- positions and using nominative case for the return type so it reads
-- naturally when displayed in hover popups.
renderConstructorSignature :: Identifier -> [Ty Ann] -> Ty Ann -> RenderCache -> FSM -> [Identifier] -> [(Identifier, [Identifier])] -> IO Text
renderConstructorSignature ctorName argTys retTy cache fsm paramTyCons tyMods = do
  argStrs <- mapM (renderTy cache fsm paramTyCons tyMods) argTys
  ctorStr <- renderIdentWithCase cache fsm ctorName (if null argTys then Nom else P3s)
  retTyStr <- renderTyNom cache fsm [] tyMods retTy
  let paramTexts = map (\t -> "(" ++ t ++ ")") argStrs
      retText = "(" ++ ctorStr ++ " " ++ retTyStr ++ ")"
  return $ T.pack $ unwords (paramTexts ++ [retText])

normalizeTyForRender :: Ty Ann -> Ty Ann
normalizeTyForRender ty =
  let ann = mkAnn (annCase (annTy ty)) NoSpan
  in case ty of
    TyString {} -> TyString ann
    TyInt {} -> TyInt ann
    TyFloat {} -> TyFloat ann
    TyChar {} -> TyChar ann
    Arr _ domTy imgTy -> Arr ann (normalizeTyForRender domTy) (normalizeTyForRender imgTy)
    TyInd _ name -> TyInd ann name
    TyVar _ name -> TyVar ann name
    TySkolem _ name -> TySkolem ann name
    TyApp _ ctor args -> TyApp ann (normalizeTyForRender ctor) (map normalizeTyForRender args)

-- | Render a type in nominative case with a per-document cache.
--
-- This is a hot path during hover. We normalize annotations to eliminate
-- span noise and cache the rendered text for the lifetime of the document
-- version.
--
-- The cache is a hash table because it is read-heavy and should be O(1)
-- on average; this keeps hover latency low even with many repeated types.
-- | Render cache key for a type.
--
-- We normalize spans and then use the 'Show' instance as a stable key.
tyCacheKey :: Ty Ann -> Text
tyCacheKey =
  T.pack . show . normalizeTyForRender

renderTyNomTextCached :: DocState -> RenderCache -> FSM -> [Identifier] -> [(Identifier, [Identifier])] -> Ty Ann -> IO Text
renderTyNomTextCached doc cache fsm paramTyCons tyMods ty = do
  let key = tyCacheKey ty
  existing <- HT.lookup (dsTyRenderCache doc) key
  case existing of
    Just t -> return t
    Nothing -> do
      t <- renderTyNomText cache fsm paramTyCons tyMods ty
      HT.insert (dsTyRenderCache doc) key t
      return t

#if MIN_VERSION_lsp_types(2,3,0)
onHover :: TRequestMessage 'Method_TextDocumentHover -> (Either (TResponseError 'Method_TextDocumentHover) (MessageResult 'Method_TextDocumentHover) -> LspM Config ()) -> LspM Config ()
#else
onHover :: TRequestMessage 'Method_TextDocumentHover -> (Either ResponseError (MessageResult 'Method_TextDocumentHover) -> LspM Config ()) -> LspM Config ()
#endif
-- | Handle hover requests.
--
-- The hover pipeline is ordered from “most specific” to “most general”:
--
-- 1. Definition hover (signature at the definition site).
-- 2. Constructor hover (type of the constructor in patterns).
-- 3. Resolved signature hover (overload-aware).
-- 4. Pattern-bound variable type.
-- 5. Keyword-bound arg (“bu” in clause heads).
-- 6. Expression/type inference fallback.
--
-- We first short-circuit on keywords/whitespace and then use the per-document
-- indices to avoid expensive AST walks.
onHover req respond = do
  st <- readState
  let params = req ^. L.params
      uri = params ^. L.textDocument . L.uri
      pos = params ^. L.position
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InR Null))
    Just doc -> do
      token <- liftIO (tokenAtPositionIO doc pos)
      case token of
        Nothing -> respond (Right (InR Null))
        Just (TokenKeyword _) -> respond (Right (InR Null))
        Just (TokenIdent tokenIdent) -> do
          let resolved = resolveAtPosition pos doc
              pst = dsParser doc
              paramTyCons = [name | (name, arity) <- parserTyCons pst, arity > 0]
              tyMods = parserTyMods pst
              tcSt = dsTC doc
              baseInfinitives = tcInfinitives (lsBaseTC st)
              docInfinitives = tcInfinitives tcSt
              allInfinitives = Set.union baseInfinitives docInfinitives
          let nonEmpty t = if T.null t then Nothing else Just t
              cursorVarNames =
                case raVar resolved of
                  Just (name, candidates) -> name : map fst candidates
                  Nothing -> []
              cursorMatchesSig sigName = sigName `elem` cursorVarNames
              renderTyNomMaybe ty = do
                t <- liftIO $ renderTyNomTextCached doc (lsCache st) (lsFsm st) paramTyCons tyMods ty
                return (nonEmpty t)
              renderHoverSignatureMaybe fnName args retTy isInfinitive = do
                t <- liftIO $ renderHoverSignature fnName args retTy isInfinitive allInfinitives (lsCache st) (lsFsm st) paramTyCons tyMods
                return (nonEmpty t)
          let hoverFromDefinition =
                case raDefIdent resolved of
                  Just (ident, _) -> do
                      let mFnDef = findFunctionStmt ident (dsStmts doc)
                          isInfBySet = Set.member ident allInfinitives
                      case mFnDef of
                        Just (fnName, args, parsedRetTy, stmtInf) -> do
                          let argTys = map snd args
                              mInferredRetTy = Map.lookup (fnName, argTys) (tcFuncSigRets tcSt)
                              retTy = fromMaybe parsedRetTy mInferredRetTy
                              isInfinitive = stmtInf || isInfBySet
                          renderHoverSignatureMaybe fnName args retTy isInfinitive
                        Nothing -> return Nothing
                  Nothing -> return Nothing
          let hoverFromBindDefinition =
                case findBindExpAt pos (Just tokenIdent) doc of
                  Just bindExp ->
                    case binderTypeFromUses pos doc of
                      Just ty -> renderTyNomMaybe ty
                      Nothing -> do
                        let tcSt' = dsTC doc
                            boundExp = bindValueExp bindExp
                        res <- liftIO (runTCM (inferType boundExp) tcSt')
                        case res of
                          Right (Just ty, _) -> renderTyNomMaybe ty
                          _ -> return Nothing
                  Nothing -> return Nothing
          let hoverFromResolvedSig =
                case raSpanInfo resolved >>= siSig of
                  Just (sigName, argTys)
                    | cursorMatchesSig sigName -> do
                      let mRetTy = Map.lookup (sigName, argTys) (tcFuncSigRets tcSt)
                          matchingArgs = [args | args <- Map.findWithDefault [] sigName (tcFuncSigs tcSt), map snd args == argTys]
                          mArgs = listToMaybe matchingArgs
                          candidatePool = infinitiveCandidates [sigName]
                          defInf = findInfinitiveDef candidatePool (dsStmts doc)
                          isInfinitive =
                            case defInf of
                              Just _ -> True
                              Nothing -> any (`Set.member` allInfinitives) candidatePool
                          infinitiveName =
                            case defInf of
                              Just inf -> inf
                              Nothing ->
                                case filter (`Set.member` allInfinitives) candidatePool of
                                  (inf:_) -> inf
                                  [] -> sigName
                      case (mArgs, mRetTy) of
                        (Just args, Just retTy) ->
                          renderHoverSignatureMaybe infinitiveName args retTy isInfinitive
                        _ -> return Nothing
                  Just _ -> return Nothing
                  Nothing -> return Nothing
          let tryRenderSignature exp' varExp = do
                let
                  expSpan = annSpan (annExp varExp)
                  mSpanInfo = spanInfoForSpan expSpan (dsSpanIndex doc) <|> spanInfoAtPosition pos (dsSpanIndex doc)
                  mResolved = siIdent =<< mSpanInfo
                  mResolvedSig = siSig =<< mSpanInfo  -- Get overload-specific signature
                  mResolvedType = siType =<< mSpanInfo
                  mResolvedIdent = case mResolved of
                    Just resolved -> Just resolved
                    Nothing -> case varExp of
                      Var _ varName candidates ->
                        -- Fallback: try all candidate names
                        listToMaybe (varName : map fst candidates)
                      _ -> Nothing
                  sigMatchesCursorVar sigName =
                    case varExp of
                      Var _ varName candidates ->
                        sigName `elem` (varName : map fst candidates)
                      _ ->
                        Just sigName == mResolvedIdent
                  fallbackToType = do
                    case mResolvedType of
                      Just ty -> renderTyNomMaybe ty
                      Nothing -> do
                        let tcSt' = dsTC doc
                        res <- liftIO (runTCM (inferType exp') tcSt')
                        case res of
                          Right (Just ty, _) -> renderTyNomMaybe ty
                          _ -> return Nothing
                case (mResolvedIdent, mResolvedSig) of
                  (Just resolvedName, Just (sigName, argTys))
                    | sigMatchesCursorVar sigName -> do
                    -- We have the resolved signature with specific argument types (overload info)
                    let tcSt = dsTC doc
                        mFnDef = findFunctionDef resolvedName (dsStmts doc)
                        mRetTy = Map.lookup (sigName, argTys) (tcFuncSigRets tcSt)
                        -- Find the function signature that matches the argument types
                        matchingArgs = [(sigName, args) | args <- Map.findWithDefault [] sigName (tcFuncSigs tcSt), map snd args == argTys]
                        mArgs = listToMaybe [args | (_, args) <- matchingArgs]
                        -- Check both base TC state (prelude) and document TC state (current file) for infinitives
                        -- Check if any morphological candidate is an infinitive
                        candidates = case varExp of
                          Var _ _ cs -> sigName : map fst cs
                          _ -> [sigName]
                        candidatePool = infinitiveCandidates candidates
                        defInf = findInfinitiveDef candidatePool (dsStmts doc)
                        isInfinitive =
                          case defInf of
                            Just _ -> True
                            Nothing -> any (`Set.member` allInfinitives) candidatePool
                        -- Find the infinitive form from candidates
                        infinitiveName =
                          case defInf of
                            Just inf -> inf
                            Nothing ->
                              case filter (`Set.member` allInfinitives) candidatePool of
                                (inf:_) -> inf
                                [] -> sigName
                    case (mFnDef, mArgs, mRetTy) of
                      (Just (fnName, args, _), _, Just retTy) -> do
                        -- Function in current file with TC return type
                        -- Use infinitive form if this is an infinitive function
                        let displayName = if isInfinitive then infinitiveName else fnName
                        renderHoverSignatureMaybe displayName args retTy isInfinitive
                      (Just (fnName, args, parsedRetTy), _, Nothing) -> do
                        -- Function in current file, use parsed return type
                        -- Use infinitive form if this is an infinitive function
                        let displayName = if isInfinitive then infinitiveName else fnName
                        renderHoverSignatureMaybe displayName args parsedRetTy isInfinitive
                      (Nothing, Just args, Just retTy) -> do
                        -- Prelude function with signature and return type matching the overload
                        -- Use infinitiveName for correct infinitive form
                        renderHoverSignatureMaybe infinitiveName args retTy isInfinitive
                      _ -> fallbackToType
                  (Just _, Just _) ->
                    fallbackToType
                  (Just resolvedName, Nothing) -> do
                    -- No resolved signature, try to find function definition
                    let tcSt = dsTC doc
                        mFnDef = findFunctionDef resolvedName (dsStmts doc)
                        -- Get first signature (for hover, we show the first overload)
                        mTCFuncSig = listToMaybe (Map.findWithDefault [] resolvedName (tcFuncSigs tcSt))
                        -- Check both base TC state (prelude) and document TC state (current file) for infinitives
                        -- Check if any morphological candidate is an infinitive
                        candidates = case varExp of
                          Var _ _ cs -> resolvedName : map fst cs
                          _ -> [resolvedName]
                        candidatePool = infinitiveCandidates candidates
                        defInf = findInfinitiveDef candidatePool (dsStmts doc)
                        isInfinitive =
                          case defInf of
                            Just _ -> True
                            Nothing -> any (`Set.member` allInfinitives) candidatePool
                        -- Find the infinitive form from candidates
                        infinitiveName =
                          case defInf of
                            Just inf -> inf
                            Nothing ->
                              case filter (`Set.member` allInfinitives) candidatePool of
                                (inf:_) -> inf
                                [] -> resolvedName
                    case (mFnDef, mTCFuncSig) of
                      (Just (fnName, args, _parsedRetTy), _) -> do
                        let argTys = map snd args
                            mInferredRetTy = Map.lookup (fnName, argTys) (tcFuncSigRets tcSt)
                            retTy = fromMaybe _parsedRetTy mInferredRetTy
                        renderHoverSignatureMaybe fnName args retTy isInfinitive
                      (Nothing, Just args) -> do
                        let argTys = map snd args
                            mInferredRetTy = Map.lookup (infinitiveName, argTys) (tcFuncSigRets tcSt)
                        case mInferredRetTy of
                          Just retTy -> do
                            renderHoverSignatureMaybe infinitiveName args retTy isInfinitive
                          Nothing -> do
                            fallbackToType
                      _ -> fallbackToType
                  (Nothing, _) ->
                    fallbackToType
          let hoverFromConstructor =
                case raCtor resolved of
                  Just (ctorIdent, _, pats, mScrutinee) -> do
                    case Map.lookup ctorIdent (tcCtors tcSt) of
                      Just (argTys, retTy) -> do
                        let argTysAligned =
                              if length pats < length argTys
                                then drop (length argTys - length pats) argTys
                                else argTys
                            patSpan pat =
                              case pat of
                                PWildcard ann -> Just (annSpan ann)
                                PVar _ ann -> Just (annSpan ann)
                                PCtor (_, ann) _ -> Just (annSpan ann)
                                PIntLit _ ann -> Just (annSpan ann)
                                PFloatLit _ ann -> Just (annSpan ann)
                                PStrLit _ ann -> Just (annSpan ann)
                                PCharLit _ ann -> Just (annSpan ann)
                                PListLit _ -> Nothing
                            patArgType pat =
                              case patSpan pat of
                                Just sp ->
                                  case sp of
                                    Span s _ _ -> siType =<< spanInfoAtPosition (posToLsp s) (dsSpanIndex doc)
                                    NoSpan -> Nothing
                                Nothing -> Nothing
                            mPatArgTys = mapM patArgType pats
                        retTy' <- case mScrutinee of
                          Just scrutExp -> do
                            mScrutTy <- liftIO (scrutineeTypeForExp pos scrutExp doc)
                            case mScrutTy of
                              Just scrutTy ->
                                case unifyTypes (Map.toList (tcTyCons tcSt)) [retTy] [scrutTy] of
                                  Just subst ->
                                    return (applySubst subst retTy)
                                  Nothing -> return retTy
                              Nothing -> do
                                case mPatArgTys of
                                  Just patTys
                                    | length patTys == length argTysAligned ->
                                        case unifyTypes (Map.toList (tcTyCons tcSt)) argTysAligned patTys of
                                          Just subst ->
                                            return (applySubst subst retTy)
                                          Nothing -> return retTy
                                  _ -> do
                                    res <- liftIO (runTCM (inferType scrutExp) tcSt)
                                    case res of
                                      Right (Just scrutTy, _) ->
                                        case unifyTypes (Map.toList (tcTyCons tcSt)) [retTy] [scrutTy] of
                                          Just subst ->
                                            return (applySubst subst retTy)
                                          Nothing -> return retTy
                                      _ -> return retTy
                          Nothing ->
                            case findFunctionClauseAt pos doc of
                              Just (args, _) ->
                                case args of
                                  ((_, _), scrutTy):_ ->
                                    case unifyTypes (Map.toList (tcTyCons tcSt)) [retTy] [scrutTy] of
                                      Just subst ->
                                        return (applySubst subst retTy)
                                      Nothing -> return retTy
                                  [] -> return retTy
                              Nothing -> return retTy
                        renderTyNomMaybe retTy'
                      Nothing -> return Nothing
                  Nothing -> return Nothing
          let hoverFromPatternBinding =
                case raExp resolved of
                  Nothing ->
                    case raPatVar resolved of
                      Just _ ->
                        case raResolvedType resolved of
                          Just ty -> renderTyNomMaybe ty
                          Nothing -> do
                            mPatTy <- liftIO (patternBoundTypeAtIdent pos doc)
                            case mPatTy of
                              Just ty -> renderTyNomMaybe ty
                              Nothing -> return Nothing
                      Nothing -> return Nothing
                  Just _ -> return Nothing
          let hoverFromArgKeyword = hoverFunctionArgKeyword pos doc
          let hoverFromExp =
                case raExp resolved of
                  Nothing -> return Nothing
                  Just exp' -> case exp' of
                    Bind {bindNameAnn = bindNameAnn, bindExp = bindExp} ->
                      if posInSpan pos (annSpan bindNameAnn)
                        then do
                          let binderTy =
                                spanInfoForSpan (annSpan bindNameAnn) (dsSpanIndex doc) >>= siType
                          case binderTy of
                            Just ty -> renderTyNomMaybe ty
                            Nothing -> do
                              let tcSt' = dsTC doc
                              res <- liftIO (runTCM (inferType bindExp) tcSt')
                              case res of
                                Right (Just ty, _) -> renderTyNomMaybe ty
                                _ -> return Nothing
                        else return Nothing
                    var@Var{} -> do
                      sig <- tryRenderSignature exp' var
                      case sig of
                        Just sigText -> return (Just sigText)
                        Nothing ->
                          case raResolvedType resolved of
                            Just ty -> renderTyNomMaybe ty
                            Nothing -> do
                              mPatTy <- liftIO (patternBoundTypeAt pos var doc)
                              case mPatTy of
                                Just ty -> renderTyNomMaybe ty
                                Nothing -> return Nothing
                    App {} -> do
                      case raResolvedType resolved of
                        Just ty -> renderTyNomMaybe ty
                        Nothing -> do
                          let tcSt' = dsTC doc
                          res <- liftIO (runTCM (inferType exp') tcSt')
                          case res of
                            Right (Just ty, _) -> renderTyNomMaybe ty
                            _ -> return Nothing
                    _ -> do
                      case raResolvedType resolved of
                        Just ty -> renderTyNomMaybe ty
                        Nothing -> do
                          let tcSt' = dsTC doc
                          res <- liftIO (runTCM (inferType exp') tcSt')
                          case res of
                            Right (Just ty, _) -> renderTyNomMaybe ty
                            _ -> return Nothing
          hoverText <- firstJustM
            [ hoverFromDefinition
            , hoverFromBindDefinition
            , hoverFromConstructor
            , hoverFromResolvedSig
            , hoverFromPatternBinding
            , hoverFromArgKeyword
            , hoverFromExp
            ]
          case hoverText of
            Just txt -> do
              let contents = InL (MarkupContent MarkupKind_PlainText txt)
                  hover = Hover contents Nothing
              respond (Right (InL hover))
            Nothing -> respond (Right (InR Null))
  where
    bindValueExp exp' =
      case exp' of
        Seq _ first _ -> first
        _ -> exp'
    
#if MIN_VERSION_lsp_types(2,3,0)
onDefinition :: TRequestMessage 'Method_TextDocumentDefinition -> (Either (TResponseError 'Method_TextDocumentDefinition) (MessageResult 'Method_TextDocumentDefinition) -> LspM Config ()) -> LspM Config ()
#else
onDefinition :: TRequestMessage 'Method_TextDocumentDefinition -> (Either ResponseError (MessageResult 'Method_TextDocumentDefinition) -> LspM Config ()) -> LspM Config ()
#endif
-- | Handle go-to-definition requests.
--
-- Resolution order:
--
-- 1. Ignore keywords/whitespace.
-- 2. Check bound variables via binder scopes.
-- 3. Check constructor uses in patterns.
-- 4. Consult typechecker-resolved definitions and overload signatures.
-- 5. Fall back to local definition spans, then workspace index.
--
-- This ordering ensures local bindings shadow global definitions while
-- still allowing cross-file navigation.
onDefinition req respond = do
  st <- readState
  let params = req ^. L.params
      uri = params ^. L.textDocument . L.uri
      pos = params ^. L.position
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL (Definition (InR []))))
    Just doc -> do
      token <- liftIO (tokenAtPositionIO doc pos)
      case token of
        Nothing -> respond (Right (InL (Definition (InR []))))
        Just (TokenKeyword _) -> respond (Right (InL (Definition (InR []))))
        _ -> do
          let resolved = resolveAtPosition pos doc
              mIdent = raVar resolved
              respondEmptyOrTypeFallback = do
                mLoc <- definitionTypeFallbackLocation st doc uri pos token resolved
                case mLoc of
                  Just loc -> respond (Right (InL (Definition (InR [loc]))))
                  Nothing -> respond (Right (InL (Definition (InR []))))
          case mIdent of
            Nothing -> do
              case raPatVar resolved of
                Just ident -> do
                  let keys = dedupeIdents [ident]
                      binderLoc = lookupBinderRange pos keys (fmap (annSpan . annExp) (raExp resolved)) (dsBinderSpans doc)
                  case binderLoc of
                    Just range -> do
                      let loc = Location uri range
                      respond (Right (InL (Definition (InR [loc]))))
                    Nothing ->
                      case raCtor resolved of
                        Just (ctorIdent, _, _, _) -> do
                          let keysCtor = dedupeIdents [ctorIdent]
                          case lookupDefRange keysCtor (dsDefSpans doc) of
                            Just range -> do
                              let loc = Location uri range
                              respond (Right (InL (Definition (InR [loc]))))
                            Nothing -> do
                              case lookupDefLocPreferExternal uri keysCtor (lsDefIndex st) of
                                Just loc -> respond (Right (InL (Definition (InR [loc]))))
                                Nothing -> do
                                  let currentDefs = defLocationsForUri uri (dsDefSpans doc)
                                  idx <- liftIO (buildDefinitionIndex st uri currentDefs)
                                  withState $ \s -> return (s { lsDefIndex = idx }, ())
                                  case lookupDefLocPreferExternal uri keysCtor idx of
                                    Nothing -> respondEmptyOrTypeFallback
                                    Just loc -> respond (Right (InL (Definition (InR [loc]))))
                        Nothing -> respondEmptyOrTypeFallback
                Nothing ->
                  case raCtor resolved of
                    Just (ctorIdent, _, _, _) -> do
                      let keysCtor = dedupeIdents [ctorIdent]
                      case lookupDefRange keysCtor (dsDefSpans doc) of
                        Just range -> do
                          let loc = Location uri range
                          respond (Right (InL (Definition (InR [loc]))))
                        Nothing -> do
                          case lookupDefLocPreferExternal uri keysCtor (lsDefIndex st) of
                            Just loc -> respond (Right (InL (Definition (InR [loc]))))
                            Nothing -> do
                              let currentDefs = defLocationsForUri uri (dsDefSpans doc)
                              idx <- liftIO (buildDefinitionIndex st uri currentDefs)
                              withState $ \s -> return (s { lsDefIndex = idx }, ())
                              case lookupDefLocPreferExternal uri keysCtor idx of
                                Nothing -> respondEmptyOrTypeFallback
                                Just loc -> respond (Right (InL (Definition (InR [loc]))))
                    Nothing ->
                      case definitionForArgKeyword uri pos doc of
                        Just loc -> respond (Right (InL (Definition (InR [loc]))))
                        Nothing -> respondEmptyOrTypeFallback
            Just (ident, candidates) -> do
              -- First check if this is a bound variable (pattern variable or let/bind binding)
              let keys = dedupeIdents (ident : map fst candidates)
                  binderLoc = lookupBinderRange pos keys (fmap (annSpan . annExp) (raExp resolved)) (dsBinderSpans doc)
              case binderLoc of
                Just range -> do
                  let loc = Location uri range
                  respond (Right (InL (Definition (InR [loc]))))
                Nothing -> do
                  -- Not a bound variable, continue with normal definition lookup
                  let mExp = raExp resolved
                      mSpanInfo =
                        case mExp of
                          Just Var{annExp = annExp'} -> spanInfoForSpan (annSpan annExp') (dsSpanIndex doc)
                          _ -> Nothing
                      mResolved = siIdent =<< mSpanInfo
                      mResolvedSig = siSig =<< mSpanInfo
                      resolvedKeys =
                        case mResolved of
                          Just resolved -> [resolved]
                          Nothing -> keys
                      tcLoc = case mResolvedSig of
                        Just sig -> defLocationFromSig sig (dsTC doc)
                        Nothing ->
                          case mResolved of
                            Just resolved -> defLocationFromTC resolved (dsTC doc)
                            Nothing -> Nothing
                  case tcLoc of
                    Just loc -> respond (Right (InL (Definition (InR [loc]))))
                    Nothing -> do
                      case lookupDefRange resolvedKeys (dsDefSpans doc) of
                        Just range -> do
                          let loc = Location uri range
                          respond (Right (InL (Definition (InR [loc]))))
                        Nothing -> do
                          case lookupDefLocPreferExternal uri resolvedKeys (lsDefIndex st) of
                            Just loc -> respond (Right (InL (Definition (InR [loc]))))
                            Nothing -> do
                              let currentDefs = defLocationsForUri uri (dsDefSpans doc)
                              idx <- liftIO (buildDefinitionIndex st uri currentDefs)
                              withState $ \s -> return (s { lsDefIndex = idx }, ())
                              case lookupDefLocPreferExternal uri resolvedKeys idx of
                                Nothing -> respondEmptyOrTypeFallback
                                Just loc -> respond (Right (InL (Definition (InR [loc]))))

#if MIN_VERSION_lsp_types(2,3,0)
onTypeDefinition :: TRequestMessage 'Method_TextDocumentTypeDefinition -> (Either (TResponseError 'Method_TextDocumentTypeDefinition) (MessageResult 'Method_TextDocumentTypeDefinition) -> LspM Config ()) -> LspM Config ()
#else
onTypeDefinition :: TRequestMessage 'Method_TextDocumentTypeDefinition -> (Either ResponseError (MessageResult 'Method_TextDocumentTypeDefinition) -> LspM Config ()) -> LspM Config ()
#endif
-- | Handle go-to-type-definition requests.
--
-- Type resolution strategy:
--
-- 1. Prefer resolved/cursor-local type information.
-- 2. For constructor uses, use the constructor return type from TC state.
-- 3. Fall back to token heuristics for numerics and sayı-like identifiers.
-- 4. Resolve type constructor identifiers to locations.
onTypeDefinition req respond = do
  st <- readState
  let params = req ^. L.params
      uri = params ^. L.textDocument . L.uri
      pos = params ^. L.position
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL (Definition (InR []))))
    Just doc -> do
      token <- liftIO (tokenAtPositionIO doc pos)
      let tcSt = dsTC doc
          mTyFromCtor =
            case findCtorInPattern pos doc of
              Just (ctorIdent, _, _, _) ->
                snd <$> Map.lookup ctorIdent (tcCtors tcSt)
              Nothing -> Nothing
          mTyFromResolved = resolvedTypeAt pos (dsResolvedTypes doc)
          mTy = mTyFromCtor <|> mTyFromResolved
      case mTy of
        Just ty -> do
          let keys = dedupeIdents (typeConstructorsFromTy ty)
          if null keys
            then respond (Right (InL (Definition (InR []))))
            else do
              locs <- resolveTypeDefinitionLocations st doc uri keys
              respond (Right (InL (Definition (InR locs))))
        Nothing ->
          case token of
            Just (TokenIdent tokenIdent) -> do
              let numericToken = maybe False (isDigit . fst) (T.uncons tokenIdent)
                  directKeysFromToken =
                    [([T.pack "tam"], T.pack "sayı") | numericToken]
                  sayıLikeToken =
                    T.pack "sayı" `T.isInfixOf` tokenIdent
                  sayıLikeKeys =
                    [([T.pack "tam"], T.pack "sayı")]
              if numericToken
                then do
                  locs <- resolveTypeDefinitionLocations st doc uri (dedupeIdents directKeysFromToken)
                  respond (Right (InL (Definition (InR locs))))
                else
                  if sayıLikeToken
                    then do
                      locs <- resolveTypeDefinitionLocations st doc uri (dedupeIdents sayıLikeKeys)
                      respond (Right (InL (Definition (InR locs))))
                    else respond (Right (InL (Definition (InR []))))
            _ -> respond (Right (InL (Definition (InR []))))
typeDefinitionLocationsByKeys :: LspState -> DocState -> Uri -> [Identifier] -> [Location]
typeDefinitionLocationsByKeys st doc uri keys =
  case listToMaybe (mapMaybe (\k -> defLocationFromTC k (dsTC doc) <|> defLocationFromTC k (lsBaseTC st)) keys) of
    Just loc -> [loc]
    Nothing ->
      case lookupDefRange keys (dsDefSpans doc) of
        Just range -> [Location uri range]
        Nothing ->
          case lookupDefLocPreferExternal uri keys (lsDefIndex st) of
            Just loc -> [loc]
            Nothing -> []

-- | Fallback go-to-definition resolution for type tokens.
--
-- This is used by "go to definition" when normal symbol lookup fails,
-- so type-ascription tokens (e.g. @tam-sayı@ in @tam-sayı olarak 5@)
-- can jump to the type definition just like "go to type definition".
definitionTypeFallbackLocation :: LspState -> DocState -> Uri -> Position -> Maybe TokenAtPosition -> ResolvedAt -> LspM Config (Maybe Location)
definitionTypeFallbackLocation st doc uri _pos token resolved = do
  let mTyFromCtor =
        case raCtor resolved of
          Just (ctorIdent, _, _, _) ->
            snd <$> Map.lookup ctorIdent (tcCtors (dsTC doc))
          Nothing -> Nothing
      mTy = mTyFromCtor <|> raResolvedType resolved
      mTokenIdent =
        case token of
          Just (TokenIdent tokenIdent) -> Just tokenIdent
          _ -> Nothing
      keysFromToken =
        case mTokenIdent of
          Nothing -> []
          Just tokenIdent ->
            let numericToken = maybe False (isDigit . fst) (T.uncons tokenIdent)
                sayıLikeToken = T.pack "sayı" `T.isInfixOf` tokenIdent
                parsedTypeToken = maybeToList (identifierFromTypeToken tokenIdent)
                sayıLikeKeys =
                  [ ([T.pack "tam"], T.pack "sayı")
                  , ([T.pack "ondalık"], T.pack "sayı")
                  ]
                numericKeys = [([T.pack "tam"], T.pack "sayı")]
            in dedupeIdents (parsedTypeToken ++ if numericToken then numericKeys else if sayıLikeToken then sayıLikeKeys else [])
      keys =
        case mTy of
          Just ty -> dedupeIdents (typeConstructorsFromTy ty)
          Nothing -> keysFromToken
  if null keys
    then return Nothing
    else do
      locs <- resolveTypeDefinitionLocations st doc uri keys
      return (listToMaybe locs)

-- | Parse a hyphenated type token into a Kip identifier.
--
-- Examples:
--   @tam-sayı@ -> @([\"tam\"], \"sayı\")@
--   @dizge@    -> @([], \"dizge\")@
identifierFromTypeToken :: Text -> Maybe Identifier
identifierFromTypeToken raw =
  let stripped = T.takeWhile (\c -> c /= '\'' && c /= '’') raw
      parts = filter (not . T.null) (T.splitOn (T.pack "-") stripped)
  in case parts of
      [] -> Nothing
      [x] -> Just ([], x)
      _ ->
        case reverse parts of
          (name:modsRev) -> Just (reverse modsRev, name)
          [] -> Nothing


resolveTypeDefinitionLocations :: LspState -> DocState -> Uri -> [Identifier] -> LspM Config [Location]
resolveTypeDefinitionLocations st doc uri keys = do
  let initial = typeDefinitionLocationsByKeys st doc uri keys
  return initial
    
#if MIN_VERSION_lsp_types(2,3,0)
onCompletion :: TRequestMessage 'Method_TextDocumentCompletion -> (Either (TResponseError 'Method_TextDocumentCompletion) (MessageResult 'Method_TextDocumentCompletion) -> LspM Config ()) -> LspM Config ()
#else
onCompletion :: TRequestMessage 'Method_TextDocumentCompletion -> (Either ResponseError (MessageResult 'Method_TextDocumentCompletion) -> LspM Config ()) -> LspM Config ()
#endif
-- | Handle completion requests.
--
-- We currently return a simple, static set consisting of:
--
-- * In-scope identifiers known to the parser.
-- * Type names and constructors.
-- * Function names from the typechecker.
--
-- This is fast and conservative, trading smart ranking for responsiveness.
onCompletion req respond = do
  st <- readState
  let uri = req ^. L.params . L.textDocument . L.uri
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL []))
    Just doc -> do
      let pst = dsParser doc
          ctxIdents = parserCtx pst
          typeNames = map fst (parserTyCons pst) ++ parserPrimTypes pst
          funcNames = Map.keys (tcFuncSigs (dsTC doc))
          candidates = Set.toList (ctxIdents `Set.union` Set.fromList (typeNames ++ funcNames))
          items = map completionItem candidates
      respond (Right (InL items))

#if MIN_VERSION_lsp_types(2,3,0)
onFormatting :: TRequestMessage 'Method_TextDocumentFormatting -> (Either (TResponseError 'Method_TextDocumentFormatting) (MessageResult 'Method_TextDocumentFormatting) -> LspM Config ()) -> LspM Config ()
#else
onFormatting :: TRequestMessage 'Method_TextDocumentFormatting -> (Either ResponseError (MessageResult 'Method_TextDocumentFormatting) -> LspM Config ()) -> LspM Config ()
#endif
-- | Handle formatting requests.
--
-- The formatter is intentionally minimal: it trims trailing whitespace and
-- enforces a trailing newline. This keeps it safe for all code paths.
onFormatting req respond = do
  st <- readState
  let uri = req ^. L.params . L.textDocument . L.uri
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL []))
    Just doc -> do
      let formatted = formatText (dsText doc)
      if formatted == dsText doc
        then respond (Right (InL []))
        else do
          let endPos = posFromDoc doc
              range = Range (Position 0 0) endPos
          respond (Right (InL [TextEdit range formatted]))

#if MIN_VERSION_lsp_types(2,3,0)
onDocumentHighlight :: TRequestMessage 'Method_TextDocumentDocumentHighlight -> (Either (TResponseError 'Method_TextDocumentDocumentHighlight) (MessageResult 'Method_TextDocumentDocumentHighlight) -> LspM Config ()) -> LspM Config ()
#else
onDocumentHighlight :: TRequestMessage 'Method_TextDocumentDocumentHighlight -> (Either ResponseError (MessageResult 'Method_TextDocumentDocumentHighlight) -> LspM Config ()) -> LspM Config ()
#endif
-- | Handle document highlight requests.
--
-- Highlights are computed by collecting all morphologically related names
-- and then walking the AST to find uses and definitions. We short-circuit
-- on keywords/whitespace and use indexed lookups for the initial symbol.
onDocumentHighlight req respond = do
  st <- readState
  let params = req ^. L.params
      uri = params ^. L.textDocument . L.uri
      pos = params ^. L.position
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL []))
    Just doc -> do
      token <- liftIO (tokenAtPositionIO doc pos)
      case token of
        Nothing -> respond (Right (InL []))
        Just (TokenKeyword _) -> respond (Right (InL []))
        _ -> do
          let mIdent = findVarAt pos doc
              -- If not a variable use, check if clicking on a definition
              mDefIdent = case mIdent of
                Just _ -> mIdent
                Nothing -> findDefinitionAt pos (dsDefSpans doc)
          case mDefIdent of
            Nothing -> respond (Right (InL []))
            Just (ident, candidates) -> do
              let allIdents = ident : map fst candidates
              highlights <- liftIO $ findHighlights st doc pos ident allIdents
              respond (Right (InL highlights))

-- | Apply a sequence of LSP content changes to document text.
--
-- The protocol allows both full-document replacements and ranged edits.
-- Clients may send incremental edits even when full sync is requested, so
-- we handle both forms here.
applyContentChanges :: Text -> [TextDocumentContentChangeEvent] -> Text
applyContentChanges =
  foldl' applyContentChange

-- | Apply one LSP content change to document text.
applyContentChange :: Text -> TextDocumentContentChangeEvent -> Text
#if MIN_VERSION_lsp_types(2,3,0)
applyContentChange oldText (TextDocumentContentChangeEvent change) =
  case change of
    InL (TextDocumentContentChangePartial range _ t) -> applyRangeEdit oldText range t
    InR (TextDocumentContentChangeWholeDocument t) -> t
#else
applyContentChange oldText (TextDocumentContentChangeEvent change) =
  case change of
    InL rec -> applyRangeEdit oldText (rec .! #range) (rec .! #text)
    InR rec -> rec .! #text
#endif

-- | Apply a ranged text edit to a UTF-16 position-based document.
applyRangeEdit :: Text -> Range -> Text -> Text
applyRangeEdit txt (Range startPos endPos) replacement =
  let (startOff, endOff) = offsetsAtRange txt startPos endPos
      prefix = T.take startOff txt
      suffix = T.drop endOff txt
  in prefix <> replacement <> suffix

-- | Convert an LSP position to a text offset, clamped to valid bounds.
--
-- LSP positions are UTF-16 line/character pairs. This scanner handles both
-- LF and CRLF line endings and clamps to end-of-line/file when needed.
offsetAtPosition :: Text -> Position -> Int
offsetAtPosition txt pos = fst (offsetsAtRange txt pos pos)

-- | Convert start/end positions to byte offsets in one text scan.
--
-- For range edits we need two offsets from the same source text. Doing this
-- in one pass avoids repeated traversal/allocation in incremental edit paths.
offsetsAtRange :: Text -> Position -> Position -> (Int, Int)
offsetsAtRange txt startPos endPos
  | (startLine, startCol) <= (endLine, endCol) =
      go 0 0 0 Nothing Nothing txt
  | otherwise =
      let (endOff, startOff) = offsetsAtRange txt endPos startPos
      in (startOff, endOff)
  where
    startLine = max 0 (fromIntegral (startPos ^. L.line))
    startCol = max 0 (fromIntegral (startPos ^. L.character))
    endLine = max 0 (fromIntegral (endPos ^. L.line))
    endCol = max 0 (fromIntegral (endPos ^. L.character))

    reached :: Int -> Int -> Int -> Int -> Bool
    reached line col targetL targetC =
      line > targetL || (line == targetL && col >= targetC)

    stepMatch :: Int -> Int -> Int -> Int -> Int -> Maybe Int -> Maybe Int
    stepMatch off line col targetL targetC m =
      case m of
        Just _ -> m
        Nothing ->
          if reached line col targetL targetC
            then Just off
            else Nothing

    finish :: Int -> Maybe Int -> Maybe Int -> (Int, Int)
    finish off mStart mEnd =
      let startOff = fromMaybe off mStart
          endOff = fromMaybe off mEnd
      in (startOff, endOff)

    go :: Int -> Int -> Int -> Maybe Int -> Maybe Int -> Text -> (Int, Int)
    go !off !line !col !mStart !mEnd !restTxt =
      let !mStart' = stepMatch off line col startLine startCol mStart
          !mEnd' = stepMatch off line col endLine endCol mEnd
      in case mStart' of
           Just startOff ->
             case mEnd' of
               Just endOff -> (startOff, endOff)
               Nothing ->
                 case T.uncons restTxt of
                   Nothing -> finish off mStart' mEnd'
                   Just (c, rest1)
                     | c == '\r' ->
                         case T.uncons rest1 of
                           Just ('\n', rest2) ->
                             if line == endLine
                               then (startOff, off)
                               else go (off + 2) (line + 1) 0 mStart' mEnd' rest2
                           _ ->
                             if line == endLine
                               then (startOff, off)
                               else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                     | c == '\n' ->
                         if line == endLine
                           then (startOff, off)
                           else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                     | otherwise ->
                         let !w = utf16Width c
                             !nextCol = col + w
                         in if line == endLine && nextCol > endCol
                              then (startOff, off)
                              else go (off + 1) line nextCol mStart' mEnd' rest1
           Nothing ->
             case T.uncons restTxt of
               Nothing -> finish off mStart' mEnd'
               Just (c, rest1)
                 | c == '\r' ->
                     case T.uncons rest1 of
                       Just ('\n', rest2) ->
                         if line == startLine
                           then finish off mStart' mEnd'
                           else go (off + 2) (line + 1) 0 mStart' mEnd' rest2
                       _ ->
                         if line == startLine
                           then finish off mStart' mEnd'
                           else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                 | c == '\n' ->
                     if line == startLine
                       then finish off mStart' mEnd'
                       else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                 | otherwise ->
                     let !w = utf16Width c
                         !nextCol = col + w
                     in if line == startLine && nextCol > startCol
                          then finish off mStart' mEnd'
                          else go (off + 1) line nextCol mStart' mEnd' rest1

    utf16Width :: Char -> Int
    utf16Width c =
      if ord c > 0xFFFF then 2 else 1

-- | Delay applied to edit-triggered analysis so bursts collapse to one job.
analysisDebounceMicros :: Int
analysisDebounceMicros = 50000

-- | Start a versioned document-analysis worker and cancel its predecessor.
scheduleDocumentAnalysis :: Uri -> Text -> Maybe Int32 -> Bool -> Bool -> LspM Config ()
scheduleDocumentAnalysis uri text version publish debounce = do
  env <- getLspEnv
  Config var <- getConfig
  oldWorker <- liftIO $ modifyMVar var $ \st -> do
    let jobId = lsNextAnalysisJob st
        oldThread = ajThread <$> Map.lookup uri (lsAnalysisJobs st)
        worker = do
          when debounce (threadDelay analysisDebounceMicros)
          runLspT env (processDocument jobId uri text version publish)
    thread <- forkIO (worker `finally` clearAnalysisJob var uri jobId)
    let job = AnalysisJob jobId thread
    return
      ( st
          { lsAnalysisJobs = Map.insert uri job (lsAnalysisJobs st)
          , lsNextAnalysisJob = jobId + 1
          }
      , oldThread
      )
  liftIO (mapM_ killThread oldWorker)

-- | Remove a completed worker without deleting a newer replacement.
clearAnalysisJob :: MVar LspState -> Uri -> Int -> IO ()
clearAnalysisJob var uri jobId =
  modifyMVar var $ \st ->
    let jobs = lsAnalysisJobs st
    in case Map.lookup uri jobs of
         Just job | ajId job == jobId ->
           return (st { lsAnalysisJobs = Map.delete uri jobs }, ())
         _ -> return (st, ())

-- | Parse/typecheck a document and optionally publish diagnostics.
--
-- This is the entry point for document change handling. It replaces the
-- cached 'DocState' with the newly analyzed one and merges definition
-- spans into the workspace index.
processDocument :: Int -> Uri -> Text -> Maybe Int32 -> Bool -> LspM Config ()
processDocument jobId uri text version publish = do
  st <- readState
  let current = maybe False ((== jobId) . ajId) (Map.lookup uri (lsAnalysisJobs st))
  if not current
    then return ()
    else do
      (doc, diags) <- liftIO (analyzeDocument st uri text)
      accepted <- withState $ \s ->
        let stillCurrent = maybe False ((== jobId) . ajId) (Map.lookup uri (lsAnalysisJobs s))
            textCurrent = lookupLatestTextByUri uri s == Just text
        in if stillCurrent && textCurrent
             then
               let defsForDoc = defLocationsForUri uri (dsDefSpans doc)
                   merged = Map.union (lsDefIndex s) defsForDoc
               in return
                    ( s
                        { lsDocs = Map.insert uri doc (lsDocs s)
                        , lsDefIndex = merged
                        }
                    , True
                    )
             else return (s, False)
      when (accepted && publish) $ do
        let bySource = partitionBySource diags
            bySource' = Map.insertWith (<>) (Just "kip") mempty bySource
        publishDiagnostics 100 (toNormalizedUri uri) version bySource'

-- | Analyze a document and produce a fresh 'DocState'.
--
-- The analysis pipeline:
--
-- 1. Try to load a cached module that matches the current text hash.
-- 2. Otherwise parse the text and register forward declarations.
-- 3. Typecheck the statements (LSP mode: avoid effect rejection).
-- 4. Build all resolution maps and indices for fast LSP queries.
--
-- Returns diagnostics so the caller can publish them.
analyzeDocument :: LspState -> Uri -> Text -> IO (DocState, [Diagnostic])
analyzeDocument st uri text = do
  let !docLines = V.fromList (T.lines text)
      !docLineStarts = buildLineStarts text
  mCached <- loadCachedDoc st uri text
  case mCached of
    Just (pstCached, tcCached, stmts) -> do
      -- Compute these once for cached path
      let defSpans = defSpansFromParser (lsBaseParser st) stmts pstCached
          -- NOTE: Span keys do not carry file paths, so cached resolved maps
          -- can contain entries from other modules that collide on (line,col).
          -- We filter to spans that appear in the current document to keep
          -- go-to-definition stable and overload-specific.
          docSpans = docSpanSet stmts
          resolved = Map.fromList (filterResolved docSpans (tcResolvedNames tcCached))
          resolvedSigs = Map.fromList (filterResolved docSpans (tcResolvedSigs tcCached))
          resolvedTypes = Map.fromList (filterResolved docSpans (tcResolvedTypes tcCached))
          binderSpans = collectBinderSpans stmts
          spanIndex = buildSpanIndex resolved resolvedSigs resolvedTypes binderSpans
          (expIndex, varIndex, patVarIndex, ctorIndex, matchClauseIndex, funcClauseIndex) = buildDocIndices stmts
      tyRenderCache <- HT.new
      tokenCache <- HT.new
      let doc = DocState text docLines docLineStarts pstCached tcCached stmts [] defSpans resolved resolvedSigs resolvedTypes binderSpans spanIndex expIndex varIndex patVarIndex ctorIndex matchClauseIndex funcClauseIndex tyRenderCache tokenCache
      return (doc, [])
    Nothing -> do
      let basePst = lsBaseParser st
          baseTC = lsBaseTC st
      parseRes <- parseFromFile basePst text
      case parseRes of
        Left err -> do
          let diag = parseErrorToDiagnostic text err
          tyRenderCache <- HT.new
          tokenCache <- HT.new
          let emptySpanIndex = SpanIndex HM.empty []
              emptyPosIndex = posIndexFromEntries []
              doc = DocState text docLines docLineStarts basePst baseTC [] [diag] Map.empty Map.empty Map.empty Map.empty [] emptySpanIndex emptyPosIndex emptyPosIndex emptyPosIndex emptyPosIndex emptyPosIndex emptyPosIndex tyRenderCache tokenCache
          return (doc, [diag])
        Right (stmts, pst') -> do
          -- Compute these once, early, and reuse throughout
          let !defSpans = defSpansFromParser (lsBaseParser st) stmts pst'
              !binderSpans = collectBinderSpans stmts
              (expIndex, varIndex, patVarIndex, ctorIndex, matchClauseIndex, funcClauseIndex) = buildDocIndices stmts
          declRes <- runTCM (registerForwardDecls stmts) baseTC
          case declRes of
            Left tcErr -> do
              diag <- tcErrorToDiagnostic st text tcErr
              let spanIndex = buildSpanIndex Map.empty Map.empty Map.empty binderSpans
              tyRenderCache <- HT.new
              tokenCache <- HT.new
              let doc = DocState text docLines docLineStarts pst' baseTC stmts [diag] defSpans Map.empty Map.empty Map.empty binderSpans spanIndex expIndex varIndex patVarIndex ctorIndex matchClauseIndex funcClauseIndex tyRenderCache tokenCache
              return (doc, [diag])
            Right (_, tcStWithDecls) -> do
              tcStWithDefs <- case uriToFilePath uri of
                Nothing -> return tcStWithDecls
                Just path -> do
                  let defSpansRaw = defSpansFromParserRaw (lsBaseParser st) stmts pst'
                      -- NOTE: Use only definition spans originating from this
                      -- document. The parser state includes prelude spans, so
                      -- using it directly can bind overloads to *stdlib* defs.
                      sigSpans = funcSigSpansFromStmts stmts (defSpanListsFromParser (lsBaseParser st) stmts pst')
                  res <- runTCM (recordDefLocations path defSpansRaw >> recordFuncSigLocations path sigSpans) tcStWithDecls
                  case res of
                    Left _ -> return tcStWithDecls
                    Right (_, tcStDefs) -> return tcStDefs
              (tcStFinal, diags) <- typecheckStmts st text tcStWithDefs stmts
              -- Compute docSpans once and reuse for both resolved maps
              let !docSpans = docSpanSet stmts
                  !resolved = Map.fromList (filterResolved docSpans (tcResolvedNames tcStFinal))
                  !resolvedSigs = Map.fromList (filterResolved docSpans (tcResolvedSigs tcStFinal))
                  !resolvedTypes = Map.fromList (filterResolved docSpans (tcResolvedTypes tcStFinal))
                  !spanIndex = buildSpanIndex resolved resolvedSigs resolvedTypes binderSpans
              tyRenderCache <- HT.new
              tokenCache <- HT.new
              let doc = DocState text docLines docLineStarts pst' tcStFinal stmts diags defSpans resolved resolvedSigs resolvedTypes binderSpans spanIndex expIndex varIndex patVarIndex ctorIndex matchClauseIndex funcClauseIndex tyRenderCache tokenCache
              return (doc, diags)

-- | Attempt to load a cached document if the on-disk cache matches the
-- current in-memory text hash.
--
-- This enables instant LSP responses when the file is unchanged.
loadCachedDoc :: LspState -> Uri -> Text -> IO (Maybe (ParserState, TCState, [Stmt Ann]))
loadCachedDoc st uri text =
  case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> do
      absPath <- canonicalizePathCached path
      exists <- doesFileExist absPath
      if not exists
        then return Nothing
        else do
          let cachePath = cacheFilePath absPath
          mCached <- loadCachedModule cachePath
          case mCached of
            Nothing -> return Nothing
            Just cached -> do
              -- Compare hash of in-memory text with cached sourceHash to detect unsaved changes
              let textBytes = encodeUtf8 text
                  textHash = hash textBytes
                  cachedHash = sourceHash (metadata cached)
              if textHash == cachedHash
                then do
                  pstCached <- fromCachedParserState (lsFsm st) (Just path) (lsUpsCache st) (lsDownsCache st) (cachedParser cached)
                  let tcCached = fromCachedTCState (cachedTC cached)
                      stmts = cachedTypedStmts cached
                  return (Just (pstCached, tcCached, stmts))
                else return Nothing  -- Text in memory differs from cached version

-- | Type-check statements without evaluation.
--
-- We clear the infinitive table between statements so effect restrictions
-- never abort the LSP pass. This yields a best-effort typed state for editor
-- features even in ill-formed code.
typecheckStmts :: LspState -> Text -> TCState -> [Stmt Ann] -> IO (TCState, [Diagnostic])
typecheckStmts st source tcSt stmts = do
  -- NOTE: The LSP server should stay useful even when a file violates
  -- effect restrictions (e.g. calling an infinitive function from a pure
  -- context). For editor features like "go to definition" we prefer
  -- a best-effort typed state over early failure. We therefore
  -- *continuously* clear the infinitive table during LSP-only typechecking
  -- so `rejectReadEffect` never aborts the pass and we can still collect
  -- resolved symbols and overload information.
  let tcStLsp = tcSt { tcInfinitives = Set.empty }
  let go acc stt =
        case acc of
          Left diags -> return (Left diags)
          Right current ->
            do
              -- Reset infinitives before each statement to avoid effect
              -- rejection inside that statement (previous definitions
              -- may have populated tcInfinitives).
              let currentLsp = current { tcInfinitives = Set.empty }
              res <- runTCM (tcStmt stt) currentLsp
              case res of
                Left tcErr -> do
                  diag <- tcErrorToDiagnostic st source tcErr
                  return (Left [diag])
                Right (_, next) ->
                  -- Clear again after the statement so later statements
                  -- also run without effect rejections.
                  return (Right next { tcInfinitives = Set.empty })
  res <- foldl' (\ioAcc stt -> ioAcc >>= \acc -> go acc stt) (return (Right tcStLsp)) stmts
  case res of
    Left diags -> return (tcStLsp, diags)
    Right final -> return (final, [])

-- | Write cache for a document.
--
-- Persists parser, typed AST, and metadata to speed up future LSP starts.
writeCacheForDoc :: LspState -> Uri -> DocState -> IO ()
writeCacheForDoc st uri doc = do
  case uriToFilePath uri of
    Nothing -> return ()
    Just path -> do
      absPath <- canonicalizePathCached path
      let cachePath = cacheFilePath absPath
      mCompilerHash <- getCompilerHash
      case mCompilerHash of
        Nothing -> return ()
        Just compilerHash -> do
          mSourceMeta <- getFileMeta absPath
          cachedParserState <- toCachedParserStateNoMorph (dsParser doc)
          let sourceBytes = encodeUtf8 (dsText doc)
              sourceDigest = hash sourceBytes
              fallbackSourceSize = fromIntegral (BS.length sourceBytes)
              (srcSize, srcMTime) = fromMaybe (fallbackSourceSize, 0) mSourceMeta
          let meta = CacheMetadata
                { compilerHash = compilerHash
                , sourceHash = sourceDigest
                , sourceSize = srcSize
                , sourceMTime = srcMTime
                , dependencies = []
                }
          let cachedModule = CachedModule
                { metadata = meta
                , cachedStmts = dsStmts doc
                , cachedTypedStmts = dsStmts doc
                , cachedParser = cachedParserState
                , cachedTC = toCachedTCState (dsTC doc)
                }
          saveCachedModule cachePath cachedModule

-- | Diagnostics helpers.
--
-- These map parser/typechecker errors into LSP diagnostics with stable ranges.
parseErrorToDiagnostic :: Text -> ParseErrorBundle Text ParserError -> Diagnostic
parseErrorToDiagnostic source bundle =
  let (ParseErrorBundle errs _posState) = bundle
      err = case errs of
        e :| _ -> e
      (line, col) = offsetToPos (T.take (errorOffset err) source)
      pos = Position line col
      customRange =
        case err of
          FancyError _ xs ->
            listToMaybe
              [ spanToRange sp
              | ErrorCustom (ErrUnrecognizedTurkishWord _ sp _) <- Set.toList xs
              ]
          _ -> Nothing
      range = fromMaybe (Range pos pos) customRange
      msg = renderParseErrorFor ParseErrorForLsp LangTr bundle
  in Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "kip") msg Nothing Nothing Nothing

-- | Render a typechecker error into a diagnostic using the LSP render context.
tcErrorToDiagnostic :: LspState -> Text -> TCError -> IO Diagnostic
tcErrorToDiagnostic st source tcErr = do
  let ctx = RenderCtx LangEn (lsCache st) (lsFsm st) (lsUpsCache st) (lsDownsCache st)
  msg <- runReaderT (renderTCError [] [] tcErr) ctx
  let range = case tcErrSpan tcErr of
        Nothing -> Range (Position 0 0) (Position 0 0)
        Just sp -> spanToRange sp
  return (Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "kip") msg Nothing Nothing Nothing)

-- | Range helpers.
--
-- These convert between Megaparsec positions and LSP positions/ranges.
posToLsp :: SourcePos -> Position
posToLsp (SourcePos _ line col) =
  let l = max 0 (unPos line - 1)
      c = max 0 (unPos col - 1)
  in Position (fromIntegral l) (fromIntegral c)

-- | Convert a Kip 'Span' to an LSP 'Range'.
spanToRange :: Span -> Range
spanToRange NoSpan = Range (Position 0 0) (Position 0 0)
spanToRange (Span s e _) = Range (posToLsp s) (posToLsp e)

-- | Merge an arbitrary list of spans, preserving the earliest start and latest end.
mergeSpanAll :: [Span] -> Span
mergeSpanAll spans =
  case [ (s, e) | Span s e _ <- spans ] of
    [] -> NoSpan
    pairs ->
      let starts = map fst pairs
          ends = map snd pairs
          path = case [ p | Span _ _ (Just p) <- spans ] of
            [] -> Nothing
            ps -> Just (head ps)  -- Assume all spans share the same path if present
      in Span (minimum starts) (maximum ends) path

-- | Collect all expression spans from a document's statements.
-- We use this to filter resolved-name/signature maps because 'Span'
-- does not encode file paths, and collisions across modules are common.
docSpanSet :: [Stmt Ann] -> Set.Set Span
docSpanSet = foldl' Set.union Set.empty . map stmtSpans
  where
    stmtSpans stt =
      case stt of
        Defn _ _ e -> expSpans e
        Function _ args _ clauses _ ->
          let argSpans = Set.fromList (map (annSpan . argIdentAnn) args)
              clauseSet = foldl' Set.union Set.empty (map clauseSpans clauses)
          in argSpans `Set.union` clauseSet
        ExpStmt e -> expSpans e
        _ -> Set.empty
    clauseSpans (Clause pat body) = expSpans body `Set.union` patSpans pat
    expSpans e =
      let here = Set.singleton (annSpan (annExp e))
          children =
            case e of
              App _ f args -> foldl' Set.union Set.empty (map expSpans (f:args))
              Bind _ _ nameAnn b ->
                Set.singleton (annSpan nameAnn) `Set.union` expSpans b
              Seq _ a b -> expSpans a `Set.union` expSpans b
              Match _ scr clauses ->
                expSpans scr `Set.union` foldl' Set.union Set.empty (map clauseSpans clauses)
              Let _ _ body -> expSpans body
              _ -> Set.empty
      in here `Set.union` children
    patSpans pat =
      case pat of
        PWildcard ann -> Set.singleton (annSpan ann)
        PVar _ ann -> Set.singleton (annSpan ann)
        PCtor (_, ann) pats ->
          Set.singleton (annSpan ann) `Set.union` foldl' Set.union Set.empty (map patSpans pats)
        PIntLit _ ann -> Set.singleton (annSpan ann)
        PFloatLit _ ann -> Set.singleton (annSpan ann)
        PStrLit _ ann -> Set.singleton (annSpan ann)
        PCharLit _ ann -> Set.singleton (annSpan ann)
        PListLit pats -> foldl' Set.union Set.empty (map patSpans pats)

-- | Keep only resolved entries that belong to the current document.
--
-- This avoids span collisions between the current file and cached modules.
filterResolved :: Set.Set Span -> [(Span, a)] -> [(Span, a)]
filterResolved allowed = filter (\(sp, _) -> Set.member sp allowed)

-- | Find expression at a position.
findExpAt :: Position -> DocState -> Maybe (Exp Ann)
findExpAt pos doc =
  lookupByPosition pos (dsExpIndex doc)

findVarAt :: Position -> DocState -> Maybe (Identifier, [(Identifier, Case)])
findVarAt pos doc =
  lookupByPosition pos (dsVarIndex doc)

-- | Find the enclosing match clause (scrutinee + pattern) for a position.
findMatchClauseAt :: Position -> DocState -> Maybe (Exp Ann, Pat Ann)
findMatchClauseAt pos doc =
  lookupByPosition pos (dsMatchClauseIndex doc)

-- | Find a pattern variable at a given position.
findPatVarAt :: Position -> DocState -> Maybe Identifier
findPatVarAt pos doc =
  lookupByPosition pos (dsPatVarIndex doc)

patternBoundTypeAtIdent :: Position -> DocState -> IO (Maybe (Ty Ann))
patternBoundTypeAtIdent pos doc =
  case findPatVarAt pos doc of
    Nothing -> return Nothing
    Just ident -> do
      let tcSt = dsTC doc
          lookupPatTy env = listToMaybe [ty | (name, ty) <- env, name == ident]
      case findMatchClauseAt pos doc of
        Just (scrutExp, pat) -> do
          mScrutTy <- scrutineeTypeForExp pos scrutExp doc
          case mScrutTy of
            Just scrutTy -> do
              let scrutArg = [((([], T.pack "_scrutinee"), mkAnn Nom NoSpan), scrutTy)]
              res <- runTCM (inferPatTypes pat scrutArg) tcSt
              case res of
                Right (patTys, _) -> return (lookupPatTy patTys)
                _ -> return Nothing
            Nothing -> return Nothing
        Nothing ->
          case findFunctionClauseAt pos doc of
            Just (args, pat) -> do
              res <- runTCM (inferPatTypes pat args) tcSt
              case res of
                Right (patTys, _) -> return (lookupPatTy patTys)
                _ -> return Nothing
            Nothing -> return Nothing

-- | Find the enclosing function clause (args + pattern) for a position.
--
-- This checks that the cursor is actually inside the clause pattern, which
-- prevents accidentally returning a clause from another part of the file.
findFunctionClauseAt :: Position -> DocState -> Maybe ([Arg Ann], Pat Ann)
findFunctionClauseAt pos doc =
  case lookupByPosition pos (dsFuncClauseIndex doc) of
    Just (args, pat)
      | posInPat pos pat -> Just (args, pat)
      | otherwise -> Nothing
    Nothing -> Nothing

-- | Find the enclosing function clause by scope span.
--
-- This is a faster variant used when the caller already knows the cursor is
-- inside a clause body and only needs the arguments in scope.
findFunctionClauseAtScope :: Position -> DocState -> Maybe ([Arg Ann], Pat Ann)
findFunctionClauseAtScope pos doc =
  lookupByPosition pos (dsFuncClauseIndex doc)

-- | Find function arguments for a line that contains a clause pattern.
--
-- Used for the special “bu” keyword hover/definition in clause heads.
findFunctionArgsAtLine :: Position -> [Stmt Ann] -> Maybe [Arg Ann]
findFunctionArgsAtLine pos stmts =
  listToMaybe
    [ args
    | Function _ args _ clauses _ <- stmts
    , any (patLineContains pos) clauses
    ]

-- | Check whether a clause pattern spans the given line.
--
-- This is line-based to allow looking up clause arguments without requiring
-- exact cursor placement on the pattern itself.
patLineContains :: Position -> Clause Ann -> Bool
patLineContains pos (Clause pat _) =
  case patRootSpan pat of
    Span start end _ ->
      let Position l _ = pos
          line = fromIntegral l
          startLine = unPos (sourceLine start) - 1
          endLine = unPos (sourceLine end) - 1
      in line >= startLine && line <= endLine
    NoSpan -> False

-- | Compute the span that covers a pattern and all its nested children.
--
-- This is used to approximate clause scope for line-based lookups.
patRootSpan :: Pat Ann -> Span
patRootSpan pat =
  case pat of
    PWildcard ann -> annSpan ann
    PVar _ ann -> annSpan ann
    PCtor (_, ann) pats -> mergeSpanAll (annSpan ann : map patRootSpan pats)
    PIntLit _ ann -> annSpan ann
    PFloatLit _ ann -> annSpan ann
    PStrLit _ ann -> annSpan ann
    PCharLit _ ann -> annSpan ann
    PListLit pats -> mergeSpanAll (map patRootSpan pats)

-- | Determine scrutinee type for a match clause when possible.
--
-- We try several fast paths before falling back to inference:
--
-- * Resolved signature of a function call.
-- * Resolved type on the scrutinee span.
-- * In-scope function argument types.
scrutineeTypeForExp :: Position -> Exp Ann -> DocState -> IO (Maybe (Ty Ann))
scrutineeTypeForExp pos scrutExp doc = do
  let tcSt = dsTC doc
      scrutSpan = annSpan (annExp scrutExp)
      mScrutTyFromSig =
        case scrutExp of
          App _ fn _ ->
            case fn of
              Var {annExp = annFn} ->
                case spanInfoForSpan (annSpan annFn) (dsSpanIndex doc) >>= siSig of
                  Just (sigName, argTys) -> Map.lookup (sigName, argTys) (tcFuncSigRets tcSt)
                  Nothing -> Nothing
              _ -> Nothing
          _ -> Nothing
      mScrutTyFromResolved =
        (spanInfoForSpan scrutSpan (dsSpanIndex doc) >>= siType) <|>
          case scrutSpan of
            Span s _ _ -> spanInfoAtPosition (posToLsp s) (dsSpanIndex doc) >>= siType
            NoSpan -> Nothing
      posForArgLookup =
        case scrutSpan of
          Span s _ _ -> posToLsp s
          NoSpan -> pos
      mScrutTyFromArgs =
        case scrutExp of
          Var {varName = name} -> argTypeForVarAt posForArgLookup name doc
          _ -> Nothing
  case mScrutTyFromSig <|> mScrutTyFromArgs <|> mScrutTyFromResolved of
    Just ty -> return (Just ty)
    Nothing -> do
      let mFnArgs = findFunctionArgsAt posForArgLookup doc
      res <- case mFnArgs of
        Just args ->
          let bindings = [ (ident, ty) | ((ident, _), ty) <- args ]
          in runTCM (withVarTypes bindings (inferType scrutExp)) tcSt
        Nothing -> runTCM (inferType scrutExp) tcSt
      case res of
        Right (Just ty, _) -> return (Just ty)
        _ -> return Nothing

-- | Infer a pattern-bound variable type at a position within a match clause.
-- | Infer a pattern-bound variable type at a position within a match clause.
--
-- This uses the enclosing match clause and runs pattern inference against
-- the scrutinee type, falling back to nothing if any step fails.
patternBoundTypeAt :: Position -> Exp Ann -> DocState -> IO (Maybe (Ty Ann))
patternBoundTypeAt pos varExp doc =
  case varExp of
    Var {varName = name, varCandidates = candidates} ->
      case findMatchClauseAt pos doc of
        Just (scrutExp, pat) -> do
          mScrutTy <- scrutineeTypeForExp pos scrutExp doc
          case mScrutTy of
            Just scrutTy -> do
              let scrutArg = [((([], T.pack "_scrutinee"), mkAnn Nom NoSpan), scrutTy)]
              res <- runTCM (inferPatTypes pat scrutArg) (dsTC doc)
              case res of
                Right (patTys, _) ->
                  return (lookupByCandidates (name : map fst candidates) patTys)
                _ -> return Nothing
            Nothing -> return Nothing
        Nothing -> return Nothing
    _ -> return Nothing
  where
    lookupByCandidates names env =
      listToMaybe [ty | (ident, ty) <- env, ident `elem` names]

-- | Try to find an argument type for a variable at a given position.
--
-- This is a fast scope-only lookup used to avoid full inference when
-- the variable is a function argument.
argTypeForVarAt :: Position -> Identifier -> DocState -> Maybe (Ty Ann)
argTypeForVarAt pos ident doc =
  case findFunctionClauseAtScope pos doc of
    Just (args, _) ->
      listToMaybe
        [ ty
        | ((argIdent, _), ty) <- args
        , argIdent == ident
        ]
    Nothing -> Nothing

-- | Find function arguments in scope at a position.
--
-- Used to seed type inference for variables when the resolved type is absent.
findFunctionArgsAt :: Position -> DocState -> Maybe [Arg Ann]
findFunctionArgsAt pos doc =
  fmap fst (findFunctionClauseAtScope pos doc)

-- | Recover binder type by inspecting resolved variable uses in binder scope.
--
-- Some binder sites can overlap with enclosing expression spans whose type is
-- not the bound value type. This fallback uses resolved identifier+type data
-- from in-scope variable uses to recover the binder's actual type.
binderTypeFromUses :: Position -> DocState -> Maybe (Ty Ann)
binderTypeFromUses pos doc = do
  binder <- listToMaybe . sortOn (rangeSizeForSort . biRange) $
    [ bi | bi <- dsBinderSpans doc, positionInRange pos (biRange bi) ]
  fmap snd . listToMaybe . sortOn fst $
    [ (spanSizeForSort sp, ty)
    | (sp, ident) <- Map.toList (dsResolved doc)
    , ident == biIdent binder
    , spanInScope sp (biScope binder)
    , spanToRange sp /= biRange binder
    , Just ty <- [Map.lookup sp (dsResolvedTypes doc)]
    ]
  where
    spanInScope sp scope =
      case sp of
        Span s e _ -> posInSpan (posToLsp s) scope && posInSpan (posToLsp e) scope
        NoSpan -> False

-- | Find the bound expression of a binder definition at cursor position.
--
-- This matches only the binder name site (e.g. \"x\" in \"x için ...\")
-- and returns the expression being bound so hover can show the binder type.
findBindExpAt :: Position -> Maybe Text -> DocState -> Maybe (Exp Ann)
findBindExpAt pos mWord doc =
  fmap thd3 . listToMaybe . sortOn candidateSortKey . concatMap collectStmt $ dsStmts doc
  where
    thd3 (_, _, c) = c
    spanKey sp =
      case sp of
        NoSpan -> (maxBound :: Int, maxBound :: Int)
        _ -> spanSizeForSort sp
    candidateSortKey (bindSp, expSp, _) = (spanKey bindSp, spanKey expSp)
    collectStmt stt =
      case stt of
        Defn _ _ e -> collectExp e
        Function _ _ _ clauses _ -> concatMap collectClause clauses
        ExpStmt e -> collectExp e
        _ -> []
    collectClause (Clause pat body) = collectPat pat ++ collectExp body
    collectPat pat =
      case pat of
        PWildcard _ -> []
        PVar _ _ -> []
        PCtor _ pats -> concatMap collectPat pats
        PIntLit _ _ -> []
        PFloatLit _ _ -> []
        PStrLit _ _ -> []
        PCharLit _ _ -> []
        PListLit pats -> concatMap collectPat pats
    collectExp e =
      let nested =
            case e of
              App _ fn args -> concatMap collectExp (fn : args)
              Bind _ _ _ bindExp -> collectExp bindExp
              Seq _ a b -> collectExp a ++ collectExp b
              Match _ scr clauses -> collectExp scr ++ concatMap collectClause clauses
              Let _ _ body -> collectExp body
              Ascribe _ _ exp' -> collectExp exp'
              _ -> []
      in case e of
          Bind {bindName = bindName, bindNameAnn = bindNameAnn, bindExp = bindExp} ->
            let bindSpan = annSpan bindNameAnn
                bindToken = snd bindName
                tokenMatches =
                  case mWord of
                    Just w -> w == bindToken
                    Nothing -> False
            in case bindSpan of
                NoSpan -> nested
                _ | tokenMatches && posInSpan pos bindSpan -> (bindSpan, annSpan (annExp bindExp), bindExp) : nested
                  | otherwise -> nested
          _ -> nested

-- | Check whether a clause body or pattern contains a cursor position.
clauseContainsPos :: Position -> Clause Ann -> Bool
clauseContainsPos p (Clause pat body) =
  posInSpan p (annSpan (annExp body)) || posInPat p pat

-- | Check whether a cursor is inside a pattern, including nested patterns.
posInPat :: Position -> Pat Ann -> Bool
posInPat p pat =
  case pat of
    PWildcard ann -> posInSpan p (annSpan ann)
    PVar _ ann -> posInSpan p (annSpan ann)
    PCtor (_, ann) pats -> posInSpan p (annSpan ann) || any (posInPat p) pats
    PIntLit _ ann -> posInSpan p (annSpan ann)
    PFloatLit _ ann -> posInSpan p (annSpan ann)
    PStrLit _ ann -> posInSpan p (annSpan ann)
    PCharLit _ ann -> posInSpan p (annSpan ann)
    PListLit pats -> any (posInPat p) pats

-- | Hover handler for the special clause-argument keyword \"bu\".
--
-- When the cursor is on \"bu\" in a clause head, we show the argument type
-- rather than a keyword hover.
hoverFunctionArgKeyword :: Position -> DocState -> LspM Config (Maybe Text)
hoverFunctionArgKeyword pos doc = do
  token <- liftIO (tokenAtPositionIO doc pos)
  case token of
    Just (TokenIdent "bu") ->
      case findFunctionArgsAtLine pos (dsStmts doc) of
        Just args ->
          case lookupArg "bu" args of
            Just (_, ty) -> do
              st <- readState
              let pst = dsParser doc
                  paramTyCons = [name | (name, arity) <- parserTyCons pst, arity > 0]
                  tyMods = parserTyMods pst
              hoverText <- liftIO $ renderTyNomTextCached doc (lsCache st) (lsFsm st) paramTyCons tyMods ty
              return (Just hoverText)
            Nothing -> return Nothing
        Nothing -> return Nothing
    _ -> return Nothing
  where
    lookupArg name args =
      listToMaybe
        [ (ann, ty)
        | ((ident, ann), ty) <- args
        , ident == ([], name)
        ]

-- | Go-to-definition handler for the special clause-argument keyword \"bu\".
--
-- Returns the argument binder location if \"bu\" refers to a clause argument.
definitionForArgKeyword :: Uri -> Position -> DocState -> Maybe Location
definitionForArgKeyword uri pos doc =
  case tokenAtPosition doc pos of
    Just (TokenIdent "bu") ->
      case findFunctionArgsAtLine pos (dsStmts doc) of
        Just args ->
          case lookupArg "bu" args of
            Just (ann, _) -> Just (Location uri (spanToRange (annSpan ann)))
            Nothing -> Nothing
        Nothing -> Nothing
    _ -> Nothing
  where
    lookupArg name args =
      listToMaybe
        [ (ann, ty)
        | ((ident, ann), ty) <- args
        , ident == ([], name)
        ]

-- | List of reserved keywords that should not trigger symbol lookups.
keywords :: [Text]
keywords = ["ya", "var", "için", "olarak", "dersek"]

-- | Classify the token at a cursor position.
--
-- Returns 'Nothing' for whitespace or out-of-range positions.
tokenAtPosition :: DocState -> Position -> Maybe TokenAtPosition
tokenAtPosition doc (Position line char) = do
  word <- wordAtPosition doc (fromIntegral line) (fromIntegral char)
  if word `elem` keywords
    then Just (TokenKeyword word)
    else Just (TokenIdent word)

-- | Cached token classification for a position within the current document.
--
-- This avoids repeated text slicing when hover/definition/highlight are
-- called frequently on the same cursor position.
tokenAtPositionIO :: DocState -> Position -> IO (Maybe TokenAtPosition)
tokenAtPositionIO doc pos = do
  let key = (pos ^. L.line, pos ^. L.character)
  existing <- HT.lookup (dsTokenCache doc) key
  case existing of
    Just token -> return token
    Nothing -> do
      let token = tokenAtPosition doc pos
      HT.insert (dsTokenCache doc) key token
      return token

-- | Evaluate actions in order and return the first 'Just' result.
--
-- This supports the hover pipeline where earlier steps are more specific.
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (action:rest) = do
  res <- action
  case res of
    Just _ -> return res
    Nothing -> firstJustM rest

-- | Extract the word at a given (line, column) position.
--
-- The word definition includes letters, digits, hyphen and apostrophes to
-- accommodate Kip’s identifiers and possessive forms.
wordAtPosition :: DocState -> Int -> Int -> Maybe Text
wordAtPosition doc lineIdx colIdx = do
  line <- safeIndexVec (dsLines doc) lineIdx
  if colIdx < 0 || colIdx > T.length line
    then Nothing
    else
      let (left, right) = T.splitAt colIdx line
          leftWord = T.takeWhileEnd isWordChar left
          rightWord = T.takeWhile isWordChar right
          word = leftWord <> rightWord
      in if T.null word then Nothing else Just word
  where
    isWordChar c = isAlphaNum c || c == '-' || c == '\'' || c == '’'

safeIndexVec :: V.Vector a -> Int -> Maybe a
safeIndexVec vec i
  | i < 0 || i >= V.length vec = Nothing
  | otherwise = Just (vec V.! i)

-- | Find a constructor in a pattern at the given position.
--
-- Uses the precomputed constructor index and returns the constructor name,
-- its annotation, nested patterns, and (if known) the scrutinee expression.
findCtorInPattern :: Position -> DocState -> Maybe (Identifier, Ann, [Pat Ann], Maybe (Exp Ann))
findCtorInPattern pos doc =
  lookupByPosition pos (dsCtorIndex doc)

-- | Find if the cursor is on a definition by range lookup.
--
-- This is used for go-to-definition when no resolved symbol is available.
findDefinitionAt :: Position -> Map.Map Identifier Range -> Maybe (Identifier, [(Identifier, Case)])
findDefinitionAt pos =
  Map.foldrWithKey
    (\ident range acc ->
        if positionInRange pos range
          then Just (ident, [])
          else acc)
    Nothing
  where
    positionInRange (Position line char) (Range (Position startLine startChar) (Position endLine endChar))
      | line < startLine || line > endLine = False
      | line == startLine && char < startChar = False
      | line == endLine && char >= endChar = False
      | otherwise = True

-- | Compute document highlights for a target identifier and its variants.
--
-- We expand related identifiers (morphological closure), then collect all
-- variable uses and definition sites.
findHighlights :: LspState -> DocState -> Position -> Identifier -> [Identifier] -> IO [DocumentHighlight]
findHighlights _st doc _pos _ident allIdents = do
  let stmts = dsStmts doc
      defSpans = dsDefSpans doc
      -- Collect all morphological variants by computing transitive closure
      -- over all Vars in the AST and definitions
      targetIdents = Set.fromList (collectAllRelatedIdents allIdents stmts defSpans)

  -- Traverse the AST to find all variable uses that match
  let usageHighlights = concatMap (findHighlightsInStmt targetIdents) stmts

  -- Also highlight function/value definitions
  let defHighlights = mapMaybe (highlightDefinition targetIdents) (Map.toList defSpans)

  return (usageHighlights ++ defHighlights)

-- | Collect all morphologically related identifiers by computing transitive closure.
--
-- The closure includes related AST variables and definitions that share a root.
collectAllRelatedIdents :: [Identifier] -> [Stmt Ann] -> Map.Map Identifier Range -> [Identifier]
collectAllRelatedIdents initial stmts defSpans =
  let allVarData = collectAllVars stmts
      allDefIdents = Map.keys defSpans
      -- Compute transitive closure: keep expanding the set of identifiers
      -- until it stops growing, using both exact matches and prefix matches
      closure identSet =
        let -- Add identifiers from Var expressions
            newFromVars = Set.fromList
              [ ident
              | (varName, candidates) <- allVarData
              , let allNames = varName : map fst candidates
              , any (\name -> name `Set.member` identSet || shareCommonRoot name identSet) allNames
              , ident <- allNames
              ]
            -- Add identifiers from definitions that share a root
            newFromDefs = Set.fromList
              [ defIdent
              | defIdent <- allDefIdents
              , defIdent `Set.member` identSet || shareCommonRoot defIdent identSet
              ]
            combined = Set.unions [identSet, newFromVars, newFromDefs]
        in if combined == identSet then identSet else closure combined
  in Set.toList (closure (Set.fromList initial))

-- | Check if an identifier shares a common root with any identifier in the set.
--
-- The heuristic is tuned for Kip morphology (prefix/suffix changes).
-- Two identifiers share a common root if:
-- 1. They share a common prefix of at least 5 characters, OR
-- 2. One is a prefix of the other (for base forms like "bastır" and "bastırmak"), OR
-- 3. They share 4+ chars prefix and lengths differ by at most 4 (for infinitive variants)
shareCommonRoot :: Identifier -> Set.Set Identifier -> Bool
shareCommonRoot (ns1, name1) identSet =
  let minPrefixLen = 5
      shortPrefixLen = 4
      maxLenDiff = 4
      relatedTo name2 =
        let len1 = T.length name1
            len2 = T.length name2
            prefix5 = len1 >= minPrefixLen && len2 >= minPrefixLen &&
                     T.take minPrefixLen name1 == T.take minPrefixLen name2
            isPrefix = (len1 >= shortPrefixLen && name1 `T.isPrefixOf` name2) ||
                      (len2 >= shortPrefixLen && name2 `T.isPrefixOf` name1)
            similarLength = len1 >= shortPrefixLen && len2 >= shortPrefixLen &&
                           T.take shortPrefixLen name1 == T.take shortPrefixLen name2 &&
                           abs (len1 - len2) <= maxLenDiff
        in prefix5 || isPrefix || similarLength
  in any (\(ns2, name2) -> ns1 == ns2 && relatedTo name2) (Set.toList identSet)

-- | Collect all Var nodes from the AST with their names and candidates.
--
-- This supports highlight expansion and related-identifier discovery.
collectAllVars :: [Stmt Ann] -> [(Identifier, [(Identifier, Case)])]
collectAllVars = concatMap collectVarsInStmt
  where
    collectVarsInStmt stmt = case stmt of
      Defn _ _ e -> collectVarsInExp e
      Function _ _ _ clauses _ -> concatMap collectVarsInClause clauses
      ExpStmt e -> collectVarsInExp e
      _ -> []

    collectVarsInClause (Clause _ body) = collectVarsInExp body

    collectVarsInExp e =
      let sub = case e of
            App _ fn args -> collectVarsInExp fn ++ concatMap collectVarsInExp args
            Bind _ _ _ body -> collectVarsInExp body
            Seq _ a b -> collectVarsInExp a ++ collectVarsInExp b
            Match _ scrut clauses -> collectVarsInExp scrut ++ concatMap collectVarsInClause clauses
            Let _ _ body -> collectVarsInExp body
            _ -> []
          current = case e of
            Var{varName = name, varCandidates = candidates} -> [(name, candidates)]
            _ -> []
      in current ++ sub

-- | Create a highlight for a definition if it matches our targets.
highlightDefinition :: Set.Set Identifier -> (Identifier, Range) -> Maybe DocumentHighlight
highlightDefinition targets (ident, range) =
  if ident `Set.member` targets
    then Just (DocumentHighlight range (Just DocumentHighlightKind_Text))
    else Nothing

-- | Find all highlights in a statement.
findHighlightsInStmt :: Set.Set Identifier -> Stmt Ann -> [DocumentHighlight]
findHighlightsInStmt targets stmt =
  case stmt of
    Defn _ _ e -> findHighlightsInExp targets e
    Function _ _ _ clauses _ -> concatMap (findHighlightsInClause targets) clauses
    ExpStmt e -> findHighlightsInExp targets e
    _ -> []

-- | Find all highlights in a clause.
findHighlightsInClause :: Set.Set Identifier -> Clause Ann -> [DocumentHighlight]
findHighlightsInClause targets (Clause _ body) = findHighlightsInExp targets body

-- | Find all highlights in an expression.
findHighlightsInExp :: Set.Set Identifier -> Exp Ann -> [DocumentHighlight]
findHighlightsInExp targets e =
  let current = case e of
        Var{annExp = ann, varName = name, varCandidates = candidates} ->
          -- Check if this variable matches any of our targets
          let allNames = name : map fst candidates
          in [DocumentHighlight (spanToRange (annSpan ann)) (Just DocumentHighlightKind_Text)
             | any (`Set.member` targets) allNames]
        _ -> []
      children = case e of
        App _ f args -> concatMap (findHighlightsInExp targets) (f:args)
        Bind _ _ _ b -> findHighlightsInExp targets b
        Seq _ a b -> findHighlightsInExp targets a ++ findHighlightsInExp targets b
        Match _ scr clauses ->
          findHighlightsInExp targets scr ++ concatMap (findHighlightsInClause targets) clauses
        Let _ _ body -> findHighlightsInExp targets body
        _ -> []
  in current ++ children


-- | Lookup the first definition range for any of the candidate identifiers.
--
-- Used as a local fallback when resolved symbols are unavailable.
lookupDefRange :: [Identifier] -> Map.Map Identifier Range -> Maybe Range
lookupDefRange keys m =
  listToMaybe (mapMaybe (`Map.lookup` m) keys)

-- | Find the binder for a variable at a given position.
-- Searches for binders whose scope contains the position, picking the innermost one.
lookupBinderRange :: Position -> [Identifier] -> Maybe Span -> [BinderInfo] -> Maybe Range
lookupBinderRange pos keys mScope binders =
  let -- Find all binders with matching identifiers
      matching = [bi | bi <- binders, biIdent bi `elem` keys]
      -- Filter to those whose scope contains the position
      inScope = [bi | bi <- matching, posInSpan pos (biScope bi)]
      scopeFiltered = case mScope of
        Nothing -> inScope
        Just scope -> [bi | bi <- inScope, spanContainsSpan (biScope bi) scope]
      -- Sort by scope size (smallest first = innermost scope)
      sorted = sortOn (spanSize . biScope) scopeFiltered
  in case sorted of
       (bi:_) -> Just (biRange bi)
       [] -> Nothing
  where
    posInSpan p sp = case sp of
      NoSpan -> False
      Span start end _ ->
        let Position line char = p
            SourcePos _ startLine startCol = start
            SourcePos _ endLine endCol = end
            l = fromIntegral line
            c = fromIntegral char
            sl = unPos startLine - 1
            sc = unPos startCol - 1
            el = unPos endLine - 1
            ec = unPos endCol - 1
        in (l > sl || (l == sl && c >= sc)) && (l < el || (l == el && c < ec))

    spanSize sp = case sp of
      NoSpan -> maxBound :: Int
      Span start end _ ->
        let SourcePos _ startLine startCol = start
            SourcePos _ endLine endCol = end
            lines = unPos endLine - unPos startLine
            cols = if lines == 0 then unPos endCol - unPos startCol else maxBound :: Int
        in lines * 10000 + cols

    spanContainsSpan outer inner =
      case (outer, inner) of
        (Span s e _, Span s' e' _) -> s' >= s && e' <= e
        _ -> False

-- | Lookup the first definition location for any of the candidate identifiers.
lookupDefLoc :: [Identifier] -> Map.Map Identifier Location -> Maybe Location
lookupDefLoc keys m =
  listToMaybe (mapMaybe (`Map.lookup` m) keys)

-- | Lookup a definition location preferring external (non-current) files.
--
-- This avoids jumping to local shadowed definitions when a more canonical
-- definition exists in the standard library or another module.
lookupDefLocPreferExternal :: Uri -> [Identifier] -> Map.Map Identifier Location -> Maybe Location
lookupDefLocPreferExternal currentUri keys m =
  case lookupDefLoc keys m of
    Just loc@(Location uri _) | uri /= currentUri -> Just loc
    _ -> listToMaybe (mapMaybe pickExternal keys)
  where
    pickExternal key =
      case Map.lookup key m of
        Just loc@(Location uri _) | uri /= currentUri -> Just loc
        _ -> Nothing

-- | Remove duplicate identifiers while preserving order.
dedupeIdents :: [Identifier] -> [Identifier]
dedupeIdents =
  nub

-- | Collect all concrete type constructor identifiers from a type.
--
-- We skip type variables/skolems because they do not have global definitions.
typeConstructorsFromTy :: Ty Ann -> [Identifier]
typeConstructorsFromTy =
  go (128 :: Int)
  where
    go :: Int -> Ty Ann -> [Identifier]
    go n _ | n <= 0 = []
    go n ty =
      case ty of
        TyInt _ ->
          [([T.pack "tam"], T.pack "sayı")]
        TyFloat _ ->
          [([T.pack "ondalık"], T.pack "sayı")]
        TyString _ -> [([], T.pack "dizge")]
        TyChar _ -> [([], T.pack "karakter")]
        TyVar _ _ -> []
        TySkolem _ _ -> []
        TyInd _ ident -> [ident]
        Arr _ inTy outTy -> go (n - 1) inTy ++ go (n - 1) outTy
        TyApp _ ctor args -> go (n - 1) ctor ++ concatMap (go (n - 1)) args

-- | Check whether an LSP position lies within a Kip span.
posInSpan :: Position -> Span -> Bool
posInSpan _ NoSpan = False
posInSpan (Position l c) (Span s e _) =
  let sl = fromIntegral (unPos (sourceLine s) - 1)
      sc = fromIntegral (unPos (sourceColumn s) - 1)
      el = fromIntegral (unPos (sourceLine e) - 1)
      ec = fromIntegral (unPos (sourceColumn e) - 1)
  in (l > sl || (l == sl && c >= sc)) && (l < el || (l == el && c <= ec))

-- | Compute an ordering key for spans (smaller spans sort first).
spanSizeForSort :: Span -> (Int, Int)
spanSizeForSort sp = case sp of
  NoSpan -> (maxBound :: Int, maxBound :: Int)
  Span start end _ ->
    let SourcePos _ startLine startCol = start
        SourcePos _ endLine endCol = end
        lines = unPos endLine - unPos startLine
        cols = if lines == 0 then unPos endCol - unPos startCol else maxBound :: Int
    in (lines, cols)

-- | Find the smallest resolved type span that contains the given position.
resolvedTypeAt :: Position -> Map.Map Span (Ty Ann) -> Maybe (Ty Ann)
resolvedTypeAt pos m =
  let matches = [(spanSizeForSort sp, ty) | (sp, ty) <- Map.toList m, posInSpan pos sp]
  in fmap snd (listToMaybe (sortOn fst matches))

-- | Build a definition map directly from parser spans.
--
-- This filters out stdlib spans to keep local definition mapping precise.
defSpansFromParser :: ParserState -> [Stmt Ann] -> ParserState -> Map.Map Identifier Range
defSpansFromParser base stmts pst =
  Map.map spanToRange (defSpansFromParserRaw base stmts pst)

-- | Like 'defSpansFromParser', but returns raw spans instead of ranges.
defSpansFromParserRaw :: ParserState -> [Stmt Ann] -> ParserState -> Map.Map Identifier Span
defSpansFromParserRaw base stmts pst =
  let baseDefs = latestDefSpans (parserDefSpans base)
      allowed = Set.fromList (stmtNamesFromStmts stmts)
      localDefs =
        Map.filterWithKey
          (\ident sp ->
            Set.member ident allowed && Map.lookup ident baseDefs /= Just sp)
          (latestDefSpans (parserDefSpans pst))
  in localDefs

-- | Collect per-identifier definition span lists for the current document only.
-- We must exclude spans inherited from the base parser (prelude), otherwise
-- overloads in the current file may be mapped to stdlib definitions.
defSpanListsFromParser :: ParserState -> [Stmt Ann] -> ParserState -> Map.Map Identifier [Span]
defSpanListsFromParser base stmts pst =
  let baseLists = parserDefSpans base
      allowed = Set.fromList (stmtNamesFromStmts stmts)
      stripBase ident spans =
        let baseSpans = Map.findWithDefault [] ident baseLists
            -- Keep only spans that do not appear in the base list.
            -- This is robust even if the base list is not a strict prefix.
            localOnly = filter (`notElem` baseSpans) spans
        in localOnly
      localLists =
        Map.mapWithKey stripBase (parserDefSpans pst)
      filtered =
        Map.filterWithKey (\ident spans -> Set.member ident allowed && not (null spans)) localLists
  in filtered
-- | Collect names defined by statements.
stmtNamesFromStmts :: [Stmt Ann] -> [Identifier]
stmtNamesFromStmts = concatMap stmtNames'
  where
    stmtNames' stt =
      case stt of
        Defn name _ _ -> [name]
        Function name _ _ _ _ -> [name]
        PrimFunc name _ _ _ -> [name]
        NewType name _ ctors -> name : map (fst . fst) ctors
        PrimType name _ -> [name]
        _ -> []

-- | Build definition spans without stripping base definitions.
--
-- Used when indexing the standard library itself.
defSpansFromParserIncludeBase :: [Stmt Ann] -> ParserState -> Map.Map Identifier Range
defSpansFromParserIncludeBase stmts pst =
  let allowed = Set.fromList (stmtNamesFromStmts stmts)
      spans = Map.filterWithKey (\ident _ -> Set.member ident allowed) (latestDefSpans (parserDefSpans pst))
  in Map.map spanToRange spans

-- | Choose the most recent span for each identifier.
latestDefSpans :: Map.Map Identifier [Span] -> Map.Map Identifier Span
latestDefSpans =
  Map.mapMaybe (\spans -> case reverse spans of
    sp:_ -> Just sp
    [] -> Nothing)

-- | Build signature span map by pairing argument types with definition spans.
funcSigSpansFromStmts :: [Stmt Ann] -> Map.Map Identifier [Span] -> Map.Map (Identifier, [Ty Ann]) Span
funcSigSpansFromStmts stmts defSpans =
  fst (foldl' step (Map.empty, defSpans) stmts)
  where
    step (acc, spans) stmt =
      case stmt of
        Function name args _ _ _ ->
          let (mSp, spans') = takeSpan name spans
          in (maybe acc (\sp -> Map.insert (name, map snd args) sp acc) mSp, spans')
        PrimFunc name args _ _ ->
          let (mSp, spans') = takeSpan name spans
          in (maybe acc (\sp -> Map.insert (name, map snd args) sp acc) mSp, spans')
        _ -> (acc, spans)
    takeSpan name spans =
      case Map.lookup name spans of
        Just (sp:rest) -> (Just sp, Map.insert name rest spans)
        _ -> (Nothing, spans)

-- | Convert definition ranges to locations for a specific URI.
defLocationsForUri :: Uri -> Map.Map Identifier Range -> Map.Map Identifier Location
defLocationsForUri uri =
  Map.map (Location uri)

-- | Find a definition location from the typechecker state.
defLocationFromTC :: Identifier -> TCState -> Maybe Location
defLocationFromTC ident tcSt =
  case Map.lookup ident (tcDefLocations tcSt) of
    Just (path, sp) -> Just (Location (filePathToUri path) (spanToRange sp))
    Nothing -> Nothing

-- | Find a definition location from a function signature.
defLocationFromSig :: (Identifier, [Ty Ann]) -> TCState -> Maybe Location
defLocationFromSig sig tcSt =
  case Map.lookup sig (tcFuncSigLocs tcSt) of
    Just (path, sp) -> Just (Location (filePathToUri path) (spanToRange sp))
    Nothing -> Nothing

-- | Build a definition index for the workspace and standard library.
--
-- This scans all module roots and stores the best-definition location for
-- each identifier, preferring closer roots and non-test files.
buildDefinitionIndex :: LspState -> Uri -> Map.Map Identifier Location -> IO (Map.Map Identifier Location)
buildDefinitionIndex st uri currentDefs = do
  (roots, mRoot) <- resolveIndexRoots st uri
  files <- concat <$> mapM listKipFilesRecursive roots
  index <- foldl' (\ioAcc path -> ioAcc >>= \acc -> indexFile mRoot acc path) (return Map.empty) files
  return (Map.union index currentDefs)
  where
    indexFile mRoot acc path = do
      defs <- loadDefsForFile st path
      let newScore = pathScore st mRoot path
          merged = Map.foldlWithKey' (insertWithScore newScore) acc defs
      return merged
    insertWithScore newScore acc ident loc =
      Map.insertWith (preferByScore newScore) ident loc acc

-- | Resolve module roots and project root for indexing.
resolveIndexRoots :: LspState -> Uri -> IO ([FilePath], Maybe FilePath)
resolveIndexRoots st uri = do
  mRoot <- case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> findProjectRoot path
  let roots = lsModuleDirs st ++ maybeToList mRoot
  return (nub roots, mRoot)

-- | Walk upward from a path to find the project root.
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot path = do
  let startDir = takeDirectory path
  go startDir
  where
    go dir = do
      let cabalPath = dir </> "kip.cabal"
          stackPath = dir </> "stack.yaml"
      cabalExists <- doesFileExist cabalPath
      stackExists <- doesFileExist stackPath
      if cabalExists || stackExists
        then return (Just dir)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then return Nothing
            else go parent

-- | Recursively list all `.kip` files under a root, skipping common build dirs.
listKipFilesRecursive :: FilePath -> IO [FilePath]
listKipFilesRecursive root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then return []
    else go root
  where
    skipDirs = Set.fromList [".git", ".stack-work", "dist-newstyle", "node_modules", "vendor", "playground", "dist", ".tmp"]
    go dir = do
      entries <- listDirectory dir
      fmap concat $
        forM entries $ \entry -> do
          let path = dir </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then if Set.member entry skipDirs
              then return []
              else go path
            else if takeExtension path == ".kip"
              then return [path]
              else return []

-- | Load definition locations for a file, using cache when possible.
loadDefsForFile :: LspState -> FilePath -> IO (Map.Map Identifier Location)
loadDefsForFile st path = do
  absPath <- canonicalizePathCached path
  let normalized = addTrailingPathSeparator (normalise absPath)
      moduleRoots = map (addTrailingPathSeparator . normalise) (lsModuleDirs st)
      isStdlib = any (`isPrefixOf` normalized) moduleRoots
  let cachePath = cacheFilePath absPath
  mCached <- loadCachedModule cachePath
  case mCached of
    Just cached -> do
      pst <- fromCachedParserState (lsFsm st) (Just path) (lsUpsCache st) (lsDownsCache st) (cachedParser cached)
      let stmts = cachedStmts cached
          defSpans =
            if isStdlib
              then defSpansFromParserIncludeBase stmts pst
              else defSpansFromParser (lsBaseParser st) stmts pst
      return (defLocationsForUri (filePathToUri absPath) defSpans)
    Nothing -> do
      src <- TIO.readFile absPath
      parseRes <- parseFromFile (lsBaseParser st) src
      case parseRes of
        Left _ -> return Map.empty
        Right (stmts, pst) ->
          let defSpans =
                if isStdlib
                  then defSpansFromParserIncludeBase stmts pst
                  else defSpansFromParser (lsBaseParser st) stmts pst
          in return (defLocationsForUri (filePathToUri absPath) defSpans)

-- | Prefer the lower-scored location (closer root, non-test) when merging.
preferByScore :: Int -> Location -> Location -> Location
preferByScore newScore newLoc oldLoc =
  let oldScore = locationScore oldLoc
  in if newScore < oldScore then newLoc else oldLoc

-- | Compute a score for a file path (lower is better).
pathScore :: LspState -> Maybe FilePath -> FilePath -> Int
pathScore st mRoot path =
  let normalized = addTrailingPathSeparator (normalise path)
      moduleRoots = map (addTrailingPathSeparator . normalise) (lsModuleDirs st)
      inModule = any (`isPrefixOf` normalized) moduleRoots
      inRoot = maybe False (\root -> addTrailingPathSeparator (normalise root) `isPrefixOf` normalized) mRoot
      isTest = "/tests/" `T.isInfixOf` T.pack normalized
  in if inModule
       then 0
       else if inRoot
         then if isTest then 2 else 1
         else 3

-- | Compute a score for a location (used for tie-breaking).
locationScore :: Location -> Int
locationScore (Location uri _) =
  case uriToFilePath uri of
    Nothing -> 3
    Just path -> if "/tests/" `T.isInfixOf` T.pack path then 2 else 1


-- | Convert an offset into (line, column) coordinates.
offsetToPos :: Text -> (UInt, UInt)
offsetToPos prefix =
  let (!lineInt, !colInt) =
        T.foldl'
          (\(!ln, !col) c ->
              if c == '\n'
                then (ln + 1, 0)
                else (ln, col + 1))
          (0, 0)
          prefix
  in (fromIntegral lineInt, fromIntegral colInt)

-- | Compute line start offsets for quick line-based lookups.
buildLineStarts :: Text -> U.Vector Int
buildLineStarts txt =
  U.fromList (reverse startsRev)
  where
    (startsRev, _) =
      T.foldl'
        (\(acc, !off) c ->
            let !off' = off + 1
            in if c == '\n'
                 then (off' : acc, off')
                 else (acc, off'))
        ([0], 0)
        txt

-- | Resolve previous document text for incremental didChange handling.
--
-- We first try exact URI lookup, then normalized-URI lookup, and finally
-- fall back to on-disk text when this is the first seen change for a URI.
docTextForChange :: Uri -> Map.Map Uri DocState -> IO Text
docTextForChange uri docs =
  case lookupDocByUri uri docs of
    Just txt -> return txt
    Nothing ->
      case uriToFilePath uri of
        Nothing -> return ""
        Just path -> do
          exists <- doesFileExist path
          if exists then TIO.readFile path else return ""

-- | Lookup the latest raw text for a URI using exact/normalized matching.
lookupLatestTextByUri :: Uri -> LspState -> Maybe Text
lookupLatestTextByUri uri st =
  case Map.lookup uri (lsLatestText st) of
    Just txt -> Just txt
    Nothing -> lookupByNormalizedUri uri (lsLatestText st)

-- | Lookup a document by exact or normalized URI.
lookupDocByUri :: Uri -> Map.Map Uri DocState -> Maybe Text
lookupDocByUri uri docs =
  dsText <$> (lookupDocKeyByUri uri docs >>= (`Map.lookup` docs))

-- | Find the map key for a URI using exact/normalized matching.
lookupDocKeyByUri :: Uri -> Map.Map Uri DocState -> Maybe Uri
lookupDocKeyByUri uri docs =
  case Map.lookup uri docs of
    Just _ -> Just uri
    Nothing ->
      let norm = toNormalizedUri uri
      in Map.foldrWithKey
           (\k _ acc ->
               if toNormalizedUri k == norm
                 then Just k
                 else acc)
           Nothing
           docs

lookupByNormalizedUri :: Uri -> Map.Map Uri a -> Maybe a
lookupByNormalizedUri uri mp =
  let norm = toNormalizedUri uri
  in Map.foldrWithKey
       (\k v acc ->
           if toNormalizedUri k == norm
             then Just v
             else acc)
       Nothing
       mp

-- | Format document: trim trailing whitespace and ensure trailing newline.
formatText :: Text -> Text
formatText txt =
  let trimmed = T.unlines (map T.stripEnd (T.lines txt))
  in if T.null trimmed || T.last trimmed == '\n' then trimmed else trimmed <> "\n"

-- | Compute the final position at the end of the current document.
posFromDoc :: DocState -> Position
posFromDoc doc =
  if V.null ls
    then Position 0 0
    else Position (fromIntegral (V.length ls - 1)) (fromIntegral (T.length (V.last ls)))
  where
    ls = dsLines doc

-- | Convert an identifier to a completion item.
completionItem :: Identifier -> CompletionItem
completionItem ident =
  CompletionItem
    { _label = T.pack (prettyIdent ident)
    , _labelDetails = Nothing
    , _kind = Just CompletionItemKind_Variable
    , _tags = Nothing
    , _detail = Nothing
    , _documentation = Nothing
    , _deprecated = Nothing
    , _preselect = Nothing
    , _sortText = Nothing
    , _filterText = Nothing
    , _insertText = Nothing
    , _insertTextFormat = Nothing
    , _insertTextMode = Nothing
    , _textEdit = Nothing
    , _textEditText = Nothing
    , _additionalTextEdits = Nothing
    , _commitCharacters = Nothing
    , _command = Nothing
    , _data_ = Nothing
    }

-- | Collect all bound variable spans from statements with their scopes.
--
-- This powers go-to-definition for local variables (arguments, pattern vars,
-- let/bind bindings) by tracking the binder range and its validity scope.
collectBinderSpans :: [Stmt Ann] -> [BinderInfo]
collectBinderSpans = concatMap stmtBinders
  where
    stmtBinders stmt =
      case stmt of
        Defn _ ty e ->
          -- For Defn, the scope is the entire statement
          let scope = mergeSpan (annSpan (annTy ty)) (annSpan (annExp e))
          in expBinders scope e
        Function _ args retTy clauses _ ->
          -- For Function, arguments are valid in all clause bodies
          -- Use the merged span of all clause bodies as the scope
          let bodySpans = [annSpan (annExp body) | Clause _ body <- clauses]
              fnScope = case bodySpans of
                [] -> annSpan (annTy retTy)
                (first:rest) -> foldl' mergeSpan first rest
              argBinders' = concatMap (argBinder fnScope) args
              clauseBinders' = concatMap clauseBindersWithOwnScope clauses
          in argBinders' ++ clauseBinders'
        PrimFunc _ args retTy _ ->
          let scope = annSpan (annTy retTy)
          in concatMap (argBinder scope) args
        ExpStmt e ->
          let scope = annSpan (annExp e)
          in expBinders scope e
        _ -> []

    argBinder scope ((ident, ann), _ty) =
      [BinderInfo ident (spanToRange (annSpan ann)) scope]

    clauseBindersWithOwnScope (Clause pat body) =
      -- Each clause uses its own body as the scope for pattern variables
      let bodyScope = annSpan (annExp body)
      in patBinders bodyScope pat ++ expBinders bodyScope body

    clauseBinders scope (Clause pat body) =
      patBinders scope pat ++ expBinders scope body

    patBinders scope pat =
      case pat of
        PWildcard _ -> []
        PVar ident ann -> [BinderInfo ident (spanToRange (annSpan ann)) scope]
        PCtor _ pats -> concatMap (patBinders scope) pats
        PIntLit _ _ -> []
        PFloatLit _ _ -> []
        PStrLit _ _ -> []
        PCharLit _ _ -> []
        PListLit pats -> concatMap (patBinders scope) pats

    expBinders scope e =
      case e of
        Let ann name body ->
          -- For Let, the variable is bound in the body
          let letScope = annSpan (annExp body)
          in BinderInfo name (spanToRange (annSpan ann)) letScope : expBinders letScope body
        Bind _ name nameAnn b ->
          -- For Bind, use the provided scope (from the enclosing context)
          BinderInfo name (spanToRange (annSpan nameAnn)) scope : expBinders scope b
        App _ f args -> concatMap (expBinders scope) (f:args)
        Seq _ a b -> expBinders scope a ++ expBinders scope b
        Match _ scr clauses ->
          expBinders scope scr ++ concatMap (clauseBinders scope) clauses
        _ -> []
