{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Language Server Protocol implementation for Kip.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (void, when, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.List (foldl', nub, isPrefixOf)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Row.Records ((.!))
import System.Directory (canonicalizePath, doesFileExist, listDirectory, doesDirectoryExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, takeExtension, (</>), normalise, addTrailingPathSeparator)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)

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
import Kip.Eval (EvalState, emptyEvalState)
import Kip.Parser
import Kip.Render
import Kip.Runner (RenderCtx(..), Lang(..), renderTCError, tcErrSpan, loadPreludeState)
import Kip.TypeCheck
import Language.Foma
import Paths_kip (getDataFileName)

import Control.Lens ((^.))
import Text.Megaparsec (ParseErrorBundle(..), errorOffset)
import Text.Megaparsec.Pos (SourcePos(..), unPos, sourceLine, sourceColumn)
import Data.List.NonEmpty (NonEmpty(..))

-- | Server configuration/state.
data LspState = LspState
  { lsCache :: !RenderCache
  , lsFsm :: !FSM
  , lsUpsCache :: !MorphCache
  , lsDownsCache :: !MorphCache
  , lsModuleDirs :: ![FilePath]
  , lsBaseParser :: !ParserState
  , lsBaseTC :: !TCState
  , lsDocs :: Map.Map Uri DocState
  , lsDefIndex :: Map.Map Identifier Location
  }

-- | Per-document cached state.
data DocState = DocState
  { dsText :: !Text
  , dsParser :: !ParserState
  , dsTC :: !TCState
  , dsStmts :: ![Stmt Ann]
  , dsDiagnostics :: ![Diagnostic]
  , dsDefSpans :: Map.Map Identifier Range
  , dsResolved :: Map.Map Span Identifier
  , dsResolvedSigs :: Map.Map Span (Identifier, [Ty Ann])
  }

newtype Config = Config { cfgVar :: MVar LspState }

main :: IO ()
main = do
  initialState <- initState
  let config = Config initialState
  let serverDef = ServerDefinition
        { defaultConfig = config
        , onConfigurationChange = \cfg _ -> Right cfg
        , doInitialize = \env _ -> pure (Right env)
        , staticHandlers = handlers
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = lspOptions
        }
  void (runServer serverDef)

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
  , requestHandler SMethod_TextDocumentCompletion onCompletion
  , requestHandler SMethod_TextDocumentFormatting onFormatting
  ]

onInitialized :: TNotificationMessage 'Method_Initialized -> LspM Config ()
onInitialized _ = return ()

onSetTrace :: TNotificationMessage 'Method_SetTrace -> LspM Config ()
onSetTrace _ = return ()

onDidChangeWatchedFiles :: TNotificationMessage 'Method_WorkspaceDidChangeWatchedFiles -> LspM Config ()
onDidChangeWatchedFiles _ = return ()

onCancelRequest _ = return ()

withState :: (LspState -> IO (LspState, a)) -> LspM Config a
withState f = do
  Config var <- getConfig
  liftIO (modifyMVar var f)

readState :: LspM Config LspState
readState = do
  Config var <- getConfig
  liftIO (readMVar var)

initState :: IO (MVar LspState)
initState = do
  trmorphPath <- getDataFileName "vendor/trmorph.fst"
  libPath <- getDataFileName "lib/temel.kip"
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
    , lsDocs = Map.empty
    , lsDefIndex = Map.empty
    }

-- | Handlers
onDidOpen :: TNotificationMessage 'Method_TextDocumentDidOpen -> LspM Config ()
onDidOpen msg = do
  let doc = msg ^. L.params . L.textDocument
      uri = doc ^. L.uri
      text = doc ^. L.text
  processDocument uri text True

onDidChange :: TNotificationMessage 'Method_TextDocumentDidChange -> LspM Config ()
onDidChange msg = do
  let params = msg ^. L.params
      uri = params ^. L.textDocument . L.uri
      changes = params ^. L.contentChanges
  case changes of
    (change:_) -> processDocument uri (changeText change) True
    [] -> return ()

onDidClose :: TNotificationMessage 'Method_TextDocumentDidClose -> LspM Config ()
onDidClose msg = do
  let uri = msg ^. L.params . L.textDocument . L.uri
  withState $ \s -> return (s { lsDocs = Map.delete uri (lsDocs s) }, ())

onDidSave :: TNotificationMessage 'Method_TextDocumentDidSave -> LspM Config ()
onDidSave msg = do
  let uri = msg ^. L.params . L.textDocument . L.uri
  st <- readState
  case Map.lookup uri (lsDocs st) of
    Nothing -> return ()
    Just doc -> do
      _ <- liftIO (writeCacheForDoc st uri doc)
      return ()

onHover :: TRequestMessage 'Method_TextDocumentHover -> (Either ResponseError (MessageResult 'Method_TextDocumentHover) -> LspM Config ()) -> LspM Config ()
onHover req respond = do
  st <- readState
  let params = req ^. L.params
      uri = params ^. L.textDocument . L.uri
      pos = params ^. L.position
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InR Null))
    Just doc -> do
      let mExp = findExpAt pos (dsStmts doc)
      case mExp of
        Nothing -> respond (Right (InR Null))
        Just exp' -> do
          let tcSt = dsTC doc
          res <- liftIO (runTCM (inferType exp') tcSt)
          case res of
            Left _ -> respond (Right (InR Null))
            Right (Nothing, _) -> respond (Right (InR Null))
            Right (Just ty, _) -> do
              let pst = dsParser doc
                  paramTyCons = [name | (name, arity) <- parserTyCons pst, arity > 0]
                  tyMods = parserTyMods pst
              tyText <- liftIO $ renderTyText (lsCache st) (lsFsm st) paramTyCons tyMods ty
              let contents = InL (MarkupContent MarkupKind_PlainText tyText)
                  hover = Hover contents Nothing
              respond (Right (InL hover))

onDefinition :: TRequestMessage 'Method_TextDocumentDefinition -> (Either ResponseError (MessageResult 'Method_TextDocumentDefinition) -> LspM Config ()) -> LspM Config ()
onDefinition req respond = do
  st <- readState
  let params = req ^. L.params
      uri = params ^. L.textDocument . L.uri
      pos = params ^. L.position
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL (Definition (InR []))))
    Just doc -> do
      let mIdent = findVarAt pos (dsStmts doc)
      case mIdent of
        Nothing -> respond (Right (InL (Definition (InR []))))
        Just (ident, candidates) -> do
          let mResolved =
                case findExpAt pos (dsStmts doc) of
                  Just Var{annExp = annExp'} -> Map.lookup (annSpan annExp') (dsResolved doc)
                  _ -> Nothing
              mResolvedSig =
                case findExpAt pos (dsStmts doc) of
                  Just Var{annExp = annExp'} -> Map.lookup (annSpan annExp') (dsResolvedSigs doc)
                  _ -> Nothing
              keys =
                case mResolved of
                  Just resolved -> [resolved]
                  Nothing -> dedupeIdents (ident : map fst candidates)
              tcLoc = case mResolvedSig of
                Just sig -> defLocationFromSig sig (dsTC doc)
                Nothing ->
                  case mResolved of
                    Just resolved -> defLocationFromTC resolved (dsTC doc)
                    Nothing -> Nothing
          case tcLoc of
            Just loc -> respond (Right (InL (Definition (InR [loc]))))
            Nothing -> do
              case lookupDefRange keys (dsDefSpans doc) of
                Just range -> do
                  let loc = Location uri range
                  respond (Right (InL (Definition (InR [loc]))))
                Nothing -> do
                  case lookupDefLocPreferExternal uri keys (lsDefIndex st) of
                    Just loc -> respond (Right (InL (Definition (InR [loc]))))
                    Nothing -> do
                      let currentDefs = defLocationsForUri uri (dsDefSpans doc)
                      idx <- liftIO (buildDefinitionIndex st uri currentDefs)
                      withState $ \s -> return (s { lsDefIndex = idx }, ())
                      case lookupDefLocPreferExternal uri keys idx of
                        Nothing -> respond (Right (InL (Definition (InR []))))
                        Just loc -> respond (Right (InL (Definition (InR [loc]))))

onCompletion :: TRequestMessage 'Method_TextDocumentCompletion -> (Either ResponseError (MessageResult 'Method_TextDocumentCompletion) -> LspM Config ()) -> LspM Config ()
onCompletion req respond = do
  st <- readState
  let uri = req ^. L.params . L.textDocument . L.uri
  case Map.lookup uri (lsDocs st) of
    Nothing -> respond (Right (InL []))
    Just doc -> do
      let pst = dsParser doc
          ctxIdents = parserCtx pst
          typeNames = map fst (parserTyCons pst) ++ parserPrimTypes pst
          funcNames = map fst (tcFuncSigs (dsTC doc))
          candidates = Set.toList (Set.fromList (ctxIdents ++ typeNames ++ funcNames))
          items = map completionItem candidates
      respond (Right (InL items))

onFormatting :: TRequestMessage 'Method_TextDocumentFormatting -> (Either ResponseError (MessageResult 'Method_TextDocumentFormatting) -> LspM Config ()) -> LspM Config ()
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
          let endPos = posFromText (dsText doc) (T.length (dsText doc))
              range = Range (Position 0 0) endPos
          respond (Right (InL [TextEdit range formatted]))

changeText :: TextDocumentContentChangeEvent -> Text
changeText (TextDocumentContentChangeEvent change) =
  case change of
    InL rec -> rec .! #text
    InR rec -> rec .! #text

-- | Parse/typecheck and publish diagnostics.
processDocument :: Uri -> Text -> Bool -> LspM Config ()
processDocument uri text publish = do
  st <- readState
  (doc, diags) <- liftIO (analyzeDocument st uri text)
  when publish $ publishDiagnostics 100 (toNormalizedUri uri) Nothing (partitionBySource diags)
  withState $ \s ->
    let defsForDoc = defLocationsForUri uri (dsDefSpans doc)
        merged = Map.union (lsDefIndex s) defsForDoc
    in return (s { lsDocs = Map.insert uri doc (lsDocs s)
                 , lsDefIndex = merged
                 }, ())

analyzeDocument :: LspState -> Uri -> Text -> IO (DocState, [Diagnostic])
analyzeDocument st uri text = do
  mCached <- loadCachedDoc st uri text
  case mCached of
    Just (pstCached, tcCached, stmts) -> do
      let defSpans = defSpansFromParser (lsBaseParser st) stmts pstCached
          -- NOTE: Span keys do not carry file paths, so cached resolved maps
          -- can contain entries from other modules that collide on (line,col).
          -- We filter to spans that appear in the current document to keep
          -- go-to-definition stable and overload-specific.
          docSpans = docSpanSet stmts
          resolved = Map.fromList (filterResolved docSpans (tcResolvedNames tcCached))
          resolvedSigs = Map.fromList (filterResolved docSpans (tcResolvedSigs tcCached))
          doc = DocState text pstCached tcCached stmts [] defSpans resolved resolvedSigs
      return (doc, [])
    Nothing -> do
      let basePst = lsBaseParser st
          baseTC = lsBaseTC st
      parseRes <- parseFromFile basePst text
      case parseRes of
        Left err -> do
          let diag = parseErrorToDiagnostic text err
              doc = DocState text basePst baseTC [] [diag] Map.empty Map.empty Map.empty
          return (doc, [diag])
        Right (stmts, pst') -> do
          let defSpans = defSpansFromParser (lsBaseParser st) stmts pst'
          declRes <- runTCM (registerForwardDecls stmts) baseTC
          case declRes of
            Left tcErr -> do
              diag <- tcErrorToDiagnostic st text tcErr
              let doc = DocState text pst' baseTC stmts [diag] defSpans Map.empty Map.empty
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
              let docSpans = docSpanSet stmts
                  resolved = Map.fromList (filterResolved docSpans (tcResolvedNames tcStFinal))
                  resolvedSigs = Map.fromList (filterResolved docSpans (tcResolvedSigs tcStFinal))
                  doc = DocState text pst' tcStFinal stmts diags defSpans resolved resolvedSigs
              return (doc, diags)

loadCachedDoc :: LspState -> Uri -> Text -> IO (Maybe (ParserState, TCState, [Stmt Ann]))
loadCachedDoc st uri text =
  case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> do
      absPath <- canonicalizePath path
      exists <- doesFileExist absPath
      if not exists
        then return Nothing
        else do
          diskText <- TIO.readFile absPath
          if diskText /= text
            then return Nothing
            else do
              let cachePath = cacheFilePath absPath
              mCached <- loadCachedModule cachePath
              return (fmap cachedDocFrom mCached)
  where
    cachedDocFrom cached =
      let pstCached = fromCachedParserState (lsFsm st) (lsUpsCache st) (lsDownsCache st) (cachedParser cached)
          tcCached = fromCachedTCState (cachedTC cached)
          stmts = cachedTypedStmts cached
      in (pstCached, tcCached, stmts)

-- | Type-check statements without evaluation.
typecheckStmts :: LspState -> Text -> TCState -> [Stmt Ann] -> IO (TCState, [Diagnostic])
typecheckStmts st source tcSt stmts = do
  -- NOTE: The LSP server should stay useful even when a file violates
  -- effect restrictions (e.g. calling a gerund function from a pure
  -- context). For editor features like "go to definition" we prefer
  -- a best-effort typed state over early failure. We therefore
  -- *continuously* clear the gerund table during LSP-only typechecking
  -- so `rejectReadEffect` never aborts the pass and we can still collect
  -- resolved symbols and overload information.
  let tcStLsp = tcSt { tcGerunds = [] }
  let go acc stt =
        case acc of
          Left diags -> return (Left diags)
          Right current ->
            do
              -- Reset gerunds before each statement to avoid effect
              -- rejection inside that statement (previous definitions
              -- may have populated tcGerunds).
              let currentLsp = current { tcGerunds = [] }
              res <- runTCM (tcStmt stt) currentLsp
              case res of
                Left tcErr -> do
                  diag <- tcErrorToDiagnostic st source tcErr
                  return (Left [diag])
                Right (_, next) ->
                  -- Clear again after the statement so later statements
                  -- also run without effect rejections.
                  return (Right next { tcGerunds = [] })
  res <- foldl' (\ioAcc stt -> ioAcc >>= \acc -> go acc stt) (return (Right tcStLsp)) stmts
  case res of
    Left diags -> return (tcStLsp, diags)
    Right final -> return (final, [])

-- | Write cache for a document.
writeCacheForDoc :: LspState -> Uri -> DocState -> IO ()
writeCacheForDoc st uri doc = do
  case uriToFilePath uri of
    Nothing -> return ()
    Just path -> do
      absPath <- canonicalizePath path
      let cachePath = cacheFilePath absPath
      mCompilerHash <- getCompilerHash
      case mCompilerHash of
        Nothing -> return ()
        Just compilerHash -> do
          mSourceMeta <- getFileMeta absPath
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
                , cachedParser = toCachedParserState (dsParser doc)
                , cachedTC = toCachedTCState (dsTC doc)
                , cachedEval = toCachedEvalState emptyEvalState
                }
          saveCachedModule cachePath cachedModule

-- | Diagnostics helpers
parseErrorToDiagnostic :: Text -> ParseErrorBundle Text ParserError -> Diagnostic
parseErrorToDiagnostic source bundle =
  let (ParseErrorBundle errs _posState) = bundle
      err = case errs of
        e :| _ -> e
      (line, col) = offsetToPos (T.take (errorOffset err) source)
      pos = Position line col
      range = Range pos pos
  in Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "kip") (T.pack (show err)) Nothing Nothing Nothing

tcErrorToDiagnostic :: LspState -> Text -> TCError -> IO Diagnostic
tcErrorToDiagnostic st source tcErr = do
  let ctx = RenderCtx LangEn (lsCache st) (lsFsm st) (lsUpsCache st) (lsDownsCache st)
  msg <- runReaderT (renderTCError [] [] tcErr) ctx
  let range = case tcErrSpan tcErr of
        Nothing -> Range (Position 0 0) (Position 0 0)
        Just sp -> spanToRange sp
  return (Diagnostic range (Just DiagnosticSeverity_Error) Nothing Nothing (Just "kip") msg Nothing Nothing Nothing)

-- | Range helpers
posToLsp :: SourcePos -> Position
posToLsp (SourcePos _ line col) =
  let l = max 0 (unPos line - 1)
      c = max 0 (unPos col - 1)
  in Position (fromIntegral l) (fromIntegral c)

spanToRange :: Span -> Range
spanToRange NoSpan = Range (Position 0 0) (Position 0 0)
spanToRange (Span s e) = Range (posToLsp s) (posToLsp e)

-- | Collect all expression spans from a document's statements.
-- We use this to filter resolved-name/signature maps because 'Span'
-- does not encode file paths, and collisions across modules are common.
docSpanSet :: [Stmt Ann] -> Set.Set Span
docSpanSet = foldl' Set.union Set.empty . map stmtSpans
  where
    stmtSpans stt =
      case stt of
        Defn _ _ e -> expSpans e
        Function _ _ _ clauses _ -> foldl' Set.union Set.empty (map clauseSpans clauses)
        ExpStmt e -> expSpans e
        _ -> Set.empty
    clauseSpans (Clause _ body) = expSpans body
    expSpans e =
      let here = Set.singleton (annSpan (annExp e))
          children =
            case e of
              App _ f args -> foldl' Set.union Set.empty (map expSpans (f:args))
              Bind _ _ b -> expSpans b
              Seq _ a b -> expSpans a `Set.union` expSpans b
              Match _ scr clauses ->
                expSpans scr `Set.union` foldl' Set.union Set.empty (map clauseSpans clauses)
              Let _ _ body -> expSpans body
              _ -> Set.empty
      in here `Set.union` children

-- | Keep only resolved entries that belong to the current document.
filterResolved :: Set.Set Span -> [(Span, a)] -> [(Span, a)]
filterResolved allowed = filter (\(sp, _) -> Set.member sp allowed)

-- | Find expression at a position.
findExpAt :: Position -> [Stmt Ann] -> Maybe (Exp Ann)
findExpAt pos = foldl' (<|>) Nothing . map (stmtExpAt pos)
  where
    stmtExpAt :: Position -> Stmt Ann -> Maybe (Exp Ann)
    stmtExpAt p stt =
      case stt of
        Defn _ _ e -> expAt p e
        Function _ _ _ clauses _ -> foldl' (<|>) Nothing (map (clauseExpAt p) clauses)
        ExpStmt e -> expAt p e
        _ -> Nothing
    clauseExpAt p (Clause _ body) = expAt p body

expAt :: Position -> Exp Ann -> Maybe (Exp Ann)
expAt p e =
  let inside = posInSpan p (annSpan (annExp e))
      sub = case e of
        App _ f args -> foldl' (<|>) Nothing (map (expAt p) (f:args))
        Bind _ _ b -> expAt p b
        Seq _ a b -> expAt p a <|> expAt p b
        Match _ scr clauses -> expAt p scr <|> foldl' (<|>) Nothing (map (\(Clause _ body) -> expAt p body) clauses)
        Let _ _ body -> expAt p body
        _ -> Nothing
  in if inside then sub <|> Just e else sub

findVarAt :: Position -> [Stmt Ann] -> Maybe (Identifier, [(Identifier, Case)])
findVarAt pos stmts =
  let mExp = findExpAt pos stmts
  in case mExp of
       Just Var{varName = name, varCandidates = candidates} -> Just (name, candidates)
       _ -> Nothing

lookupDefRange :: [Identifier] -> Map.Map Identifier Range -> Maybe Range
lookupDefRange keys m =
  listToMaybe (mapMaybe (`Map.lookup` m) keys)

lookupDefLoc :: [Identifier] -> Map.Map Identifier Location -> Maybe Location
lookupDefLoc keys m =
  listToMaybe (mapMaybe (`Map.lookup` m) keys)

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

dedupeIdents :: [Identifier] -> [Identifier]
dedupeIdents =
  nub

posInSpan :: Position -> Span -> Bool
posInSpan _ NoSpan = False
posInSpan (Position l c) (Span s e) =
  let sl = fromIntegral (unPos (sourceLine s) - 1)
      sc = fromIntegral (unPos (sourceColumn s) - 1)
      el = fromIntegral (unPos (sourceLine e) - 1)
      ec = fromIntegral (unPos (sourceColumn e) - 1)
  in (l > sl || (l == sl && c >= sc)) && (l < el || (l == el && c <= ec))

-- | Build a definition map directly from parser spans.
defSpansFromParser :: ParserState -> [Stmt Ann] -> ParserState -> Map.Map Identifier Range
defSpansFromParser base stmts pst =
  Map.map spanToRange (defSpansFromParserRaw base stmts pst)

defSpansFromParserRaw :: ParserState -> [Stmt Ann] -> ParserState -> Map.Map Identifier Span
defSpansFromParserRaw base stmts pst =
  let baseDefs = latestDefSpans (parserDefSpans base)
      allowed = Set.fromList (stmtNames stmts)
      localDefs =
        Map.filterWithKey
          (\ident sp ->
            Set.member ident allowed && Map.lookup ident baseDefs /= Just sp)
          (latestDefSpans (parserDefSpans pst))
  in localDefs
  where
    stmtNames stts = concatMap stmtNames' stts
    stmtNames' stt =
      case stt of
        Defn name _ _ -> [name]
        Function name _ _ _ _ -> [name]
        PrimFunc name _ _ _ -> [name]
        NewType name _ ctors -> name : map (fst . fst) ctors
        PrimType name -> [name]
        _ -> []

-- | Collect per-identifier definition span *lists* for the current document only.
-- We must exclude spans inherited from the base parser (prelude), otherwise
-- overloads in the current file may be mapped to stdlib definitions.
defSpanListsFromParser :: ParserState -> [Stmt Ann] -> ParserState -> Map.Map Identifier [Span]
defSpanListsFromParser base stmts pst =
  let baseLists = parserDefSpans base
      allowed = Set.fromList (stmtNames stmts)
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
  where
    stmtNames stts = concatMap stmtNames' stts
    stmtNames' stt =
      case stt of
        Defn name _ _ -> [name]
        Function name _ _ _ _ -> [name]
        PrimFunc name _ _ _ -> [name]
        NewType name _ ctors -> name : map (fst . fst) ctors
        PrimType name -> [name]
        _ -> []

latestDefSpans :: Map.Map Identifier [Span] -> Map.Map Identifier Span
latestDefSpans =
  Map.mapMaybe (\spans -> case reverse spans of
    sp:_ -> Just sp
    [] -> Nothing)

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

defLocationsForUri :: Uri -> Map.Map Identifier Range -> Map.Map Identifier Location
defLocationsForUri uri =
  Map.map (Location uri)

defLocationFromTC :: Identifier -> TCState -> Maybe Location
defLocationFromTC ident tcSt =
  case Map.lookup ident (tcDefLocations tcSt) of
    Just (path, sp) -> Just (Location (filePathToUri path) (spanToRange sp))
    Nothing -> Nothing

defLocationFromSig :: (Identifier, [Ty Ann]) -> TCState -> Maybe Location
defLocationFromSig sig tcSt =
  case Map.lookup sig (tcFuncSigLocs tcSt) of
    Just (path, sp) -> Just (Location (filePathToUri path) (spanToRange sp))
    Nothing -> Nothing

-- | Build a definition index for the workspace and standard library.
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

resolveIndexRoots :: LspState -> Uri -> IO ([FilePath], Maybe FilePath)
resolveIndexRoots st uri = do
  mRoot <- case uriToFilePath uri of
    Nothing -> return Nothing
    Just path -> findProjectRoot path
  let roots = lsModuleDirs st ++ maybeToList mRoot
  return (nub roots, mRoot)

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

loadDefsForFile :: LspState -> FilePath -> IO (Map.Map Identifier Location)
loadDefsForFile st path = do
  absPath <- canonicalizePath path
  let cachePath = cacheFilePath absPath
  mCached <- loadCachedModule cachePath
  case mCached of
    Just cached -> do
      let pst = fromCachedParserState (lsFsm st) (lsUpsCache st) (lsDownsCache st) (cachedParser cached)
          stmts = cachedStmts cached
      return (defLocationsForUri (filePathToUri absPath) (defSpansFromParser (lsBaseParser st) stmts pst))
    Nothing -> do
      src <- TIO.readFile absPath
      parseRes <- parseFromFile (lsBaseParser st) src
      case parseRes of
        Left _ -> return Map.empty
        Right (stmts, pst) ->
          return (defLocationsForUri (filePathToUri absPath) (defSpansFromParser (lsBaseParser st) stmts pst))

preferByScore :: Int -> Location -> Location -> Location
preferByScore newScore newLoc oldLoc =
  let oldScore = locationScore oldLoc
  in if newScore < oldScore then newLoc else oldLoc

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

locationScore :: Location -> Int
locationScore (Location uri _) =
  case uriToFilePath uri of
    Nothing -> 3
    Just path -> if "/tests/" `T.isInfixOf` T.pack path then 2 else 1


offsetToPos :: Text -> (UInt, UInt)
offsetToPos prefix =
  let ls = T.lines prefix
      line = max 0 (fromIntegral (length ls) - 1)
      col = fromIntegral (T.length (if null ls then "" else last ls))
  in (line, col)

-- | Format document: trim trailing whitespace and ensure trailing newline.
formatText :: Text -> Text
formatText txt =
  let trimmed = T.unlines (map T.stripEnd (T.lines txt))
  in if T.null trimmed || T.last trimmed == '\n' then trimmed else trimmed <> "\n"

posFromText :: Text -> Int -> Position
posFromText txt _ =
  let ls = T.lines txt
  in Position (fromIntegral (max 0 (length ls - 1))) (fromIntegral (T.length (if null ls then "" else last ls)))

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
