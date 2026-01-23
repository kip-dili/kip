{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DisambiguateRecordFields #-}

-- | Language Server Protocol implementation for Kip.
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Row.Records ((.!))
import System.Directory (canonicalizePath, doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)
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
  }

-- | Per-document cached state.
data DocState = DocState
  { dsText :: !Text
  , dsParser :: !ParserState
  , dsTC :: !TCState
  , dsStmts :: ![Stmt Ann]
  , dsDiagnostics :: ![Diagnostic]
  , dsDefSpans :: Map.Map Identifier Range
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
  [ notificationHandler SMethod_TextDocumentDidOpen onDidOpen
  , notificationHandler SMethod_TextDocumentDidChange onDidChange
  , notificationHandler SMethod_TextDocumentDidSave onDidSave
  , requestHandler SMethod_TextDocumentHover onHover
  , requestHandler SMethod_TextDocumentDefinition onDefinition
  , requestHandler SMethod_TextDocumentCompletion onCompletion
  , requestHandler SMethod_TextDocumentFormatting onFormatting
  ]

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
        Just ident ->
          case Map.lookup ident (dsDefSpans doc) of
            Nothing -> respond (Right (InL (Definition (InR []))))
            Just range -> do
              let loc = Location uri range
              respond (Right (InL (Definition (InR [loc]))))

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
  withState $ \s -> return (s { lsDocs = Map.insert uri doc (lsDocs s) }, ())

analyzeDocument :: LspState -> Uri -> Text -> IO (DocState, [Diagnostic])
analyzeDocument st uri text = do
  mCached <- loadCachedDoc st uri text
  case mCached of
    Just (pstCached, tcCached, stmts) -> do
      let defSpans = buildDefSpans text stmts
          doc = DocState text pstCached tcCached stmts [] defSpans
      return (doc, [])
    Nothing -> do
      let basePst = lsBaseParser st
          baseTC = lsBaseTC st
      parseRes <- parseFromFile basePst text
      case parseRes of
        Left err -> do
          let diag = parseErrorToDiagnostic text err
              doc = DocState text basePst baseTC [] [diag] Map.empty
          return (doc, [diag])
        Right (stmts, pst') -> do
          let defSpans = buildDefSpans text stmts
          declRes <- runTCM (registerForwardDecls stmts) baseTC
          case declRes of
            Left tcErr -> do
              diag <- tcErrorToDiagnostic st text tcErr
              let doc = DocState text pst' baseTC stmts [diag] defSpans
              return (doc, [diag])
            Right (_, tcStWithDecls) -> do
              (tcStFinal, diags) <- typecheckStmts st text tcStWithDecls stmts
              let doc = DocState text pst' tcStFinal stmts diags defSpans
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
  let go acc stt =
        case acc of
          Left diags -> return (Left diags)
          Right current ->
            do
              res <- runTCM (tcStmt stt) current
              case res of
                Left tcErr -> do
                  diag <- tcErrorToDiagnostic st source tcErr
                  return (Left [diag])
                Right (_, next) -> return (Right next)
  res <- foldl' (\ioAcc stt -> ioAcc >>= \acc -> go acc stt) (return (Right tcSt)) stmts
  case res of
    Left diags -> return (tcSt, diags)
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

findVarAt :: Position -> [Stmt Ann] -> Maybe Identifier
findVarAt pos stmts =
  let mExp = findExpAt pos stmts
  in case mExp of
       Just Var{varName = name} -> Just name
       _ -> Nothing

posInSpan :: Position -> Span -> Bool
posInSpan _ NoSpan = False
posInSpan (Position l c) (Span s e) =
  let sl = fromIntegral (unPos (sourceLine s) - 1)
      sc = fromIntegral (unPos (sourceColumn s) - 1)
      el = fromIntegral (unPos (sourceLine e) - 1)
      ec = fromIntegral (unPos (sourceColumn e) - 1)
  in (l > sl || (l == sl && c >= sc)) && (l < el || (l == el && c <= ec))

-- | Build a best-effort definition map by scanning for first occurrences.
buildDefSpans :: Text -> [Stmt Ann] -> Map.Map Identifier Range
buildDefSpans src stmts =
  let names = concatMap stmtNames stmts
      unique = Set.toList (Set.fromList names)
  in Map.fromList (mapMaybe (\ident -> (,) ident <$> findFirstOccurrence src ident) unique)
  where
    stmtNames stt =
      case stt of
        Defn name _ _ -> [name]
        Function name _ _ _ _ -> [name]
        PrimFunc name _ _ _ -> [name]
        NewType name _ ctors -> name : map fst ctors
        PrimType name -> [name]
        _ -> []

findFirstOccurrence :: Text -> Identifier -> Maybe Range
findFirstOccurrence src ident =
  case T.breakOn needle src of
    (_, rest) | T.null rest -> Nothing
    (before, _) ->
      let (line, col) = offsetToPos before
          start = Position line col
          end = Position line (col + fromIntegral (T.length needle))
      in Just (Range start end)
  where
    needle = T.pack (prettyIdent ident)

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
