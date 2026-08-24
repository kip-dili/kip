{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Command-line interface and REPL for Kip.
module Main where

import System.Exit
import System.IO (hFlush, stdout, stderr)
#ifdef HAVE_UNIX_EXIT
import qualified System.Posix.Process as Posix
#endif
import System.Directory (doesFileExist, doesDirectoryExist, getHomeDirectory, createDirectoryIfMissing, getCurrentDirectory)
import Paths_kip (version)
import Data.List
import Options.Applicative hiding (ParseError)
import System.FilePath ((</>), joinPath, takeDirectory, replaceExtension, makeRelative, splitDirectories, normalise)

import Control.Monad (forM, forM_, when, unless, filterM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Exception (AsyncException(UserInterrupt), SomeException, catch, displayException, try)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, readMVar, tryReadMVar)

import System.Console.Haskeline
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe, isJust, maybeToList, listToMaybe, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map.Strict as Map
import Text.Megaparsec (ParseErrorBundle(..), PosState(..), errorBundlePretty)
import Text.Megaparsec.Error (ParseError(..), ErrorFancy(..), ShowErrorComponent(..))
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec.Pos (sourceLine, sourceColumn, unPos)

import Language.Foma
import System.Console.Chalk
import Kip.Parser
import Kip.AST
import Kip.Eval (EvalState, EvalM, EvalError, emptyEvalState, runEvalM, evalExp, evalExpTraced, evalStmt, evalStmtInFile, evalRender, isRuntimeValue)
import qualified Kip.Eval as Eval
import Kip.TypeCheck
import qualified Kip.TypeCheck as TC
import Kip.Render
import Kip.Cache
import Kip.MorphCache (beginMorphTracking, finishMorphTracking)
import Repl.Steps (formatStepsStreaming, setTopCaseNom, shouldSkipInfinitiveSteps, stripStepsCopulaTRmorph)
import Kip.Runner (Lang(..), renderEvalError, locateDataFile, resolveBuildTargets)
import Kip.Codegen.JS (codegenProgram, codegenRuntime, codegenStmtsInProgram, definedJsNames, definedJsNamesInProgram, pruneProgramTaggedStmts)
import Data.Word
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS

import Data.Version (showVersion)

-- | REPL runtime state (parser/type context + evaluator).
data ReplState =
  ReplState
    { replParserState :: ParserState
    , replTCState :: TCState
    , replEvalState :: EvalState
    , replModuleDirs :: [FilePath]
    , replLoaded :: Set FilePath
    , replPreludeLoaded :: Bool
    , replAutoPrelude :: Bool
    , replPreludeFuture :: Maybe (MVar (Either Text (ParserState, TCState, EvalState, Set FilePath)))
    }

replCtx :: ReplState -> Set.Set Identifier
replCtx = parserCtx . replParserState

replCtors :: ReplState -> [Identifier]
replCtors = parserCtors . replParserState

replTyParams :: ReplState -> [Identifier]
replTyParams = parserTyParams . replParserState

replTyCons :: ReplState -> [(Identifier, Int)]
replTyCons = parserTyCons . replParserState

replTyMods :: ReplState -> [(Identifier, [Identifier])]
replTyMods = parserTyMods . replParserState

replPrimTypes :: ReplState -> [Identifier]
replPrimTypes = parserPrimTypes . replParserState

replFuncArities :: ReplState -> Map.Map Identifier (Set.Set Int)
replFuncArities = parserFuncArities . replParserState

-- | Reset invocation-specific parser metadata while retaining REPL context.
replParserFor :: Maybe FilePath -> ReplState -> ParserState
replParserFor path rs =
  (replParserState rs) { parserDefSpans = Map.empty, parserFilePath = path }

-- | Supported CLI modes.
data CliMode
  = ModeRepl
  | ModeTest
  | ModeExec
  | ModeBuild
  | ModeCodegen Text
  deriving (Eq, Show)

-- | Parsed CLI options.
data CliOptions =
  CliOptions
    { optMode :: CliMode
    , optFiles :: [FilePath]
    , optIncludeDirs :: [FilePath]
    , optOutDir :: Maybe FilePath
    , optLang :: Lang
    , optNoPrelude :: Bool
    }

-- | Renderable compiler and REPL messages.
data CompilerMsg
  = MsgHeader Text
  | MsgSeparator Text
  | MsgCtrlC
  | MsgNeedFile
  | MsgNeedFileOrDir
  | MsgTrmorphMissing
  | MsgLibMissing
  | MsgFileNotFound FilePath
  | MsgModuleNotFound [Text] Identifier
  | MsgUnknownCodegenTarget Text
  | MsgParseError (ParseErrorBundle Text ParserError)
  | MsgRunFailed
  | MsgTCError TCError (Maybe Text) [Identifier] [(Identifier, [Identifier])]
  | MsgEvalError EvalError
  | MsgTypeInferFailed
  | MsgTypeOf [(String, Bool)]
  | MsgLoaded Identifier
  | MsgDefnAdded Identifier
  | MsgFuncAdded Identifier [Arg Ann] Bool [Identifier] [(Identifier, [Identifier])]
  | MsgFuncLoaded Identifier [Arg Ann] Bool [Identifier] [(Identifier, [Identifier])]
  | MsgPrimFuncAdded Identifier [Arg Ann] Bool [Identifier] [(Identifier, [Identifier])]
  | MsgTypeAdded Identifier
  | MsgPrimTypeAdded Identifier

-- | Internal renderer-context failures that indicate a programming bug.
data InternalRenderError
  = MissingRenderCacheAndFsm
  | MissingFsmOnly

-- | Rendering context for diagnostics and output.
data RenderCtx =
  RenderCtx
    { rcLang :: Lang
    , rcUseColor :: Bool
    , rcCache :: Maybe RenderCache
    , rcFsm :: Maybe FSM
    }

-- | Typeclass for rendering structured messages.
class Render a where
  render :: a -- ^ Value to render.
         -> RenderM Text -- ^ Rendered text.

-- | Wrapper for parse errors to use the render pipeline.
newtype RenderParseError = RenderParseError (ParseErrorBundle Text ParserError)

-- | Structured type-checking error details.
data RenderTCError =
  RenderTCError
    { rteErr :: TCError
    , rteSource :: Maybe Text
    , rteParamTyCons :: [Identifier]
    , rteTyMods :: [(Identifier, [Identifier])]
    }

-- | App-level Reader context.
type AppM = ReaderT RenderCtx IO
-- | REPL Reader context stacked on InputT.
type ReplM = ReaderT RenderCtx (InputT IO)
-- | Rendering helper context.
type RenderM = ReaderT RenderCtx IO
type TaggedStmt = (FilePath, Stmt Ann)

-- | Turkish parse error wrapper for Megaparsec rendering.
newtype ParserErrorTr = ParserErrorTr ParserError
  deriving (Eq, Ord, Show)

-- | English parse error wrapper for Megaparsec rendering.
newtype ParserErrorEn = ParserErrorEn ParserError
  deriving (Eq, Ord, Show)

instance ShowErrorComponent ParserErrorTr where
  showErrorComponent (ParserErrorTr err) = T.unpack (renderParserErrorTr err)

instance ShowErrorComponent ParserErrorEn where
  showErrorComponent (ParserErrorEn err) = T.unpack (renderParserErrorEn err)

-- | Compute the width of a header box for a title.
headerWidth :: Text -- ^ Header title.
            -> Int -- ^ Box width.
headerWidth title = T.length title + 2

-- | Render a boxed header line.
renderHeader :: Text -- ^ Header title.
             -> Text -- ^ Boxed header.
renderHeader title =
  let width = headerWidth title
      top = T.concat ["┌", T.replicate width "─", "┐"]
      mid = T.concat ["│ ", title, " │"]
      bot = T.concat ["└", T.replicate width "─", "┘"]
  in T.intercalate "\n" [top, mid, bot]

-- | Render a separator line matching a header width.
renderSeparator :: Text -- ^ Header title.
                -> Text -- ^ Separator line.
renderSeparator title =
  let width = headerWidth title + 2
  in T.replicate width "─"

-- | Apply a Chalk color function when color is enabled.
applyColor :: Bool -- ^ Whether to colorize output.
           -> (String -> String) -- ^ Chalk color function.
           -> Text -- ^ Input text.
           -> Text -- ^ Colorized text.
applyColor useColor f s =
  if useColor
    then T.pack (f (T.unpack s))
    else s

-- | Style a definition status line.
renderDefnLine :: Bool -- ^ Whether to colorize output.
               -> Text -- ^ Input text.
               -> Text -- ^ Styled output.
renderDefnLine useColor = applyColor useColor dim

-- | Style a type name in output.
renderTypeText :: Bool -- ^ Whether to colorize output.
               -> Text -- ^ Input text.
               -> Text -- ^ Styled output.
renderTypeText useColor = applyColor useColor blue

-- | Style a type variable name in output.
renderTypeVarText :: Bool -- ^ Whether to colorize output.
                  -> Text -- ^ Input text.
                  -> Text -- ^ Styled output.
renderTypeVarText useColor = applyColor useColor yellow

-- | Colorize type parts, marking variables.
colorizeTyParts :: Bool -- ^ Whether to colorize output.
                -> [(String, Bool)] -- ^ Type parts and variable flags.
                -> Text -- ^ Colorized type text.
colorizeTyParts useColor =
  T.concat
    . map (\(txt, isVar) -> if isVar then renderTypeVarText useColor (T.pack txt) else renderTypeText useColor (T.pack txt))

-- | Render a name in bold when color is enabled.
renderNameBold :: Bool -- ^ Whether to colorize output.
               -> Text -- ^ Input text.
               -> Text -- ^ Styled output.
renderNameBold useColor = applyColor useColor bold

-- | Render an error line in red when color is enabled.
renderError :: Bool -- ^ Whether to colorize output.
            -> Text -- ^ Input text.
            -> Text -- ^ Styled output.
renderError useColor = applyColor useColor red

-- | Render internal context failures in the selected language.
renderInternalRenderError :: Lang -- ^ Language selection.
                          -> InternalRenderError -- ^ Internal failure category.
                          -> Text -- ^ Localized diagnostic text.
renderInternalRenderError lang err =
  case (lang, err) of
    (LangTr, MissingRenderCacheAndFsm) -> "İç hata: render için önbellek ve biçimbirim çözümleyici gerekli."
    (LangEn, MissingRenderCacheAndFsm) -> "Internal error: rendering requires RenderCache and FSM."
    (LangTr, MissingFsmOnly) -> "İç hata: render için biçimbirim çözümleyici gerekli."
    (LangEn, MissingFsmOnly) -> "Internal error: rendering requires FSM."

-- | Emit a message using a concrete render context in IO.
emitMsgIO :: RenderCtx -- ^ Render context.
          -> CompilerMsg -- ^ Message to render.
          -> IO () -- ^ No result.
emitMsgIO ctx msg = do
  rendered <- runReaderT (render msg) ctx
  TIO.putStrLn rendered

-- | Emit a message using a concrete render context in InputT.
emitMsgT :: RenderCtx -- ^ Render context.
         -> CompilerMsg -- ^ Message to render.
         -> InputT IO () -- ^ No result.
emitMsgT ctx msg = do
  rendered <- liftIO (runReaderT (render msg) ctx)
  outputStrLn (T.unpack rendered)

-- | Render a compiler message to text.
renderMsg :: CompilerMsg -- ^ Message to render.
          -> RenderM Text -- ^ Rendered text.
renderMsg = render

-- | Emit a message from the AppM context.
emitMsgIOCtx :: CompilerMsg -- ^ Message to render.
             -> AppM () -- ^ No result.
emitMsgIOCtx msg = do
  rendered <- render msg
  liftIO (TIO.putStrLn rendered)

-- | Emit a message from the REPL context.
emitMsgTCtx :: CompilerMsg -- ^ Message to render.
            -> ReplM () -- ^ No result.
emitMsgTCtx msg = do
  rendered <- runApp (render msg)
  lift (outputStrLn (T.unpack rendered))

-- | Run an AppM action inside the REPL context.
runApp :: AppM a -- ^ App computation.
       -> ReplM a -- ^ Lifted REPL computation.
runApp action = do
  ctx <- ask
  liftIO (runReaderT action ctx)

renderCompilerMsgBasicOrDie :: CompilerMsg -- ^ Message to render.
                            -> RenderM Text -- ^ Rendered text.
renderCompilerMsgBasicOrDie msg = do
  ctx <- ask
  mBasic <- renderCompilerMsgBasic msg
  case mBasic of
    Just rendered -> return rendered
    Nothing ->
      return $
        case rcLang ctx of
          LangTr -> "Beklenmeyen hata."
          LangEn -> "Unexpected error."

-- | Render a span into human-readable text.
renderSpan :: Lang -- ^ Language selection.
           -> Span -- ^ Source span.
           -> Text -- ^ Rendered span.
renderSpan lang sp =
  case sp of
    NoSpan -> ""
    Span start end path ->
      case path
        of 
          Nothing ->
            (case lang of
              LangTr ->
                T.concat
                  [ " (satır "
                  , T.pack (show (unPos (sourceLine start)))
                  , ", sütun "
                  , T.pack (show (unPos (sourceColumn start)))
                  , " - satır "
                  , T.pack (show (unPos (sourceLine end)))
                  , ", sütun "
                  , T.pack (show (unPos (sourceColumn end)))
                  , ")"
                  ]
              LangEn ->
                T.concat
                  [ " (line "
                  , T.pack (show (unPos (sourceLine start)))
                  , ", column "
                  , T.pack (show (unPos (sourceColumn start)))
                  , " - line "
                  , T.pack (show (unPos (sourceLine end)))
                  , ", column "
                  , T.pack (show (unPos (sourceColumn end)))
                  , ")"
                  ])
          Just p ->
             "\n" <> T.pack p <>":" <> T.pack (show (unPos (sourceLine start))) <> ":" <> T.pack (show (unPos (sourceColumn start))) <> "-"
            <> T.pack (show (unPos (sourceLine end))) <> ":" <> T.pack (show (unPos (sourceColumn end)))

-- | Check whether a return type annotation was written explicitly.
isExplicitRetTy :: Ty Ann -- ^ Return type annotation.
                -> Bool -- ^ True when return type is explicit.
isExplicitRetTy ty =
  annSpan (annTy ty) /= NoSpan

-- | Render a type checker error without source context.
renderTCError :: [Identifier] -- ^ Type parameters for rendering.
              -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
              -> TCError -- ^ Type checker error.
              -> RenderM Text -- ^ Rendered error text.
renderTCError paramTyCons tyMods tcErr = do
  ctx <- ask
  case rcLang ctx of
    LangTr ->
      case tcErr of
        TC.Unknown ->
          return "Tip hatası: bilinmeyen hata."
        NoType sp ->
          return ("Tip hatası: uygun bir tip bulunamadı (beklenen ve bulunan tipler uyuşmuyor olabilir)." <> renderSpan (rcLang ctx) sp)
        EffectfulExprInPureCtx _ sp ->
          return (effectBoundaryHint LangTr <> renderSpan (rcLang ctx) sp)
        Ambiguity sp ->
          return ("Tip hatası: ifade belirsiz." <> renderSpan (rcLang ctx) sp)
        UnknownName name sp ->
          return ("Tip hatası: " <> T.pack (prettyIdent name) <> " tanınmıyor." <> renderSpan (rcLang ctx) sp)
        NoMatchingOverload name argTys sigs sp -> do
          argStrs <- mapM (renderTyOpt paramTyCons tyMods) argTys
          (cache, fsm) <- requireCacheFsm
          nameStr <- liftIO (renderIdentWithCase cache fsm name Gen)
          sigStrs <- liftIO (mapM (renderSigText cache fsm paramTyCons tyMods) sigs)
          let baseName = T.pack (prettyIdent name)
              nameStr' =
                if T.isSuffixOf "ne" baseName && T.isSuffixOf "nin" (T.pack nameStr)
                  then T.dropEnd 3 (T.pack nameStr) <> "'n"
                  else T.pack nameStr
              header =
                "Tip hatası: " <> nameStr' <> " için uygun bir tanım bulunamadı." <> renderSpan (rcLang ctx) sp
              argsLine = "Argüman tipleri: " <> T.intercalate ", " argStrs
              sigLines =
                case sigStrs of
                  [] -> []
                  _ -> (nameStr' <> " için verili tanımlar:") : map ("- " <>) sigStrs
          return (T.intercalate "\n" (header : argsLine : sigLines))
        NoMatchingCtor name argTys tys sp -> do
          argStrs <- mapM (renderTyOpt paramTyCons tyMods) argTys
          (cache, fsm) <- requireCacheFsm
          nameStr <- liftIO (renderIdentWithCase cache fsm name Nom)
          expStrs <- liftIO (mapM (renderTyText cache fsm paramTyCons tyMods) tys)
          let header =
                "Tip hatası: " <> T.pack nameStr <> " için uygun bir örnek bulunamadı." <> renderSpan (rcLang ctx) sp
              argsLine = "Argüman tipleri: " <> T.intercalate ", " argStrs
              expLine = "Beklenen tipler: " <> T.intercalate ", " expStrs
          return (T.intercalate "\n" [header, argsLine, expLine])
        PatternTypeMismatch ctor expectedTy actualTy availableCtors _ -> do
          (cache, fsm) <- requireCacheFsm
          expStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods expectedTy)
          actStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods actualTy)
          availableStrs <- liftIO (mapM (\ident -> renderIdentWithCase cache fsm ident Nom) availableCtors)
          let availableLine =
                [ "Örüntülerin tipi " <> actStr <> " olmalı; bu yüzden yalnızca "
                    <> T.intercalate ", " (map T.pack availableStrs)
                    <> " yapkılarını kullanabilirsiniz."
                | not (null availableStrs)
                ]
          let header =
                if ctor == ([], T.pack "ascribe")
                  then "Tip ataması uyuşmuyor: beklenen tip " <> expStr <> ", bulunan tip " <> actStr
                  else T.pack (prettyIdent ctor) <> " yapkısı " <> expStr <> " tipindendir, ancak burada " <> actStr <> " bekleniyor"
          return (T.intercalate "\n" (header : availableLine))
        ArgTypeMismatch expectedTy actualTy _ -> do
          (cache, fsm) <- requireCacheFsm
          expStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods expectedTy)
          actStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods actualTy)
          return ("Argüman tipi uyuşmuyor: beklenen tip " <> expStr <> ", bulunan tip " <> actStr)
        NonExhaustivePattern _ pats sp -> do
          missing <- renderMissingPatterns LangTr pats
          let header = "Tip hatası: örüntü eksik." <> renderSpan (rcLang ctx) sp
          return (T.intercalate "\n" [header, missing])
        UnimplementedPrimitive name _ sp ->
          return ("Tip hatası: " <> T.pack (prettyIdent name) <> " için yerleşik fonksiyon uygulanmamış." <> renderSpan (rcLang ctx) sp)
        InvalidReturnCase cas sp ->
          let caseTr = case cas of
                Acc  -> "belirtme"
                Dat  -> "yönelme"
                Loc  -> "bulunma"
                Abl  -> "ayrılma"
                Gen  -> "tamlayan"
                Ins  -> "vasıta"
                Cond -> "şart"
                _    -> T.pack (show cas)
          in return ("Tip hatası: dönüş tipi yalın ya da iyelik halinde olmalı. " <> caseTr <> " hali geçersiz." <> renderSpan (rcLang ctx) sp)
    LangEn ->
      case tcErr of
        TC.Unknown ->
          return "Type error: unknown error."
        NoType sp ->
          return ("Type error: no suitable type found (expected and actual types may not match)." <> renderSpan (rcLang ctx) sp)
        EffectfulExprInPureCtx _ sp ->
          return (effectBoundaryHint LangEn <> renderSpan (rcLang ctx) sp)
        Ambiguity sp ->
          return ("Type error: expression is ambiguous." <> renderSpan (rcLang ctx) sp)
        UnknownName name sp ->
          return ("Type error: " <> T.pack (prettyIdent name) <> " is not recognized." <> renderSpan (rcLang ctx) sp)
        NoMatchingOverload name argTys sigs sp -> do
          argStrs <- mapM (renderTyOpt paramTyCons tyMods) argTys
          (cache, fsm) <- requireCacheFsm
          sigStrs <- liftIO (mapM (renderSigText cache fsm paramTyCons tyMods) sigs)
          let header =
                "Type error: no matching definition for " <> T.pack (prettyIdent name) <> "." <> renderSpan (rcLang ctx) sp
              argsLine = "Argument types: " <> T.intercalate ", " argStrs
              sigLines =
                case sigStrs of
                  [] -> []
                  _ -> ("Available definitions for " <> T.pack (prettyIdent name) <> ":") : map ("- " <>) sigStrs
          return (T.intercalate "\n" (header : argsLine : sigLines))
        NoMatchingCtor name argTys tys sp -> do
          argStrs <- mapM (renderTyOpt paramTyCons tyMods) argTys
          (cache, fsm) <- requireCacheFsm
          nameStr <- liftIO (renderIdentWithCase cache fsm name Nom)
          expStrs <- liftIO (mapM (renderTyText cache fsm paramTyCons tyMods) tys)
          let header =
                "Type error: no matching constructor for " <> T.pack nameStr <> "." <> renderSpan (rcLang ctx) sp
              argsLine = "Argument types: " <> T.intercalate ", " argStrs
              expLine = "Expected types: " <> T.intercalate ", " expStrs
          return (T.intercalate "\n" [header, argsLine, expLine])
        PatternTypeMismatch ctor expectedTy actualTy availableCtors _ -> do
          (cache, fsm) <- requireCacheFsm
          expStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods expectedTy)
          actStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods actualTy)
          availableStrs <- liftIO (mapM (\ident -> renderIdentWithCase cache fsm ident Nom) availableCtors)
          let availableLine =
                [ "Since the patterns are expected to have type " <> actStr
                    <> ", you can only use the "
                    <> T.intercalate ", " (map T.pack availableStrs)
                    <> " constructors."
                | not (null availableStrs)
                ]
          let header =
                if ctor == ([], T.pack "ascribe")
                  then "Type ascription mismatch: expected " <> expStr <> ", found " <> actStr
                  else T.pack (prettyIdent ctor) <> " constructor has type " <> expStr <> ", but " <> actStr <> " is expected here"
          return (T.intercalate "\n" (header : availableLine))
        ArgTypeMismatch expectedTy actualTy _ -> do
          (cache, fsm) <- requireCacheFsm
          expStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods expectedTy)
          actStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods actualTy)
          return ("Argument type mismatch: expected " <> expStr <> ", found " <> actStr)
        NonExhaustivePattern _ pats sp -> do
          missing <- renderMissingPatterns LangEn pats
          let header = "Type error: non-exhaustive pattern match." <> renderSpan (rcLang ctx) sp
          return (T.intercalate "\n" [header, missing])
        UnimplementedPrimitive name _ sp ->
          return ("Type error: unimplemented primitive function for " <> T.pack (prettyIdent name) <> "." <> renderSpan (rcLang ctx) sp)
        InvalidReturnCase cas sp ->
          let caseEn = case cas of
                Acc  -> "accusative"
                Dat  -> "dative"
                Loc  -> "locative"
                Abl  -> "ablative"
                Gen  -> "genitive"
                Ins  -> "instrumental"
                Cond -> "conditional"
                _    -> T.pack (show cas)
          in return ("Type error: return type must be nominative or possessive. Found " <> caseEn <> "." <> renderSpan (rcLang ctx) sp)

-- | Render a type checker error with a source snippet.
renderTCErrorWithSource :: [Identifier] -- ^ Type parameters for rendering.
                        -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                        -> Text -- ^ Source input.
                        -> TCError -- ^ Type checker error.
                        -> RenderM Text -- ^ Rendered error text.
renderTCErrorWithSource paramTyCons tyMods source tcErr = do
  ctx <- ask
  msg <- renderTCError paramTyCons tyMods tcErr
  let withPrimary =
        case tcErrSpan tcErr of
          Nothing -> msg
          Just sp -> msg <> "\n" <> renderSpanSnippet source sp
  case tcErrRelatedSpan tcErr of
    Just relatedSp
      | Just relatedSp /= tcErrSpan tcErr ->
          let relatedHeader =
                case rcLang ctx of
                  LangTr -> "İlgili konum:"
                  LangEn -> "Related location:"
              relatedBody =
                case tcErrSpan tcErr of
                  Just primarySp
                    | sameSpanPath primarySp relatedSp ->
                        "\n" <> renderSpanSnippet source relatedSp
                  _ -> renderSpan (rcLang ctx) relatedSp
          in return (withPrimary <> "\n" <> relatedHeader <> relatedBody)
    _ -> return withPrimary

-- | User-facing guidance for using effectful forms in the right context.
effectBoundaryHint :: Lang -- ^ Message language.
                   -> Text -- ^ Rendered hint.
effectBoundaryHint lang =
  case lang of
    LangTr ->
      T.intercalate
        "\n"
        [ "Tip hatası: bu ifade etkili bağlamda kullanılmalı."
        , "Öneri: yazdırmak için `... yaz.` kullanın."
        , "Öneri: etkili bir sonucu kullanmak için `x için ...yup, ...` biçimini kullanın."
        ]
    LangEn ->
      T.intercalate
        "\n"
        [ "Hint: this expression must be used in an effectful context."
        , "Suggestion: use `... yaz.` for printing."
        , "Suggestion: use `x için ...yup, ...` to bind and continue with an effectful result."
        ]

-- | Extract a span from a type checker error when present.
tcErrSpan :: TCError -- ^ Type checker error.
          -> Maybe Span -- ^ Associated span.
tcErrSpan tcErr =
  case tcErr of
    NoType sp -> Just sp
    EffectfulExprInPureCtx _ sp -> Just sp
    Ambiguity sp -> Just sp
    UnknownName _ sp -> Just sp
    NoMatchingOverload _ _ _ sp -> Just sp
    NoMatchingCtor _ _ _ sp -> Just sp
    PatternTypeMismatch _ _ _ _ sp -> Just sp
    ArgTypeMismatch _ _ sp -> Just sp
    NonExhaustivePattern _ _ sp -> Just sp
    UnimplementedPrimitive _ _ sp -> Just sp
    InvalidReturnCase _ sp -> Just sp
    TC.Unknown -> Nothing

-- | Extract one secondary span for unification-style diagnostics.
--
-- We use the expected type annotation span when available so REPL users can
-- see a distant but relevant location (for example, a declared parameter type
-- that conflicts with the call site).
tcErrRelatedSpan :: TCError -- ^ Type checker error.
                 -> Maybe Span -- ^ Secondary related span.
tcErrRelatedSpan tcErr =
  case tcErr of
    ArgTypeMismatch expectedTy _ _ -> nonNoSpan (annSpan (annTy expectedTy))
    PatternTypeMismatch _ expectedTy _ _ _ -> nonNoSpan (annSpan (annTy expectedTy))
    _ -> Nothing
  where
    nonNoSpan sp =
      case sp of
        NoSpan -> Nothing
        _ -> Just sp

-- | Check whether two spans refer to the same source file path.
sameSpanPath :: Span -- ^ Primary span.
             -> Span -- ^ Related span.
             -> Bool -- ^ True when both spans are in the same file.
sameSpanPath (Span _ _ p1) (Span _ _ p2) = p1 == p2
sameSpanPath _ _ = False

-- | Render missing patterns for error messages.
renderMissingPatterns :: Lang -> [Pat Ann] -> RenderM Text
renderMissingPatterns lang pats = do
  patTexts <- mapM (renderPatText False) pats
  let listed = T.intercalate ", " patTexts
  return $
    case lang of
      LangTr -> "Eksik kalan örüntüler şunlar: " <> listed <> "."
      LangEn -> "The missing patterns are: " <> listed <> "."
  where
    renderPatText :: Bool -- ^ Whether this is an argument position.
                  -> Pat Ann
                  -> RenderM Text
    renderPatText isArg pat = do
      (cache, fsm) <- requireCacheFsm
      let renderIdent cas ident = T.pack <$> liftIO (renderIdentWithCase cache fsm ident cas)
      case pat of
        PWildcard _ -> return "değilse"
        PVar n ann -> renderIdent (annCase ann) n
        PCtor (ctor, _) args -> do
          argTexts <- mapM (renderPatText True) args
          ctorTxt <- renderIdent (if null args then Nom else P3s) ctor
          let txt = T.unwords (argTexts ++ [ctorTxt])
          return $
            if isArg && not (null args)
              then "(" <> txt <> ")"
              else txt

-- | Render a caret snippet for a source span.
renderSpanSnippet :: Text -- ^ Source input.
                  -> Span -- ^ Source span.
                  -> Text -- ^ Rendered snippet.
renderSpanSnippet source sp =
  case sp of
    NoSpan -> ""
    Span start end _ ->
      let ls = V.fromList (T.lines source)
          sLine = unPos (sourceLine start)
          sCol = unPos (sourceColumn start)
          eLine = unPos (sourceLine end)
          eCol = unPos (sourceColumn end)
          getLine n =
            fromMaybe "" (safeIndexVec ls (n - 1))
          caretLine lineText fromCol toCol =
            let len = max 1 (toCol - fromCol)
                prefix = T.replicate (max 0 (fromCol - 1)) " "
                carets = T.replicate len "^"
            in T.concat [lineText, "\n", prefix, carets]
      in if sLine == eLine
           then caretLine (getLine sLine) sCol eCol
           else
             let first = caretLine (getLine sLine) sCol (T.length (getLine sLine) + 1)
                 lastLine = caretLine (getLine eLine) 1 eCol
             in T.concat [first, "\n", lastLine]
  where
    safeIndexVec :: V.Vector Text -> Int -> Maybe Text
    safeIndexVec vec i
      | i < 0 || i >= V.length vec = Nothing
      | otherwise = Just (vec V.! i)

-- | Render a span snippet with Megaparsec-style location and gutter lines.
renderLocatedSpanSnippet :: Text -- ^ Source name.
                         -> Text -- ^ Source input.
                         -> Span -- ^ Source span.
                         -> Text -- ^ Rendered snippet.
renderLocatedSpanSnippet sourceName source sp =
  case sp of
    NoSpan -> ""
    Span start _ _ ->
      let lineNo = T.pack (show (unPos (sourceLine start)))
          colNo = T.pack (show (unPos (sourceColumn start)))
          gutterPad = T.replicate (T.length lineNo) " "
          snippetLines = T.lines (renderSpanSnippet source sp)
      in case snippetLines of
           codeLn:caretLn:_ ->
             T.concat
               [ sourceName, ":", lineNo, ":", colNo, ":\n"
               , gutterPad, " |\n"
               , lineNo, " | ", codeLn, "\n"
               , gutterPad, " | ", caretLn
               ]
           _ -> renderSpanSnippet source sp

-- | Render an optional type for diagnostics.
renderTyOpt :: [Identifier] -- ^ Type parameters for rendering.
            -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
            -> Maybe (Ty Ann) -- ^ Optional type.
            -> RenderM Text -- ^ Rendered type.
renderTyOpt paramTyCons tyMods mty = do
  ctx <- ask
  case mty of
    Nothing ->
      return $
        case rcLang ctx of
          LangTr -> "bilinmiyor"
          LangEn -> "unknown"
    Just ty -> do
      (cache, fsm) <- requireCacheFsm
      liftIO (renderTyText cache fsm paramTyCons tyMods ty)

-- | Require the render cache and FSM from the context.
requireCacheFsm :: RenderM (RenderCache, FSM) -- ^ Render cache and FSM.
requireCacheFsm = do
  ctx <- ask
  case (rcCache ctx, rcFsm ctx) of
    (Just cache, Just fsm) -> return (cache, fsm)
    _ -> liftIO . ioError . userError . T.unpack $
      renderInternalRenderError (rcLang ctx) MissingRenderCacheAndFsm

-- | Require an FSM from the context.
requireFsm :: RenderM FSM -- ^ Morphology FSM.
requireFsm = do
  ctx <- ask
  case rcFsm ctx of
    Just fsm -> return fsm
    Nothing -> liftIO . ioError . userError . T.unpack $
      renderInternalRenderError (rcLang ctx) MissingFsmOnly

-- | Render messages that do not require extra context.
renderCompilerMsgBasic :: CompilerMsg -- ^ Message to render.
                       -> RenderM (Maybe Text) -- ^ Rendered message when supported.
renderCompilerMsgBasic msg = do
  ctx <- ask
  return $
    case msg of
      MsgHeader title ->
        Just (renderHeader title)
      MsgSeparator title ->
        Just (renderSeparator title)
      MsgCtrlC ->
        Just "^C"
      MsgNeedFile ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) "En az bir dosya bekleniyor."
            LangEn -> renderError (rcUseColor ctx) "Expected at least one file."
      MsgNeedFileOrDir ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) "En az bir dosya veya dizin bekleniyor."
            LangEn -> renderError (rcUseColor ctx) "Expected at least one file or directory."
      MsgTrmorphMissing ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) "vendor/trmorph.fst bulunamadı."
            LangEn -> renderError (rcUseColor ctx) "vendor/trmorph.fst not found."
      MsgLibMissing ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) "lib/temel.kip bulunamadı."
            LangEn -> renderError (rcUseColor ctx) "lib/temel.kip not found."
      MsgFileNotFound path ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) ("Dosya bulunamadı: " <> T.pack path)
            LangEn -> renderError (rcUseColor ctx) ("File not found: " <> T.pack path)
      MsgModuleNotFound dirPath name ->
        let prefix = if null dirPath then "" else T.intercalate "/" dirPath <> "/"
        in Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) (prefix <> T.pack (prettyIdent name) <> " modülü bulunamadı.")
            LangEn -> renderError (rcUseColor ctx) ("Module not found: " <> prefix <> T.pack (prettyIdent name))
      MsgUnknownCodegenTarget target ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) ("Bilinmeyen kod üretim hedefi: " <> target)
            LangEn -> renderError (rcUseColor ctx) ("Unknown codegen target: " <> target)
      MsgParseError err ->
        Just (renderError (rcUseColor ctx) (renderParseError (rcLang ctx) err))
      MsgRunFailed ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) "Dosya çalıştırılamadı."
            LangEn -> renderError (rcUseColor ctx) "File could not be executed."
      MsgEvalError evalErr ->
        Just $ renderError (rcUseColor ctx) $ renderEvalError (rcLang ctx) evalErr
      MsgTypeInferFailed ->
        Just $
          case rcLang ctx of
            LangTr -> "Tipi çıkarılamadı."
            LangEn -> "Type could not be inferred."
      MsgTypeOf tyParts ->
        Just $
          case rcLang ctx of
            LangTr -> "İfadenin tipi " <> colorizeTyParts (rcUseColor ctx) tyParts
            LangEn -> "Expression type is " <> colorizeTyParts (rcUseColor ctx) tyParts
      MsgLoaded name ->
        Just $
          case rcLang ctx of
            LangTr -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " yüklendi.")
            LangEn -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " loaded.")
      MsgDefnAdded name ->
        Just $
          case rcLang ctx of
            LangTr -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " tanımlandı.")
            LangEn -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " definition defined.")
      MsgTypeAdded name ->
        Just $
          case rcLang ctx of
            LangTr -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " tipi tanımlandı.")
            LangEn -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " type defined.")
      MsgPrimTypeAdded name ->
        Just $
          case rcLang ctx of
            LangTr -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " tipi tanımlandı.")
            LangEn -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " type defined.")
      MsgTCError {} ->
        Nothing
      MsgFuncAdded {} ->
        Nothing
      MsgFuncLoaded {} ->
        Nothing
      MsgPrimFuncAdded {} ->
        Nothing

instance Render CompilerMsg where
  render msg = do
    ctx <- ask
    mBasic <- renderCompilerMsgBasic msg
    case mBasic of
      Just rendered -> return rendered
      Nothing ->
        case msg of
          MsgTCError tcErr mSource paramTyCons tyMods ->
            render (RenderTCError tcErr mSource paramTyCons tyMods)
          MsgFuncAdded name args isInfinitive paramTyCons tyMods -> do
            (cache, fsm) <- requireCacheFsm
            (sigArgs, sigName) <- liftIO (renderFunctionSignatureParts cache fsm paramTyCons tyMods isInfinitive name args)
            let argStrs =
                  [ T.concat
                      [ "("
                      , T.pack argName
                      , if null argName then "" else " "
                      , colorizeTyParts (rcUseColor ctx) tyParts
                      , ")"
                      ]
                  | (argName, tyParts) <- sigArgs
                  ]
                base = T.intercalate " " (argStrs ++ [renderNameBold (rcUseColor ctx) (T.pack sigName)])
                suffix =
                  case rcLang ctx of
                    LangTr -> " tanımlandı."
                    LangEn -> " defined."
            return (renderDefnLine (rcUseColor ctx) (base <> suffix))
          MsgFuncLoaded name args isInfinitive paramTyCons tyMods -> do
            (cache, fsm) <- requireCacheFsm
            (sigArgs, sigName) <- liftIO (renderFunctionSignatureParts cache fsm paramTyCons tyMods isInfinitive name args)
            let argStrs =
                  [ T.concat
                      [ "("
                      , T.pack argName
                      , if null argName then "" else " "
                      , colorizeTyParts (rcUseColor ctx) tyParts
                      , ")"
                      ]
                  | (argName, tyParts) <- sigArgs
                  ]
                base = T.intercalate " " (argStrs ++ [renderNameBold (rcUseColor ctx) (T.pack sigName)])
                suffix =
                  case rcLang ctx of
                    LangTr -> " yüklendi."
                    LangEn -> " loaded."
            return (renderDefnLine (rcUseColor ctx) (base <> suffix))
          MsgPrimFuncAdded name args isInfinitive paramTyCons tyMods -> do
            (cache, fsm) <- requireCacheFsm
            (sigArgs, sigName) <- liftIO (renderFunctionSignatureParts cache fsm paramTyCons tyMods isInfinitive name args)
            let argStrs =
                  [ T.concat
                      [ "("
                      , T.pack argName
                      , if null argName then "" else " "
                      , colorizeTyParts (rcUseColor ctx) tyParts
                      , ")"
                      ]
                  | (argName, tyParts) <- sigArgs
                  ]
                base = T.intercalate " " (argStrs ++ [renderNameBold (rcUseColor ctx) (T.pack sigName)])
                suffix =
                  case rcLang ctx of
                    LangTr -> " tanımlandı."
                    LangEn -> " defined."
            return (renderDefnLine (rcUseColor ctx) (base <> suffix))
          _ -> return ""

-- | Render instance for parse errors.
instance Render RenderParseError where
  render (RenderParseError err) = do
    ctx <- ask
    return (renderParseError (rcLang ctx) err)

-- | Render instance for type checker errors with optional source.
instance Render RenderTCError where
  render RenderTCError{rteErr, rteSource, rteParamTyCons, rteTyMods} =
    case rteSource of
      Nothing -> renderTCError rteParamTyCons rteTyMods rteErr
      Just source -> renderTCErrorWithSource rteParamTyCons rteTyMods source rteErr

-- | Render a parse error bundle in the requested language.
renderParseError :: Lang -- ^ Language selection.
                 -> ParseErrorBundle Text ParserError -- ^ Parse error bundle.
                 -> Text -- ^ Rendered error text.
renderParseError lang err =
  case findPatternBinderRepeatedError err of
    Just (ident, sp, source) ->
      let header =
            case lang of
              LangTr -> "Sözdizim hatası:\n"
              LangEn -> "Syntax error:\n"
          msg =
            case lang of
              LangTr -> renderParserErrorTr (ErrPatternBinderRepeated ident sp)
              LangEn -> renderParserErrorEn (ErrPatternBinderRepeated ident sp)
      in header <> renderSpanSnippet source sp <> "\n" <> msg
    Nothing ->
      case findUnrecognizedWordError err of
        Just (wordTxt, sp, suggestions, source) ->
          let header =
                case lang of
                  LangTr -> "Sözdizim hatası:\n"
                  LangEn -> "Syntax error:\n"
              msg =
                case lang of
                  LangTr -> renderParserErrorTr (ErrUnrecognizedTurkishWord wordTxt sp suggestions)
                  LangEn -> renderParserErrorEn (ErrUnrecognizedTurkishWord wordTxt sp suggestions)
          in header <> renderSpanSnippet source sp <> "\n" <> msg
        Nothing ->
          case findAmbiguousBareApplicationError err of
            Just (ambErr, sp, source) ->
              let header =
                    case lang of
                      LangTr -> "Sözdizim hatası:\n"
                      LangEn -> "Syntax error:\n"
                  msg =
                    case lang of
                      LangTr -> renderParserErrorTr ambErr
                      LangEn -> renderParserErrorEn ambErr
              in header <> renderLocatedSpanSnippet "Kip" source sp <> "\n" <> msg
            Nothing ->
              case lang of
                LangTr ->
                  let trBundle = mapParseErrorBundle ParserErrorTr err
                  in "Sözdizim hatası:\n" <> T.pack (turkifyParseError (errorBundlePretty trBundle))
                LangEn ->
                  let enBundle = mapParseErrorBundle ParserErrorEn err
                  in "Syntax error:\n" <> T.pack (errorBundlePretty enBundle)

-- | Find the custom repeated-pattern-binder parser error, if present.
findPatternBinderRepeatedError :: ParseErrorBundle Text ParserError -> Maybe (Identifier, Span, Text)
findPatternBinderRepeatedError (ParseErrorBundle errs posState) = do
  (ident, sp) <- listToMaybe (concatMap extract (NE.toList errs))
  return (ident, sp, pstateInput posState)
  where
    extract :: ParseError Text ParserError -> [(Identifier, Span)]
    extract parseErr =
      case parseErr of
        FancyError _ xs ->
          [ (ident, sp)
          | ErrorCustom (ErrPatternBinderRepeated ident sp) <- Set.toList xs
          ]
        _ -> []

-- | Find the custom unrecognized-word parser error, if present.
findUnrecognizedWordError :: ParseErrorBundle Text ParserError -> Maybe (Text, Span, [Text], Text)
findUnrecognizedWordError (ParseErrorBundle errs posState) = do
  (w, sp, suggestions) <- listToMaybe (concatMap extract (NE.toList errs))
  return (w, sp, suggestions, pstateInput posState)
  where
    extract :: ParseError Text ParserError -> [(Text, Span, [Text])]
    extract parseErr =
      case parseErr of
        FancyError _ xs ->
          [ (w, sp, suggestions)
          | ErrorCustom (ErrUnrecognizedTurkishWord w sp suggestions) <- Set.toList xs
          ]
        _ -> []

-- | Find the custom ambiguous bare-application parser error, if present.
findAmbiguousBareApplicationError :: ParseErrorBundle Text ParserError -> Maybe (ParserError, Span, Text)
findAmbiguousBareApplicationError (ParseErrorBundle errs posState) = do
  (errComp, sp) <- listToMaybe (concatMap extract (NE.toList errs))
  return (errComp, sp, pstateInput posState)
  where
    extract :: ParseError Text ParserError -> [(ParserError, Span)]
    extract parseErr =
      case parseErr of
        FancyError _ xs ->
          [ (ErrAmbiguousBareApplication sp, sp)
          | ErrorCustom (ErrAmbiguousBareApplication sp) <- Set.toList xs
          ] <>
          [ (ErrAmbiguousBareApplicationOverload ident arities sp, sp)
          | ErrorCustom (ErrAmbiguousBareApplicationOverload ident arities sp) <- Set.toList xs
          ]
        _ -> []

-- | Map custom error components inside a parse error bundle.
mapParseErrorBundle :: Ord e'
                    => (e -> e') -- ^ Error mapping function.
                    -> ParseErrorBundle s e -- ^ Source bundle.
                    -> ParseErrorBundle s e' -- ^ Mapped bundle.
mapParseErrorBundle f (ParseErrorBundle errs posState) =
  ParseErrorBundle (NE.map (mapParseError f) errs) posState
  where
    mapParseError :: Ord e' => (e -> e') -> ParseError s e -> ParseError s e'
    mapParseError g err =
      case err of
        TrivialError o u e -> TrivialError o u e
        FancyError o xs -> FancyError o (Set.map (mapFancy g) xs)
    mapFancy :: (e -> e') -> ErrorFancy e -> ErrorFancy e'
    mapFancy g fancy =
      case fancy of
        ErrorCustom e -> ErrorCustom (g e)
        ErrorFail msg -> ErrorFail msg
        ErrorIndentation o r lvl -> ErrorIndentation o r lvl

-- | Translate parse error text into Turkish labels.
turkifyParseError :: String -- ^ Raw error text.
                  -> String -- ^ Translated error text.
turkifyParseError =
  replace "unexpected end of input" "beklenmeyen girişin sonu"
  . replace "unexpected" "beklenmeyen"
  . replace "expecting" "bekleniyor"
  . replace "end of input" "girişin sonu"
  . replace "line" "satır"
  . replace "column" "sütun"

-- | Replace all occurrences of a substring.
replace :: String -- ^ Substring to replace.
        -> String -- ^ Replacement substring.
        -> String -- ^ Input string.
        -> String -- ^ Output string.
replace old new = intercalate new . splitOn old

-- | Split a string on a substring.
splitOn :: String -- ^ Separator substring.
        -> String -- ^ Input string.
        -> [String] -- ^ Split components.
splitOn pat s =
  case breakOn pat s of
    Nothing -> [s]
    Just (before, after) -> before : splitOn pat after

-- | Break a string on the first occurrence of a substring.
breakOn :: String -- ^ Separator substring.
        -> String -- ^ Input string.
        -> Maybe (String, String) -- ^ Prefix and suffix when found.
breakOn pat s =
  case findIndex (isPrefixOf pat) (tails s) of
    Nothing -> Nothing
    Just idx ->
      let (before, rest) = splitAt idx s
          after = drop (length pat) rest
      in Just (before, after)

-- | Exit successfully, skipping the RTS's normal shutdown sequence (joining
-- GC worker threads, returning committed memory to the OS) when possible.
--
-- ==== Performance note (Optimization: skip RTS shutdown on CLI exit)
-- @+RTS -s@ showed ~9 ms of EXIT time per invocation for a batch-mode run
-- (near-zero CPU, mostly RTS teardown) that is pure waste for a process
-- about to terminate anyway. All output must be explicitly flushed first
-- since @exitImmediately@ bypasses Haskell's normal handle-closing cleanup.
-- Only safe for one-shot batch CLI modes (@--test@\/@--exec@\/@--codegen@\/
-- @--build@) that have no further work after printing their result; the
-- REPL and LSP must not use this, since it would apply to every REPL
-- iteration otherwise and skip cleanup that later iterations depend on.
fastExitSuccess :: IO a -- ^ Never returns.
fastExitSuccess = do
  hFlush stdout
  hFlush stderr
#ifdef HAVE_UNIX_EXIT
  Posix.exitImmediately ExitSuccess
#endif
  exitSuccess

-- | Entry point for CLI modes and REPL.
main :: IO () -- ^ Program entry point.
main = do
  opts <- execParser (info (cliParser <**> helper) (fullDesc <> progDesc "The compiler and interpreter for the Kip programming language"))
  let lang = optLang opts
      useColor = optMode opts == ModeRepl
      title = T.pack ("Kip " ++ showVersion version)
      showHeader = optMode opts == ModeRepl
      showDefn = optMode opts == ModeRepl || optMode opts == ModeTest
      basicCtx = RenderCtx lang useColor Nothing Nothing
      -- | Initialize runtime resources only for modes that actually execute code.
      --
      -- This intentionally defers TRmorph/FSM + shared morphology/render cache setup
      -- until after mode-specific argument validation, so non-REPL invocations do not
      -- eagerly start runtime machinery they may not need.
      initRuntime :: IO (RenderCtx, [FilePath], RenderCache, FSM)
      initRuntime = do
        trmorphPath <- locateTrmorph lang useColor
        libDir <- locateLibDir lang useColor
        fsm <- fsmReadBinaryFile trmorphPath
        renderCache <- newRenderCache
        moduleDirs <- internModuleRoots (libDir : optIncludeDirs opts)
        let moduleDirs' = uniquePreserve moduleDirs
            renderCtx = RenderCtx lang useColor (Just renderCache) (Just fsm)
        return (renderCtx, moduleDirs', renderCache, fsm)
  case optMode opts of
    ModeTest -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFile) basicCtx
      (renderCtx, moduleDirs, renderCache, fsm) <- initRuntime
      (preludePst, preludeTC, preludeEval, preludeLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm) renderCtx
      _ <- runReaderT (runFiles showDefn showDefn False preludePst preludeTC preludeEval moduleDirs preludeLoaded (optFiles opts)) renderCtx
      fastExitSuccess
    ModeExec -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFile) basicCtx
      (renderCtx, moduleDirs, renderCache, fsm) <- initRuntime
      (preludePst, preludeTC, preludeEval, preludeLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm) renderCtx
      let entryPath:progArgs = optFiles opts
          execEval = preludeEval { Eval.evalArgs = map T.pack (entryPath : progArgs) }
      _ <- runReaderT (runFiles False False False preludePst preludeTC execEval moduleDirs preludeLoaded [entryPath]) renderCtx
      fastExitSuccess
    ModeCodegen target -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFile) basicCtx
      case target of
        "js" -> do
          (renderCtx, moduleDirs, renderCache, fsm) <- initRuntime
          -- Parse and type-check files, collect all statements
          (codegenPst, codegenTC, codegenLoaded) <-
            runReaderT (loadPreludeCodegenState (optNoPrelude opts) moduleDirs renderCache fsm) renderCtx
          (finalTC, taggedStmts) <- runReaderT (codegenFilesTagged codegenPst codegenTC moduleDirs codegenLoaded (optFiles opts)) renderCtx
          entryAbs <- mapM canonicalizePathCached (optFiles opts)
          let entrySet = Set.fromList entryAbs
              prunedTaggedStmts = pruneProgramTaggedStmts (Map.fromList (tcResolvedSigs finalTC)) (`Set.member` entrySet) taggedStmts
          -- Emit JS and print
          let resolvMap = Map.fromList (tcResolvedSigs finalTC)
              allStmts = map snd prunedTaggedStmts
          TIO.putStrLn (codegenProgram resolvMap allStmts)
          fastExitSuccess
        "js-modules" -> do
          (renderCtx, moduleDirs, renderCache, fsm) <- initRuntime
          outDir <- case optOutDir opts of
            Just dir -> return dir
            Nothing -> die "--codegen js-modules requires --outdir <dir>"
          createDirectoryIfMissing True outDir
          outDirAbs <- canonicalizePathCached outDir
          (codegenPst, codegenTC, codegenLoaded) <-
            runReaderT (loadPreludeCodegenState (optNoPrelude opts) moduleDirs renderCache fsm) renderCtx
          (finalTC, taggedStmts) <- runReaderT (codegenFilesTagged codegenPst codegenTC moduleDirs codegenLoaded (optFiles opts)) renderCtx
          let resolvMap = Map.fromList (tcResolvedSigs finalTC)
          cwd <- getCurrentDirectory
          entryAbs <- mapM canonicalizePathCached (optFiles opts)
          let modulePaths = uniquePreserve (map fst taggedStmts)
              allStmts = map snd taggedStmts
              moduleDefs =
                [ (p, definedJsNamesInProgram resolvMap allStmts [s | (p', s) <- taggedStmts, p' == p, not (isLoadStmt s)])
                | p <- modulePaths
                ]
              allDefs = concatMap snd moduleDefs
              runtimeDefs = runtimeGlobalNames
              providerPairs = [(name, p) | (p, names) <- moduleDefs, name <- names]
              (_, providersRev) = foldl' addProvider (Set.empty, []) providerPairs
              providers = reverse providersRev
              addProvider (seen, acc) (name, p)
                | Set.member name seen = (seen, acc)
                | otherwise = (Set.insert name seen, (name, p) : acc)
          TIO.writeFile (outDirAbs </> "__kip_runtime.mjs") codegenRuntime
          forM_ modulePaths $ \modulePath -> do
            let rel = makeRelative cwd modulePath
                moduleOut = outDirAbs </> replaceExtension rel "mjs"
                stmts0 = [stmt | (p, stmt) <- taggedStmts, p == modulePath]
                isEntry = modulePath `elem` entryAbs
                codeStmts0 = filter (not . isLoadStmt) stmts0
                stmts = if isEntry then codeStmts0 else filter (not . isExpStmt) codeStmts0
                localDefs = definedJsNamesInProgram resolvMap allStmts codeStmts0
                importedDefs =
                  [ (n, p)
                  | (n, p) <- providers
                  , p /= modulePath
                  , n `notElem` localDefs
                  , n `notElem` runtimeDefs
                  ]
                runtimeImports = [n | n <- runtimeDefs, n `notElem` localDefs]
                importByModule = foldl' addImport Map.empty importedDefs
                addImport acc (name, p) = Map.insertWith (++) p [name] acc
                runtimeImport =
                  "import { " <> T.intercalate ", " runtimeImports <> " } from './"
                    <> importRelPath moduleOut (outDirAbs </> "__kip_runtime.mjs") <> "';"
                depImports =
                  [ "import { " <> T.intercalate ", " names <> " } from './"
                      <> importRelPath moduleOut (outDirAbs </> replaceExtension (makeRelative cwd p) "mjs")
                      <> "';"
                  | (p, names) <- Map.toList importByModule
                  ]
                body = codegenStmtsInProgram resolvMap allStmts stmts
                exportLine =
                  if null localDefs
                    then ""
                    else "export { " <> T.intercalate ", " localDefs <> " };"
                content = T.unlines (runtimeImport : depImports) <> "\n" <> body <> "\n\n" <> exportLine <> "\n"
            createDirectoryIfMissing True (takeDirectory moduleOut)
            TIO.writeFile moduleOut content
          let importLines =
                "import { __kip_close_stdin } from './__kip_runtime.mjs';"
                : [ "import './" <> toPosixRel outDirAbs cwd p <> "';" | p <- entryAbs ]
              entryContent = T.unlines (importLines ++ ["__kip_close_stdin();"])
          TIO.writeFile (outDirAbs </> "entry.mjs") entryContent
          fastExitSuccess
        _ ->
          die . T.unpack =<< runReaderT (render (MsgUnknownCodegenTarget target)) basicCtx
    ModeBuild -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFileOrDir) basicCtx
      (renderCtx, moduleDirs, renderCache, fsm) <- initRuntime
      buildTargets <- resolveBuildTargets (optFiles opts)
      let extraDirs = uniquePreserve (concatMap takeDirectories buildTargets)
          buildModuleDirs = uniquePreserve (moduleDirs ++ extraDirs)
      (preludeBuildPst, preludeBuildTC, preludeBuildEval, preludeBuildLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) buildModuleDirs renderCache fsm) renderCtx
      _ <- runReaderT (runFiles False False True preludeBuildPst preludeBuildTC preludeBuildEval buildModuleDirs preludeBuildLoaded buildTargets) renderCtx
      fastExitSuccess
    ModeRepl ->
      do
        (renderCtx, moduleDirs, renderCache, fsm) <- initRuntime
        if null (optFiles opts)
        then do
          emitMsgIO renderCtx (MsgHeader title)
          emitMsgIO renderCtx (MsgSeparator title)
          let replAuto = not (optNoPrelude opts)
          preludeFuture <-
            if replAuto
              then Just <$> startPreludeWarmup renderCtx moduleDirs renderCache fsm
              else return Nothing
          let baseRs = emptyReplState moduleDirs renderCache fsm replAuto preludeFuture
          kipSettings >>= \s -> runInputT s (runReaderT (loop baseRs) renderCtx)
        else do
          (preludePst, preludeTC, preludeEval, preludeLoaded) <-
            runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm) renderCtx
          when showHeader $ do
            emitMsgIO renderCtx (MsgHeader title)
            emitMsgIO renderCtx (MsgSeparator title)
          rs <- runReaderT (runFiles showDefn showDefn False preludePst preludeTC preludeEval moduleDirs preludeLoaded (optFiles opts)) renderCtx
          when showHeader $
            emitMsgIO renderCtx (MsgSeparator title)
          kipSettings >>= \s -> runInputT s (runReaderT (loop rs) renderCtx)
  where
    kipSettings :: IO (Settings IO)
    kipSettings = do
      home <- getHomeDirectory
      let dir = home </> ".kip"
      createDirectoryIfMissing True dir
      return defaultSettings { historyFile = Just (dir </> "history.txt") }

    -- | Canonicalize existing module roots once and reuse interned paths.
    --
    -- ==== Performance note (Optimization: module root canonicalization)
    -- Module resolution repeatedly combines search roots with relative module
    -- paths. Canonicalizing roots once avoids repeated canonicalization churn.
    internModuleRoots :: [FilePath] -> IO [FilePath]
    internModuleRoots = mapM (\dir -> do
      isDir <- doesDirectoryExist dir
      if isDir then canonicalizePathCached dir else return dir)

    -- | Construct an empty REPL state and defer prelude loading until needed.
    --
    -- ==== Performance note (Optimization: lazy REPL bootstrap)
    -- Starting the REPL no longer eagerly loads @lib/giriş.kip@ when
    -- @autoPrelude@ is enabled. Instead, we start from empty parser/type/eval
    -- state and load the prelude on the first command that requires language
    -- definitions. This significantly reduces cold-start time for quick
    -- interactive sessions (for example launching and quitting).
    emptyReplState ::
      [FilePath] ->
      RenderCache ->
      FSM ->
      Bool ->
      Maybe (MVar (Either Text (ParserState, TCState, EvalState, Set FilePath))) ->
      ReplState
    emptyReplState moduleDirs cache fsm autoPrelude =
      ReplState
        (newParserStateWithCaches fsm Nothing cache)
        emptyTCState
        (mkEvalState cache fsm)
        moduleDirs
        Set.empty
        (not autoPrelude)
        autoPrelude

    -- | Start prelude loading in the background while REPL waits for input.
    --
    -- ==== Performance note (Optimization: parallel REPL warmup)
    -- REPL startup now overlaps prelude load with user think-time: prompt is
    -- shown immediately, and `giriş.kip` loading runs on a background thread.
    -- Commands that do not require prelude stay non-blocking.
    startPreludeWarmup ::
      RenderCtx ->
      [FilePath] ->
      RenderCache ->
      FSM ->
      IO (MVar (Either Text (ParserState, TCState, EvalState, Set FilePath)))
    startPreludeWarmup renderCtx moduleDirs cache fsm = do
      done <- newEmptyMVar
      _ <- forkIO $ do
        result <- try (runReaderT (loadPreludeState False moduleDirs cache fsm) renderCtx)
          :: IO (Either SomeException (ParserState, TCState, EvalState, Set FilePath))
        putMVar done (either (Left . T.pack . displayException) Right result)
      return done
    -- | CLI option parser.
    cliParser :: Parser CliOptions -- ^ CLI option parser.
    cliParser =
        CliOptions
          <$> modeParser
          <*> many (strArgument (metavar "FILE..."))
          <*> many (strOption (short 'I' <> metavar "DIR" <> help "Additional module directory (used by `temeli yükle` etc.)"))
          <*> optional (strOption (long "outdir" <> metavar "DIR" <> help "Output directory (required for --codegen js-modules)"))
          <*> langParser
          <*> switch (long "no-prelude" <> help "Disable automatic loading of lib/giriş.kip")

    -- | Language option parser.
    langParser :: Parser Lang -- ^ Language option parser.
    langParser =
      option (eitherReader parseLang)
        ( long "lang"
        <> metavar "LANG"
        <> value LangTr
        <> help "Language for diagnostics (tr|en)"
        )
      where
        -- | Parse a language flag.
        parseLang :: String -- ^ Raw language flag.
                  -> Either String Lang -- ^ Parsed language or error.
        parseLang s =
          case s of
            "tr" -> Right LangTr
            "en" -> Right LangEn
            _ -> Left "LANG must be 'tr' or 'en'"

    -- | Mode option parser.
    modeParser :: Parser CliMode -- ^ Mode option parser.
    modeParser =
      flag' ModeExec (long "exec" <> help "Run files and exit (no REPL, no definition logs)")
        <|> flag' ModeTest (long "test" <> help "Test mode: run files without REPL (definition logs on)")
        <|> flag' ModeBuild (long "build" <> help "Build cache files for the given files or directories")
        <|> (ModeCodegen . T.pack <$> strOption
              ( long "codegen"
              <> metavar "TARGET"
              <> help "Codegen target: js (single stdout bundle) or js-modules (ES modules in --outdir)"
              ))
        <|> pure ModeRepl
    -- | Locate the morphology FST data file or exit.
    locateTrmorph :: Lang -- ^ Language selection.
                  -> Bool -- ^ Whether to colorize output.
                  -> IO FilePath -- ^ Path to morphology FST.
    locateTrmorph lang useColor = do
      path <- locateDataFile "vendor/trmorph.fst"
      exists <- doesFileExist path
      if exists
        then return path
        else die . T.unpack =<< runReaderT (renderCompilerMsgBasicOrDie MsgTrmorphMissing) (RenderCtx lang useColor Nothing Nothing)
    -- | Locate the standard library directory or exit.
    locateLibDir :: Lang -- ^ Language selection.
                 -> Bool -- ^ Whether to colorize output.
                 -> IO FilePath -- ^ Library directory.
    locateLibDir lang useColor = do
      path <- locateDataFile "lib/temel.kip"
      exists <- doesFileExist path
      if exists
        then return (takeDirectory path)
        else die . T.unpack =<< runReaderT (renderCompilerMsgBasicOrDie MsgLibMissing) (RenderCtx lang useColor Nothing Nothing)
    -- | REPL input loop.
    loop :: ReplState -- ^ Current REPL state.
         -> ReplM () -- ^ No result.
    loop rs = do
      ctx <- ask
      minput <-
        lift $
          handleInterrupt
            (return (Just ""))
            (getInputLine (T.unpack (applyColor (rcUseColor ctx) blue "Kip> ")))
      case minput of
          Nothing -> return ()
          Just "" -> loop rs
          Just ":çık" -> return ()
          Just ":quit" -> return ()
          Just input ->
            handleInterrupt
              (emitMsgTCtx MsgCtrlC >> loop rs)
              (handleInput rs input)

    -- | Handle a single REPL input line.
    handleInput :: ReplState -- ^ Current REPL state.
                -> String -- ^ Input line.
                -> ReplM () -- ^ No result.
    handleInput rs input
      | input == ":modules" = do
            forM_ (Set.toAscList (replLoaded rs)) $ \path ->
              lift (outputStrLn path)
            loop rs
      | input == ":functions" = do
          rs <- ensurePreludeLoaded rs
          ctx <- ask
          (cache, fsm) <- runApp requireCacheFsm
          let paramTyCons = [name | (name, arity) <- replTyCons rs, arity > 0]
              tcSt = replTCState rs
              allSigs = nubBy (\(n1, a1) (n2, a2) -> n1 == n2 && a1 == a2)
                        [ (name, args)
                        | (name, argsList) <- Map.toList (tcFuncSigs tcSt)
                        , args <- argsList
                        ]
          rendered <- forM allSigs $ \(name, args) -> do
            let isInf = Set.member name (tcInfinitives tcSt)
                mRet = Map.lookup (name, map snd args) (tcFuncSigRets tcSt)
            liftIO (renderReplSig ctx cache fsm paramTyCons (replTyMods rs) isInf name args mRet)
          forM_ (sort rendered) $ \line ->
            lift (outputStrLn (T.unpack line))
          loop rs
      | input == ":types" = do
          rs <- ensurePreludeLoaded rs
          let names = nub [name | ((_, name), _arity) <- replTyCons rs]
          forM_ (sort names) $ \name ->
            lift (outputStrLn (T.unpack name))
          loop rs
      | Just word <- stripPrefix ":name " input = do
          fsm <- runApp requireFsm
          liftIO (ups fsm (T.pack word)) >>= \xs -> lift (mapM_ (outputStrLn . T.unpack) xs)
          loop rs
      | Just word <- stripPrefix ":up " input = do
          fsm <- runApp requireFsm
          liftIO (ups fsm (T.pack word)) >>= \xs -> lift (mapM_ (outputStrLn . T.unpack) xs)
          loop rs
      | Just word <- stripPrefix ":down " input = do
          fsm <- runApp requireFsm
          liftIO (downs fsm (T.pack word)) >>= \xs -> lift (mapM_ (outputStrLn . T.unpack) xs)
          loop rs
      | Just expr <- stripPrefix ":t " input = do
          rs <- ensurePreludeLoaded rs
          ctx <- ask
          let pst = replParserFor Nothing rs
          liftIO (parseExpFromRepl pst (T.pack expr)) >>= \case
            Left err -> do
              emitMsgTCtx (MsgParseError err)
              loop rs
            Right parsed -> do
              let paramTyCons = [name | (name, arity) <- replTyCons rs, arity > 0]
              case parsed of
                Var {varName, varCandidates} -> do
                  let candidateNames = map fst varCandidates
                      sigs =
                        [ (name, args)
                        | name <- candidateNames
                        , args <- Map.findWithDefault [] name (tcFuncSigs (replTCState rs))
                        ]
                      isInfinitive = isJust (infinitiveRoot varName)
                      isEffectfulName ident =
                        ident == ([], T.pack "oku")
                          || Set.member ident (tcInfinitives (replTCState rs))
                      hasEffectfulCandidate = any isEffectfulName candidateNames
                      hasAmbiguousEffectCall = hasEffectfulCandidate && not isInfinitive && length sigs > 1
                  if null sigs
                    then inferExprType rs ctx paramTyCons parsed expr
                    else if hasAmbiguousEffectCall
                      then do
                        emitMsgTCtx (MsgTCError (Ambiguity (annSpan (annExp parsed))) (Just (T.pack expr)) paramTyCons (replTyMods rs))
                        loop rs
                    else do
                      (cache, fsm) <- runApp requireCacheFsm
                      let sigs' = reverse sigs
                          sigs'' = nubBy (\(n1, a1) (n2, a2) -> n1 == n2 && a1 == a2) sigs'
                      forM_ sigs'' $ \(name, args) -> do
                        let mRet = Map.lookup (name, map snd args) (tcFuncSigRets (replTCState rs))
                        line <- liftIO (renderReplSig ctx cache fsm paramTyCons (replTyMods rs) isInfinitive name args mRet)
                        lift (outputStrLn (T.unpack line))
                      loop rs
                _ -> inferExprType rs ctx paramTyCons parsed expr
      | Just expr <- stripPrefix ":parse " input = do
          rs <- ensurePreludeLoaded rs
          let pst = replParserFor Nothing rs
          -- Decide statement vs expression based on trailing period
          let isStmt = case dropWhile (== ' ') (reverse expr) of '.':_ -> True; _ -> False
          if isStmt
            then do
              result <- liftIO (parseForDebug pst (T.pack expr))
              case result of
                Left err -> emitMsgTCtx (MsgParseError err)
                Right (stmt, remaining) -> do
                  lift (outputStrLn (ppStmt stmt))
                  unless (T.null (T.strip remaining)) $
                    lift (outputStrLn ("Remaining: " ++ T.unpack remaining))
            else do
              result <- liftIO (parseExpForDebug pst (T.pack expr))
              case result of
                Left err -> emitMsgTCtx (MsgParseError err)
                Right (expr', remaining) -> do
                  lift (outputStrLn (ppExp 0 expr'))
                  unless (T.null (T.strip remaining)) $
                    lift (outputStrLn ("Remaining: " ++ T.unpack remaining))
          loop rs
      | Just expr <- stripPrefix ":steps " input = do
          rs <- ensurePreludeLoaded rs
          (cache, fsm) <- runApp requireCacheFsm
          let pst = replParserFor Nothing rs
          liftIO (parseExpFromRepl pst (T.pack expr)) >>= \case
            Left err -> do
              emitMsgTCtx (MsgParseError err)
              loop rs
            Right parsed -> do
              let paramTyCons = [name | (name, arity) <- replTyCons rs, arity > 0]
              liftIO (runTCM (tcExp1With True parsed) (replTCState rs)) >>= \case
                Left tcErr -> do
                  emitMsgTCtx (MsgTCError tcErr (Just (T.pack expr)) paramTyCons (replTyMods rs))
                  loop rs
                Right (parsed', _) -> do
                  (cache, fsm') <- runApp requireCacheFsm
                  skipSteps <- liftIO (shouldSkipInfinitiveSteps cache fsm' parsed')
                  if skipSteps
                    then loop rs
                    else do
                      res <- liftIO $ catch
                        (Right <$> runEvalM (evalExpTraced parsed') (replEvalState rs))
                        (\UserInterrupt -> return (Left ()))
                      case res of
                        Left () -> do
                          emitMsgTCtx MsgCtrlC
                          loop rs
                        Right (Left evalErr) -> do
                          emitMsgTCtx (MsgEvalError evalErr)
                          loop rs
                        Right (Right ((result, steps), evalSt')) -> do
                          if isRuntimeValue evalSt' result
                            then do
                              ctx <- ask
                              let renderSteps exp = renderExpPreservingCase cache fsm' evalSt' exp >>= stripStepsCopulaTRmorph cache fsm'
                                  rInput = renderSteps
                                  rOutput = renderSteps . setTopCaseNom
                              let rInputM = liftIO . rInput
                                  rOutputM = liftIO . rOutput
                              formatStepsStreaming (rcUseColor ctx) rInputM rOutputM result steps (lift . outputStrLn)
                            else emitMsgTCtx (MsgEvalError Eval.RuntimeTypeErrorNonValue)
                          loop rs
      | otherwise = do
          rs <- ensurePreludeLoaded rs
          let pst = replParserFor Nothing rs
          -- If input ends with a period, parse as statement; otherwise parse as expression
          if case dropWhile (== ' ') (reverse input) of
               '.':_ -> True
               _ -> False
            then do
              liftIO (parseFromRepl pst (T.pack input)) >>= \case
                Left err -> do
                  emitMsgTCtx (MsgParseError err)
                  loop rs
                Right (stmt, pst') -> do
                  case stmt of
                    Load dirPath name -> do
                      path <- runApp (resolveModulePath (replModuleDirs rs) dirPath name)
                      absPath <- liftIO (canonicalizePathCached path)
                      let loadPst = replParserFor (Just path) rs
                      if Set.member absPath (replLoaded rs)
                        then loop rs
                        else do
                          (pstLoaded, tcSt', evalSt', loaded') <- runApp (runFile False False False (replModuleDirs rs) (loadPst, replTCState rs, replEvalState rs, replLoaded rs) path)
                          emitMsgTCtx (MsgLoaded name)
                          loop (rs { replParserState = pstLoaded
                                               , replTCState = tcSt'
                                               , replEvalState = evalSt'
                                               , replLoaded = loaded'
                                               })
                    _ -> do
                      let ptycons = parserTyCons pst'
                          ptymods = parserTyMods pst'
                      let paramTyCons = [name | (name, arity) <- ptycons, arity > 0]
                      liftIO (runTCM (tcStmt stmt) (replTCState rs)) >>= \case
                        Left tcErr -> do
                          emitMsgTCtx (MsgTCError tcErr (Just (T.pack input)) paramTyCons ptymods)
                          loop rs
                        Right (stmt', tcSt) -> do
                          evalReplStmt paramTyCons ptymods (replEvalState rs) stmt' >>= \case
                            Nothing -> loop rs
                            Just evalSt ->
                              loop (rs { replParserState = pst'
                                                   , replTCState = tcSt
                                                   , replEvalState = evalSt
                                                   })
                          return ()
            else do
              -- Parse as expression and evaluate
              liftIO (parseExpFromRepl pst (T.pack input)) >>= \case
                Left err -> do
                  emitMsgTCtx (MsgParseError err)
                  loop rs
                Right parsed -> do
                  let paramTyCons = [name | (name, arity) <- replTyCons rs, arity > 0]
                  liftIO (runTCM (tcExp1 parsed) (replTCState rs)) >>= \case
                    Left tcErr -> do
                      emitMsgTCtx (MsgTCError tcErr (Just (T.pack input)) paramTyCons (replTyMods rs))
                      loop rs
                    Right (parsed', _) -> do
                      res <- liftIO $ catch
                        (Right <$> runEvalM (evalExp parsed') (replEvalState rs))
                        (\UserInterrupt -> return (Left ()))
                      case res of
                        Left () -> do
                          emitMsgTCtx MsgCtrlC
                          loop rs
                        Right (Left evalErr) -> do
                          emitMsgTCtx (MsgEvalError evalErr)
                          loop rs
                        Right (Right (result, _)) -> do
                          if isRuntimeValue (replEvalState rs) result
                            then do
                              rendered <- liftIO (evalRender (replEvalState rs) (replEvalState rs) result)
                              lift (outputStrLn rendered)
                            else emitMsgTCtx (MsgEvalError Eval.RuntimeTypeErrorNonValue)
                          loop rs
      where
        -- | Infer and print a type for a REPL expression.
        inferExprType :: ReplState -- ^ Current REPL state.
                      -> RenderCtx -- ^ Render context.
                      -> [Identifier] -- ^ Type parameters for rendering.
                      -> Exp Ann -- ^ Parsed expression.
                      -> String -- ^ Original input string.
                      -> ReplM () -- ^ No result.
        inferExprType currentRs ctx paramTyCons parsed expr =
          liftIO (runTCM (tcExp1 parsed >>= inferType) (replTCState currentRs)) >>= \case
            Left tcErr -> do
              emitMsgTCtx (MsgTCError tcErr (Just (T.pack expr)) paramTyCons (replTyMods currentRs))
              loop currentRs
            Right (mty, _) -> do
              case mty of
                Nothing -> emitMsgTCtx MsgTypeInferFailed
                Just ty -> do
                  (cache, fsm) <- runApp requireCacheFsm
                  let tyNom = normalizeTyNom ty
                  tyParts <- liftIO (renderTyParts cache fsm paramTyCons (replTyMods currentRs) tyNom)
                  emitMsgTCtx (MsgTypeOf tyParts)
              loop currentRs

    -- | Lazily load prelude state on first command that needs language context.
    --
    -- ==== Performance note (Optimization: lazy prelude loading)
    -- The REPL carries a lightweight empty state at startup and only pays the
    -- prelude parse/typecheck/eval cost when a command requires names/types.
    ensurePreludeLoaded :: ReplState -> ReplM ReplState
    ensurePreludeLoaded rs
      | replPreludeLoaded rs || not (replAutoPrelude rs) = return rs
      | otherwise = do
          let applyPrelude (preludePst, preludeTC, preludeEval, preludeLoaded) =
                rs
                  { replParserState = preludePst
                  , replTCState = preludeTC
                  , replEvalState = preludeEval
                  , replLoaded = preludeLoaded
                  , replPreludeLoaded = True
                  , replPreludeFuture = Nothing
                  }
              disablePrelude =
                rs
                  { replAutoPrelude = False
                  , replPreludeFuture = Nothing
                  }
          case replPreludeFuture rs of
            Just done -> do
              ready <- liftIO (tryReadMVar done)
              case ready of
                Just (Right st) -> return (applyPrelude st)
                Just (Left errMsg) -> do
                  lift (outputStrLn (T.unpack errMsg))
                  return disablePrelude
                Nothing -> do
                  -- User reached a prelude-dependent command before warmup
                  -- finished: block here and consume the completed result.
                  liftIO (readMVar done) >>= \case
                    Right st -> return (applyPrelude st)
                    Left errMsg -> do
                      lift (outputStrLn (T.unpack errMsg))
                      return disablePrelude
            Nothing -> do
              -- Fallback path when background warmup was not started.
              (cache, fsm) <- runApp requireCacheFsm
              st <- runApp (loadPreludeState False (replModuleDirs rs) cache fsm)
              return (applyPrelude st)

    -- | Render a REPL function signature with return type.
    renderReplSig :: RenderCtx -- ^ Render context.
                  -> RenderCache -- ^ Render cache.
                  -> FSM -- ^ Morphology FSM.
                  -> [Identifier] -- ^ Type parameters for rendering.
                  -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                  -> Bool -- ^ Whether the function is an infinitive.
                  -> Identifier -- ^ Canonical function name.
                  -> [Arg Ann] -- ^ Argument types.
                  -> Maybe (Ty Ann) -- ^ Optional return type.
                  -> IO Text -- ^ Rendered signature.
    renderReplSig ctx cache fsm paramTyCons tyMods isInfinitive name args mRet = do
      argParts <- mapM (renderArgParts cache fsm paramTyCons tyMods) args
      let argStrs =
            [ T.concat ["(", T.pack argName, " ", colorizeTyParts (rcUseColor ctx) tyParts, ")"]
            | (argName, tyParts) <- argParts
            ]
      nameStr <-
        if isInfinitive
          then renderInfinitiveName cache fsm name
          else renderIdentWithCase cache fsm name Nom
      retPart <-
        case mRet of
          Just ty -> do
            tyParts <- renderTyPartsPossessive cache fsm paramTyCons tyMods (normalizeTyNom ty)
            return (Just (colorizeTyParts (rcUseColor ctx) tyParts))
          Nothing -> return Nothing
      let retStr =
            case retPart of
              Just tyStr -> T.concat ["(", T.pack nameStr, " ", tyStr, ")"]
              Nothing -> T.concat ["(", T.pack nameStr, ")"]
      return (T.intercalate " " (argStrs ++ [retStr]))

    -- | Normalize a type to nominative case for display.
    normalizeTyNom :: Ty Ann -- ^ Type to normalize.
                   -> Ty Ann -- ^ Nominative type.
    normalizeTyNom ty =
      case ty of
        TyString ann -> TyString (setAnnCase ann Nom)
        TyInt ann -> TyInt (setAnnCase ann Nom)
        TyFloat ann -> TyFloat (setAnnCase ann Nom)
        TyChar ann -> TyChar (setAnnCase ann Nom)
        TyInd ann name -> TyInd (setAnnCase ann Nom) name
        TyVar ann name -> TyVar (setAnnCase ann Nom) name
        TySkolem ann name -> TySkolem (setAnnCase ann Nom) name
        Arr ann d i -> Arr (setAnnCase ann Nom) (normalizeTyNom d) (normalizeTyNom i)
        TyApp ann ctor args -> TyApp (setAnnCase ann Nom) (normalizeTyNom ctor) (map normalizeTyNom args)

    -- | Evaluate a REPL statement and update the evaluator state.
    evalReplStmt :: [Identifier] -- ^ Type parameters for rendering.
                 -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                 -> EvalState -- ^ Current evaluator state.
                 -> Stmt Ann -- ^ Statement to evaluate.
                 -> ReplM (Maybe EvalState) -- ^ Updated evaluator state.
    evalReplStmt paramTyCons tyMods evalSt stmt = do
      res <- liftIO $ catch
        (Right <$> runEvalM (evalStmtInFile Nothing stmt) evalSt)
        (\UserInterrupt -> return (Left ()))
      case res of
        Left () -> do
          emitMsgTCtx MsgCtrlC
          return Nothing
        Right (Left evalErr) -> do
          emitMsgTCtx (MsgEvalError evalErr)
          return (Just evalSt)
        Right (Right (_, evalSt')) -> do
          case stmt of
            Defn name _ _ ->
              emitMsgTCtx (MsgDefnAdded name)
            Function name args retTy _ isInfinitive ->
              if isExplicitRetTy retTy
                then emitMsgTCtx (MsgFuncLoaded name args isInfinitive paramTyCons tyMods)
                else emitMsgTCtx (MsgFuncAdded name args isInfinitive paramTyCons tyMods)
            PrimFunc name args _ isInfinitive ->
              emitMsgTCtx (MsgPrimFuncAdded name args isInfinitive paramTyCons tyMods)
            NewType name _ _ ->
              emitMsgTCtx (MsgTypeAdded name)
            PrimType name _ ->
              emitMsgTCtx (MsgPrimTypeAdded name)
            _ -> return ()
          return (Just evalSt')
    codegenFilesTagged :: ParserState
                       -> TCState
                       -> [FilePath]
                       -> Set FilePath
                       -> [FilePath]
                       -> AppM (TCState, [TaggedStmt])
    codegenFilesTagged basePst baseTC moduleDirs loaded files = do
      (_, finalTC, stmtsRev, _) <- foldM' (collectFileStmts moduleDirs) (basePst, baseTC, [], loaded) files
      return (finalTC, reverse stmtsRev)

    -- | Collect statements from a single file (recursively handles Load).
    --
    -- ==== Performance note (Optimization: reverse accumulator)
    -- Statement collection uses reverse accumulation to avoid repeated
    -- @acc ++ [x]@ allocation in recursive load/typecheck traversal.
    collectFileStmts :: [FilePath] -- ^ Module search paths.
                     -> (ParserState, TCState, [TaggedStmt], Set FilePath) -- ^ Current state with reverse statement accumulator.
                     -> FilePath -- ^ File to process.
                     -> AppM (ParserState, TCState, [TaggedStmt], Set FilePath) -- ^ Updated state.
    collectFileStmts moduleDirs (pst, tcSt, accStmtsRev, loaded) path = do
      exists <- liftIO (doesFileExist path)
      unless exists $ do
        msg <- renderMsg (MsgFileNotFound path)
        liftIO (die (T.unpack msg))
      absPath <- liftIO (canonicalizePathCached path)
      if Set.member absPath loaded
        then return (pst, tcSt, accStmtsRev, loaded)
        else do
          (cache, fsm) <- requireCacheFsm
          let cachePath = cacheFilePath absPath
          mCachedRaw <- liftIO (loadCachedModule cachePath)
          let mCached =
                case mCachedRaw of
                  Just cached
                    | tcOutputModeSupports
                        (tcOutputMode (fromCachedTCState (cachedTC cached)))
                        TCOutputCodegen -> Just cached
                  _ -> Nothing
          case mCached of
            Just cached -> do
              pstCached <- liftIO (fromCachedParserStateDelta fsm (Just path) cache pst (cachedParser cached))
              let loaded' = Set.insert absPath loaded
                  tcCached = mergeTCState tcSt (fromCachedTCState (cachedTC cached))
                  stmts = cachedTypedStmts cached
              (pstFinal, tcFinal, newStmtsRev, loaded'') <-
                foldM' (collectCachedStmt moduleDirs absPath) (pstCached, tcCached, [], loaded') stmts
              return (pstFinal, tcFinal, newStmtsRev ++ accStmtsRev, loaded'')
            Nothing -> do
              input <- liftIO (TIO.readFile path)
              liftIO (parseFromFile pst input) >>= \case
                Left err -> do
                  emitMsgIOCtx (MsgParseError err)
                  msg <- renderMsg MsgRunFailed
                  liftIO (die (T.unpack msg))
                Right (stmts, pst') -> do
                  let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                  -- Type-check
                  liftIO (runTCM (registerForwardDecls stmts) tcSt) >>= \case
                    Left tcErr -> do
                      msg <- renderMsg (MsgTCError tcErr (Just input) paramTyCons (parserTyMods pst'))
                      liftIO (die (T.unpack msg))
                    Right (_, tcStWithDecls) -> do
                      -- Type-check each statement
                      (pst'', tcSt'', newStmtsRev, loaded') <- foldM' (collectStmt moduleDirs absPath paramTyCons (parserTyMods pst') input)
                        (pst', tcStWithDecls, [], Set.insert absPath loaded) stmts
                      return (pst'', tcSt'', newStmtsRev ++ accStmtsRev, loaded')

    -- | Collect a single statement, recursively loading modules.
    collectStmt :: [FilePath] -- ^ Module search paths.
                -> FilePath -- ^ Current file path.
                -> [Identifier] -- ^ Type parameter names for error messages.
                -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                -> Text -- ^ Source input.
                -> (ParserState, TCState, [TaggedStmt], Set FilePath) -- ^ Current state with reverse statement accumulator.
                -> Stmt Ann -- ^ Statement to process.
                -> AppM (ParserState, TCState, [TaggedStmt], Set FilePath) -- ^ Updated state.
    collectStmt moduleDirs currentPath paramTyCons tyMods source (pst, tcSt, accStmtsRev, loaded) stmt =
      case stmt of
        Load dirPath name -> do
          path <- resolveModulePath moduleDirs dirPath name
          absPath <- liftIO (canonicalizePathCached path)
          if Set.member absPath loaded
            then return (pst, tcSt, (currentPath, stmt) : accStmtsRev, loaded)
            else collectFileStmts moduleDirs (pst, tcSt, (currentPath, stmt) : accStmtsRev, loaded) path
        _ -> do
          -- Type-check the statement
          liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (stmt', tcSt') ->
              return (pst, tcSt', (currentPath, stmt') : accStmtsRev, loaded)

    -- | Collect a cached statement, expanding Load statements without re-typechecking.
    collectCachedStmt :: [FilePath] -- ^ Module search paths.
                      -> FilePath -- ^ Current module file path.
                      -> (ParserState, TCState, [TaggedStmt], Set FilePath) -- ^ Current state with reverse statement accumulator.
                      -> Stmt Ann -- ^ Statement to process.
                      -> AppM (ParserState, TCState, [TaggedStmt], Set FilePath) -- ^ Updated state.
    collectCachedStmt moduleDirs currentPath (pst, tcSt, accStmtsRev, loaded) stmt =
      case stmt of
        Load dirPath name -> do
          path <- resolveModulePath moduleDirs dirPath name
          collectFileStmts moduleDirs (pst, tcSt, (currentPath, stmt) : accStmtsRev, loaded) path
        _ ->
          return (pst, tcSt, (currentPath, stmt) : accStmtsRev, loaded)

    -- | Run multiple files through parsing, type checking, and evaluation.
    runFiles :: Bool -- ^ Whether to show definitions.
             -> Bool -- ^ Whether to show load messages.
             -> Bool -- ^ Whether to build-only.
             -> ParserState -- ^ Base parser state.
             -> TCState -- ^ Base type checker state.
             -> EvalState -- ^ Base evaluator state.
             -> [FilePath] -- ^ Module search paths.
             -> Set FilePath -- ^ Loaded files.
             -> [FilePath] -- ^ Files to run.
             -> AppM ReplState -- ^ Updated REPL state.
    runFiles showDefn showLoad buildOnly basePst baseTC baseEval moduleDirs loaded files = do
      (pst', tcSt', evalSt', loaded') <- foldM' (runFile showDefn showLoad buildOnly moduleDirs) (basePst, baseTC, baseEval, loaded) files
      return (ReplState pst' tcSt' evalSt' moduleDirs loaded' True False Nothing)
    -- | Run a single file and update all states.
    runFile :: Bool -- ^ Whether to show definitions.
            -> Bool -- ^ Whether to show load messages.
            -> Bool -- ^ Whether to build-only.
            -> [FilePath] -- ^ Module search paths.
            -> (ParserState, TCState, EvalState, Set FilePath) -- ^ Current states.
            -> FilePath -- ^ File to run.
            -> AppM (ParserState, TCState, EvalState, Set FilePath) -- ^ Updated states.
    runFile showDefn showLoad buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path = do
      exists <- liftIO (doesFileExist path)
      unless exists $ do
        msg <- renderMsg (MsgFileNotFound path)
        liftIO (die (T.unpack msg))
      absPath <- liftIO (canonicalizePathCached path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded)
        else do
          (cache, fsm) <- requireCacheFsm
          let cachePath = cacheFilePath absPath
          mCached <- liftIO (loadCachedModule cachePath)
          case mCached of
            Just cached -> do
              let loaded' = Set.insert absPath loaded
              if buildOnly
                then return (pst, tcSt, evalSt, loaded')
                else do
                  pst' <- liftIO (fromCachedParserStateDelta fsm (Just path) cache pst (cachedParser cached))
                  let tcSt' = mergeTCState tcSt (fromCachedTCState (cachedTC cached))
                      evalSt' = evalSt
                      stmts = cachedTypedStmts cached
                      paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                      source = ""
                      primRefs = collectNonInfinitiveRefs stmts
                  foldM' (runTypedStmt showDefn showLoad buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') primRefs source) (pst', tcSt', evalSt', loaded') stmts
            Nothing -> do
              morphToken <- liftIO beginMorphTracking
              input <- liftIO (TIO.readFile path)
              liftIO (parseFromFile pst input) >>= \case
                Left err -> do
                  emitMsgIOCtx (MsgParseError err)
                  msg <- renderMsg MsgRunFailed
                  liftIO (die (T.unpack msg))
                Right (stmts, pst') -> do
                  let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                      source = input
                      primRefs = collectNonInfinitiveRefs stmts
                  -- Pre-register forward declarations for all functions and types
                  liftIO (runTCM (registerForwardDecls stmts) tcSt) >>= \case
                    Left tcErr -> do
                      msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons (parserTyMods pst'))
                      liftIO (die (T.unpack msg))
                    Right (_, tcStWithDecls) -> do
                      let startState = (pst', tcStWithDecls, evalSt, Set.insert absPath loaded, [])
                      (pstFinal, tcSt', evalSt', loaded', typedStmtsRev) <-
                        foldM' (runStmtCollect showDefn showLoad buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') primRefs source) startState stmts
                      let typedStmts = reverse typedStmtsRev

                      -- Save to cache
                      let depStmts = [(dp, n) | Load dp n <- stmts]
                      depPathsRaw <- mapM (uncurry (resolveModulePath moduleDirs)) depStmts
                      depPaths <- liftIO (uniquePreserve <$> mapM canonicalizePathCached depPathsRaw)
                      depHashes <- liftIO $ mapM (\p -> do
                        mFp <- fileFingerprint p
                        case mFp of
                          Just fp -> return fp
                          -- Fallback path is intentionally conservative; it
                          -- should be rare and only used when metadata calls fail.
                          Nothing -> do
                            digest <- hash <$> BS.readFile p
                            return (p, digest, 0, 0)) depPaths
                      morphDelta <- liftIO (finishMorphTracking morphToken)
                      mCompilerHash <- liftIO getCompilerHash
                      case mCompilerHash of
                        Nothing -> return ()
                        Just compilerHash -> do
                          mSourceMeta <- liftIO (getFileMeta absPath)
                          cachedParserBase <- liftIO (toCachedParserStateDelta pst pst')
                          let cachedParserState = attachMorphDelta morphDelta cachedParserBase
                          let sourceBytes = encodeUtf8 input
                              sourceDigest = hash sourceBytes
                              fallbackSourceSize = fromIntegral (BS.length sourceBytes)
                              (srcSize, srcMTime) = fromMaybe (fallbackSourceSize, 0) mSourceMeta
                              meta = CacheMetadata
                                { compilerHash = compilerHash
                                , sourceHash = sourceDigest
                                , sourceSize = srcSize
                                , sourceMTime = srcMTime
                                , dependencyRootHash = dependencyMerkleRoot depHashes
                                , dependencies = depHashes
                                }
                              cachedModule = CachedModule
                                { metadata = meta
                                , cachedTypedStmts = typedStmts
                                , cachedParser = cachedParserState
                                , cachedTC = toCachedTCStateDelta tcSt tcSt'
                                }
                          liftIO (saveCachedModule cachePath cachedModule)

                      return (pstFinal, tcSt', evalSt', loaded')

    -- | Merge a cached type-checker snapshot into the current state.
    --
    -- Cached entries are preferred for overlapping keys so that overloaded
    -- signature order stays stable with the source module that produced the
    -- cache. Current-state entries are retained for keys not present in cache.
    mergeTCState :: TCState -- ^ Current TC state.
                 -> TCState -- ^ Cached TC state.
                 -> TCState -- ^ Combined TC state.
    mergeTCState = mergeCachedTCState

    -- | Run a single statement in the context of a file.
    runStmt :: Bool -- ^ Whether to show definitions.
            -> Bool -- ^ Whether to show load messages.
            -> Bool -- ^ Whether to build-only.
            -> [FilePath] -- ^ Module search paths.
            -> FilePath -- ^ Current file path.
            -> [Identifier] -- ^ Type parameters for rendering.
            -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
            -> [Identifier] -- ^ Non-infinitive primitive refs.
            -> Text -- ^ Source input.
            -> (ParserState, TCState, EvalState, Set FilePath) -- ^ Current states.
            -> Stmt Ann -- ^ Statement to run.
            -> AppM (ParserState, TCState, EvalState, Set FilePath) -- ^ Updated states.
    runStmt showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs source (pst, tcSt, evalSt, loaded) stmt =
      case stmt of
        Load dirPath name -> do
          path <- resolveModulePath moduleDirs dirPath name
          absPath <- liftIO (canonicalizePathCached path)
          if Set.member absPath loaded
            then do
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst, tcSt, evalSt, loaded)
            else do
              (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst', tcSt', evalSt', loaded')
        _ ->
          liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (stmt', tcSt') -> do
              when showDefn $
                case stmt' of
                  Defn name _ _ -> emitMsgIOCtx (MsgDefnAdded name)
                  Function name args retTy _ isInfinitive ->
                    if isExplicitRetTy retTy
                      then emitMsgIOCtx (MsgFuncLoaded name args isInfinitive paramTyCons tyMods)
                      else emitMsgIOCtx (MsgFuncAdded name args isInfinitive paramTyCons tyMods)
                  PrimFunc name args _ isInfinitive -> do
                    when (name `elem` primRefs || isWritePrim name) $
                      emitMsgIOCtx (MsgPrimFuncAdded name args isInfinitive paramTyCons tyMods)
                  NewType name _ _ -> emitMsgIOCtx (MsgTypeAdded name)
                  PrimType name _ -> emitMsgIOCtx (MsgPrimTypeAdded name)
                  _ -> return ()
              if buildOnly
                then
                  case stmt' of
                    ExpStmt _ -> return (pst, tcSt', evalSt, loaded)
                    _ ->
                      liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                        Left evalErr -> do
                          msg <- renderMsg (MsgEvalError evalErr)
                          liftIO (die (T.unpack msg))
                        Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded)
                else
                  liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                    Left evalErr -> do
                      msg <- renderMsg (MsgEvalError evalErr)
                      liftIO (die (T.unpack msg))
                    Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded)

    -- | Run a pre-typechecked statement in the context of a file.
    --
    -- This path is used when a valid module cache is loaded. It avoids
    -- re-running 'tcStmt' and any forward-declaration pre-pass by assuming the
    -- incoming statement list is already type-checked ('cachedTypedStmts').
    runTypedStmt :: Bool -- ^ Whether to show definitions.
                 -> Bool -- ^ Whether to show load messages.
                 -> Bool -- ^ Whether to build-only.
                 -> [FilePath] -- ^ Module search paths.
                 -> FilePath -- ^ Current file path.
                 -> [Identifier] -- ^ Type parameters for rendering.
                 -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                 -> [Identifier] -- ^ Non-infinitive primitive refs.
                 -> Text -- ^ Source input.
                 -> (ParserState, TCState, EvalState, Set FilePath) -- ^ Current states.
                 -> Stmt Ann -- ^ Statement to run.
                 -> AppM (ParserState, TCState, EvalState, Set FilePath) -- ^ Updated states.
    runTypedStmt showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs _source (pst, tcSt, evalSt, loaded) stmt =
      case stmt of
        Load dirPath name -> do
          path <- resolveModulePath moduleDirs dirPath name
          absPath <- liftIO (canonicalizePathCached path)
          if Set.member absPath loaded
            then do
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst, tcSt, evalSt, loaded)
            else do
              (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst', tcSt', evalSt', loaded')
        _ -> do
          when showDefn $
            case stmt of
              Defn name _ _ -> emitMsgIOCtx (MsgDefnAdded name)
              Function name args retTy _ isInfinitive ->
                if isExplicitRetTy retTy
                  then emitMsgIOCtx (MsgFuncLoaded name args isInfinitive paramTyCons tyMods)
                  else emitMsgIOCtx (MsgFuncAdded name args isInfinitive paramTyCons tyMods)
              PrimFunc name args _ isInfinitive -> do
                when (name `elem` primRefs || isWritePrim name) $
                  emitMsgIOCtx (MsgPrimFuncAdded name args isInfinitive paramTyCons tyMods)
              NewType name _ _ -> emitMsgIOCtx (MsgTypeAdded name)
              PrimType name _ -> emitMsgIOCtx (MsgPrimTypeAdded name)
              _ -> return ()
          if buildOnly
            then
              case stmt of
                ExpStmt _ -> return (pst, tcSt, evalSt, loaded)
                _ ->
                  liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt) evalSt) >>= \case
                    Left evalErr -> do
                      msg <- renderMsg (MsgEvalError evalErr)
                      liftIO (die (T.unpack msg))
                    Right (_, evalSt') -> return (pst, tcSt, evalSt', loaded)
            else
              liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt) evalSt) >>= \case
                Left evalErr -> do
                  msg <- renderMsg (MsgEvalError evalErr)
                  liftIO (die (T.unpack msg))
                Right (_, evalSt') -> return (pst, tcSt, evalSt', loaded)

    -- | Run a single statement while collecting type-checked statements for caching.
    --
    -- ==== Performance note (Optimization: cache-builder consing)
    -- Typechecked statements are accumulated with cons and reversed once at
    -- the end of file processing, avoiding per-statement list append cost.
    runStmtCollect :: Bool -- ^ Whether to show definitions.
                   -> Bool -- ^ Whether to show load messages.
                   -> Bool -- ^ Whether to build-only.
                   -> [FilePath] -- ^ Module search paths.
                   -> FilePath -- ^ Current file path.
                   -> [Identifier] -- ^ Type parameters for rendering.
                   -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                   -> [Identifier] -- ^ Non-infinitive primitive refs.
                   -> Text -- ^ Source input.
                   -> (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann]) -- ^ Current states with reverse typed statement accumulator.
                   -> Stmt Ann -- ^ Statement to run.
                   -> AppM (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann]) -- ^ Updated states.
    runStmtCollect showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs source (pst, tcSt, evalSt, loaded, typedAccRev) stmt =
      case stmt of
        Load dirPath name -> do
          path <- resolveModulePath moduleDirs dirPath name
          absPath <- liftIO (canonicalizePathCached path)
          if Set.member absPath loaded
            then do
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst, tcSt, evalSt, loaded, stmt : typedAccRev)
            else do
              (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst', tcSt', evalSt', loaded', stmt : typedAccRev)
        _ ->
          liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (stmt', tcSt') -> do
              when showDefn $
                case stmt' of
                  Defn name _ _ -> emitMsgIOCtx (MsgDefnAdded name)
                  Function name args retTy _ isInfinitive ->
                    if isExplicitRetTy retTy
                      then emitMsgIOCtx (MsgFuncLoaded name args isInfinitive paramTyCons tyMods)
                      else emitMsgIOCtx (MsgFuncAdded name args isInfinitive paramTyCons tyMods)
                  PrimFunc name args _ isInfinitive -> do
                    when (name `elem` primRefs || isWritePrim name) $
                      emitMsgIOCtx (MsgPrimFuncAdded name args isInfinitive paramTyCons tyMods)
                  NewType name _ _ -> emitMsgIOCtx (MsgTypeAdded name)
                  PrimType name _ -> emitMsgIOCtx (MsgPrimTypeAdded name)
                  _ -> return ()
              if buildOnly
                then
                  case stmt' of
                    ExpStmt _ -> return (pst, tcSt', evalSt, loaded, stmt' : typedAccRev)
                    _ ->
                      liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                        Left evalErr -> do
                          msg <- renderMsg (MsgEvalError evalErr)
                          liftIO (die (T.unpack msg))
                        Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded, stmt' : typedAccRev)
                else
                  liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                    Left evalErr -> do
                      msg <- renderMsg (MsgEvalError evalErr)
                      liftIO (die (T.unpack msg))
                    Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded, stmt' : typedAccRev)

    -- | Collect non-infinitive primitive references from statements.
    --
    -- ==== Performance note (Optimization: Set-backed dedupe)
    -- This walk accumulates references in a strict set instead of building
    -- intermediate lists and deduplicating afterward.
    collectNonInfinitiveRefs :: [Stmt Ann] -- ^ Statements to inspect.
                         -> [Identifier] -- ^ Referenced identifiers.
    collectNonInfinitiveRefs stmts =
      Set.toList (foldl' stmtRefs Set.empty stmts)
      where
        -- | Collect references from a statement into a set.
        stmtRefs :: Set Identifier -- ^ Accumulated references.
                 -> Stmt Ann -- ^ Statement to inspect.
                 -> Set Identifier -- ^ Updated references.
        stmtRefs acc stmt =
          case stmt of
            Defn name _ body ->
              expRefs (Set.singleton name) body acc
            Function _ args _ clauses _ ->
              let bound = Set.fromList (map argIdent args)
              in foldl' (clauseRefs bound) acc clauses
            ExpStmt e ->
              expRefs Set.empty e acc
            _ -> acc
        -- | Collect references from a clause into a set.
        clauseRefs :: Set Identifier -- ^ Bound identifiers.
                   -> Set Identifier -- ^ Accumulated references.
                   -> Clause Ann -- ^ Clause to inspect.
                   -> Set Identifier -- ^ Updated references.
        clauseRefs bound acc (Clause _ body) = expRefs bound body acc
        -- | Collect references from an expression into a set.
        expRefs :: Set Identifier -- ^ Bound identifiers.
                -> Exp Ann -- ^ Expression to inspect.
                -> Set Identifier -- ^ Accumulated references.
                -> Set Identifier -- ^ Updated references.
        expRefs bound exp acc =
          case exp of
            Var {varCandidates} ->
              foldl'
                (\acc' (ident, _) ->
                  if Set.member ident bound then acc' else Set.insert ident acc')
                acc
                varCandidates
            Bind {bindExp} ->
              expRefs bound bindExp acc
            App {fn, args} ->
              foldl' (flip (expRefs bound)) (expRefs bound fn acc) args
            Match {scrutinee, clauses} ->
              foldl' (clauseRefs bound) (expRefs bound scrutinee acc) clauses
            Seq {first, second} ->
              case first of
                Bind {bindName, bindExp} ->
                  expRefs (Set.insert bindName bound) second (expRefs bound bindExp acc)
                _ -> expRefs bound second (expRefs bound first acc)
            Let {varName, body} ->
              expRefs (Set.insert varName bound) body acc
            _ -> acc

    -- | Check whether an identifier refers to the write primitive.
    isWritePrim :: Identifier -- ^ Identifier to inspect.
                -> Bool -- ^ True when identifier is `yaz`.
    isWritePrim ident =
      prettyIdent ident == "yaz"

    -- | Resolve a module name to a file path.
    resolveModulePath :: [FilePath] -- ^ Module search paths.
                      -> [Text] -- ^ Directory path components.
                      -> Identifier -- ^ Module identifier.
                      -> AppM FilePath -- ^ Resolved file path.
    resolveModulePath dirs dirPath name@(xs, x) = do
      let dirComponents = map T.unpack dirPath
          parts = map T.unpack xs
          nm = T.unpack x
          fileName = intercalate "-" (parts ++ [nm]) ++ ".kip"
          relPath = joinPath (dirComponents ++ [fileName])
          candidates = map (</> relPath) dirs
      found <- liftIO (filterM doesFileExist candidates)
      case found of
        path:_ -> return path
        [] -> do
          msg <- renderMsg (MsgModuleNotFound dirPath name)
          liftIO (die (T.unpack msg))

    -- | Return parent directories for a path.
    takeDirectories :: FilePath -- ^ Path to split.
                    -> [FilePath] -- ^ Parent directories.
    takeDirectories path = [takeDirectory path]

    -- | Remove duplicates while preserving first occurrence order.
    --
    -- ==== Performance note (Optimization: strict order-preserving dedupe)
    -- Set-membership dedupe avoids quadratic @nub@ behavior on large module
    -- and dependency lists while keeping deterministic first-seen order.
    uniquePreserve :: Ord a => [a] -> [a]
    uniquePreserve xs = reverse (snd (foldl' go (Set.empty, []) xs))
      where
        go (seen, acc) x
          | Set.member x seen = (seen, acc)
          | otherwise = (Set.insert x seen, x : acc)

    isExpStmt :: Stmt Ann -> Bool
    isExpStmt ExpStmt {} = True
    isExpStmt _ = False

    isLoadStmt :: Stmt Ann -> Bool
    isLoadStmt Load {} = True
    isLoadStmt _ = False

    toPosixRel :: FilePath -> FilePath -> FilePath -> Text
    toPosixRel outDir cwd modulePath =
      let rel = makeRelative cwd modulePath
          moduleOut = outDir </> replaceExtension rel "mjs"
      in T.pack (map (\c -> if c == '\\' then '/' else c) (makeRelative outDir moduleOut))

    runtimeGlobalNames :: [Text]
    runtimeGlobalNames =
      [ "__kip_close_stdin"
      , "__kip_call"
      , "__kip_float"
      , "__kip_is_float"
      , "__kip_num"
      , "__kip_prim_ters"
      , "__kip_prim_birleşim"
      , "__kip_prim_uzunluk"
      , "__kip_prim_toplam"
      , "__kip_prim_fark"
      , "__kip_prim_oku_stdin"
      , "__kip_prim_oku_dosya"
      , "__kip_prim_yaz_dosya"
      , "doğru"
      , "yanlış"
      , "varlık"
      , "yokluk"
      , "bitimlik"
      , "yaz"
      , "çarpım"
      , "fark"
      , "bölüm"
      , "kalan"
      , "karekök"
      , "radyan"
      , "derece"
      , "pi_sayısı"
      , "taban"
      , "tavan"
      , "tam_sayı_ondalık_sayı_hali"
      , "sayı_çek"
      , "eşitlik"
      , "küçüklük"
      , "küçük_eşitlik"
      , "büyüklük"
      , "büyük_eşitlik"
      , "dizge_hal"
      , "tam_sayı_hal"
      , "ondalık_sayı_hal"
      ]

    importRelPath :: FilePath -> FilePath -> Text
    importRelPath fromAbs toAbs =
      let fromDirs = splitDirectories (normalise (takeDirectory fromAbs))
          toDirs = splitDirectories (normalise toAbs)
          commonLen = length (takeWhile id (zipWith (==) fromDirs toDirs))
          up = replicate (length fromDirs - commonLen) ".."
          down = drop commonLen toDirs
          rel =
            if null up && null down
              then "."
              else joinPath (up ++ down)
      in T.pack (map (\c -> if c == '\\' then '/' else c) rel)

    -- | Load the prelude module into a parser state.
    loadPreludeParserState :: [FilePath] -- ^ Module search paths.
                           -> RenderCache -- ^ Shared morphology caches.
                           -> FSM -- ^ Morphology FSM.
                           -> AppM ParserState -- ^ Loaded parser state.
    loadPreludeParserState moduleDirs cache fsm = do
      path <- resolveModulePath moduleDirs [] ([], T.pack "temel")
      absPath <- liftIO (canonicalizePathCached path)
      let pst = newParserStateWithCaches fsm (Just path) cache
      let cachePath = cacheFilePath absPath
      liftIO (loadCachedModule cachePath) >>= \case
        Just cached -> liftIO (fromCachedParserStateDelta fsm (Just path) cache pst (cachedParser cached))
        Nothing -> do
          input <- liftIO (TIO.readFile path)
          liftIO (parseFromFile pst input) >>= \case
            Left _ -> return pst
            Right (_, pst') -> return pst'

    -- | Load the prelude module for code generation (no eval, cached when possible).
    loadPreludeCodegenState :: Bool -- ^ Whether to skip the prelude.
                            -> [FilePath] -- ^ Module search paths.
                            -> RenderCache -- ^ Shared morphology/render caches.
                            -> FSM -- ^ Morphology FSM.
                            -> AppM (ParserState, TCState, Set FilePath) -- ^ Loaded parser/TC states.
    loadPreludeCodegenState noPrelude moduleDirs cache fsm = do
      let pst = newParserStateWithCaches fsm Nothing cache
          tcSt = setTCOutputMode TCOutputCodegen emptyTCState
      if noPrelude
        then return (pst, tcSt, Set.empty)
        else do
          snapshotPath <- liftIO preludeSnapshotPath
          -- ==== Performance note (Optimization: prelude snapshot reuse)
          -- Reuse the merged prelude graph snapshot for codegen startup. Even
          -- though codegen does not evaluate terms, restoring parser+TC from a
          -- validated snapshot avoids reparsing and re-typechecking stdlib.
          liftIO (loadCachedPrelude snapshotPath cache fsm) >>= \case
            Just (pstSnap, tcSnap, _, loadedSnap) ->
              return (pstSnap, setTCOutputMode TCOutputCodegen tcSnap, loadedSnap)
            Nothing -> do
              path <- resolveModulePath moduleDirs [] ([], T.pack "giriş")
              let pstFile = pst { parserFilePath = Just path }
              (pst', tcSt', _, loaded') <- collectFileStmts moduleDirs (pstFile, tcSt, [], Set.empty) path
              return (pst', tcSt', loaded')

    -- | Load the prelude module into parser/type/eval states unless disabled.
    loadPreludeState :: Bool -- ^ Whether to skip the prelude.
                     -> [FilePath] -- ^ Module search paths.
                     -> RenderCache -- ^ Render cache.
                     -> FSM -- ^ Morphology FSM.
                     -> AppM (ParserState, TCState, EvalState, Set FilePath) -- ^ Loaded states.
    loadPreludeState noPrelude moduleDirs cache fsm = do
      let pst = newParserStateWithCaches fsm Nothing cache
          tcSt = emptyTCState
          evalSt = mkEvalState cache fsm
      if noPrelude
        then return (pst, tcSt, evalSt, Set.empty)
        else do
          snapshotPath <- liftIO preludeSnapshotPath
          -- ==== Performance note (Optimization: prelude snapshot/image cache)
          -- The first successful prelude load persists a merged snapshot. Later
          -- startups validate file fingerprints and restore parser/TC/eval in
          -- one step instead of traversing the whole stdlib graph.
          liftIO (loadCachedPrelude snapshotPath cache fsm) >>= \case
            Just snapState -> return snapState
            Nothing -> do
              path <- resolveModulePath moduleDirs [] ([], T.pack "giriş")
              let pst' = pst { parserFilePath = Just path }
              state'@(pstLoaded, tcLoaded, evalLoaded, loaded') <-
                runFile False False False moduleDirs (pst', tcSt, evalSt, Set.empty) path
              liftIO (saveCachedPrelude snapshotPath pstLoaded tcLoaded evalLoaded loaded')
              return state'

    -- | Build an evaluator state wired to the render cache.
    mkEvalState :: RenderCache -- ^ Render cache.
                -> FSM -- ^ Morphology FSM.
                -> EvalState -- ^ Evaluator state.
    mkEvalState cache fsm =
      emptyEvalState { evalRender = renderExpValue cache fsm }

    -- | Strict monadic left fold to avoid building thunks on large inputs.
    foldM' :: forall m b a.
              Monad m
           => (b -> a -> m b) -- ^ Step function.
           -> b -- ^ Initial accumulator.
           -> [a] -- ^ Input list.
           -> m b -- ^ Final accumulator.
    foldM' f = go
      where
        go :: b -> [a] -> m b
        go acc [] = return acc
        go acc (y:ys) = do
          acc' <- f acc y
          acc' `seq` go acc' ys
