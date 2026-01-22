{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Command-line interface and REPL for Kip.
module Main where

import System.Exit
import System.Directory (doesFileExist, canonicalizePath, doesDirectoryExist, listDirectory)
import Paths_kip (version, getDataFileName)
import Data.List
import Options.Applicative hiding (ParseError)
import System.FilePath ((</>), takeDirectory, takeExtension)

import Control.Monad (forM, forM_, when, unless, filterM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Exception (AsyncException(UserInterrupt), catch)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)

import System.Console.Haskeline
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Set (Set)
import Text.Megaparsec (ParseErrorBundle(..), errorBundlePretty)
import Text.Megaparsec.Error (ParseError(..), ErrorFancy(..), ShowErrorComponent(..))
import qualified Data.List.NonEmpty as NE
import Text.Megaparsec.Pos (sourceLine, sourceColumn, unPos)

import Language.Foma
import System.Console.Chalk
import Kip.Parser
import Kip.AST
import qualified Data.HashTable.IO as HT
import Kip.Eval hiding (Unknown, inferType)
import Kip.TypeCheck
import Kip.Render
import Kip.Cache
import Kip.Codegen.JS (codegenProgram)
import Data.Word
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString as BS

import Data.Version (showVersion)

-- | REPL runtime state (parser/type context + evaluator).
data ReplState =
  ReplState
    { replCtx :: [Identifier]
    , replTyParams :: [Identifier]
    , replTyCons :: [(Identifier, Int)]
    , replTyMods :: [(Identifier, [Identifier])]
    , replPrimTypes :: [Identifier]
    , replTCState :: TCState
    , replEvalState :: EvalState
    , replModuleDirs :: [FilePath]
    , replLoaded :: Set FilePath
    }

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
    , optLang :: Lang
    , optNoPrelude :: Bool
    }

-- | Diagnostic language selection.
data Lang
  = LangTr
  | LangEn
  deriving (Eq, Show)

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
  | MsgModuleNotFound Identifier
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

-- | Rendering context for diagnostics and output.
data RenderCtx =
  RenderCtx
    { rcLang :: Lang
    , rcUseColor :: Bool
    , rcCache :: Maybe RenderCache
    , rcFsm :: Maybe FSM
    , rcUpsCache :: Maybe MorphCache
    , rcDownsCache :: Maybe MorphCache
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

-- | Render a message or fall back to a generic error.
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
    Span start end ->
      case lang of
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
            ]

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
        Unknown ->
          return "Tip hatası: bilinmeyen hata."
        NoType sp ->
          return ("Tip hatası: uygun bir tip bulunamadı." <> renderSpan (rcLang ctx) sp)
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
        PatternTypeMismatch ctor expectedTy actualTy _ -> do
          (cache, fsm) <- requireCacheFsm
          expStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods expectedTy)
          actStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods actualTy)
          let header =
                T.pack (prettyIdent ctor) <> " yapıcısı " <> expStr <> " tipindendir, ancak burada " <> actStr <> " bekleniyor"
          return header
    LangEn ->
      case tcErr of
        Unknown ->
          return "Type error: unknown error."
        NoType sp ->
          return ("Type error: no suitable type found." <> renderSpan (rcLang ctx) sp)
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
        PatternTypeMismatch ctor expectedTy actualTy _ -> do
          (cache, fsm) <- requireCacheFsm
          expStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods expectedTy)
          actStr <- liftIO (renderTyNomText cache fsm paramTyCons tyMods actualTy)
          let header =
                T.pack (prettyIdent ctor) <> " constructor has type " <> expStr <> ", but " <> actStr <> " is expected here"
          return header

-- | Render a type checker error with a source snippet.
renderTCErrorWithSource :: [Identifier] -- ^ Type parameters for rendering.
                        -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                        -> Text -- ^ Source input.
                        -> TCError -- ^ Type checker error.
                        -> RenderM Text -- ^ Rendered error text.
renderTCErrorWithSource paramTyCons tyMods source tcErr = do
  msg <- renderTCError paramTyCons tyMods tcErr
  case tcErrSpan tcErr of
    Nothing -> return msg
    Just sp ->
      let snippet = renderSpanSnippet source sp
      in return (msg <> "\n" <> snippet)

-- | Extract a span from a type checker error when present.
tcErrSpan :: TCError -- ^ Type checker error.
          -> Maybe Span -- ^ Associated span.
tcErrSpan tcErr =
  case tcErr of
    NoType sp -> Just sp
    Ambiguity sp -> Just sp
    UnknownName _ sp -> Just sp
    NoMatchingOverload _ _ _ sp -> Just sp
    NoMatchingCtor _ _ _ sp -> Just sp
    PatternTypeMismatch _ _ _ sp -> Just sp
    Unknown -> Nothing

-- | Render a caret snippet for a source span.
renderSpanSnippet :: Text -- ^ Source input.
                  -> Span -- ^ Source span.
                  -> Text -- ^ Rendered snippet.
renderSpanSnippet source sp =
  case sp of
    NoSpan -> ""
    Span start end ->
      let ls = T.lines source
          sLine = unPos (sourceLine start)
          sCol = unPos (sourceColumn start)
          eLine = unPos (sourceLine end)
          eCol = unPos (sourceColumn end)
          getLine n =
            if n > 0 && n <= length ls then ls !! (n - 1) else ""
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
    _ -> error "rendering requires RenderCache and FSM"

-- | Require parser morphology caches from the context.
requireParserCaches :: RenderM (MorphCache, MorphCache) -- ^ Parser ups/downs caches.
requireParserCaches = do
  ctx <- ask
  case (rcUpsCache ctx, rcDownsCache ctx) of
    (Just ups, Just downs) -> return (ups, downs)
    _ -> error "parser cache access requires morphology caches"

-- | Require an FSM from the context.
requireFsm :: RenderM FSM -- ^ Morphology FSM.
requireFsm = do
  ctx <- ask
  case rcFsm ctx of
    Just fsm -> return fsm
    Nothing -> error "rendering requires FSM"

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
      MsgModuleNotFound name ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) (T.pack (prettyIdent name) <> " modülü bulunamadı.")
            LangEn -> renderError (rcUseColor ctx) ("Module not found: " <> T.pack (prettyIdent name))
      MsgParseError err ->
        Just (renderError (rcUseColor ctx) (renderParseError (rcLang ctx) err))
      MsgRunFailed ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) "Dosya çalıştırılamadı."
            LangEn -> renderError (rcUseColor ctx) "File could not be executed."
      MsgEvalError evalErr ->
        Just $
          case rcLang ctx of
            LangTr -> renderError (rcUseColor ctx) ("Değerleme hatası: " <> T.pack (show evalErr))
            LangEn -> renderError (rcUseColor ctx) ("Evaluation error: " <> T.pack (show evalErr))
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
            LangTr -> renderDefnLine (rcUseColor ctx) (renderNameBold (rcUseColor ctx) (T.pack (prettyIdent name)) <> " tanımı tanımlandı.")
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
          MsgFuncAdded name args isGerund paramTyCons tyMods -> do
            (cache, fsm) <- requireCacheFsm
            (sigArgs, sigName) <- liftIO (renderFunctionSignatureParts cache fsm paramTyCons tyMods isGerund name args)
            let argStrs =
                  [ T.concat ["(", T.pack argName, " ", colorizeTyParts (rcUseColor ctx) tyParts, ")"]
                  | (argName, tyParts) <- sigArgs
                  ]
                base = T.intercalate " " (argStrs ++ [renderNameBold (rcUseColor ctx) (T.pack sigName)])
                suffix =
                  case rcLang ctx of
                    LangTr -> " tanımlandı."
                    LangEn -> " defined."
            return (renderDefnLine (rcUseColor ctx) (base <> suffix))
          MsgFuncLoaded name args isGerund paramTyCons tyMods -> do
            (cache, fsm) <- requireCacheFsm
            (sigArgs, sigName) <- liftIO (renderFunctionSignatureParts cache fsm paramTyCons tyMods isGerund name args)
            let argStrs =
                  [ T.concat ["(", T.pack argName, " ", colorizeTyParts (rcUseColor ctx) tyParts, ")"]
                  | (argName, tyParts) <- sigArgs
                  ]
                base = T.intercalate " " (argStrs ++ [renderNameBold (rcUseColor ctx) (T.pack sigName)])
                suffix =
                  case rcLang ctx of
                    LangTr -> " yüklendi."
                    LangEn -> " loaded."
            return (renderDefnLine (rcUseColor ctx) (base <> suffix))
          MsgPrimFuncAdded name args isGerund paramTyCons tyMods -> do
            (cache, fsm) <- requireCacheFsm
            (sigArgs, sigName) <- liftIO (renderFunctionSignatureParts cache fsm paramTyCons tyMods isGerund name args)
            let argStrs =
                  [ T.concat ["(", T.pack argName, " ", colorizeTyParts (rcUseColor ctx) tyParts, ")"]
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
  case lang of
    LangTr ->
      let trBundle = mapParseErrorBundle ParserErrorTr err
      in "Sözdizim hatası:\n" <> T.pack (turkifyParseError (errorBundlePretty trBundle))
    LangEn ->
      let enBundle = mapParseErrorBundle ParserErrorEn err
      in "Syntax error:\n" <> T.pack (errorBundlePretty enBundle)

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

-- | Entry point for CLI modes and REPL.
main :: IO () -- ^ Program entry point.
main = do
  opts <- execParser (info (cliParser <**> helper) (fullDesc <> progDesc "The compiler and interpreter for the Kip programming language"))
  let lang = optLang opts
      useColor = optMode opts == ModeRepl
  trmorphPath <- locateTrmorph lang useColor
  libDir <- locateLibDir lang useColor
  fsm <- fsmReadBinaryFile trmorphPath
  -- Create shared caches that will be used by both parser and renderer
  upsCache <- HT.new
  downsCache <- HT.new
  let renderCache = mkRenderCache upsCache downsCache
      title = T.pack ("Kip " ++ showVersion version)
      moduleDirs = nub (libDir : optIncludeDirs opts)
      showHeader = optMode opts == ModeRepl
      showDefn = optMode opts == ModeRepl || optMode opts == ModeTest
      basicCtx = RenderCtx lang useColor Nothing Nothing Nothing Nothing
      renderCtx = RenderCtx lang useColor (Just renderCache) (Just fsm) (Just upsCache) (Just downsCache)
  case optMode opts of
    ModeTest -> do
      (preludePst, preludeTC, preludeEval, preludeLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm upsCache downsCache) renderCtx
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFile) basicCtx
      _ <- runReaderT (runFiles showDefn showDefn False preludePst preludeTC preludeEval moduleDirs preludeLoaded (optFiles opts)) renderCtx
      exitSuccess
    ModeExec -> do
      (preludePst, preludeTC, preludeEval, preludeLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm upsCache downsCache) renderCtx
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFile) basicCtx
      _ <- runReaderT (runFiles False False False preludePst preludeTC preludeEval moduleDirs preludeLoaded (optFiles opts)) renderCtx
      exitSuccess
    ModeCodegen target -> do
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFile) basicCtx
      case target of
        "js" -> do
          -- Parse and type-check files, collect all statements
          (codegenPst, codegenTC, codegenLoaded) <-
            runReaderT (loadPreludeCodegenState (optNoPrelude opts) moduleDirs fsm upsCache downsCache) renderCtx
          allStmts <- runReaderT (codegenFiles codegenPst codegenTC moduleDirs codegenLoaded (optFiles opts)) renderCtx
          -- Emit JS and print
          TIO.putStrLn (codegenProgram allStmts)
          exitSuccess
        _ ->
          die ("Unknown codegen target: " ++ T.unpack target)
    ModeBuild -> do
      (preludePst, preludeTC, preludeEval, preludeLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm upsCache downsCache) renderCtx
      when (null (optFiles opts)) $
        die . T.unpack =<< runReaderT (render MsgNeedFileOrDir) basicCtx
      buildTargets <- resolveBuildTargets (optFiles opts)
      let extraDirs = nub (concatMap takeDirectories buildTargets)
          buildModuleDirs = nub (moduleDirs ++ extraDirs)
      (preludeBuildPst, preludeBuildTC, preludeBuildEval, preludeBuildLoaded) <-
        runReaderT (loadPreludeState (optNoPrelude opts) buildModuleDirs renderCache fsm upsCache downsCache) renderCtx
      _ <- runReaderT (runFiles False False True preludeBuildPst preludeBuildTC preludeBuildEval buildModuleDirs preludeBuildLoaded buildTargets) renderCtx
      exitSuccess
    ModeRepl ->
      if null (optFiles opts)
        then do
          (preludePst, preludeTC, preludeEval, preludeLoaded) <-
            runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm upsCache downsCache) renderCtx
          emitMsgIO renderCtx (MsgHeader title)
          emitMsgIO renderCtx (MsgSeparator title)
          runInputT defaultSettings (runReaderT (loop (ReplState (parserCtx preludePst) (parserTyParams preludePst) (parserTyCons preludePst) (parserTyMods preludePst) (parserPrimTypes preludePst) preludeTC preludeEval moduleDirs preludeLoaded)) renderCtx)
        else do
          (preludePst, preludeTC, preludeEval, preludeLoaded) <-
            runReaderT (loadPreludeState (optNoPrelude opts) moduleDirs renderCache fsm upsCache downsCache) renderCtx
          when showHeader $ do
            emitMsgIO renderCtx (MsgHeader title)
            emitMsgIO renderCtx (MsgSeparator title)
          rs <- runReaderT (runFiles showDefn showDefn False preludePst preludeTC preludeEval moduleDirs preludeLoaded (optFiles opts)) renderCtx
          when showHeader $
            emitMsgIO renderCtx (MsgSeparator title)
          runInputT defaultSettings (runReaderT (loop rs) renderCtx)
  where
    -- | CLI option parser.
    cliParser :: Parser CliOptions -- ^ CLI option parser.
    cliParser =
        CliOptions
          <$> modeParser
          <*> many (strArgument (metavar "FILE..."))
          <*> many (strOption (short 'I' <> metavar "DIR" <> help "Additional module directory (used by `temeli yükle` etc.)"))
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
              <> help "Codegen target for the given files (e.g. js)"
              ))
        <|> pure ModeRepl
    -- | Locate the morphology FST data file or exit.
    locateTrmorph :: Lang -- ^ Language selection.
                  -> Bool -- ^ Whether to colorize output.
                  -> IO FilePath -- ^ Path to morphology FST.
    locateTrmorph lang useColor = do
      path <- getDataFileName "vendor/trmorph.fst"
      exists <- doesFileExist path
      if exists
        then return path
        else die . T.unpack =<< runReaderT (renderCompilerMsgBasicOrDie MsgTrmorphMissing) (RenderCtx lang useColor Nothing Nothing Nothing Nothing)
    -- | Locate the standard library directory or exit.
    locateLibDir :: Lang -- ^ Language selection.
                 -> Bool -- ^ Whether to colorize output.
                 -> IO FilePath -- ^ Library directory.
    locateLibDir lang useColor = do
      path <- getDataFileName "lib/temel.kip"
      exists <- doesFileExist path
      if exists
        then return (takeDirectory path)
        else die . T.unpack =<< runReaderT (renderCompilerMsgBasicOrDie MsgLibMissing) (RenderCtx lang useColor Nothing Nothing Nothing Nothing)
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
          ctx <- ask
          fsm <- runApp requireFsm
          (uCache, dCache) <- runApp requireParserCaches
          let pst = newParserStateWithCtxAndCaches fsm (replCtx rs) (replTyParams rs) (replTyCons rs) (replTyMods rs) (replPrimTypes rs) uCache dCache
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
                        | (name, args) <- tcFuncSigs (replTCState rs)
                        , name `elem` candidateNames
                        ]
                  if null sigs
                    then inferExprType ctx paramTyCons parsed expr
                    else do
                      (cache, fsm) <- runApp requireCacheFsm
                      let sigs' = reverse sigs
                          sigs'' = nubBy (\(n1, a1) (n2, a2) -> n1 == n2 && a1 == a2) sigs'
                          isGerund = isJust (gerundRoot varName)
                      forM_ sigs'' $ \(name, args) -> do
                        let mRet = lookup (name, map snd args) (tcFuncSigRets (replTCState rs))
                        line <- liftIO (renderReplSig ctx cache fsm paramTyCons (replTyMods rs) isGerund varName name args mRet)
                        lift (outputStrLn (T.unpack line))
                      loop rs
                _ -> inferExprType ctx paramTyCons parsed expr
      | otherwise = do
          fsm <- runApp requireFsm
          (uCache, dCache) <- runApp requireParserCaches
          let pst = newParserStateWithCtxAndCaches fsm (replCtx rs) (replTyParams rs) (replTyCons rs) (replTyMods rs) (replPrimTypes rs) uCache dCache
          -- If input ends with a period, parse as statement; otherwise parse as expression
          if not (null input) && last input == '.'
            then do
              liftIO (parseFromRepl pst (T.pack input)) >>= \case
                Left err -> do
                  emitMsgTCtx (MsgParseError err)
                  loop rs
                Right (stmt, MkParserState _ pctx pty ptycons ptymods pprim _ _) -> do
                  case stmt of
                    Load name -> do
                      let loadPst = newParserStateWithCtxAndCaches fsm (replCtx rs) (replTyParams rs) (replTyCons rs) (replTyMods rs) (replPrimTypes rs) uCache dCache
                      path <- runApp (resolveModulePath (replModuleDirs rs) name)
                      absPath <- liftIO (canonicalizePath path)
                      if Set.member absPath (replLoaded rs)
                        then loop rs
                        else do
                          (pst', tcSt', evalSt', loaded') <- runApp (runFile False False False (replModuleDirs rs) (loadPst, replTCState rs, replEvalState rs, replLoaded rs) path)
                          emitMsgTCtx (MsgLoaded name)
                          loop (rs { replCtx = parserCtx pst'
                                               , replTyParams = parserTyParams pst'
                                               , replTyCons = parserTyCons pst'
                                               , replTyMods = parserTyMods pst'
                                               , replPrimTypes = parserPrimTypes pst'
                                               , replTCState = tcSt'
                                               , replEvalState = evalSt'
                                               , replLoaded = loaded'
                                               })
                    _ -> do
                      let paramTyCons = [name | (name, arity) <- ptycons, arity > 0]
                      liftIO (runTCM (tcStmt stmt) (replTCState rs)) >>= \case
                        Left tcErr -> do
                          emitMsgTCtx (MsgTCError tcErr (Just (T.pack input)) paramTyCons ptymods)
                          loop rs
                        Right (stmt', tcSt) -> do
                          evalReplStmt paramTyCons ptymods (replEvalState rs) stmt' >>= \case
                            Nothing -> loop rs
                            Just evalSt ->
                              loop (rs { replCtx = pctx
                                                   , replTyParams = pty
                                                   , replTyCons = ptycons
                                                   , replTyMods = ptymods
                                                   , replPrimTypes = pprim
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
                          rendered <- liftIO (evalRender (replEvalState rs) (replEvalState rs) result)
                          lift (outputStrLn rendered)
                          loop rs
      where
        -- | Infer and print a type for a REPL expression.
        inferExprType :: RenderCtx -- ^ Render context.
                      -> [Identifier] -- ^ Type parameters for rendering.
                      -> Exp Ann -- ^ Parsed expression.
                      -> String -- ^ Original input string.
                      -> ReplM () -- ^ No result.
        inferExprType ctx paramTyCons parsed expr =
          liftIO (runTCM (tcExp1 parsed >>= inferType) (replTCState rs)) >>= \case
            Left tcErr -> do
              emitMsgTCtx (MsgTCError tcErr (Just (T.pack expr)) paramTyCons (replTyMods rs))
              loop rs
            Right (mty, _) -> do
              case mty of
                Nothing -> emitMsgTCtx MsgTypeInferFailed
                Just ty -> do
                  (cache, fsm) <- runApp requireCacheFsm
                  let tyNom = normalizeTyNom ty
                  tyParts <- liftIO (renderTyParts cache fsm paramTyCons (replTyMods rs) tyNom)
                  emitMsgTCtx (MsgTypeOf tyParts)
              loop rs

        -- | Render a REPL function signature with return type.
        renderReplSig :: RenderCtx -- ^ Render context.
                      -> RenderCache -- ^ Render cache.
                      -> FSM -- ^ Morphology FSM.
                      -> [Identifier] -- ^ Type parameters for rendering.
                      -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                      -> Bool -- ^ Whether the function is a gerund.
                      -> Identifier -- ^ Input name (raw).
                      -> Identifier -- ^ Canonical function name.
                      -> [Arg Ann] -- ^ Argument types.
                      -> Maybe (Ty Ann) -- ^ Optional return type.
                      -> IO Text -- ^ Rendered signature.
        renderReplSig ctx cache fsm paramTyCons tyMods isGerund inputName name args mRet = do
          argParts <- mapM (renderArgParts cache fsm paramTyCons tyMods) args
          let argStrs =
                [ T.concat ["(", T.pack argName, " ", colorizeTyParts (rcUseColor ctx) tyParts, ")"]
                | (argName, tyParts) <- argParts
                ]
          nameStr <-
            if isGerund
              then return (prettyIdent inputName)
              else renderIdentWithCase cache fsm name Nom
          retPart <-
            case mRet of
              Just ty -> do
                tyStr <- renderTy cache fsm paramTyCons tyMods (normalizeTyNom ty)
                tyStr' <- inflectLastWord cache fsm P3s tyStr
                return (Just (T.pack tyStr'))
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
            TyInd ann name -> TyInd (setAnnCase ann Nom) name
            TyVar ann name -> TyVar (setAnnCase ann Nom) name
            Arr ann d i -> Arr (setAnnCase ann Nom) (normalizeTyNom d) (normalizeTyNom i)
            TyApp ann ctor args -> TyApp (setAnnCase ann Nom) (normalizeTyNom ctor) (map normalizeTyNom args)

        -- | Inflect the last word of a type string for signature display.
        inflectLastWord :: RenderCache -- ^ Render cache.
                        -> FSM -- ^ Morphology FSM.
                        -> Case -- ^ Target case.
                        -> String -- ^ Input string.
                        -> IO String -- ^ Inflected string.
        inflectLastWord cache fsm cas s =
          case words s of
            [] -> return s
            ws -> do
              let lastWord = last ws
              inflected <- renderIdentWithCases cache fsm ([], T.pack lastWord) [cas]
              return (unwords (init ws ++ [inflected]))

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
            Function name args retTy _ isGerund ->
              if isExplicitRetTy retTy
                then emitMsgTCtx (MsgFuncLoaded name args isGerund paramTyCons tyMods)
                else emitMsgTCtx (MsgFuncAdded name args isGerund paramTyCons tyMods)
            PrimFunc name args _ isGerund ->
              emitMsgTCtx (MsgPrimFuncAdded name args isGerund paramTyCons tyMods)
            NewType name _ _ ->
              emitMsgTCtx (MsgTypeAdded name)
            PrimType name ->
              emitMsgTCtx (MsgPrimTypeAdded name)
            _ -> return ()
          return (Just evalSt')
    -- | Collect statements from files for code generation (parse, type-check, no eval).
    codegenFiles :: ParserState -- ^ Base parser state.
                 -> TCState -- ^ Base type checker state.
                 -> [FilePath] -- ^ Module search paths.
                 -> Set FilePath -- ^ Already loaded files.
                 -> [FilePath] -- ^ Files to codegen.
                 -> AppM [Stmt Ann] -- ^ Collected statements.
    codegenFiles basePst baseTC moduleDirs loaded files = do
      (_, _, stmts, _) <- foldM' (collectFileStmts moduleDirs) (basePst, baseTC, [], loaded) files
      return stmts

    -- | Collect statements from a single file (recursively handles Load).
    collectFileStmts :: [FilePath] -- ^ Module search paths.
                     -> (ParserState, TCState, [Stmt Ann], Set FilePath) -- ^ Current state.
                     -> FilePath -- ^ File to process.
                     -> AppM (ParserState, TCState, [Stmt Ann], Set FilePath) -- ^ Updated state.
    collectFileStmts moduleDirs (pst, tcSt, accStmts, loaded) path = do
      exists <- liftIO (doesFileExist path)
      unless exists $ do
        msg <- renderMsg (MsgFileNotFound path)
        liftIO (die (T.unpack msg))
      absPath <- liftIO (canonicalizePath path)
      if Set.member absPath loaded
        then return (pst, tcSt, accStmts, loaded)
        else do
          (uCache, dCache) <- requireParserCaches
          (_, fsm) <- requireCacheFsm
          let cachePath = cacheFilePath absPath
          mCached <- liftIO (loadCachedModule cachePath)
          case mCached of
            Just cached -> do
              let loaded' = Set.insert absPath loaded
                  pstCached = fromCachedParserState fsm uCache dCache (cachedParser cached)
                  tcCached = fromCachedTCState (cachedTC cached)
                  stmts = cachedTypedStmts cached
              (_, _, newStmts, loaded'') <-
                foldM' (collectCachedStmt moduleDirs) (pst, tcSt, [], loaded') stmts
              return (pstCached, tcCached, accStmts ++ newStmts, loaded'')
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
                      (pst'', tcSt'', newStmts, loaded') <- foldM' (collectStmt moduleDirs absPath paramTyCons (parserTyMods pst') input)
                        (pst', tcStWithDecls, [], Set.insert absPath loaded) stmts
                      return (pst'', tcSt'', accStmts ++ newStmts, loaded')

    -- | Collect a single statement, recursively loading modules.
    collectStmt :: [FilePath] -- ^ Module search paths.
                -> FilePath -- ^ Current file path.
                -> [Identifier] -- ^ Type parameter names for error messages.
                -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                -> Text -- ^ Source input.
                -> (ParserState, TCState, [Stmt Ann], Set FilePath) -- ^ Current state.
                -> Stmt Ann -- ^ Statement to process.
                -> AppM (ParserState, TCState, [Stmt Ann], Set FilePath) -- ^ Updated state.
    collectStmt moduleDirs currentPath paramTyCons tyMods source (pst, tcSt, accStmts, loaded) stmt =
      case stmt of
        Load name -> do
          path <- resolveModulePath moduleDirs name
          absPath <- liftIO (canonicalizePath path)
          if Set.member absPath loaded
            then return (pst, tcSt, accStmts, loaded)
            else collectFileStmts moduleDirs (pst, tcSt, accStmts, loaded) path
        _ -> do
          -- Type-check the statement
          liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (stmt', tcSt') ->
              return (pst, tcSt', accStmts ++ [stmt'], loaded)

    -- | Collect a cached statement, expanding Load statements without re-typechecking.
    collectCachedStmt :: [FilePath] -- ^ Module search paths.
                      -> (ParserState, TCState, [Stmt Ann], Set FilePath) -- ^ Current state.
                      -> Stmt Ann -- ^ Statement to process.
                      -> AppM (ParserState, TCState, [Stmt Ann], Set FilePath) -- ^ Updated state.
    collectCachedStmt moduleDirs (pst, tcSt, accStmts, loaded) stmt =
      case stmt of
        Load name -> do
          path <- resolveModulePath moduleDirs name
          collectFileStmts moduleDirs (pst, tcSt, accStmts, loaded) path
        _ ->
          return (pst, tcSt, accStmts ++ [stmt], loaded)

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
      return (ReplState (parserCtx pst') (parserTyParams pst') (parserTyCons pst') (parserTyMods pst') (parserPrimTypes pst') tcSt' evalSt' moduleDirs loaded')
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
      absPath <- liftIO (canonicalizePath path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded)
        else do
          (cache, fsm) <- requireCacheFsm
          (uCache, dCache) <- requireParserCaches
          let cachePath = cacheFilePath absPath
          mCached <- liftIO (loadCachedModule cachePath)
          case mCached of
            Just cached -> do
              let loaded' = Set.insert absPath loaded
              if buildOnly
                then return (pst, tcSt, evalSt, loaded')
                else do
                  let pst' = fromCachedParserState fsm uCache dCache (cachedParser cached)
                      tcSt' = tcSt
                      evalSt' = evalSt
                      stmts = cachedStmts cached
                      paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                      source = ""
                      primRefs = collectNonGerundRefs stmts
                  foldM' (runStmt showDefn showLoad buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') primRefs source) (pst', tcSt', evalSt', loaded') stmts
            Nothing -> do
              input <- liftIO (TIO.readFile path)
              liftIO (parseFromFile pst input) >>= \case
                Left err -> do
                  emitMsgIOCtx (MsgParseError err)
                  msg <- renderMsg MsgRunFailed
                  liftIO (die (T.unpack msg))
                Right (stmts, pst') -> do
                  let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                      source = input
                      primRefs = collectNonGerundRefs stmts
                  -- Pre-register forward declarations for all functions and types
                  liftIO (runTCM (registerForwardDecls stmts) tcSt) >>= \case
                    Left tcErr -> do
                      msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons (parserTyMods pst'))
                      liftIO (die (T.unpack msg))
                    Right (_, tcStWithDecls) -> do
                      let startState = (pst', tcStWithDecls, evalSt, Set.insert absPath loaded, [])
                      (pstFinal, tcSt', evalSt', loaded', typedStmts) <-
                        foldM' (runStmtCollect showDefn showLoad buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') primRefs source) startState stmts

                      -- Save to cache
                      let depStmts = [name | Load name <- stmts]
                      depPaths <- mapM (resolveModulePath moduleDirs) depStmts
                      depHashes <- liftIO $ mapM (\p -> do
                        -- Reuse cached hashes when possible to avoid re-reading deps.
                        mDigest <- hashFile p
                        digest <- case mDigest of
                          Just d -> return d
                          Nothing -> hash <$> BS.readFile p
                        mMeta <- getFileMeta p
                        let fallbackSize = maybe 0 fst mMeta
                            (depSize, depMTime) = fromMaybe (fallbackSize, 0) mMeta
                        return (p, digest, depSize, depMTime)) depPaths
                      mCompilerHash <- liftIO getCompilerHash
                      case mCompilerHash of
                        Nothing -> return ()
                        Just compilerHash -> do
                          mSourceMeta <- liftIO (getFileMeta absPath)
                          let sourceBytes = encodeUtf8 input
                              sourceDigest = hash sourceBytes
                              fallbackSourceSize = fromIntegral (BS.length sourceBytes)
                              (srcSize, srcMTime) = fromMaybe (fallbackSourceSize, 0) mSourceMeta
                              meta = CacheMetadata
                                { compilerHash = compilerHash
                                , sourceHash = sourceDigest
                                , sourceSize = srcSize
                                , sourceMTime = srcMTime
                                , dependencies = depHashes
                                }
                              cachedModule = CachedModule
                                { metadata = meta
                                , cachedStmts = stmts
                                , cachedTypedStmts = typedStmts
                                , cachedParser = toCachedParserState pstFinal
                                , cachedTC = toCachedTCState tcSt'
                                , cachedEval = toCachedEvalState evalSt'
                                }
                          liftIO (saveCachedModule cachePath cachedModule)

                      return (pstFinal, tcSt', evalSt', loaded')
    -- | Run a single statement in the context of a file.
    runStmt :: Bool -- ^ Whether to show definitions.
            -> Bool -- ^ Whether to show load messages.
            -> Bool -- ^ Whether to build-only.
            -> [FilePath] -- ^ Module search paths.
            -> FilePath -- ^ Current file path.
            -> [Identifier] -- ^ Type parameters for rendering.
            -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
            -> [Identifier] -- ^ Non-gerund primitive refs.
            -> Text -- ^ Source input.
            -> (ParserState, TCState, EvalState, Set FilePath) -- ^ Current states.
            -> Stmt Ann -- ^ Statement to run.
            -> AppM (ParserState, TCState, EvalState, Set FilePath) -- ^ Updated states.
    runStmt showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs source (pst, tcSt, evalSt, loaded) stmt =
      case stmt of
        Load name -> do
          path <- resolveModulePath moduleDirs name
          absPath <- liftIO (canonicalizePath path)
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
                  Function name args retTy _ isGerund ->
                    if isExplicitRetTy retTy
                      then emitMsgIOCtx (MsgFuncLoaded name args isGerund paramTyCons tyMods)
                      else emitMsgIOCtx (MsgFuncAdded name args isGerund paramTyCons tyMods)
                  PrimFunc name args _ isGerund -> do
                    when (name `elem` primRefs || isWritePrim name) $
                      emitMsgIOCtx (MsgPrimFuncAdded name args isGerund paramTyCons tyMods)
                  NewType name _ _ -> emitMsgIOCtx (MsgTypeAdded name)
                  PrimType name -> emitMsgIOCtx (MsgPrimTypeAdded name)
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

    -- | Run a single statement while collecting type-checked statements for caching.
    runStmtCollect :: Bool -- ^ Whether to show definitions.
                   -> Bool -- ^ Whether to show load messages.
                   -> Bool -- ^ Whether to build-only.
                   -> [FilePath] -- ^ Module search paths.
                   -> FilePath -- ^ Current file path.
                   -> [Identifier] -- ^ Type parameters for rendering.
                   -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                   -> [Identifier] -- ^ Non-gerund primitive refs.
                   -> Text -- ^ Source input.
                   -> (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann]) -- ^ Current states.
                   -> Stmt Ann -- ^ Statement to run.
                   -> AppM (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann]) -- ^ Updated states.
    runStmtCollect showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs source (pst, tcSt, evalSt, loaded, typedAcc) stmt =
      case stmt of
        Load name -> do
          path <- resolveModulePath moduleDirs name
          absPath <- liftIO (canonicalizePath path)
          if Set.member absPath loaded
            then do
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst, tcSt, evalSt, loaded, typedAcc ++ [stmt])
            else do
              (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
              when showLoad $
                emitMsgIOCtx (MsgLoaded name)
              return (pst', tcSt', evalSt', loaded', typedAcc ++ [stmt])
        _ ->
          liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
            Left tcErr -> do
              msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
              liftIO (die (T.unpack msg))
            Right (stmt', tcSt') -> do
              when showDefn $
                case stmt' of
                  Defn name _ _ -> emitMsgIOCtx (MsgDefnAdded name)
                  Function name args retTy _ isGerund ->
                    if isExplicitRetTy retTy
                      then emitMsgIOCtx (MsgFuncLoaded name args isGerund paramTyCons tyMods)
                      else emitMsgIOCtx (MsgFuncAdded name args isGerund paramTyCons tyMods)
                  PrimFunc name args _ isGerund -> do
                    when (name `elem` primRefs || isWritePrim name) $
                      emitMsgIOCtx (MsgPrimFuncAdded name args isGerund paramTyCons tyMods)
                  NewType name _ _ -> emitMsgIOCtx (MsgTypeAdded name)
                  PrimType name -> emitMsgIOCtx (MsgPrimTypeAdded name)
                  _ -> return ()
              if buildOnly
                then
                  case stmt' of
                    ExpStmt _ -> return (pst, tcSt', evalSt, loaded, typedAcc ++ [stmt'])
                    _ ->
                      liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                        Left evalErr -> do
                          msg <- renderMsg (MsgEvalError evalErr)
                          liftIO (die (T.unpack msg))
                        Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded, typedAcc ++ [stmt'])
                else
                  liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                    Left evalErr -> do
                      msg <- renderMsg (MsgEvalError evalErr)
                      liftIO (die (T.unpack msg))
                    Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded, typedAcc ++ [stmt'])

    -- | Collect non-gerund primitive references from statements.
    collectNonGerundRefs :: [Stmt Ann] -- ^ Statements to inspect.
                         -> [Identifier] -- ^ Referenced identifiers.
    collectNonGerundRefs stmts =
      nub (concatMap (stmtRefs []) stmts)
      where
        -- | Collect references from a statement.
        stmtRefs :: [Identifier] -- ^ Bound identifiers.
                 -> Stmt Ann -- ^ Statement to inspect.
                 -> [Identifier] -- ^ Referenced identifiers.
        stmtRefs bound stmt =
          case stmt of
            Defn name _ body ->
              expRefs (name : bound) body
            Function _ args _ clauses _ ->
              concatMap (clauseRefs (map fst args ++ bound)) clauses
            ExpStmt e ->
              expRefs bound e
            _ -> []
        -- | Collect references from a clause.
        clauseRefs :: [Identifier] -- ^ Bound identifiers.
                   -> Clause Ann -- ^ Clause to inspect.
                   -> [Identifier] -- ^ Referenced identifiers.
        clauseRefs bound (Clause _ body) = expRefs bound body
        -- | Collect references from an expression.
        expRefs :: [Identifier] -- ^ Bound identifiers.
                -> Exp Ann -- ^ Expression to inspect.
                -> [Identifier] -- ^ Referenced identifiers.
        expRefs bound exp =
          case exp of
            Var {varCandidates} ->
              if any (\(ident, _) -> ident `elem` bound) varCandidates
                then []
                else map fst varCandidates
            Bind {bindName, bindExp} ->
              expRefs bound bindExp
            App {fn, args} -> expRefs bound fn ++ concatMap (expRefs bound) args
            Match {scrutinee, clauses} ->
              expRefs bound scrutinee ++ concatMap (clauseRefs bound) clauses
            Seq {first, second} ->
              case first of
                Bind {bindName, bindExp} ->
                  expRefs bound bindExp ++ expRefs (bindName : bound) second
                _ -> expRefs bound first ++ expRefs bound second
            Let {varName, body} ->
              expRefs (varName : bound) body
            _ -> []

    -- | Check whether an identifier refers to the write primitive.
    isWritePrim :: Identifier -- ^ Identifier to inspect.
                -> Bool -- ^ True when identifier is `yaz`.
    isWritePrim ident =
      prettyIdent ident == "yaz"

    -- | Resolve a module name to a file path.
    resolveModulePath :: [FilePath] -- ^ Module search paths.
                      -> Identifier -- ^ Module identifier.
                      -> AppM FilePath -- ^ Resolved file path.
    resolveModulePath dirs name = do
      let base = prettyIdent name ++ ".kip"
          candidates = map (</> base) dirs
      found <- liftIO (filterM doesFileExist candidates)
      case found of
        path:_ -> return path
        [] -> do
          msg <- renderMsg (MsgModuleNotFound name)
          liftIO (die (T.unpack msg))

    -- | Resolve build targets from file or directory inputs.
    resolveBuildTargets :: [FilePath] -- ^ Input paths.
                        -> IO [FilePath] -- ^ `.kip` files to build.
    resolveBuildTargets paths = fmap nub (concat <$> mapM expandPath paths)
      where
        -- | Expand a directory into .kip files or keep a file path.
        expandPath :: FilePath -- ^ Input path.
                   -> IO [FilePath] -- ^ Expanded paths.
        expandPath p = do
          isDir <- doesDirectoryExist p
          if isDir
            then listKipFilesRecursive p
            else return [p]
    -- | Recursively list .kip files in a directory tree.
    listKipFilesRecursive :: FilePath -- ^ Directory to scan.
                          -> IO [FilePath] -- ^ `.kip` files.
    listKipFilesRecursive dir = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \entry -> do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir
          then listKipFilesRecursive path
          else return [path | takeExtension path == ".kip"]
    -- | Return parent directories for a path.
    takeDirectories :: FilePath -- ^ Path to split.
                    -> [FilePath] -- ^ Parent directories.
    takeDirectories path = [takeDirectory path]

    -- | Load the prelude module into a parser state.
    loadPreludeParserState :: [FilePath] -- ^ Module search paths.
                           -> MorphCache -- ^ Shared ups cache.
                           -> MorphCache -- ^ Shared downs cache.
                           -> FSM -- ^ Morphology FSM.
                           -> AppM ParserState -- ^ Loaded parser state.
    loadPreludeParserState moduleDirs uCache dCache fsm = do
      let pst = newParserStateWithCaches fsm uCache dCache
      path <- resolveModulePath moduleDirs ([], T.pack "temel")
      absPath <- liftIO (canonicalizePath path)
      let cachePath = cacheFilePath absPath
      liftIO (loadCachedModule cachePath) >>= \case
        Just cached -> return (fromCachedParserState fsm uCache dCache (cachedParser cached))
        Nothing -> do
          input <- liftIO (TIO.readFile path)
          liftIO (parseFromFile pst input) >>= \case
            Left _ -> return pst
            Right (_, pst') -> return pst'

    -- | Load the prelude module for code generation (no eval, cached when possible).
    loadPreludeCodegenState :: Bool -- ^ Whether to skip the prelude.
                            -> [FilePath] -- ^ Module search paths.
                            -> FSM -- ^ Morphology FSM.
                            -> MorphCache -- ^ Shared ups cache.
                            -> MorphCache -- ^ Shared downs cache.
                            -> AppM (ParserState, TCState, Set FilePath) -- ^ Loaded parser/TC states.
    loadPreludeCodegenState noPrelude moduleDirs fsm uCache dCache = do
      let pst = newParserStateWithCaches fsm uCache dCache
          tcSt = emptyTCState
      if noPrelude
        then return (pst, tcSt, Set.empty)
        else do
          path <- resolveModulePath moduleDirs ([], T.pack "giriş")
          absPath <- liftIO (canonicalizePath path)
          let cachePath = cacheFilePath absPath
          liftIO (loadCachedModule cachePath) >>= \case
            Just cached -> do
              depPaths <- liftIO $ mapM canonicalizePath (map (\(p, _, _, _) -> p) (dependencies (metadata cached)))
              let pst' = fromCachedParserState fsm uCache dCache (cachedParser cached)
                  tcSt' = fromCachedTCState (cachedTC cached)
                  loaded = Set.fromList (absPath : depPaths)
              return (pst', tcSt', loaded)
            Nothing -> do
              (pst', tcSt', _, loaded') <- collectFileStmts moduleDirs (pst, tcSt, [], Set.empty) path
              return (pst', tcSt', loaded')

    -- | Load the prelude module into parser/type/eval states unless disabled.
    loadPreludeState :: Bool -- ^ Whether to skip the prelude.
                     -> [FilePath] -- ^ Module search paths.
                     -> RenderCache -- ^ Render cache.
                     -> FSM -- ^ Morphology FSM.
                     -> MorphCache -- ^ Shared ups cache.
                     -> MorphCache -- ^ Shared downs cache.
                     -> AppM (ParserState, TCState, EvalState, Set FilePath) -- ^ Loaded states.
    loadPreludeState noPrelude moduleDirs cache fsm uCache dCache = do
      let pst = newParserStateWithCaches fsm uCache dCache
          tcSt = emptyTCState
          evalSt = mkEvalState cache fsm
      if noPrelude
        then return (pst, tcSt, evalSt, Set.empty)
        else do
          path <- resolveModulePath moduleDirs ([], T.pack "giriş")
          runFile False False False moduleDirs (pst, tcSt, evalSt, Set.empty) path

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
