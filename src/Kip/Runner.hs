{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Shared runner functionality for Kip CLI and Playground.
module Kip.Runner
  ( -- * Types
    Lang(..)
  , parseLang
  , RenderCtx(..)
  , ReplState(..)
  , CompilerState
  , CompilerMsg(..)
  , RenderM
    -- * Error rendering
  , ParserErrorTr(..)
  , ParserErrorEn(..)
  , ParseErrorRenderTarget(..)
  , renderMsg
  , renderParseError
  , renderParseErrorFor
  , renderEvalError
  , renderTCError
  , renderTCErrorWithSource
  , renderSpan
  , renderSpanSnippet
  , renderLocatedSpanSnippet
  , renderTyOpt
  , effectBoundaryHint
  , tcErrSpan
  , tcErrRelatedSpan
  , sameSpanPath
  , findPatternBinderRepeatedError
  , findUnrecognizedWordError
  , findAmbiguousBareApplicationError
  , requireCacheFsm
    -- * File running
  , runFiles
  , runFile
  , runStmt
  , loadPreludeState
  , loadPreludeStateWithMode
  , mkEvalState
  , resolveModulePath
  , resolveBuildTargets
  , listKipFilesRecursive
  , listKipFilesRecursiveSkipping
  , collectNonInfinitiveRefs
  , mergeTCState
  , locateDataFile
    -- * Utilities
  , foldM'
  , mapParseErrorBundle
  , turkifyParseError
  , replace
  , splitOn
  , breakOn
  , uniquePreserve
  ) where

import Control.Monad (forM, unless, filterM, foldM)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.List (intercalate, isPrefixOf, tails, findIndex, foldl')
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getExecutablePath, lookupEnv)
import System.Exit (die)
import System.FilePath ((</>), joinPath, takeDirectory, takeExtension)
import Text.Megaparsec (ParseErrorBundle(..), PosState(..), errorBundlePretty)
import Text.Megaparsec.Error (ParseError(..), ErrorFancy(..), ShowErrorComponent(..))
import Text.Megaparsec.Pos (sourceLine, sourceColumn, unPos)
import qualified Data.List.NonEmpty as NE
import Paths_kip (getDataFileName)

import Language.Foma
import Kip.AST
import Kip.Cache
import Kip.MorphCache (beginMorphTracking, finishMorphTracking)
import Kip.Eval (EvalState, EvalM, EvalError, emptyEvalState, runEvalM, evalExp, evalStmtInFile, evalRender)
import qualified Kip.Eval as Eval
import Kip.Parser
import Kip.Render
import Kip.TypeCheck
import qualified Kip.TypeCheck as TC
import Kip.Util (stableNub)

-- | Diagnostic language selection.
data Lang
  = LangTr
  | LangEn
  deriving (Eq, Show)

-- | Parse a diagnostic language flag.
parseLang :: String -> Either String Lang
parseLang "tr" = Right LangTr
parseLang "en" = Right LangEn
parseLang _ = Left "LANG must be 'tr' or 'en'"

-- | Rendering context for diagnostics.
data RenderCtx =
  RenderCtx
    { rcLang :: Lang
    , rcCache :: RenderCache
    , rcFsm :: FSM
    }

-- | Parser, typechecker, evaluator, and loaded-module state for file execution.
type CompilerState = (ParserState, TCState, EvalState, Set FilePath)

-- | REPL runtime state (parser/type context + evaluator).
data ReplState =
  ReplState
    { replCtx :: Set.Set Identifier
    , replCtors :: [Identifier]
    , replTyParams :: [Identifier]
    , replTyCons :: [(Identifier, Int)]
    , replTyMods :: [(Identifier, [Identifier])]
    , replPrimTypes :: [Identifier]
    , replFuncArities :: Map.Map Identifier (Set.Set Int)
    , replTCState :: TCState
    , replEvalState :: EvalState
    , replModuleDirs :: [FilePath]
    , replLoaded :: Set FilePath
    }

-- | Messages emitted by the runner.
data CompilerMsg
  = MsgNeedFile
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

-- | Output target for parse error rendering.
data ParseErrorRenderTarget
  = ParseErrorForCli
  | ParseErrorForLsp
  deriving (Eq, Show)

{- | Render evaluation errors with proper localization.

Evaluation errors occur at runtime and include:
- UnboundVariable: Variable not found in any namespace
- NoMatchingFunction: Function call with no matching overload
- NoMatchingClause: Pattern match with no matching clause
- Unknown: Unexpected evaluation failure
-}
renderEvalError :: Lang -> EvalError -> Text
renderEvalError lang evalErr =
  case lang of
    LangTr ->
      case evalErr of
        Eval.Unknown -> "Değerleme hatası: bilinmeyen hata."
        Eval.UnboundVariable name -> "Değerleme hatası: " <> T.pack (prettyIdent name) <> " tanımlı değil."
        Eval.NoMatchingFunction name -> "Değerleme hatası: " <> T.pack (prettyIdent name) <> " için uygun bir tanım bulunamadı."
        Eval.NoMatchingClause -> "Değerleme hatası: eşleşen bir dal bulunamadı."
        Eval.RuntimeTypeErrorNonValue -> "Değerleme hatası: sonuç bir değer değil (çalışma zamanı tip hatası)."
    LangEn ->
      case evalErr of
        Eval.Unknown -> "Evaluation error: unknown error."
        Eval.UnboundVariable name -> "Evaluation error: " <> T.pack (prettyIdent name) <> " is not defined."
        Eval.NoMatchingFunction name -> "Evaluation error: no matching definition found for " <> T.pack (prettyIdent name) <> "."
        Eval.NoMatchingClause -> "Evaluation error: no matching clause found."
        Eval.RuntimeTypeErrorNonValue -> "Evaluation error: result is not a value (runtime type error)."

{- |
Resolve a packaged data file path from runtime candidates.

Lookup order:

1. @KIP_DATADIR/<rel>@
2. Cabal data-files path via 'getDataFileName'
3. Relative to the executable directory
4. Relative to the executable parent directory
5. Relative path in current working directory
-}
locateDataFile :: FilePath -> IO FilePath
locateDataFile rel = do
  mEnv <- lookupEnv "KIP_DATADIR"
  cabalPath <- getDataFileName rel
  exePath <- getExecutablePath
  let exeDir = takeDirectory exePath
      parentDir = takeDirectory exeDir
      envPaths = maybe [] (\base -> [base </> rel]) mEnv
      candidates =
        uniquePreserve
          ( envPaths
            ++ [ cabalPath
               , exeDir </> rel
               , parentDir </> rel
               , rel
               ]
          )
  found <- filterM doesFileExist candidates
  case found of
    p:_ -> return p
    [] -> return cabalPath

-- | Render a compiler message to text.
renderMsg :: CompilerMsg -> RenderM Text
renderMsg msg = do
  ctx <- ask
  case msg of
    MsgNeedFile ->
      return $
        case rcLang ctx of
          LangTr -> "En az bir dosya bekleniyor."
          LangEn -> "Expected at least one file."
    MsgNeedFileOrDir ->
      return $
        case rcLang ctx of
          LangTr -> "En az bir dosya veya dizin bekleniyor."
          LangEn -> "Expected at least one file or directory."
    MsgTrmorphMissing ->
      return $
        case rcLang ctx of
          LangTr -> "vendor/trmorph.fst bulunamadı."
          LangEn -> "vendor/trmorph.fst not found."
    MsgLibMissing ->
      return $
        case rcLang ctx of
          LangTr -> "lib/temel.kip bulunamadı."
          LangEn -> "lib/temel.kip not found."
    MsgFileNotFound path ->
      return $
        case rcLang ctx of
          LangTr -> "Dosya bulunamadı: " <> T.pack path
          LangEn -> "File not found: " <> T.pack path
    MsgModuleNotFound dirPath name ->
      let prefix = if null dirPath then "" else T.intercalate "/" dirPath <> "/"
      in return $
        case rcLang ctx of
          LangTr -> prefix <> T.pack (prettyIdent name) <> " modülü bulunamadı."
          LangEn -> "Module not found: " <> prefix <> T.pack (prettyIdent name)
    MsgUnknownCodegenTarget target ->
      return $
        case rcLang ctx of
          LangTr -> "Bilinmeyen kod üretim hedefi: " <> target
          LangEn -> "Unknown codegen target: " <> target
    MsgParseError err ->
      return (renderParseError (rcLang ctx) err)
    MsgRunFailed ->
      return $
        case rcLang ctx of
          LangTr -> "Dosya çalıştırılamadı."
          LangEn -> "File could not be executed."
    MsgEvalError evalErr ->
      return $ renderEvalError (rcLang ctx) evalErr
    MsgTCError tcErr mSource paramTyCons tyMods ->
      case mSource of
        Nothing -> renderTCError paramTyCons tyMods tcErr
        Just source -> renderTCErrorWithSource paramTyCons tyMods source tcErr

-- | Emit a message using a concrete render context in IO.
emitMsgIO :: RenderCtx -> CompilerMsg -> IO ()
emitMsgIO ctx msg = do
  rendered <- runReaderT (renderMsg msg) ctx
  TIO.putStrLn rendered

-- | Render a parse error bundle in the requested language.
renderParseError :: Lang -> ParseErrorBundle Text ParserError -> Text
renderParseError = renderParseErrorFor ParseErrorForCli

-- | Render a parse error bundle for a concrete output target.
renderParseErrorFor :: ParseErrorRenderTarget -> Lang -> ParseErrorBundle Text ParserError -> Text
renderParseErrorFor target lang err =
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
      in case target of
           ParseErrorForCli -> header <> renderSpanSnippet source sp <> "\n" <> msg
           ParseErrorForLsp -> header <> msg
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
          in case target of
               ParseErrorForCli -> header <> renderSpanSnippet source sp <> "\n" <> msg
               ParseErrorForLsp -> header <> msg
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
              in case target of
                   ParseErrorForCli -> header <> renderLocatedSpanSnippet "Kip" source sp <> "\n" <> msg
                   ParseErrorForLsp -> header <> msg
            Nothing ->
              case lang of
                LangTr ->
                  let trBundle = mapParseErrorBundle ParserErrorTr err
                      pretty = T.pack (turkifyParseError (errorBundlePretty trBundle))
                  in "Sözdizim hatası:\n" <> compactPretty target pretty
                LangEn ->
                  let enBundle = mapParseErrorBundle ParserErrorEn err
                      pretty = T.pack (errorBundlePretty enBundle)
                  in "Syntax error:\n" <> compactPretty target pretty

-- | Remove location/snippet gutter emitted by Megaparsec pretty printer.
compactPretty :: ParseErrorRenderTarget -> Text -> Text
compactPretty target txt =
  case target of
    ParseErrorForCli -> txt
    ParseErrorForLsp ->
      T.intercalate "\n" (filter (not . isSnippetLine) (T.lines txt))
  where
    isSnippetLine ln =
      let s = T.strip ln
      in isLocationLine s || isGutterLine s || isCodeLine s || isCaretLine s
    isLocationLine s =
      case T.breakOnEnd ":" s of
        ("", _) -> False
        _ ->
          case reverse (T.splitOn ":" (T.dropWhileEnd (== ':') s)) of
            colTxt:lineTxt:_ ->
              T.all isDigit colTxt && T.all isDigit lineTxt
            _ -> False
    isGutterLine s = s == "|"
    isCodeLine s =
      case T.breakOn "|" s of
        (lhs, rhs) ->
          not (T.null rhs) && T.all (\c -> isDigit c || c == ' ') (T.strip lhs)
    isCaretLine s =
      case T.breakOn "|" s of
        (_, rhs) | T.null rhs -> False
        (_, rhs) ->
          let marker = T.strip (T.drop 1 rhs)
          in not (T.null marker) && T.all (== '^') marker

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
                    => (e -> e') -> ParseErrorBundle s e -> ParseErrorBundle s e'
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
        ErrorFail m -> ErrorFail m
        ErrorIndentation o r lvl -> ErrorIndentation o r lvl

-- | Translate parse error text into Turkish labels.
turkifyParseError :: String -> String
turkifyParseError =
  replace "unexpected end of input" "beklenmeyen girişin sonu"
  . replace "unexpected" "beklenmeyen"
  . replace "expecting" "bekleniyor"
  . replace "end of input" "girişin sonu"
  . replace "line" "satır"
  . replace "column" "sütun"

-- | Replace all occurrences of a substring.
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

-- | Split a string on a substring.
splitOn :: String -> String -> [String]
splitOn pat s =
  case breakOn pat s of
    Nothing -> [s]
    Just (before, after) -> before : splitOn pat after

-- | Break a string on the first occurrence of a substring.
breakOn :: String -> String -> Maybe (String, String)
breakOn pat s =
  case findIndex (isPrefixOf pat) (tails s) of
    Nothing -> Nothing
    Just idx ->
      let (before, rest) = splitAt idx s
          after = drop (length pat) rest
      in Just (before, after)

-- | Render a type checker error without source context.
renderTCError :: [Identifier] -> [(Identifier, [Identifier])] -> TCError -> RenderM Text
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
renderTCErrorWithSource :: [Identifier] -> [(Identifier, [Identifier])] -> Text -> TCError -> RenderM Text
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
effectBoundaryHint :: Lang -> Text
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
tcErrSpan :: TCError -> Maybe Span
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
tcErrRelatedSpan :: TCError -> Maybe Span
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
sameSpanPath :: Span -> Span -> Bool
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
renderSpanSnippet :: Text -> Span -> Text
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
renderLocatedSpanSnippet :: Text -> Text -> Span -> Text
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

-- | Render a span into human-readable text.
renderSpan :: Lang -> Span -> Text
renderSpan lang sp =
  case sp of
    NoSpan -> ""
    Span start end path ->
      case path of
        Nothing ->
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
        Just p ->
          "\n" <> T.pack p <> ":" <> T.pack (show (unPos (sourceLine start))) <> ":" <> T.pack (show (unPos (sourceColumn start))) <> "-" <> T.pack (show (unPos (sourceLine end))) <> ":" <> T.pack (show (unPos (sourceColumn end)))

-- | Render an optional type for diagnostics.
renderTyOpt :: [Identifier] -> [(Identifier, [Identifier])] -> Maybe (Ty Ann) -> RenderM Text
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
requireCacheFsm :: RenderM (RenderCache, FSM)
requireCacheFsm = do
  ctx <- ask
  return (rcCache ctx, rcFsm ctx)

-- | Run multiple files through parsing, type checking, and evaluation.
runFiles :: Bool -> ParserState -> TCState -> EvalState -> [FilePath] -> Set FilePath -> [FilePath] -> RenderM ReplState
runFiles buildOnly basePst baseTC baseEval moduleDirs loaded files = do
  (pst', tcSt', evalSt', loaded') <- foldM' (runFile buildOnly moduleDirs) (basePst, baseTC, baseEval, loaded) files
  return (ReplState (parserCtx pst') (parserCtors pst') (parserTyParams pst') (parserTyCons pst') (parserTyMods pst') (parserPrimTypes pst') (parserFuncArities pst') tcSt' evalSt' moduleDirs loaded')

-- | Run a single file and update all states.
runFile :: Bool -> [FilePath] -> CompilerState -> FilePath -> RenderM CompilerState
runFile buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path = do
  exists <- liftIO (doesFileExist path)
  unless exists $ do
    ctx <- ask
    liftIO (emitMsgIO ctx (MsgFileNotFound path))
    msg <- renderMsg MsgRunFailed
    liftIO (die (T.unpack msg))
  absPath <- liftIO (canonicalizePathCached path)
  if Set.member absPath loaded
    then return (pst, tcSt, evalSt, loaded)
    else do
      ctx <- ask
      let cache = rcCache ctx
          fsm = rcFsm ctx
      let cachePath = cacheFilePath absPath
      mCachedRaw <- liftIO (loadCachedModule cachePath)
      let mCached =
            case mCachedRaw of
              Just cached
                | tcOutputModeSupports
                    (tcOutputMode (fromCachedTCState (cachedTC cached)))
                    (tcOutputMode tcSt) -> Just cached
              _ -> Nothing
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
              foldM' (runTypedStmt buildOnly moduleDirs absPath) (pst', tcSt', evalSt', loaded') stmts
        Nothing -> do
          morphToken <- liftIO beginMorphTracking
          input <- liftIO (TIO.readFile path)
          liftIO (parseFromFile pst { parserFilePath = Just path } input) >>= \case
            Left err -> do
              liftIO (emitMsgIO ctx (MsgParseError err))
              msg <- renderMsg MsgRunFailed
              liftIO (die (T.unpack msg))
            Right (stmts, pst') -> do
              let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                  source = input
              liftIO (runTCM (registerForwardDecls stmts) tcSt) >>= \case
                Left tcErr -> do
                  msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons (parserTyMods pst'))
                  liftIO (die (T.unpack msg))
                Right (_, tcStWithDecls) -> do
                  let defSpansRaw = defSpansFromStmts stmts (parserDefSpans pst')
                      sigSpans = funcSigSpansFromStmts stmts (parserDefSpans pst')
                  tcStWithDefs <- liftIO $ runTCM (recordDefLocations absPath defSpansRaw >> recordFuncSigLocations absPath sigSpans) tcStWithDecls >>= \case
                    Left _ -> return tcStWithDecls
                    Right (_, tcStDefs) -> return tcStDefs
                  let startState = (pst', tcStWithDefs, evalSt, Set.insert absPath loaded, [], [])
                  (pstFinal, tcSt', evalSt', loaded', typedStmtsRev, depPathsRawRev) <-
                    foldM' (runStmtCollect buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') source) startState stmts
                  let typedStmts = reverse typedStmtsRev
                      depPathsRaw = reverse depPathsRawRev
                  depPaths <- liftIO (uniquePreserve <$> mapM canonicalizePathCached depPathsRaw)
                  depHashes <- liftIO (mapM fileFingerprintOrHash depPaths)
                  morphDelta <- liftIO (finishMorphTracking morphToken)
                  mMeta <- liftIO (buildCacheMetadata absPath input depHashes)
                  case mMeta of
                    Nothing -> return ()
                    Just meta -> do
                      cachedParserBase <- liftIO (toCachedParserStateDelta pst pst')
                      let cachedParserState = attachMorphDelta morphDelta cachedParserBase
                      let cachedModule = CachedModule
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
mergeTCState :: TCState -> TCState -> TCState
mergeTCState = mergeCachedTCState

-- | Evaluate a file statement, omitting expression statements in build mode.
evalFileStmt :: Bool -> FilePath -> EvalState -> Stmt Ann -> RenderM EvalState
evalFileStmt buildOnly currentPath evalSt stmt =
  case stmt of
    ExpStmt _ | buildOnly -> return evalSt
    _ ->
      liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt) evalSt) >>= \case
        Left evalErr -> do
          msg <- renderMsg (MsgEvalError evalErr)
          liftIO (die (T.unpack msg))
        Right (_, evalSt') -> return evalSt'

-- | Run a single statement in the context of a file.
runStmt :: Bool -> [FilePath] -> FilePath -> [Identifier] -> [(Identifier, [Identifier])] -> Text -> CompilerState -> Stmt Ann -> RenderM CompilerState
runStmt buildOnly moduleDirs currentPath paramTyCons tyMods source (pst, tcSt, evalSt, loaded) stmt =
  case stmt of
    Load dirPath name -> do
      path <- resolveModulePath moduleDirs dirPath name
      absPath <- liftIO (canonicalizePathCached path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded)
        else do
          (pst', tcSt', evalSt', loaded') <- runFile buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
          return (pst', tcSt', evalSt', loaded')
    _ ->
      liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
        Left tcErr -> do
          msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
          liftIO (die (T.unpack msg))
        Right (stmt', tcSt') -> do
          evalSt' <- evalFileStmt buildOnly currentPath evalSt stmt'
          return (pst, tcSt', evalSt', loaded)

-- | Run a pre-typechecked statement in the context of a file.
--
-- This path is used when a valid module cache is loaded. It avoids
-- re-running 'tcStmt' and any forward-declaration pre-pass by assuming the
-- incoming statement list is already type-checked ('cachedTypedStmts').
runTypedStmt :: Bool -> [FilePath] -> FilePath -> CompilerState -> Stmt Ann -> RenderM CompilerState
runTypedStmt buildOnly moduleDirs currentPath (pst, tcSt, evalSt, loaded) stmt =
  case stmt of
    Load dirPath name -> do
      path <- resolveModulePath moduleDirs dirPath name
      absPath <- liftIO (canonicalizePathCached path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded)
        else do
          (pst', tcSt', evalSt', loaded') <- runFile buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
          return (pst', tcSt', evalSt', loaded')
    _ -> do
      evalSt' <- evalFileStmt buildOnly currentPath evalSt stmt
      return (pst, tcSt, evalSt', loaded)

-- | Run a single statement while collecting type-checked statements for caching.
--
-- Both typed statements and dependency paths are accumulated in reverse and
-- normalized once per file, avoiding repeated append allocation.
runStmtCollect :: Bool -> [FilePath] -> FilePath -> [Identifier] -> [(Identifier, [Identifier])] -> Text -> (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann], [FilePath]) -> Stmt Ann -> RenderM (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann], [FilePath])
runStmtCollect buildOnly moduleDirs currentPath paramTyCons tyMods source (pst, tcSt, evalSt, loaded, typedAcc, depPathsAcc) stmt =
  case stmt of
    Load dirPath name -> do
      path <- resolveModulePath moduleDirs dirPath name
      absPath <- liftIO (canonicalizePathCached path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded, stmt : typedAcc, path : depPathsAcc)
        else do
          (pst', tcSt', evalSt', loaded') <- runFile buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
          return (pst', tcSt', evalSt', loaded', stmt : typedAcc, path : depPathsAcc)
    _ ->
      liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
        Left tcErr -> do
          msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
          liftIO (die (T.unpack msg))
        Right (stmt', tcSt') -> do
          evalSt' <- evalFileStmt buildOnly currentPath evalSt stmt'
          return (pst, tcSt', evalSt', loaded, stmt' : typedAcc, depPathsAcc)

-- | Collect non-infinitive primitive references from statements.
--
-- Uses a strict set accumulator through the AST walk instead of list
-- concatenation and end-of-pass dedupe.
collectNonInfinitiveRefs :: [Stmt Ann] -> [Identifier]
collectNonInfinitiveRefs stmts =
  Set.toList (foldl' stmtRefs Set.empty stmts)
  where
    stmtRefs :: Set Identifier -> Stmt Ann -> Set Identifier
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
    clauseRefs :: Set Identifier -> Set Identifier -> Clause Ann -> Set Identifier
    clauseRefs bound acc (Clause _ body) = expRefs bound body acc
    expRefs :: Set Identifier -> Exp Ann -> Set Identifier -> Set Identifier
    expRefs bound expr acc =
      case expr of
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

-- | Resolve a module name to a file path.
resolveModulePath :: [FilePath] -> [Text] -> Identifier -> RenderM FilePath
resolveModulePath dirs dirPath name@(xs, x) = do
  let dirComponents = map T.unpack dirPath
      parts = map T.unpack xs
      nm = T.unpack x
      fileName = intercalate "-" (parts ++ [nm]) ++ ".kip"
      relPath = joinPath (dirComponents ++ [fileName])
      candidates = map (</> relPath) dirs
  found <- liftIO (filterM doesFileExist candidates)
  case found of
    path:_ -> liftIO (canonicalizePathCached path)
    [] -> do
      msg <- renderMsg (MsgModuleNotFound dirPath name)
      liftIO (die (T.unpack msg))

-- | Resolve build targets from file or directory inputs.
--
-- Performs path expansion and uniqueness filtering in one fold, avoiding
-- temporary flattened lists and quadratic @nub@ work.
resolveBuildTargets :: [FilePath] -> IO [FilePath]
resolveBuildTargets paths = do
  (_, accRev) <- foldM collectPath (Set.empty, []) paths
  return (reverse accRev)
  where
    collectPath :: (Set FilePath, [FilePath]) -> FilePath -> IO (Set FilePath, [FilePath])
    collectPath (seen, accRev) p = do
      expanded <- expandPath p
      return (foldl' insertUnique (seen, accRev) expanded)

    insertUnique :: (Set FilePath, [FilePath]) -> FilePath -> (Set FilePath, [FilePath])
    insertUnique (seen, accRev) p
      | Set.member p seen = (seen, accRev)
      | otherwise = (Set.insert p seen, p : accRev)

    expandPath :: FilePath -> IO [FilePath]
    expandPath p = do
      isDir <- doesDirectoryExist p
      if isDir
        then listKipFilesRecursive p
        else return [p]

-- | Recursively list .kip files in a directory tree.
listKipFilesRecursive :: FilePath -> IO [FilePath]
listKipFilesRecursive = listKipFilesRecursiveSkipping Set.empty

-- | Recursively list @.kip@ files while ignoring named directories.
listKipFilesRecursiveSkipping :: Set FilePath -> FilePath -> IO [FilePath]
listKipFilesRecursiveSkipping skipped root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then return []
    else go root
  where
    go dir = do
      entries <- listDirectory dir
      fmap concat $ forM entries $ \entry -> do
        let path = dir </> entry
        childIsDir <- doesDirectoryExist path
        if childIsDir
          then if Set.member entry skipped then return [] else go path
          else return [path | takeExtension path == ".kip"]

-- | Remove duplicates while preserving first occurrence order.
--
-- Set-backed dedupe preserves deterministic output ordering while avoiding
-- the O(n^2) behavior of repeated list scans.
uniquePreserve :: Ord a => [a] -> [a]
uniquePreserve = stableNub

-- | Load the prelude module into parser/type/eval states unless disabled.
loadPreludeState :: Bool -> [FilePath] -> RenderCache -> FSM -> RenderM CompilerState
loadPreludeState = loadPreludeStateWithMode TCOutputRuntime

-- | Load the prelude with resolution output appropriate for the consumer.
loadPreludeStateWithMode :: TCOutputMode -> Bool -> [FilePath] -> RenderCache -> FSM -> RenderM CompilerState
loadPreludeStateWithMode outputMode noPrelude moduleDirs cache fsm = do
  let pst = newParserStateWithCaches fsm Nothing cache
      tcSt = setTCOutputMode outputMode emptyTCState
      evalSt = mkEvalState cache fsm
  if noPrelude
    then return (pst, tcSt, evalSt, Set.empty)
    else do
      snapshotPath <- liftIO preludeSnapshotPath
      -- Restore the merged prelude graph from a validated snapshot when
      -- possible; otherwise load and persist it for future startup runs.
      mSnapshot <- liftIO (loadCachedPrelude snapshotPath cache fsm)
      case mSnapshot of
        Just (pstSnap, tcSnap, evalSnap, loadedSnap)
          | tcOutputModeSupports (tcOutputMode tcSnap) outputMode ->
              return (pstSnap, setTCOutputMode outputMode tcSnap, evalSnap, loadedSnap)
        _ -> do
          path <- resolveModulePath moduleDirs [] ([], T.pack "giriş")
          let pst' = pst { parserFilePath = Just path }
          state'@(pstLoaded, tcLoaded, evalLoaded, loaded') <-
            runFile False moduleDirs (pst', tcSt, evalSt, Set.empty) path
          liftIO (saveCachedPrelude snapshotPath pstLoaded tcLoaded evalLoaded loaded')
          return state'

-- | Build an evaluator state wired to the render cache.
mkEvalState :: RenderCache -> FSM -> EvalState
mkEvalState cache fsm =
  emptyEvalState { evalRender = renderExpValue cache fsm }

-- | Strict monadic left fold to avoid building thunks on large inputs.
foldM' :: forall m b a.
          Monad m
       => (b -> a -> m b)
       -> b
       -> [a]
       -> m b
foldM' f = go
  where
    go :: b -> [a] -> m b
    go acc [] = return acc
    go acc (y:ys) = do
      acc' <- f acc y
      acc' `seq` go acc' ys
