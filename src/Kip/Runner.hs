{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Shared runner functionality for Kip CLI and Playground.
module Kip.Runner
  ( -- * Types
    Lang(..)
  , RenderCtx(..)
  , ReplState(..)
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
  , renderTyOpt
  , tcErrSpan
  , requireCacheFsm
    -- * File running
  , runFiles
  , runFile
  , runStmt
  , loadPreludeState
  , mkEvalState
  , resolveModulePath
  , resolveBuildTargets
  , listKipFilesRecursive
  , collectNonInfinitiveRefs
    -- * Utilities
  , foldM'
  , mapParseErrorBundle
  , turkifyParseError
  , replace
  , splitOn
  , breakOn
  ) where

import Control.Monad (forM, when, unless, filterM)
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.List (intercalate, isPrefixOf, nub, tails, findIndex, foldl')
import Data.Char (isDigit)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.Exit (die)
import System.FilePath ((</>), joinPath, takeExtension)
import Text.Megaparsec (ParseErrorBundle(..), PosState(..), errorBundlePretty)
import Text.Megaparsec.Error (ParseError(..), ErrorFancy(..), ShowErrorComponent(..))
import Text.Megaparsec.Pos (sourceLine, sourceColumn, unPos)
import qualified Data.List.NonEmpty as NE
import Crypto.Hash.SHA256 (hash)

import Language.Foma
import Kip.AST
import Kip.Cache
import Kip.Eval (EvalState, EvalM, EvalError, emptyEvalState, runEvalM, evalExp, evalStmtInFile, evalRender)
import qualified Kip.Eval as Eval
import Kip.Parser
import Kip.Render
import Kip.TypeCheck
import qualified Kip.TypeCheck as TC

-- | Diagnostic language selection.
data Lang
  = LangTr
  | LangEn
  deriving (Eq, Show)

-- | Rendering context for diagnostics.
data RenderCtx =
  RenderCtx
    { rcLang :: Lang
    , rcCache :: RenderCache
    , rcFsm :: FSM
    , rcUpsCache :: MorphCache
    , rcDownsCache :: MorphCache
    }

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
    LangEn ->
      case evalErr of
        Eval.Unknown -> "Evaluation error: unknown error."
        Eval.UnboundVariable name -> "Evaluation error: " <> T.pack (prettyIdent name) <> " is not defined."
        Eval.NoMatchingFunction name -> "Evaluation error: no matching definition found for " <> T.pack (prettyIdent name) <> "."
        Eval.NoMatchingClause -> "Evaluation error: no matching clause found."

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
                if ctor == ([], T.pack "ascribe")
                  then "Tip ataması uyuşmuyor: beklenen tip " <> expStr <> ", bulunan tip " <> actStr
                  else T.pack (prettyIdent ctor) <> " yapıcısı " <> expStr <> " tipindendir, ancak burada " <> actStr <> " bekleniyor"
          return header
        NonExhaustivePattern pats sp -> do
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
                if ctor == ([], T.pack "ascribe")
                  then "Type ascription mismatch: expected " <> expStr <> ", found " <> actStr
                  else T.pack (prettyIdent ctor) <> " constructor has type " <> expStr <> ", but " <> actStr <> " is expected here"
          return header
        NonExhaustivePattern pats sp -> do
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
  msg <- renderTCError paramTyCons tyMods tcErr
  case tcErrSpan tcErr of
    Nothing -> return msg
    Just sp ->
      let snippet = renderSpanSnippet source sp
      in return (msg <> "\n" <> snippet)

-- | Extract a span from a type checker error when present.
tcErrSpan :: TCError -> Maybe Span
tcErrSpan tcErr =
  case tcErr of
    NoType sp -> Just sp
    Ambiguity sp -> Just sp
    UnknownName _ sp -> Just sp
    NoMatchingOverload _ _ _ sp -> Just sp
    NoMatchingCtor _ _ _ sp -> Just sp
    PatternTypeMismatch _ _ _ sp -> Just sp
    NonExhaustivePattern _ sp -> Just sp
    UnimplementedPrimitive _ _ sp -> Just sp
    InvalidReturnCase _ sp -> Just sp
    TC.Unknown -> Nothing

-- | Render missing patterns for error messages.
renderMissingPatterns :: Lang -> [Pat Ann] -> RenderM Text
renderMissingPatterns lang pats = do
  patTexts <- mapM (renderPatText False) pats
  let prefix =
        case lang of
          LangTr -> "Eksik örüntüler: "
          LangEn -> "Missing patterns: "
  return (prefix <> T.intercalate ", " patTexts)
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
          "\n" <> T.pack p <> (T.pack (show (unPos (sourceLine start))) <> ":" <> T.pack (show (unPos (sourceColumn start))) <> "-" <> T.pack (show (unPos (sourceLine end))) <> ":" <> T.pack (show (unPos (sourceColumn end))))

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
runFiles :: Bool -> Bool -> Bool -> ParserState -> TCState -> EvalState -> [FilePath] -> Set FilePath -> [FilePath] -> RenderM ReplState
runFiles showDefn showLoad buildOnly basePst baseTC baseEval moduleDirs loaded files = do
  (pst', tcSt', evalSt', loaded') <- foldM' (runFile showDefn showLoad buildOnly moduleDirs) (basePst, baseTC, baseEval, loaded) files
  return (ReplState (parserCtx pst') (parserCtors pst') (parserTyParams pst') (parserTyCons pst') (parserTyMods pst') (parserPrimTypes pst') (parserFuncArities pst') tcSt' evalSt' moduleDirs loaded')

-- | Run a single file and update all states.
runFile :: Bool -> Bool -> Bool -> [FilePath] -> (ParserState, TCState, EvalState, Set FilePath) -> FilePath -> RenderM (ParserState, TCState, EvalState, Set FilePath)
runFile showDefn showLoad buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path = do
  exists <- liftIO (doesFileExist path)
  unless exists $ do
    ctx <- ask
    liftIO (emitMsgIO ctx (MsgFileNotFound path))
    msg <- renderMsg MsgRunFailed
    liftIO (die (T.unpack msg))
  absPath <- liftIO (canonicalizePath path)
  if Set.member absPath loaded
    then return (pst, tcSt, evalSt, loaded)
    else do
      ctx <- ask
      let cache = rcCache ctx
          fsm = rcFsm ctx
          uCache = rcUpsCache ctx
          dCache = rcDownsCache ctx
      let cachePath = cacheFilePath absPath
      mCached <- liftIO (loadCachedModule cachePath)
      case mCached of
        Just cached -> do
          let loaded' = Set.insert absPath loaded
          if buildOnly
            then return (pst, tcSt, evalSt, loaded')
            else do
              pst' <- liftIO (fromCachedParserState fsm (Just path) uCache dCache (cachedParser cached))
              let tcSt' = mergeTCState tcSt (fromCachedTCState (cachedTC cached))
                  evalSt' = evalSt
                  stmts = cachedTypedStmts cached
                  paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                  source = ""
                  primRefs = collectNonInfinitiveRefs stmts
              foldM' (runTypedStmt showDefn showLoad buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') primRefs source) (pst', tcSt', evalSt', loaded') stmts
        Nothing -> do
          input <- liftIO (TIO.readFile path)
          liftIO (parseFromFile pst { parserFilePath = Just path } input) >>= \case
            Left err -> do
              liftIO (emitMsgIO ctx (MsgParseError err))
              msg <- renderMsg MsgRunFailed
              liftIO (die (T.unpack msg))
            Right (stmts, pst') -> do
              let paramTyCons = [name | (name, arity) <- parserTyCons pst', arity > 0]
                  source = input
                  primRefs = collectNonInfinitiveRefs stmts
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
                  (pstFinal, tcSt', evalSt', loaded', typedStmts, depPathsRaw) <-
                    foldM' (runStmtCollect showDefn showLoad buildOnly moduleDirs absPath paramTyCons (parserTyMods pst') primRefs source) startState stmts
                  let depPaths = nub depPathsRaw
                  depHashes <- liftIO $ mapM (\p -> do
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
                      cachedParserState <- liftIO (toCachedParserState pstFinal)
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
                            , cachedParser = cachedParserState
                            , cachedTC = toCachedTCState tcSt'
                            , cachedEval = toCachedEvalState evalSt'
                            }
                      liftIO (saveCachedModule cachePath cachedModule)
                  return (pstFinal, tcSt', evalSt', loaded')

-- | Merge a cached type-checker snapshot into the current state.
--
-- Cached entries are preferred for overlapping keys so that overloaded
-- signature order stays stable with the source module that produced the
-- cache. Current-state entries are retained for keys not present in cache.
mergeTCState :: TCState -> TCState -> TCState
mergeTCState cur cached =
  MkTCState
    { tcCtx = Set.union (tcCtx cur) (tcCtx cached)
    , tcFuncs = Map.union (tcFuncs cached) (tcFuncs cur)
    , tcFuncSigs = Map.union (tcFuncSigs cached) (tcFuncSigs cur)
    , tcFuncSigRets = Map.union (tcFuncSigRets cached) (tcFuncSigRets cur)
    , tcVarTys = tcVarTys cached ++ tcVarTys cur
    , tcVals = Map.union (tcVals cached) (tcVals cur)
    , tcCtors = Map.union (tcCtors cached) (tcCtors cur)
    , tcTyCons = Map.union (tcTyCons cached) (tcTyCons cur)
    , tcInfinitives = Set.union (tcInfinitives cur) (tcInfinitives cached)
    , tcResolvedNames = tcResolvedNames cached ++ tcResolvedNames cur
    , tcResolvedSigs = tcResolvedSigs cached ++ tcResolvedSigs cur
    , tcResolvedTypes = tcResolvedTypes cached ++ tcResolvedTypes cur
    , tcDefLocations = Map.union (tcDefLocations cached) (tcDefLocations cur)
    , tcFuncSigLocs = Map.union (tcFuncSigLocs cached) (tcFuncSigLocs cur)
    }

-- | Run a single statement in the context of a file.
runStmt :: Bool -> Bool -> Bool -> [FilePath] -> FilePath -> [Identifier] -> [(Identifier, [Identifier])] -> [Identifier] -> Text -> (ParserState, TCState, EvalState, Set FilePath) -> Stmt Ann -> RenderM (ParserState, TCState, EvalState, Set FilePath)
runStmt showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs source (pst, tcSt, evalSt, loaded) stmt =
  case stmt of
    Load dirPath name -> do
      path <- resolveModulePath moduleDirs dirPath name
      absPath <- liftIO (canonicalizePath path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded)
        else do
          (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
          when showLoad $ return ()
          return (pst', tcSt', evalSt', loaded')
    _ ->
      liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
        Left tcErr -> do
          msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
          liftIO (die (T.unpack msg))
        Right (stmt', tcSt') -> do
          when showDefn $ return ()
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
runTypedStmt :: Bool -> Bool -> Bool -> [FilePath] -> FilePath -> [Identifier] -> [(Identifier, [Identifier])] -> [Identifier] -> Text -> (ParserState, TCState, EvalState, Set FilePath) -> Stmt Ann -> RenderM (ParserState, TCState, EvalState, Set FilePath)
runTypedStmt showDefn showLoad buildOnly moduleDirs currentPath _paramTyCons _tyMods _primRefs _source (pst, tcSt, evalSt, loaded) stmt =
  case stmt of
    Load dirPath name -> do
      path <- resolveModulePath moduleDirs dirPath name
      absPath <- liftIO (canonicalizePath path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded)
        else do
          (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
          when showLoad $ return ()
          return (pst', tcSt', evalSt', loaded')
    _ -> do
      when showDefn $ return ()
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
runStmtCollect :: Bool -> Bool -> Bool -> [FilePath] -> FilePath -> [Identifier] -> [(Identifier, [Identifier])] -> [Identifier] -> Text -> (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann], [FilePath]) -> Stmt Ann -> RenderM (ParserState, TCState, EvalState, Set FilePath, [Stmt Ann], [FilePath])
runStmtCollect showDefn showLoad buildOnly moduleDirs currentPath paramTyCons tyMods primRefs source (pst, tcSt, evalSt, loaded, typedAcc, depPathsAcc) stmt =
  case stmt of
    Load dirPath name -> do
      path <- resolveModulePath moduleDirs dirPath name
      absPath <- liftIO (canonicalizePath path)
      if Set.member absPath loaded
        then return (pst, tcSt, evalSt, loaded, typedAcc ++ [stmt], depPathsAcc ++ [path])
        else do
          (pst', tcSt', evalSt', loaded') <- runFile False False buildOnly moduleDirs (pst, tcSt, evalSt, loaded) path
          when showLoad $ return ()
          return (pst', tcSt', evalSt', loaded', typedAcc ++ [stmt], depPathsAcc ++ [path])
    _ ->
      liftIO (runTCM (tcStmt stmt) tcSt) >>= \case
        Left tcErr -> do
          msg <- renderMsg (MsgTCError tcErr (Just source) paramTyCons tyMods)
          liftIO (die (T.unpack msg))
        Right (stmt', tcSt') -> do
          when showDefn $ return ()
          if buildOnly
            then
              case stmt' of
                ExpStmt _ -> return (pst, tcSt', evalSt, loaded, typedAcc ++ [stmt'], depPathsAcc)
                _ ->
                  liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                    Left evalErr -> do
                      msg <- renderMsg (MsgEvalError evalErr)
                      liftIO (die (T.unpack msg))
                    Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded, typedAcc ++ [stmt'], depPathsAcc)
            else
              liftIO (runEvalM (evalStmtInFile (Just currentPath) stmt') evalSt) >>= \case
                Left evalErr -> do
                  msg <- renderMsg (MsgEvalError evalErr)
                  liftIO (die (T.unpack msg))
                Right (_, evalSt') -> return (pst, tcSt', evalSt', loaded, typedAcc ++ [stmt'], depPathsAcc)

-- | Collect non-infinitive primitive references from statements.
collectNonInfinitiveRefs :: [Stmt Ann] -> [Identifier]
collectNonInfinitiveRefs stmts =
  nub (concatMap (stmtRefs []) stmts)
  where
    stmtRefs :: [Identifier] -> Stmt Ann -> [Identifier]
    stmtRefs bound stmt =
      case stmt of
        Defn name _ body ->
          expRefs (name : bound) body
        Function _ args _ clauses _ ->
          concatMap (clauseRefs (map argIdent args ++ bound)) clauses
        ExpStmt e ->
          expRefs bound e
        _ -> []
    clauseRefs :: [Identifier] -> Clause Ann -> [Identifier]
    clauseRefs bound (Clause _ body) = expRefs bound body
    expRefs :: [Identifier] -> Exp Ann -> [Identifier]
    expRefs bound expr =
      case expr of
        Var {varCandidates} ->
          if any (\(ident, _) -> ident `elem` bound) varCandidates
            then []
            else map fst varCandidates
        Bind {bindExp} ->
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

-- | Filter parser definition spans to those introduced by the given statements.
defSpansFromStmts :: [Stmt Ann] -> Map.Map Identifier [Span] -> Map.Map Identifier Span
defSpansFromStmts stmts defSpans =
  let allowed = Set.fromList (stmtNames stmts)
  in Map.filterWithKey (\ident _ -> Set.member ident allowed) (latestDefSpans defSpans)
  where
    stmtNames = concatMap stmtNames'
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

-- | Build function signature spans from statements and definition spans.
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
    path:_ -> return path
    [] -> do
      msg <- renderMsg (MsgModuleNotFound dirPath name)
      liftIO (die (T.unpack msg))

-- | Resolve build targets from file or directory inputs.
resolveBuildTargets :: [FilePath] -> IO [FilePath]
resolveBuildTargets paths = fmap nub (concat <$> mapM expandPath paths)
  where
    expandPath :: FilePath -> IO [FilePath]
    expandPath p = do
      isDir <- doesDirectoryExist p
      if isDir
        then listKipFilesRecursive p
        else return [p]

-- | Recursively list .kip files in a directory tree.
listKipFilesRecursive :: FilePath -> IO [FilePath]
listKipFilesRecursive dir = do
  entries <- listDirectory dir
  fmap concat $ forM entries $ \entry -> do
    let path = dir </> entry
    isDir <- doesDirectoryExist path
    if isDir
      then listKipFilesRecursive path
      else return [path | takeExtension path == ".kip"]

-- | Load the prelude module into parser/type/eval states unless disabled.
loadPreludeState :: Bool -> [FilePath] -> RenderCache -> FSM -> MorphCache -> MorphCache -> RenderM (ParserState, TCState, EvalState, Set FilePath)
loadPreludeState noPrelude moduleDirs cache fsm uCache dCache = do
  let pst = newParserStateWithCaches fsm Nothing uCache dCache
      tcSt = emptyTCState
      evalSt = mkEvalState cache fsm
  if noPrelude
    then return (pst, tcSt, evalSt, Set.empty)
    else do
      path <- resolveModulePath moduleDirs [] ([], T.pack "giriş")
      -- Update pst with the path
      let pst' = pst { parserFilePath = Just path }
      runFile False False False moduleDirs (pst, tcSt, evalSt, Set.empty) path

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
