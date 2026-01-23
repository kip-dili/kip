{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Parser and morphology-aware utilities for Kip syntax.
module Kip.Parser where

import Data.List
import Data.Maybe (maybeToList, mapMaybe, isJust, fromMaybe)
import qualified Data.Map.Strict as M
import Control.Applicative (optional)
import Control.Monad (forM, guard)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, modify, runStateT)
import Data.Char (isLetter, isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HT
import Data.Functor (($>))
import Text.Read (readMaybe)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


import Language.Foma
import Kip.AST

-- | Parser-specific error categories for human-friendly rendering.
data ParserError
  = ErrKeywordAsIdent
  | ErrNoMatchingNominative
  | ErrMatchPatternExpected
  | ErrDefinitionName
  | ErrDefinitionBodyMissing
  | ErrLoadNeedsAcc
  | ErrTypeNotFound
  | ErrPatternExpected
  | ErrPatternComplexExpr
  | ErrPatternAmbiguousName
  | ErrPatternOnlyNames
  | ErrPatternArgNameRepeated
  | ErrYaDaInvalid
  | ErrInternal Text
  deriving (Eq, Ord, Show)

-- | Render parser errors in Turkish.
renderParserErrorTr :: ParserError -> Text
renderParserErrorTr err =
  case err of
    ErrKeywordAsIdent -> "Anahtar kelime isim yerine kullanılamaz."
    ErrNoMatchingNominative -> "Buraya uyan yalın halde bir isim bulunamadı."
    ErrMatchPatternExpected -> "Eşleştirme için bir yapkı örüntüsü bekleniyordu."
    ErrDefinitionName -> "Tanımın ismi bir isim olmalıdır."
    ErrDefinitionBodyMissing -> "Tanımın gövdesi bulunamadı."
    ErrLoadNeedsAcc -> "`yükle` için -i hali bekleniyor."
    ErrTypeNotFound -> "Buraya uyan bir tip bulunamadı."
    ErrPatternExpected -> "Burada bir örüntü bekleniyordu."
    ErrPatternComplexExpr -> "Örüntüde karmaşık bir ifade beklenmiyor."
    ErrPatternAmbiguousName -> "Örüntüde belirsiz isim kullanılamaz."
    ErrPatternOnlyNames -> "Örüntüde sadece isimler kullanılabilir."
    ErrPatternArgNameRepeated -> "Örüntüde argüman adı tekrar edilemez."
    ErrYaDaInvalid -> "\"ya da\" yalnızca çok yapkılı tiplerin son yapkısında kullanılabilir."
    ErrInternal _ -> "Beklenmeyen hata."

-- | Render parser errors in English.
renderParserErrorEn :: ParserError -> Text
renderParserErrorEn err =
  case err of
    ErrKeywordAsIdent -> "A keyword cannot be used as an identifier."
    ErrNoMatchingNominative -> "No matching nominative identifier found here."
    ErrMatchPatternExpected -> "Expected a constructor pattern for matching."
    ErrDefinitionName -> "Definition name must be an identifier."
    ErrDefinitionBodyMissing -> "Definition body is missing."
    ErrLoadNeedsAcc -> "`yükle` expects the accusative case."
    ErrTypeNotFound -> "No matching type found here."
    ErrPatternExpected -> "Expected a pattern here."
    ErrPatternComplexExpr -> "Complex expressions are not allowed in patterns."
    ErrPatternAmbiguousName -> "Ambiguous names are not allowed in patterns."
    ErrPatternOnlyNames -> "Only identifiers are allowed in patterns."
    ErrPatternArgNameRepeated -> "Pattern argument names cannot be repeated."
    ErrYaDaInvalid -> "\"ya da\" can only appear before the last constructor."
    ErrInternal _ -> "Unexpected error."

-- | Hash table type alias for morphology caches.
type MorphCache = HT.BasicHashTable Text [Text]

-- | Parser state tracking context and morphology caches.
data ParserState =
  MkParserState
    { fsm :: FSM -- ^ Morphology FSM.
    , parserCtx :: [Identifier] -- ^ Names in scope.
    , parserTyParams :: [Identifier] -- ^ Type parameters in scope.
    , parserTyCons :: [(Identifier, Int)] -- ^ Type constructor arities.
    , parserTyMods :: [(Identifier, [Identifier])] -- ^ Type modifier expansions.
    , parserPrimTypes :: [Identifier] -- ^ Primitive types in scope.
    , parserUpsCache :: !MorphCache -- ^ Cached morphology analyses.
    , parserDownsCache :: !MorphCache -- ^ Cached morphology generations.
    }

-- | Parser monad uses IO for morphology lookups.
type Outer = IO
-- | Parser type for Kip with state and IO.
type KipParser = ParsecT ParserError Text (StateT ParserState Outer)

-- | Create a new empty parser state with fresh caches.
newParserState :: FSM -- ^ Morphology FSM.
               -> IO ParserState -- ^ Fresh parser state.
newParserState fsm' = do
  upsCache <- HT.new
  MkParserState fsm' [] [] [] [] [] upsCache <$> HT.new

-- | Create a parser state with a given context and fresh caches.
newParserStateWithCtx :: FSM -- ^ Morphology FSM.
                      -> [Identifier] -- ^ Initial identifier context.
                      -> [Identifier] -- ^ Initial type parameters.
                      -> [(Identifier, Int)] -- ^ Type constructor arities.
                      -> [(Identifier, [Identifier])] -- ^ Type modifiers.
                      -> [Identifier] -- ^ Primitive types.
                      -> IO ParserState -- ^ Fresh parser state.
newParserStateWithCtx fsm' ctx tyParams tyCons tyMods primTypes = do
  upsCache <- HT.new
  MkParserState fsm' ctx tyParams tyCons tyMods primTypes upsCache <$> HT.new

-- | Create a parser state with shared caches (for parse/render reuse).
newParserStateWithCaches :: FSM -- ^ Morphology FSM.
                         -> MorphCache -- ^ Shared ups cache.
                         -> MorphCache -- ^ Shared downs cache.
                         -> ParserState -- ^ Parser state.
newParserStateWithCaches fsm' = MkParserState fsm' [] [] [] [] []

-- | Create a parser state with context and shared caches.
newParserStateWithCtxAndCaches :: FSM -- ^ Morphology FSM.
                               -> [Identifier] -- ^ Initial identifier context.
                               -> [Identifier] -- ^ Initial type parameters.
                               -> [(Identifier, Int)] -- ^ Type constructor arities.
                               -> [(Identifier, [Identifier])] -- ^ Type modifiers.
                               -> [Identifier] -- ^ Primitive types.
                               -> MorphCache -- ^ Shared ups cache.
                               -> MorphCache -- ^ Shared downs cache.
                               -> ParserState -- ^ Parser state.
newParserStateWithCtxAndCaches = MkParserState

-- | Get the current parser state.
getP :: KipParser ParserState -- ^ Current parser state.
getP = lift get

-- | Replace the current parser state.
putP :: ParserState -- ^ New parser state.
     -> KipParser () -- ^ No result.
putP = lift . put

-- | Modify the current parser state.
modifyP :: (ParserState -> ParserState) -- ^ State update function.
        -> KipParser () -- ^ No result.
modifyP = lift . modify

-- | Parse an item and return it with a span.
withSpan :: KipParser a -- ^ Parser to wrap.
         -> KipParser (a, Span) -- ^ Parsed value with span.
withSpan p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  return (x, Span start end)

-- | Whitespace parser.
ws :: KipParser () -- ^ No result.
ws = L.space space1 empty empty <?> "boşluk"

-- | Parse a period token with surrounding whitespace.
period :: KipParser () -- ^ No result.
period = ws >> string "." >> ws

-- | Parse a lexeme with trailing whitespace.
lexeme :: KipParser a -- ^ Token parser.
       -> KipParser a -- ^ Parsed token.
lexeme p = ws *> p <* ws

-- | Parse something in parentheses.
parens :: KipParser a -- ^ Inner parser.
       -> KipParser a -- ^ Parsed value.
parens = between (char '(') (char ')')

-- | Parse a simple identifier word.
name :: KipParser Text -- ^ Parsed word.
name = T.pack <$> ((:) <$> letterChar <*> many (digitChar <|> letterChar))

-- | Parse a single word token.
word :: KipParser Text -- ^ Parsed word.
word = takeWhile1P (Just "kelime") isWordChar

-- | Parse multiple words separated by whitespace.
multiword :: KipParser Text -- ^ Parsed multiword.
multiword = T.unwords <$> some (lexeme word)

-- | Predicate for word characters.
isWordChar :: Char -- ^ Character to check.
           -> Bool -- ^ True when the character is word-like.
isWordChar c = isLetter c || c == '\''


-- | Parse an escaped character sequence.
escape :: KipParser Text -- ^ Parsed escaped character.
escape = do
    _ <- char '\\'
    c <- satisfy (`elem` ("\\\"0nrvtbf" :: String)) -- all the characters which can be escaped
    let mapped =
          case c of
            '\\' -> '\\'
            '"' -> '"'
            '0' -> '\0'
            'n' -> '\n'
            'r' -> '\r'
            'v' -> '\v'
            't' -> '\t'
            'b' -> '\b'
            'f' -> '\f'
            _ -> c
    return (T.singleton mapped)

-- | Parse a non-escaped character in a string literal.
nonEscape :: KipParser Char -- ^ Parsed character.
nonEscape = satisfy (`notElem` ("\\\"\0\n\r\v\t\b\f" :: String))

-- | Parse a single character in a string literal.
character :: KipParser Text -- ^ Parsed character.
character = fmap (T.pack . return) nonEscape <|> escape

-- | Parse a quoted string literal.
parseQuotedString :: KipParser Text -- ^ Parsed string literal.
parseQuotedString = do
    char '"'
    strings <- many character
    char '"'
    return $ T.concat strings
-- End copied from https://stackoverflow.com/a/24106749/2016295, CC BY-SA 3.0

-- | Parse a dash-separated identifier.
identifier :: KipParser Identifier -- ^ Parsed identifier.
identifier = do
  ws <- sepBy1 word (char '-') <?>
        "Tek kelimeli veya tire ile ayrılmış çok kelimeli bir isim kullanmanız"
  case reverse ws of
    w:revPrefix -> return (reverse revPrefix, w)
    [] -> customFailure (ErrInternal (T.pack "identifier"))

-- | Parse an identifier and reject keywords.
identifierNotKeyword :: KipParser Identifier -- ^ Parsed identifier.
identifierNotKeyword = do
  ident@(ss, s) <- identifier
  if null ss && s `elem` ["ya", "var", "diyelim", "olarak"]
    then customFailure ErrKeywordAsIdent
    else return ident

-- inCtx :: String -> KipParser Bool
-- inCtx x = do
--   MkParserState{..} <- getP
--   return $ x `elem` parserCtx

-- | Detect case tags in morphology analyses.
-- INLINE keeps this tiny classifier on the fast path.
{-# INLINE getPossibleCase #-}
getPossibleCase :: Text -- ^ Analysis string.
                -> Maybe (Text, Case) -- ^ Root and case when found.
getPossibleCase s
  | "<acc>" `T.isInfixOf` s = Just (root, Acc)
  | "<dat>" `T.isInfixOf` s = Just (root, Dat)
  | "<loc>" `T.isInfixOf` s = Just (root, Loc)
  | "<abl>" `T.isInfixOf` s = Just (root, Abl)
  | "<gen>" `T.isInfixOf` s = Just (root, Gen)
  | "<ins>" `T.isInfixOf` s = Just (root, Ins)
  | "<ise>" `T.isInfixOf` s = Just (root, Cond)
  | "<p3s>" `T.isInfixOf` s = Just (root, P3s)
  | otherwise = Just (root, Nom)
  where
    root = T.takeWhile (/= '<') s

-- | Estimate candidate identifiers from morphology and context.
estimateCandidates :: Bool -- ^ Whether to prefer identifiers already in context.
                   -> Identifier -- ^ Surface identifier.
                   -> KipParser [(Identifier, Case)] -- ^ Candidate roots with cases.
estimateCandidates useCtx (ss, s) = do
  MkParserState{..} <- getP
  let directIdent = (ss, s)
  if useCtx && directIdent `elem` parserCtx
    -- Fast path: already in scope, avoid morphology round-trips.
    then return [(directIdent, Nom)]
    else do
      let mCopula = stripCopulaSuffix s
      (sAnalyses, mCopulaAnalyses) <- case mCopula of
        Just stripped -> do
          analysesList <- upsCachedBatch [s, stripped]
          case analysesList of
            [sAn, strippedAn] -> return (sAn, Just (stripped, strippedAn))
            _ -> do
              sAn <- upsCached s
              return (sAn, Nothing)
        Nothing -> do
          sAn <- upsCached s
          return (sAn, Nothing)
      mCtxMatch <- if useCtx
        then findCtxCandidateWithAnalyses ss s parserCtx sAnalyses
        else return Nothing
      case mCtxMatch of
        Just match -> return [match]
        Nothing -> do
          (candidates0, filtered0) <- candidatesForWithAnalyses s parserCtx sAnalyses
          let hasCond = any (\(_, cas) -> cas == Cond) candidates0
              candidates =
                if hasCond
                  then candidates0
                  else nub (candidates0 ++ condCandidates s)
              filtered = filter (\(ident, _) -> ident `elem` parserCtx) candidates
          if useCtx && not (null filtered)
            then return filtered
            else do
              -- Fast path for ip-converb roots already in scope.
              let ipMatch = do
                    stripped <- stripIpSuffix s
                    let ipIdent = (ss, stripped)
                    if useCtx && ipIdent `elem` parserCtx
                      then Just [(ipIdent, Nom)]
                      else Nothing
              case ipMatch of
                Just match -> return match
                Nothing ->
                  case mCopula of
                    Just stripped -> do
                      baseIdent <- normalizePossessive (ss, stripped)
                      if useCtx && baseIdent `elem` parserCtx
                        then return [(baseIdent, Nom)]
                        else do
                          strippedAnalyses <- case mCopulaAnalyses of
                            Just (_, analyses) -> return analyses
                            Nothing -> upsCached stripped
                          (candidates1, filtered1) <- candidatesForWithAnalyses stripped parserCtx strippedAnalyses
                          let hasCond1 = any (\(_, cas) -> cas == Cond) candidates1
                              candidates' =
                                if hasCond1
                                  then candidates1
                                  else nub (candidates1 ++ condCandidates stripped)
                              filtered' = filter (\(ident, _) -> ident `elem` parserCtx) candidates'
                          let candidatesForMatch =
                                if null candidates'
                                  then [((ss, stripped), cas) | cas <- allCases]
                                  else candidates'
                          if useCtx && not (null filtered')
                            then return filtered'
                            else do
                              mMatch <- if useCtx then matchCtxByInflection parserCtx (ss, stripped) candidatesForMatch else return Nothing
                              case mMatch of
                                Just matched -> return [matched]
                                Nothing ->
                                  if not (null candidates')
                                    then return candidates'
                                    else if null candidates
                                      then customFailure ErrNoMatchingNominative
                                      else return candidates
                    Nothing -> do
                      let candidatesForMatch =
                            if null candidates
                              then [((ss, s), cas) | cas <- allCases]
                              else candidates
                      mMatch <- if useCtx then matchCtxByInflection parserCtx (ss, s) candidatesForMatch else return Nothing
                      case mMatch of
                        Just matched -> return [matched]
                        Nothing ->
                          if null candidates
                            then customFailure ErrNoMatchingNominative
                            else return candidates
  where
    -- | Find a matching identifier in context without building full candidate lists.
    -- Short-circuits on the first context hit to avoid extra downs calls.
    -- Returns Nothing when multiple matches exist (ambiguity) to allow full enumeration.
    findCtxCandidate :: [Text] -- ^ Identifier modifiers.
                     -> Text -- ^ Surface root.
                     -> [Identifier] -- ^ Context identifiers.
                     -> KipParser (Maybe (Identifier, Case)) -- ^ Unique context match, if any.
    findCtxCandidate mods surfaceRoot ctx = do
      analyses <- upsCached surfaceRoot
      findCtxCandidateWithAnalyses mods surfaceRoot ctx analyses

    -- | Find a matching identifier in context using pre-fetched analyses.
    findCtxCandidateWithAnalyses :: [Text] -- ^ Identifier modifiers.
                                 -> Text -- ^ Surface root.
                                 -> [Identifier] -- ^ Context identifiers.
                                 -> [Text] -- ^ Morphology analyses for the surface form.
                                 -> KipParser (Maybe (Identifier, Case)) -- ^ Unique context match, if any.
    findCtxCandidateWithAnalyses mods _surfaceRoot ctx analyses = do
      let nounAnalyses =
            filter (\a -> "<N>" `T.isInfixOf` a && not ("<V>" `T.isInfixOf` a)) analyses
          baseAnalyses =
            if null nounAnalyses then analyses else nounAnalyses
      -- Collect all matches first to detect ambiguity
      allMatches <- collectAllMatches baseAnalyses
      case allMatches of
        [single] -> return (Just single)  -- Unique match: use fast path
        _ -> return Nothing  -- Zero or multiple matches: fall through to full enumeration
      where
        collectAllMatches :: [Text] -> KipParser [(Identifier, Case)]
        collectAllMatches [] = return []
        collectAllMatches (analysis:rest) =
          case getPossibleCase analysis of
            Nothing -> collectAllMatches rest
            Just (baseRoot, cas) -> do
              let direct = (mods, baseRoot)
              directMatches <- if direct `elem` ctx
                then return [(direct, cas)]
                else do
                  let stem = stripCaseTags analysis
                  forms <- downsCached stem
                  p3sForms <-
                    if "<p3s>" `T.isInfixOf` analysis && cas /= P3s
                      then downsCached (stem <> "<p3s>")
                      else return []
                  let roots = nub (forms ++ p3sForms ++ [baseRoot])
                      matches = filter (`elem` ctx) [(mods, root) | root <- roots]
                  return [(match, cas) | match <- matches]
              restMatches <- collectAllMatches rest
              return (nub (directMatches ++ restMatches))
    allCases = [Nom, Acc, Dat, Gen, Loc, Abl, Ins, Cond, P3s]
    candidatesFor :: Text -- ^ Surface form.
                  -> [Identifier] -- ^ Context identifiers.
                  -> KipParser ([(Identifier, Case)], [(Identifier, Case)]) -- ^ All candidates and context-filtered ones.
    candidatesFor surface ctx = do
      morphAnalyses <- upsCached surface
      candidatesForWithAnalyses surface ctx morphAnalyses

    candidatesForWithAnalyses :: Text -- ^ Surface form.
                              -> [Identifier] -- ^ Context identifiers.
                              -> [Text] -- ^ Morphology analyses.
                              -> KipParser ([(Identifier, Case)], [(Identifier, Case)]) -- ^ All candidates and context-filtered ones.
    candidatesForWithAnalyses _surface ctx morphAnalyses = do
      let stems = map stripCaseTags morphAnalyses
          uniqueStems = nub stems
          -- Collect stems that need a possessive form so we can batch downs.
          p3sStems = nub
            [ stem <> "<p3s>"
            | (analysis, stem) <- zip morphAnalyses stems
            , "<p3s>" `T.isInfixOf` analysis
            ]
      -- Batch morphology generation to amortize Foma handle setup.
      stemForms <- downsCachedBatch uniqueStems
      p3sForms <- downsCachedBatch p3sStems
      let stemMap = M.fromList (zip uniqueStems stemForms)
          p3sMap = M.fromList (zip p3sStems p3sForms)
      candidates <- nub . concat <$> forM (zip morphAnalyses stems) (\(y, stem) ->
        case getPossibleCase y of
          Nothing -> return []
          Just (_, cas) -> do
            let baseRoot = T.takeWhile (/= '<') y
                direct = (ss, baseRoot)
            -- Fast path: if the base root is already in context, avoid costly downs.
            if direct `elem` ctx
              then return [(direct, cas)]
              else do
                let forms = fromMaybe [] (M.lookup stem stemMap)
                    p3sFormsFor =
                      if "<p3s>" `T.isInfixOf` y
                        then fromMaybe [] (M.lookup (stem <> "<p3s>") p3sMap)
                        else []
                    roots =
                      case forms ++ p3sFormsFor of
                        [] -> [baseRoot]
                        xs -> xs ++ [baseRoot]
                return [((ss, root), cas) | root <- nub roots])
      let filtered = filter (\(ident, _) -> ident `elem` ctx) candidates
      return (candidates, filtered)
    condCandidates :: Text -- ^ Surface form.
                   -> [(Identifier, Case)] -- ^ Candidate conditional forms.
    condCandidates surface =
      case stripCondSuffix surface of
        Just base -> [((ss, base), Cond)]
        Nothing -> []
    stripCondSuffix :: Text -- ^ Surface form.
                    -> Maybe Text -- ^ Stripped stem.
    stripCondSuffix txt =
      let suffixes = ["ysa", "yse"]
          match = find (`T.isSuffixOf` txt) suffixes
      in case match of
           Nothing -> Nothing
           Just suff ->
             let len = T.length suff
             in if T.length txt > len
                  then Just (T.take (T.length txt - len) txt)
                  else Nothing
    -- | Strip ip-converb suffixes like \"-ip\" from surface forms.
    stripIpSuffix :: Text -- ^ Surface form.
                  -> Maybe Text -- ^ Stripped stem.
    stripIpSuffix txt =
      let suffixes = ["ip", "ıp", "up", "üp"]
          match = find (`T.isSuffixOf` txt) suffixes
      in case match of
           Nothing -> Nothing
           Just suff ->
             let len = T.length suff
             in if T.length txt > len
                  then Just (T.take (T.length txt - len) txt)
                  else Nothing

-- | Strip copula suffixes like \"-dir\" to recover the base noun.
stripCopulaSuffix :: Text -- ^ Surface form.
                  -> Maybe Text -- ^ Stem without copula suffix.
stripCopulaSuffix txt =
  let lowerTxt = T.toLower txt
      suffixes = ["dir","dır","dur","dür","tir","tır","tur","tür"]
      match = find (`T.isSuffixOf` lowerTxt) suffixes
  in case match of
       Nothing -> Nothing
       Just suff ->
         let len = T.length suff
         in if T.length txt > len
              then Just (T.take (T.length txt - len) txt)
              else Nothing

-- | Strip case tags from a morphology analysis string.
-- INLINE reduces overhead in tight loops.
{-# INLINE stripCaseTags #-}
stripCaseTags :: Text -- ^ Analysis string.
              -> Text -- ^ Stem without case tags.
stripCaseTags = go
  where
    -- | Case tags recognized by the morphology analyzer.
    tags :: [Text]
    tags =
      [ "<nom>"
      , "<acc>"
      , "<dat>"
      , "<loc>"
      , "<abl>"
      , "<gen>"
      , "<ins>"
      , "<ise>"
      , "<p3s>"
      ]
    -- | Remove tags iteratively from the end.
    go :: Text -- ^ Analysis string.
       -> Text -- ^ Stripped stem.
    go str =
      case find (`T.isSuffixOf` str) tags of
        Nothing -> str
        Just tag -> go (T.take (T.length str - T.length tag) str)

-- | Resolve a candidate identifier using morphology and heuristics.
resolveCandidate :: Bool -- ^ Whether to prefer identifiers in context.
                 -> Identifier -- ^ Surface identifier.
                 -> KipParser (Identifier, Case) -- ^ Resolved identifier and case.
resolveCandidate useCtx ident = do
  candidates <- estimateCandidates useCtx ident
  MkParserState{parserCtx} <- getP
  let copulaStripped =
        case stripCopulaSuffix (identRoot ident) of
          Just stripped ->
            let (mods, _) = ident
            in Just (mods, stripped)
          Nothing -> Nothing
      mRoot = gerundRoot ident <|> (copulaStripped >>= gerundRoot)
  case mRoot of
    Just root ->
      case find (\(cand, _) -> cand == root) candidates of
        Just match -> return match
        Nothing ->
          if root `elem` parserCtx
            then return (root, Nom)
            else pickFromCandidates candidates
    Nothing -> pickFromCandidates candidates
  where
    -- | Choose a candidate, preferring inflected forms.
    pickFromCandidates :: [(Identifier, Case)] -- ^ Candidate identifiers.
                       -> KipParser (Identifier, Case) -- ^ Selected candidate.
    pickFromCandidates candidates =
      case preferInflected candidates of
        [x] -> return x
        x:_ -> return x

-- | Resolve a candidate, preferring names in scope.
resolveCandidatePreferCtx :: Identifier -- ^ Surface identifier.
                          -> KipParser (Identifier, Case) -- ^ Resolved identifier and case.
resolveCandidatePreferCtx ident = do
  MkParserState{parserCtx} <- getP
  candidates <- estimateCandidates False ident
  let filtered = filter (\(ident', _) -> ident' `elem` parserCtx) candidates
  case preferInflected filtered of
    x:_ -> return x
    [] ->
      case preferInflected candidates of
        x:_ -> return x
        [] -> customFailure ErrNoMatchingNominative

-- | Resolve a candidate, preferring nominative case.
resolveCandidatePreferNom :: Identifier -- ^ Surface identifier.
                          -> KipParser (Identifier, Case) -- ^ Resolved identifier and case.
resolveCandidatePreferNom ident = do
  candidates <- estimateCandidates False ident
  case filter (\(_, cas) -> cas == Nom) candidates of
    (cand, _):_ ->
      if cand == ident
        then return (cand, Nom)
        else return (ident, Nom)
    [] ->
      case find (\(cand, _) -> cand == ident) candidates of
        Just cand -> return cand
        Nothing ->
          case candidates of
            x:_ -> return x
            [] -> customFailure ErrNoMatchingNominative

-- | Resolve a type candidate, preferring names in scope.
-- Avoids extra morphology matching if we already have a direct candidate.
resolveTypeCandidatePreferCtx :: Identifier -- ^ Surface type identifier.
                              -> KipParser (Identifier, Case) -- ^ Resolved type identifier and case.
resolveTypeCandidatePreferCtx ident = do
  MkParserState{parserTyCons, parserTyParams, parserPrimTypes} <- getP
  let tyNames = map fst parserTyCons ++ parserTyParams ++ parserPrimTypes
  if ident `elem` tyNames
    then return (ident, Nom)
    else do
      candidates <- estimateCandidates False ident
      let filtered = filter (\(ident', _) -> ident' `elem` tyNames) candidates
      case preferInflected filtered of
        x:_ -> return x
        [] -> do
          mMatch <- matchCtxByInflection tyNames ident candidates
          case mMatch of
            Just matched -> return matched
            Nothing -> resolveCandidatePreferCtx ident

-- | Normalize a possessive surface form to its nominative root.
normalizePossessive :: Identifier -- ^ Surface identifier.
                    -> KipParser Identifier -- ^ Normalized identifier.
normalizePossessive (mods, word) = do
  analyses <- upsCached word
  case find (\a -> "<p3s>" `T.isInfixOf` a) analyses of
    Just analysis -> do
      let baseRoot = T.takeWhile (/= '<') analysis
      forms <- downsCached (stripCaseTags analysis)
      case forms of
        (x:_) -> return (mods, x)
        [] -> return (mods, baseRoot)
    Nothing -> return (mods, word)

-- | Normalize type head names when they use bare possessive suffixes.
normalizeTypeHead :: Identifier -- ^ Surface identifier.
                  -> KipParser Identifier -- ^ Normalized identifier.
normalizeTypeHead ident@(mods, word) =
  if null mods && not (hasBufferS word)
    then normalizePossessive ident
    else return ident
  where
    hasBufferS :: Text -- ^ Surface word.
               -> Bool -- ^ True when the word ends with buffer s.
    hasBufferS txt =
      any (`T.isSuffixOf` txt) ["sı", "si", "su", "sü"]

-- | Resolve a type candidate without requiring it to be in scope.
resolveTypeCandidateLoose :: Identifier -- ^ Surface type identifier.
                          -> KipParser (Identifier, Case) -- ^ Resolved type identifier and case.
resolveTypeCandidateLoose ident = do
  MkParserState{parserTyCons, parserTyParams, parserPrimTypes} <- getP
  let tyNames = map fst parserTyCons ++ parserTyParams ++ parserPrimTypes
  if ident `elem` tyNames
    then return (ident, Nom)
    else do
      mCandidates <- optional (try (estimateCandidates False ident))
      case mCandidates of
        Just candidates ->
          case preferInflected candidates of
            x:_ -> return x
            [] -> return (ident, Nom)
        Nothing -> return (ident, Nom)

-- | Prefer inflected candidates over nominative-only ones.
preferInflected :: [(Identifier, Case)] -- ^ Candidate identifiers.
                -> [(Identifier, Case)] -- ^ Preferred candidates.
preferInflected candidates =
  case filter (\(_, cas) -> cas /= Nom) candidates of
    [] -> candidates
    inflected -> inflected

-- | Extract a gerund root from a verb identifier if present.
gerundRoot :: Identifier -- ^ Surface identifier.
           -> Maybe Identifier -- ^ Root without gerund suffix.
gerundRoot (xs, x)
  | T.isSuffixOf "mak" x = Just (xs, T.dropEnd 3 x)
  | T.isSuffixOf "mek" x = Just (xs, T.dropEnd 3 x)
  | otherwise = Nothing

-- | Pick the case from candidate variants.
-- INLINE trims repeated selector overhead.
{-# INLINE pickCase #-}
pickCase :: Bool -- ^ Whether P3s (possessive) case is allowed.
         -> [(Identifier, Case)] -- ^ Candidate identifiers.
         -> Case -- ^ Selected case.
pickCase allowP3s candidates =
  -- Prefer a case that has multiple distinct identifiers (ambiguity)
  -- so the type checker can detect it
  let filtered = if allowP3s then candidates else filter (\(_, cas) -> cas /= P3s) candidates
  in case findAmbiguousCase filtered of
       Just cas -> cas
       Nothing ->
         case preferInflected filtered of
           [] ->
             case filtered of
               [] -> Nom
               (_, cas):_ -> cas
           (_, cas):_ -> cas

-- | Find a case that has multiple distinct identifiers (potential ambiguity).
findAmbiguousCase :: [(Identifier, Case)] -> Maybe Case
findAmbiguousCase candidates =
  let groupedByCases = [(cas, nub [ident | (ident, c) <- candidates, c == cas]) | cas <- nub (map snd candidates)]
      ambiguousCases = [cas | (cas, idents) <- groupedByCases, length idents > 1]
  in case ambiguousCases of
       (cas:_) -> Just cas
       [] -> Nothing

-- | Render an identifier as dash-separated text.
-- INLINE reduces allocations in hot call sites.
{-# INLINE identText #-}
identText :: Identifier -- ^ Identifier to render.
          -> Text -- ^ Rendered surface text.
identText (xs, x) = T.intercalate "-" (xs ++ [x])

-- | Extract the root part of an identifier.
identRoot :: Identifier -- ^ Identifier to inspect.
          -> Text -- ^ Root part.
identRoot (_, x) = x

-- | Match a context identifier by inflected surface forms.
matchCtxByInflection :: [Identifier] -- ^ Context identifiers.
                     -> Identifier -- ^ Surface identifier.
                     -> [(Identifier, Case)] -- ^ Candidate identifiers.
                     -> KipParser (Maybe (Identifier, Case)) -- ^ Matched candidate, if any.
matchCtxByInflection ctx ident candidates = do
  let (mods, surfaceRoot) = ident
      cases = nub (map snd candidates)
      ctxFiltered =
        if null mods
          then ctx
          else filter (\(ms, _) -> ms == mods) ctx
  if ident `elem` ctxFiltered
    -- Fast path: exact identifier already in scope.
    then return (Just (ident, Nom))
    else do
      analyses <- upsCached surfaceRoot
      -- Prefer noun analyses when they exist; they are the only ones that can
      -- participate in case inflection for identifier matching.
      let nounAnalyses =
            filter (\a -> "<N>" `T.isInfixOf` a && not ("<V>" `T.isInfixOf` a)) analyses
          baseAnalyses =
            if null nounAnalyses then analyses else nounAnalyses
      -- Optimization: analyze the surface once and only generate roots for
      -- the analyses that do not already match the context roots.
      goAnalyses mods ctxFiltered baseAnalyses cases
  where
    -- | Render a case tag for morphology lookup.
    caseTag :: Case -- ^ Case to encode.
            -> Text -- ^ Morphology case tag.
    caseTag cas =
      case cas of
        Nom -> ""
        Acc -> "<acc>"
        Dat -> "<dat>"
        Loc -> "<loc>"
        Abl -> "<abl>"
        Gen -> "<gen>"
        Ins -> "<ins>"
        Cond -> "<ise>"
        P3s -> "<p3s>"

    -- | Try analyses against context identifiers.
    goAnalyses :: [Text] -- ^ Identifier modifiers.
               -> [Identifier] -- ^ Context identifiers.
               -> [Text] -- ^ Morphology analyses for the surface form.
               -> [Case] -- ^ Candidate cases.
               -> KipParser (Maybe (Identifier, Case)) -- ^ Matched candidate.
    goAnalyses _ _ [] _ = return Nothing
    goAnalyses mods ctxFiltered (analysis:rest) cases =
      case getPossibleCase analysis of
        Nothing -> goAnalyses mods ctxFiltered rest cases
        Just (baseRoot, cas)
          | cas `elem` cases -> do
              let direct = (mods, baseRoot)
              if direct `elem` ctxFiltered
                -- Fast path: analysis root is already a context identifier.
                then return (Just (direct, cas))
                else do
                  let stem = stripCaseTags analysis
                  forms <- downsCached stem
                  p3sForms <-
                    if "<p3s>" `T.isInfixOf` analysis && cas /= P3s
                      then downsCached (stem <> "<p3s>")
                      else return []
                  let roots = nub (forms ++ p3sForms ++ [baseRoot])
                      matches = filter (`elem` ctxFiltered) [(mods, root) | root <- roots]
                  case matches of
                    (match:_) -> return (Just (match, cas))
                    [] -> goAnalyses mods ctxFiltered rest cases
          | otherwise -> goAnalyses mods ctxFiltered rest cases

-- | Cached morphology analysis lookup.
upsCached :: Text -- ^ Surface form.
          -> KipParser [Text] -- ^ Morphology analyses.
upsCached s = do
  MkParserState{fsm, parserUpsCache} <- getP
  cached <- liftIO $ HT.lookup parserUpsCache s
  case cached of
    Just res -> return res
    Nothing -> do
      res <- liftIO (ups fsm s)
      liftIO $ HT.insert parserUpsCache s res
      return res

-- | Cached batch morphology analysis lookup.
-- Uses batch FFI when multiple surfaces are missing to avoid repeated handle setup.
upsCachedBatch :: [Text] -- ^ Surface forms.
               -> KipParser [[Text]] -- ^ Analyses per surface.
upsCachedBatch [] = return []
upsCachedBatch surfaces = do
  MkParserState{fsm, parserUpsCache} <- getP
  cached <- liftIO $ mapM (HT.lookup parserUpsCache) surfaces
  let missing = [s | (s, Nothing) <- zip surfaces cached]
  fetched <-
    if null missing
      then return []
      else liftIO (upsBatch fsm missing)
  let fetchedMap = M.fromList (zip missing fetched)
  liftIO $
    mapM_ (uncurry (HT.insert parserUpsCache)) (zip missing fetched)
  let resolve s = fromMaybe (fromMaybe [] (M.lookup s fetchedMap))
  return (zipWith resolve surfaces cached)

-- | Cached morphology generation lookup.
downsCached :: Text -- ^ Morphology stem.
            -> KipParser [Text] -- ^ Generated surface forms.
downsCached s = do
  MkParserState{fsm, parserDownsCache} <- getP
  cached <- liftIO $ HT.lookup parserDownsCache s
  case cached of
    Just res -> return res
    Nothing -> do
      res <- liftIO (downs fsm s)
      liftIO $ HT.insert parserDownsCache s res
      return res

-- | Cached batch morphology generation lookup.
-- Uses batch FFI when multiple stems are missing to avoid repeated handle setup.
downsCachedBatch :: [Text] -- ^ Morphology stems.
                -> KipParser [[Text]] -- ^ Generated surface forms per stem.
downsCachedBatch [] = return []
downsCachedBatch stems = do
  MkParserState{fsm, parserDownsCache} <- getP
  cached <- liftIO (mapM (HT.lookup parserDownsCache) stems)
  let missing = [s | (s, Nothing) <- zip stems cached]
  fetched <-
    if null missing
      then return []
      else liftIO (downsBatch fsm missing)
  let fetchedMap = M.fromList (zip missing fetched)
  liftIO $
    mapM_ (uncurry (HT.insert parserDownsCache)) (zip missing fetched)
  let resolve s = fromMaybe (fromMaybe [] (M.lookup s fetchedMap))
  return (zipWith resolve stems cached)

-- | Parse an identifier with inferred case (context-aware).
casedIdentifier :: KipParser (Identifier, Case) -- ^ Identifier and inferred case.
casedIdentifier = identifier >>= resolveCandidate True

-- | Parse an identifier with inferred case (no context preference).
casedIdentifierAny :: KipParser (Identifier, Case) -- ^ Identifier and inferred case.
casedIdentifierAny = identifier >>= resolveCandidate False

-- | Parse an expression with context-sensitive name resolution.
parseExp :: KipParser (Exp Ann) -- ^ Parsed expression.
parseExp = parseExpWithCtx True

-- | Parse an expression without context-sensitive name resolution.
parseExpAny :: KipParser (Exp Ann) -- ^ Parsed expression.
parseExpAny = parseExpWithCtx False

-- | Parse an expression with optional context filtering.
parseExpWithCtx :: Bool -- ^ Whether to use context when resolving names.
                -> KipParser (Exp Ann) -- ^ Parsed expression.
parseExpWithCtx useCtx =
  seqExp
  where
    -- | Parse a numeric literal with case.
    numberLiteral :: KipParser (Exp Ann) -- ^ Parsed numeric literal.
    numberLiteral = do
      (token, sp) <- withSpan parseNumberToken
      cas <- numberCase token
      if isFloatToken token
        then
          let val = parseNumberValueFloat token
          in return (FloatLit (mkAnn cas sp) val)
        else
          let val = parseNumberValue token
          in return (IntLit (mkAnn cas sp) val)
    -- | Parse a string literal with case.
    stringLiteral :: KipParser (Exp Ann) -- ^ Parsed string literal.
    stringLiteral = do
      ((txt, cas), sp) <- withSpan parseStringToken
      return (StrLit (mkAnn cas sp) txt)
    -- | Parse a variable reference.
    var :: KipParser (Exp Ann) -- ^ Parsed variable.
    var = do
      (name, sp) <- withSpan identifierNotKeyword
      candidates <- estimateCandidates useCtx name
      return (Var (mkAnn (pickCase True candidates) sp) name candidates)
    -- | Parse an atomic expression.
    atom :: KipParser (Exp Ann) -- ^ Parsed atomic expression.
    atom = try matchExpr <|> try stringLiteral <|> try numberLiteral <|> try var <|> parens (parseExpWithCtx useCtx)
    -- | Parse application chains.
    app :: KipParser (Exp Ann) -- ^ Parsed application expression.
    app = do
      a <- some (lexeme atom)
      case a of
        [x] -> return x
        [x, y] -> do
          m <- tryApplyTypeCase x y
          case m of
            Just x' -> return x'
            Nothing -> buildAppFrom a
        _ -> buildAppFrom a
    -- | Parse binding expressions with "olarak".
    bindExp :: KipParser (Exp Ann) -- ^ Parsed binding expression.
    bindExp = try $ do
      (name, sp) <- withSpan identifierNotKeyword
      lexeme (string "olarak")
      action <- app <|> atom
      let ann = mkAnn (annCase (annExp action)) (mergeSpan sp (annSpan (annExp action)))
      return (Bind ann name action)
    -- | Parse comma-separated sequence expressions.
    seqExp :: KipParser (Exp Ann) -- ^ Parsed sequence expression.
    seqExp = do
      e1 <- bindExp <|> app <|> atom
      mcomma <- optional (try (lookAhead (lexeme (char ','))))
      case mcomma of
        Nothing -> return e1
        Just _ -> do
          ok <- isIpConverbExp e1
          if ok
            then do
              lexeme (char ',')
              e2 <- case e1 of
                Bind {bindName} -> do
                  st <- getP
                  putP st { parserCtx = bindName : parserCtx st }
                  res <- parseExpWithCtx useCtx
                  putP st
                  return res
                _ -> parseExpWithCtx useCtx
              let ann = mkAnn (annCase (annExp e2)) (mergeSpan (annSpan (annExp e1)) (annSpan (annExp e2)))
              return (Seq ann e1 e2)
            else
              -- e1 is not a converb, check if it looks like a match pattern (Cond case)
              -- This handles match expressions without outer parens (like bir-fazlası.kip)
              -- Only do this when useCtx=True to avoid triggering in pattern parsing
              if useCtx && isCondExpr e1
                then parseMatchCont e1
                else return e1
    -- | Check if expression looks like a match pattern (Cond case).
    isCondExpr :: Exp Ann -- ^ Expression to inspect.
               -> Bool -- ^ True when expression is a Cond match head.
    isCondExpr exp = case exp of
      App {fn, args} ->
        annCase (annExp fn) == Cond &&
        case args of
          (arg:_) ->
            case arg of
              StrLit{} -> False  -- String literal is not a scrutinee
              IntLit{} -> False  -- Int literal is not a scrutinee
              FloatLit{} -> False  -- Float literal is not a scrutinee
              _ -> True
          [] -> False
      Var {annExp = ann} -> annCase ann == Cond
      _ -> False
    -- | Continue parsing a match expression given the first pattern.
    parseMatchCont :: Exp Ann -- ^ First pattern expression.
                   -> KipParser (Exp Ann) -- ^ Parsed match expression.
    parseMatchCont patExp = do
      (scrutVar, scrutName) <- inferScrutineeCont patExp
      let argNames = [scrutName]
      pat <- expToPat True argNames patExp
      lexeme (char ',')
      body <- parseExpWithCtx useCtx
      let clause1 = Clause pat body
      clauses <- parseMoreClausesCont argNames
      let allClauses = clause1 : clauses
          start = annSpan (annExp scrutVar)
          end =
            case allClauses of
              [] -> annSpan (annExp scrutVar)
              _ ->
                case reverse allClauses of
                  Clause _ lastBody:_ -> annSpan (annExp lastBody)
                  [] -> annSpan (annExp scrutVar)
          ann = mkAnn (annCase (annExp scrutVar)) (mergeSpan start end)
      return (Match ann scrutVar allClauses)
    -- | Parse additional clauses for a match continuation.
    parseMoreClausesCont :: [Identifier] -- ^ Bound pattern names.
                         -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseMoreClausesCont argNames = do
      mcomma <- optional (try (lexeme (char ',')))
      case mcomma of
        Nothing -> return []
        Just _ -> do
          pat <- parsePatternCont False argNames
          lexeme (char ',')
          body <- parseExpWithCtx useCtx
          rest <- parseMoreClausesCont argNames
          return (Clause pat body : rest)
    -- | Parse a pattern in continuation form.
    parsePatternCont :: Bool -- ^ Whether to allow scrutinee.
                     -> [Identifier] -- ^ Bound pattern names.
                     -> KipParser (Pat Ann) -- ^ Parsed pattern.
    parsePatternCont allowScrutinee argNames =
      (lexeme (string "değilse") $> PWildcard) <|>
      (parseExpAny >>= expToPat allowScrutinee argNames)
    -- | Infer the scrutinee expression in continuation form.
    inferScrutineeCont :: Exp Ann -- ^ Pattern expression.
                       -> KipParser (Exp Ann, Identifier) -- ^ Scrutinee and identifier.
    inferScrutineeCont expItem =
      case expItem of
        App _ _ (v@Var{varCandidates}:_) -> do
          scrutName <- pickScrutineeNameCont v
          let hasNom = any (\(_, cas) -> cas == Nom) varCandidates
              v' =
                if annCase (annExp v) == Nom || hasNom
                  then v {annExp = setAnnCase (annExp v) Nom}
                  else v
          return (v', scrutName)
        App _ _ (e:_) -> do
          return (e, ([], T.pack "_"))
        Var{} -> customFailure ErrMatchPatternExpected
        _ -> customFailure ErrMatchPatternExpected
    -- | Pick a scrutinee name based on candidates and context.
    pickScrutineeNameCont :: Exp Ann -- ^ Scrutinee expression.
                           -> KipParser Identifier -- ^ Chosen scrutinee name.
    pickScrutineeNameCont Var{varName, varCandidates} = do
      MkParserState{parserCtx} <- getP
      let inScope = [ident | (ident, _) <- varCandidates, ident `elem` parserCtx]
      case inScope of
        n:_ -> return n
        [] ->
          case varCandidates of
            (n, _):_ -> return n
            [] -> return varName
    -- | Fail when scrutinee is not a variable.
    pickScrutineeNameCont _ = customFailure (ErrInternal (T.pack "pickScrutineeNameCont"))
    -- | Build an application expression from a list of atoms.
    buildAppFrom :: [Exp Ann] -- ^ Atom expressions in application order.
                 -> KipParser (Exp Ann) -- ^ Parsed application.
    buildAppFrom xs =
      case xs of
        [] -> error "application needs at least one item"
        [x] -> return x
        first:_ ->
          case reverse xs of
            x:revRest ->
              let rest = reverse revRest
                  start = annSpan (annExp first)
                  end = annSpan (annExp x)
                  ann = mkAnn (annCase (annExp x)) (mergeSpan start end)
              in case x of
                   Var {varCandidates} | isWriteCandidates varCandidates -> do
                     case rest of
                       [] -> return (App ann x [])
                       [arg] -> do
                         let ann' = mkAnn (annCase (annExp x)) (mergeSpan (annSpan (annExp arg)) (annSpan (annExp x)))
                         return (App ann' x [arg])
                       _ ->
                         case reverse rest of
                           lastRest:_ ->
                             case lastRest of
                               Var {} -> do
                                 arg <- buildAppFrom rest
                                 let ann' = mkAnn (annCase (annExp x)) (mergeSpan (annSpan (annExp arg)) (annSpan (annExp x)))
                                 return (App ann' x [arg])
                               App {} -> do
                                 arg <- buildAppFrom rest
                                 let ann' = mkAnn (annCase (annExp x)) (mergeSpan (annSpan (annExp arg)) (annSpan (annExp x)))
                                 return (App ann' x [arg])
                               _ -> return (App ann x rest)
                           [] -> return (App ann x rest)
                   _ -> return (App ann x rest)
            [] -> error "application needs at least one item"
    -- | Attempt to reinterpret application as a type cast.
    tryApplyTypeCase :: Exp Ann -- ^ Candidate expression.
                     -> Exp Ann -- ^ Candidate type expression.
                     -> KipParser (Maybe (Exp Ann)) -- ^ Casted expression if possible.
    tryApplyTypeCase x y = do
      MkParserState{parserTyParams, parserTyCons, parserPrimTypes, parserCtx} <- getP
      let tyNames = map fst parserTyCons ++ parserTyParams ++ parserPrimTypes
          preferCase :: [(Identifier, Case)] -- ^ Candidate identifiers.
                     -> Case -- ^ Preferred case.
                     -> [(Identifier, Case)] -- ^ Filtered candidates.
          preferCase candidates cas =
            let filtered = filter (\(_, c) -> c == cas) candidates
            in if null filtered then candidates else filtered
      case y of
        Var annY name candidates -> do
          -- Check if the original identifier is inflected (has P3s case like "olasılığı")
          -- Inflected forms are more likely to be function calls than type casts
          let originalCase = annCase annY
              isInflected = originalCase `elem` [P3s, Acc, Gen, Dat, Loc, Abl]
          -- Check if any candidate is in context but NOT a type name (i.e., it's a function)
          let isFuncCandidate = any (\(ident, _) -> ident `elem` parserCtx && ident `notElem` tyNames) candidates
          -- Don't apply type cast if inflected OR if it's a function candidate
          if isInflected || isFuncCandidate
            then return Nothing  -- Don't apply type casting if it could be a function call
            else do
              mResolved <- optional (try (resolveTypeCandidatePreferCtx name))
              case mResolved of
                Just (ident, cas) | ident `elem` tyNames ->
                  return $
                    case x of
                      Var annX name candX ->
                        Just (Var (setAnnCase annX cas) name (preferCase candX cas))
                      IntLit annX n ->
                        Just (IntLit (setAnnCase annX cas) n)
                      FloatLit annX n ->
                        Just (FloatLit (setAnnCase annX cas) n)
                      _ -> Nothing
                _ -> return Nothing
        _ -> return Nothing
    -- | Parse a parenthesized match expression.
    matchExpr :: KipParser (Exp Ann) -- ^ Parsed match expression.
    matchExpr = try $ do
      hasComma <- option False $
        try (lookAhead ((char '(' *> manyTill anySingle (char ',')) $> True))
      guard hasComma
      parens (parseMatchExpr useCtx)
    -- | Check if an expression is an ip-converb.
    isIpConverbExp :: Exp Ann -- ^ Expression to inspect.
                   -> KipParser Bool -- ^ True when expression is an ip-converb.
    isIpConverbExp exp =
      case exp of
        Var {varName} -> isIpConverbName varName
        App {fn = Var {varName}} -> isIpConverbName varName
        Bind {bindExp} -> isIpConverbExp bindExp
        _ -> return False
    -- | Check morphology for ip-converb tags.
    isIpConverbName :: Identifier -- ^ Identifier to inspect.
                    -> KipParser Bool -- ^ True when morphology has ip-converb tag.
    isIpConverbName ident = do
      analyses <- upsCached (identText ident)
      let surface = identText ident
          hasSuffix = any (`T.isSuffixOf` surface) ["ip", "ıp", "up", "üp"]
      return (any ("<cv:ip>" `T.isInfixOf`) analyses || hasSuffix)
    -- | Detect if candidates correspond to the write primitive.
    isWriteCandidates :: [(Identifier, Case)] -- ^ Candidate identifiers.
                      -> Bool -- ^ True when candidates match the write primitive.
    isWriteCandidates =
      any (\(ident, _) -> identText ident == "yaz")

-- | Parse a top-level statement.
parseStmt :: KipParser (Stmt Ann) -- ^ Parsed statement.
parseStmt = try loadStmt <|> try primTy <|> ty <|> try func <|> try def <|> expFirst
  where
    -- | Parse a module load statement.
    loadStmt :: KipParser (Stmt Ann) -- ^ Parsed load statement.
    loadStmt = do
      rawName <- identifier
      _ <- lexeme (string "yükle")
      period
      name <- resolveLoadCandidate rawName
      return (Load name)
    -- | Parse constructor identifiers.
    ctorIdent :: KipParser Identifier -- ^ Parsed constructor identifier.
    ctorIdent = fst <$> (identifier >>= resolveCandidatePreferNom)
    -- | Parse constructors without arguments.
    ctor :: KipParser (Ctor Ann) -- ^ Parsed constructor.
    ctor = try ((, []) <$> ctorIdent)
    -- | Parse "ya" separator.
    ya :: KipParser Text -- ^ Parsed separator token.
    ya = lexeme (string "ya")
    -- | Parse "da" separator.
    da :: KipParser Text -- ^ Parsed separator token.
    da = lexeme (string "da")
    -- | Parse a primitive type declaration.
    primTy :: KipParser (Stmt Ann) -- ^ Parsed primitive type declaration.
    primTy = do
      lexeme (string "Bir")
      _ <- lexeme (string "yerleşik")
      name <- fst <$> (identifier >>= resolveCandidatePreferNom)
      lexeme (string "olsun")
      period
      modifyP (\ps -> ps { parserCtx = name : parserCtx ps
                         , parserTyCons = (name, 0) : parserTyCons ps
                         , parserPrimTypes = name : parserPrimTypes ps
                         })
      return (PrimType name)
    -- | Parse a type declaration.
    ty :: KipParser (Stmt Ann) -- ^ Parsed type declaration.
    ty = do
      lexeme (string "Bir")
      (n, params, mods) <- typeHead
      modifyP (\ps -> ps { parserCtx = n : params ++ parserCtx ps
                             , parserTyParams = params ++ parserTyParams ps
                             , parserTyCons = (n, length params) : parserTyCons ps
                             , parserTyMods =
                                 case mods of
                                   [] -> parserTyMods ps
                                   _ -> (n, mods) : parserTyMods ps
                             })
      ctors <- try (lexeme (string "var olamaz") $> [])
           <|> (parseCtors <* lexeme (string "olabilir"))
      period
      modifyP (\ps -> ps {parserCtx = n : map fst ctors ++ parserCtx ps})
      return (NewType n params ctors)
    -- | Parse a type head (name and parameters).
    typeHead :: KipParser (Identifier, [Identifier], [Identifier]) -- ^ Type head parts.
    typeHead = try typeHeadParens <|> typeHeadInline
    -- | Parse a parenthesized type head.
    typeHeadParens :: KipParser (Identifier, [Identifier], [Identifier]) -- ^ Parsed type head.
    typeHeadParens = parens $ do
      param <- identifier
      ws
      rawName <- fst <$> (identifier >>= resolveCandidatePreferNom)
      name <- normalizeTypeHead rawName
      return (name, [param], [])
    -- | Parse a type head without parentheses.
    typeHeadInline :: KipParser (Identifier, [Identifier], [Identifier]) -- ^ Parsed type head.
    typeHeadInline = do
      first <- identifier
      second <- optional (try (ws *> identifierNotKeyword))
      case second of
        Nothing -> do
          rawName <- fst <$> resolveCandidatePreferNom first
          name <- normalizeTypeHead rawName
          return (name, [], [])
        Just nameIdent -> do
          rawName <- fst <$> resolveCandidatePreferNom nameIdent
          name <- normalizeTypeHead rawName
          return (name, [], [first])
    -- | Parse a constructor with or without arguments.
    ctorWithArgs :: KipParser (Ctor Ann) -- ^ Parsed constructor with arguments.
    ctorWithArgs = try ctorArgs <|> ctor
    -- | Error message for invalid "ya da" usage.
    yaDaErrorMsg = ErrYaDaInvalid
    -- | Parse constructor lists.
    parseCtors :: KipParser [Ctor Ann] -- ^ Parsed constructors.
    parseCtors = do
      firstSepIsYaDa <- parseSep
      if firstSepIsYaDa
        then customFailure yaDaErrorMsg
        else do
          firstCtor <- ctorWithArgs
          parseMore [firstCtor]
      where
        -- | Parse additional constructors separated by "ya".
        parseMore :: [Ctor Ann] -- ^ Accumulated constructors.
                  -> KipParser [Ctor Ann] -- ^ Parsed constructors.
        parseMore acc = do
          msep <- optional (try parseSep)
          case msep of
            Nothing -> return acc
            Just sepIsYaDa -> do
              ctor <- ctorWithArgs
              if sepIsYaDa
                then do
                  msep2 <- optional (try parseSep)
                  case msep2 of
                    Nothing -> return (acc ++ [ctor])
                    Just _ -> customFailure yaDaErrorMsg
                else parseMore (acc ++ [ctor])
        -- | Parse a constructor separator and track "ya da".
        parseSep :: KipParser Bool -- ^ True when the separator is "ya da".
        parseSep = do
          _ <- ya
          mda <- optional (try (lookAhead (string "da")))
          case mda of
            Just _ -> da >> return True
            Nothing -> return False
    -- | Parse a constructor with argument types.
    ctorArgs :: KipParser (Ctor Ann) -- ^ Parsed constructor with arguments.
    ctorArgs = do
      args <- some (try (lexeme parseCtorArgType))
      name <- ctorIdent
      return (name, args)
    -- | Parse a constructor argument type.
    parseCtorArgType :: KipParser (Ty Ann) -- ^ Parsed constructor argument type.
    parseCtorArgType = do
      _ <- try (lexeme (string "bir") *> lookAhead (identifier <|> (char '(' $> ([], T.pack ""))))
      parseTypeWithCase
    -- | Parse a function argument declaration.
    parseArg :: KipParser (Identifier, Ty Ann) -- ^ Parsed argument declaration.
    parseArg = do
      argName <- identifier
      ws
      ty <- try parseTypeWithCase <|> parseTypeLoose
      return (argName, ty)
    -- | Parse a type without requiring it to be in scope.
    parseTypeLoose :: KipParser (Ty Ann) -- ^ Parsed type.
    parseTypeLoose = do
      (rawIdent, sp) <- withSpan identifier
      candidates <- estimateCandidates False rawIdent
      let ann = mkAnn (pickCase False candidates) sp  -- Types are never P3s
          nameForTy =
            case candidates of
              (ident, _):_ -> ident
              [] -> rawIdent
      MkParserState{parserPrimTypes, parserTyCons} <- getP
      let tyNames = map fst parserTyCons
      case candidates of
        (ident, _):_
          | ident `elem` parserPrimTypes && isIntType ident -> return (TyInt ann)
        (ident, _):_
          | ident `elem` parserPrimTypes && isFloatType ident -> return (TyFloat ann)
        (ident, _):_
          | ident `elem` parserPrimTypes && isStringType ident -> return (TyString ann)
        _ ->
          if nameForTy `elem` tyNames
            then return (TyInd ann nameForTy)
            else return (TyVar ann nameForTy)
    -- | Parse a value definition.
    def :: KipParser (Stmt Ann) -- ^ Parsed value definition.
    def = do
      items <- some (lexeme defItem)
      let (exprItems, nameItem) =
            case reverse items of
              name:revExpr -> (reverse revExpr, name)
              [] -> error "definition needs at least two items"
      st <- getP
      exprItems' <- mapM (restrictCandidates (parserCtx st)) exprItems
      rawName <- case nameItem of
        Var _ n _ -> return n
        _ -> customFailure ErrDefinitionName
      -- Normalize the name to strip possessive suffix if present
      name <- normalizePossessive rawName
      e <- buildApp exprItems'
      lexeme (string "diyelim")
      period
      modifyP (\ps -> ps {parserCtx = name : parserCtx ps})
      return (Defn name (TyString (mkAnn Nom NoSpan)) e)
    -- | Parse a definition item (argument or name).
    defItem :: KipParser (Exp Ann) -- ^ Parsed definition item.
    defItem = try (parens parseExp) <|> try defItemStringLit <|> try defItemNumberLit <|> defItemVar
    -- | Parse a definition item as a string literal.
    defItemStringLit :: KipParser (Exp Ann) -- ^ Parsed string literal definition item.
    defItemStringLit = do
      ((txt, cas), sp) <- withSpan parseStringToken
      return (StrLit (mkAnn cas sp) txt)
    -- | Parse a definition item as a number literal.
    defItemNumberLit :: KipParser (Exp Ann) -- ^ Parsed number literal definition item.
    defItemNumberLit = do
      (token, sp) <- withSpan parseNumberToken
      cas <- numberCase token
      if isFloatToken token
        then
          let val = parseNumberValueFloat token
          in return (FloatLit (mkAnn cas sp) val)
        else
          let val = parseNumberValue token
          in return (IntLit (mkAnn cas sp) val)
    -- | Parse a definition item as a variable.
    defItemVar :: KipParser (Exp Ann) -- ^ Parsed definition item variable.
    defItemVar = do
      notFollowedBy (string "diyelim")
      (name, sp) <- withSpan identifier
      candidates <- estimateCandidates False name
      return (Var (mkAnn (pickCase False candidates) sp) name candidates)  -- Definitions are never P3s
    -- | Build an application expression from items.
    buildApp :: [Exp Ann] -- ^ Expression items.
             -> KipParser (Exp Ann) -- ^ Built application.
    buildApp xs =
      case xs of
        [] -> customFailure ErrDefinitionBodyMissing
        [x] -> return x
        first:_ ->
          case reverse xs of
            x:revRest ->
              let rest = reverse revRest
                  start = annSpan (annExp first)
                  end = annSpan (annExp x)
                  ann = mkAnn (annCase (annExp x)) (mergeSpan start end)
              in return (App ann x rest)
            [] -> customFailure ErrDefinitionBodyMissing
    -- | Restrict variable candidates to the current context.
    restrictCandidates :: [Identifier] -- ^ Context identifiers.
                       -> Exp Ann -- ^ Expression to restrict.
                       -> KipParser (Exp Ann) -- ^ Updated expression.
    restrictCandidates ctx expItem =
      case expItem of
        Var annExp name candidates ->
          case filter (\(ident, _) -> ident `elem` ctx) candidates of
            [] -> customFailure ErrNoMatchingNominative
            filtered -> return (Var (setAnnCase annExp (pickCase False filtered)) name filtered)  -- Definition values are never P3s
        _ -> return expItem
    -- | Parse a statement starting with an expression.
    expFirst :: KipParser (Stmt Ann) -- ^ Parsed expression statement.
    expFirst = do
      e <- parseExp
      period
      return (ExpStmt e)
    -- | Resolve an identifier used in a load statement.
    resolveLoadCandidate :: Identifier -- ^ Surface identifier.
                         -> KipParser Identifier -- ^ Resolved module identifier.
    resolveLoadCandidate ident = do
      candidates <- estimateCandidates False ident
      case filter (\(_, cas) -> cas == Acc) candidates of
        (cand, _):_ -> return cand
        [] -> customFailure ErrLoadNeedsAcc
    -- | Parse a function declaration or definition.
    func :: KipParser (Stmt Ann) -- ^ Parsed function statement.
    func = do
      args <- many (try (lexeme (parens parseArg) <* notFollowedBy (lexeme (char ','))))
      (rawName, mRetTy) <- parseFuncHeader
      let isGerund = isJust (gerundRoot rawName)
          baseName = fromMaybe rawName (gerundRoot rawName)
      (fname, _) <-
        if isGerund
          then return (baseName, Nom)
          else do
            basePoss <- normalizePossessive baseName
            if basePoss /= baseName
              then return (basePoss, Nom)
              else do
                candidates <- estimateCandidates False baseName
                let nomMatch = find (\(_, cas) -> cas == Nom) candidates
                case nomMatch of
                  Just (ident, _) -> return (ident, Nom)
                  Nothing -> do
                    let p3sMatch = find (\(_, cas) -> cas == P3s) candidates
                    case p3sMatch of
                      Just (ident, _) -> do
                        base <- normalizePossessive ident
                        return (base, Nom)
                      Nothing -> do
                        (cand, _) <- resolveCandidate False baseName
                        return (cand, Nom)
      lexeme (char ',')
      st <- getP
      let argNames = map fst args
      putP (st {parserCtx = fname : argNames ++ parserCtx st})
      prim <- optional (try (lexeme (string "yerleşiktir") *> period))
      let retTy = fromMaybe (TyString (mkAnn Nom NoSpan)) mRetTy
      case prim of
        Just _ -> do
          st' <- getP
          putP (st' {parserCtx = fname : parserCtx st})
          return (PrimFunc fname args retTy isGerund)
        Nothing -> do
          isBindStart <- option False (try bindStartLookahead)
          clauses <-
            if isBindStart
              then parseBodyOnly
              else try parseBodyOnly <|> parseClauses argNames
          st' <- getP
          putP (st' {parserCtx = fname : parserCtx st})
          return (Function fname args retTy clauses isGerund)
    -- | Parse function header with optional return type.
    parseFuncHeader :: KipParser (Identifier, Maybe (Ty Ann)) -- ^ Function name and optional return type.
    parseFuncHeader = do
      mHeader <- optional (try (parens ((,) <$> identifier <*> (ws *> parseReturnType))))
      case mHeader of
        Just (name, ty) -> return (name, Just ty)
        Nothing -> do
          name <- identifier
          return (name, Nothing)
    -- | Parse a return type with a lenient fallback for plain identifiers.
    parseReturnType :: KipParser (Ty Ann) -- ^ Parsed return type.
    parseReturnType =
      try parseTypeWithCase <|> parseReturnTypeLoose
    -- | Parse a return type without requiring it to be in scope.
    parseReturnTypeLoose :: KipParser (Ty Ann) -- ^ Parsed return type.
    parseReturnTypeLoose =
      try (parens parseReturnTypeLoose) <|> do
        (firstIdent, sp1) <- withSpan identifier
        mSecond <- optional (try (ws *> withSpan identifier))
        case mSecond of
          Nothing -> typeFromIdentLoose firstIdent sp1
          Just (secondIdent, sp2) -> do
            firstTy <- typeFromIdentLoose firstIdent sp1
            (ctorName, cas) <- resolveTypeCandidateLoose secondIdent
            let annApp = mkAnn cas (mergeSpan sp1 sp2)
                ctorAnn = mkAnn cas sp2
            return (TyApp annApp (TyInd ctorAnn ctorName) [firstTy])
    -- | Build a loose type node for a return type identifier.
    typeFromIdentLoose :: Identifier -- ^ Surface identifier.
                       -> Span -- ^ Source span.
                       -> KipParser (Ty Ann) -- ^ Parsed type.
    typeFromIdentLoose rawIdent sp = do
      (name, cas) <- resolveTypeCandidateLoose rawIdent
      MkParserState{parserPrimTypes} <- getP
      let ann = mkAnn cas sp
      if name `elem` parserPrimTypes && isIntType name
        then return (TyInt ann)
        else if name `elem` parserPrimTypes && isFloatType name
          then return (TyFloat ann)
          else if name `elem` parserPrimTypes && isStringType name
            then return (TyString ann)
            else return (TyVar ann name)
    -- | Parse a function body without explicit clauses.
    parseBodyOnly :: KipParser [Clause Ann] -- ^ Parsed clauses.
    parseBodyOnly = do
      -- Try parsing as a match expression first (no parens required in function body)
      body <- try (parseMatchExpr True) <|> parseExp
      period
      return [Clause PWildcard body]
    -- | Parse match clauses for a function.
    parseClauses :: [Identifier] -- ^ Function argument names.
                 -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseClauses argNames = do
      c <- parseClause True argNames
      (period >> return [c]) <|> do
        lexeme (char ',')
        (c :) <$> parseClausesRest argNames
    -- | Parse the remaining clauses after a comma.
    parseClausesRest :: [Identifier] -- ^ Function argument names.
                     -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseClausesRest argNames = do
      c <- parseClause False argNames
      (period >> return [c]) <|> do
        lexeme (char ',')
        (c :) <$> parseClausesRest argNames
    -- | Parse a single clause.
    parseClause :: Bool -- ^ Whether to allow scrutinee expressions.
                -> [Identifier] -- ^ Function argument names.
                -> KipParser (Clause Ann) -- ^ Parsed clause.
    parseClause allowScrutinee argNames = do
      pat <- parsePattern allowScrutinee argNames
      lexeme (char ',')
      Clause pat <$> parseExp
    -- | Parse a pattern, optionally allowing a scrutinee expression.
    parsePattern :: Bool -- ^ Whether to allow scrutinee expressions.
                 -> [Identifier] -- ^ Function argument names.
                 -> KipParser (Pat Ann) -- ^ Parsed pattern.
    parsePattern allowScrutinee argNames =
      (lexeme (string "değilse") $> PWildcard) <|>
      try (parseExpAny >>= expToPat allowScrutinee argNames)
    -- | Look ahead for a binding expression start.
    bindStartLookahead :: KipParser Bool -- ^ True when a binding start is found.
    bindStartLookahead = do
      lookAhead $ do
        _ <- identifierNotKeyword
        lexeme (string "olarak")
        return True
    -- | Parse a type with optional case suffix.
    parseTypeWithCase :: KipParser (Ty Ann) -- ^ Parsed type.
    parseTypeWithCase =
      try (parens parseTypeWithCase) <|> try parseModifiedType <|> do
        MkParserState{parserTyParams, parserTyCons, parserPrimTypes} <- getP
        let tyNames = map fst parserTyCons
            primNames = parserPrimTypes
            typeScope = tyNames ++ parserTyParams ++ primNames
        let
            -- | Require a type identifier to be in scope.
            requireInCtx :: Identifier -- ^ Type identifier to check.
                         -> KipParser () -- ^ No result.
            requireInCtx name =
              if name `elem` tyNames || name `elem` parserTyParams || name `elem` primNames
                then return ()
                else customFailure ErrTypeNotFound
            -- | Prefer a genitive case if the surface form is genitive.
            preferSurfaceCase :: Identifier -- ^ Surface identifier.
                              -> Case -- ^ Current case.
                              -> Case -- ^ Preferred case.
            preferSurfaceCase ident cas =
              case genitiveBase ident of
                Just _ -> Gen
                _ -> cas
            -- | Check whether an identifier ends with a genitive suffix.
            genitiveBase :: Identifier -- ^ Surface identifier.
                         -> Maybe Identifier -- ^ Base identifier without genitive.
            genitiveBase (mods, word) =
              let suffixes = ["nın", "nin", "nun", "nün", "ın", "in", "un", "ün"]
                  tryStrip suf =
                    case T.stripSuffix suf word of
                      Just base -> Just (mods, base)
                      Nothing -> Nothing
              in foldr ((<|>) . tryStrip) Nothing suffixes
            -- | Build a type node for an identifier.
            argTy :: Identifier -- ^ Identifier to convert.
                  -> Ty Ann -- ^ Type node.
            argTy ident
              | ident `elem` primNames && isIntType ident = TyInt (mkAnn Nom NoSpan)
              | ident `elem` primNames && isFloatType ident = TyFloat (mkAnn Nom NoSpan)
              | ident `elem` primNames && isStringType ident = TyString (mkAnn Nom NoSpan)
              | ident `elem` tyNames = TyInd (mkAnn Nom NoSpan) ident
              |  otherwise = TyVar (mkAnn Nom NoSpan) ident
            -- | Check if an identifier refers to a type in scope.
            isTypeIdent :: Identifier -- ^ Surface identifier.
                        -> KipParser Bool -- ^ True when identifier resolves to a type.
            isTypeIdent ident = do
              m <- optional (try (resolveTypeCandidatePreferCtx ident))
              case m of
                Just (name, _) ->
                  return (name `elem` tyNames || name `elem` parserTyParams || name `elem` primNames)
                Nothing -> return False
        arg <- optional (try (do
          first <- identifier
          ws
          nextIdent <- lookAhead identifier
          isTy <- isTypeIdent nextIdent
          if isTy then return first else empty
          ))
        (rawIdent, sp) <- withSpan identifier
        case arg of
          Just t -> do
            (name, cas) <- resolveTypeCandidatePreferCtx rawIdent
            requireInCtx name
            let cas' = preferSurfaceCase rawIdent cas
                ann = mkAnn cas' sp
            if name `elem` primNames && isIntType name
              then return (TyInt ann)
              else if name `elem` primNames && isFloatType name
                then return (TyFloat ann)
                else if name `elem` primNames && isStringType name
                  then return (TyString ann)
                  else return (TyApp ann (TyInd (mkAnn Nom NoSpan) name) [argTy t])
          Nothing -> do
            case rawIdent of
              (xs, xraw) | not (null xs) -> do
                (baseName, cas) <- resolveTypeCandidatePreferCtx ([], xraw)
                if baseName `elem` tyNames
                  then do
                    let cas' = preferSurfaceCase rawIdent cas
                        ann = mkAnn cas' sp
                    case reverse xs of
                      arg:revPrefix -> do
                        let argIdent = (reverse revPrefix, arg)
                        return (TyApp ann (TyInd (mkAnn Nom NoSpan) baseName) [argTy argIdent])
                      [] -> customFailure (ErrInternal (T.pack "parseTypeHead"))
                  else do
                    (name, cas') <- resolveTypeCandidatePreferCtx rawIdent
                    requireInCtx name
                    let cas'' = preferSurfaceCase rawIdent cas'
                    if name `elem` primNames && isIntType name
                      then return (TyInt (mkAnn cas'' sp))
                      else if name `elem` primNames && isFloatType name
                        then return (TyFloat (mkAnn cas'' sp))
                        else if name `elem` primNames && isStringType name
                          then return (TyString (mkAnn cas'' sp))
                          else if name `elem` parserTyParams
                            then return (TyVar (mkAnn cas'' sp) name)
                            else return (TyInd (mkAnn cas'' sp) name)
              _ -> do
                (name, cas) <- resolveTypeCandidatePreferCtx rawIdent
                requireInCtx name
                let cas' = preferSurfaceCase rawIdent cas
                if name `elem` primNames && isIntType name
                  then return (TyInt (mkAnn cas' sp))
                  else if name `elem` primNames && isFloatType name
                    then return (TyFloat (mkAnn cas' sp))
                    else if name `elem` primNames && isStringType name
                      then return (TyString (mkAnn cas' sp))
                      else if name `elem` parserTyParams
                        then return (TyVar (mkAnn cas' sp) name)
                        else return (TyInd (mkAnn cas' sp) name)
    -- | Parse a type name with a modifier prefix.
    parseModifiedType :: KipParser (Ty Ann) -- ^ Parsed type.
    parseModifiedType = do
      (firstIdent, sp1) <- withSpan identifier
      ws
      (secondIdent, sp2) <- withSpan identifier
      MkParserState{parserTyCons} <- getP
      let tyNames = map fst parserTyCons
          prefix = fst firstIdent ++ [snd firstIdent]
          identMatches :: Identifier -- ^ Candidate identifier.
                       -> Identifier -- ^ Target identifier.
                       -> Bool -- ^ True when identifiers match loosely.
          identMatches (xs1, x1) (xs2, x2) =
            x1 == x2 && (xs1 == xs2 || null xs1 || null xs2)
      candidates <- estimateCandidates False secondIdent
      let matches =
            [ (tyName, cas)
            | (candIdent, cas) <- candidates
            , tyName@(mods, root) <- tyNames
            , mods == prefix
            , identMatches ([], root) candIdent
            ]
      case matches of
        (fullName, cas):_ ->
          return (TyInd (mkAnn cas (mergeSpan sp1 sp2)) fullName)
        [] -> empty

-- | Parse a match expression with optional context filtering.
parseMatchExpr :: Bool -- ^ Whether to use context when resolving names.
               -> KipParser (Exp Ann) -- ^ Parsed match expression.
parseMatchExpr useCtx = do
  (patExp, scrutVar, scrutName) <- parseFirstPattern
  let argNames = [scrutName]
  pat <- expToPat True argNames patExp
  lexeme (char ',')
  body <- parseExpWithCtx useCtx
  let clause1 = Clause pat body
  clauses <- parseMoreClauses argNames
  let allClauses = clause1 : clauses
      start = annSpan (annExp scrutVar)
      end =
        case allClauses of
          [] -> annSpan (annExp scrutVar)
          _ ->
            case reverse allClauses of
              Clause _ lastBody:_ -> annSpan (annExp lastBody)
              [] -> annSpan (annExp scrutVar)
      ann = mkAnn (annCase (annExp scrutVar)) (mergeSpan start end)
  return (Match ann scrutVar allClauses)
  where
    -- | Parse the first pattern and infer the scrutinee.
    parseFirstPattern :: KipParser (Exp Ann, Exp Ann, Identifier) -- ^ Pattern, scrutinee, and name.
    parseFirstPattern = do
      patExp <- parseExpAny
      (scrutVar, scrutName) <- inferScrutinee patExp
      return (patExp, scrutVar, scrutName)
    -- | Parse additional match clauses.
    parseMoreClauses :: [Identifier] -- ^ Bound pattern names.
                     -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseMoreClauses argNames = do
      mcomma <- optional (try (lexeme (char ',')))
      case mcomma of
        Nothing -> return []
        Just _ -> do
          pat <- parsePattern False argNames
          lexeme (char ',')
          body <- parseExpWithCtx useCtx
          rest <- parseMoreClauses argNames
          return (Clause pat body : rest)
    -- | Parse a pattern for a match clause.
    parsePattern :: Bool -- ^ Whether to allow scrutinee expressions.
                 -> [Identifier] -- ^ Bound pattern names.
                 -> KipParser (Pat Ann) -- ^ Parsed pattern.
    parsePattern allowScrutinee argNames =
      (lexeme (string "değilse") $> PWildcard) <|>
      (parseExpAny >>= expToPat allowScrutinee argNames)
    -- | Infer the scrutinee expression and its name.
    inferScrutinee :: Exp Ann -- ^ Pattern expression.
                   -> KipParser (Exp Ann, Identifier) -- ^ Scrutinee and name.
    inferScrutinee expItem =
      case expItem of
        App _ _ (v@Var{varCandidates}:_) -> do
          scrutName <- pickScrutineeName v
          let hasNom = any (\(_, cas) -> cas == Nom) varCandidates
              v' =
                if annCase (annExp v) == Nom || hasNom
                  then v {annExp = setAnnCase (annExp v) Nom}
                  else v
          return (v', scrutName)
        App _ _ (e:_) -> do
          -- Complex expression as scrutinee (e.g., function application result)
          -- Use a dummy name that won't match real pattern variables
          return (e, ([], T.pack "_"))
        Var{} -> customFailure ErrMatchPatternExpected
        _ -> customFailure ErrMatchPatternExpected
    -- | Pick a scrutinee name based on candidates and scope.
    pickScrutineeName :: Exp Ann -- ^ Scrutinee expression.
                      -> KipParser Identifier -- ^ Chosen scrutinee name.
    pickScrutineeName Var{varName, varCandidates} = do
      MkParserState{parserCtx} <- getP
      let inScope = [ident | (ident, _) <- varCandidates, ident `elem` parserCtx]
      case inScope of
        n:_ -> return n
        [] ->
          case varCandidates of
            (n, _):_ -> return n
            [] -> return varName

-- | Parse a numeric token with an optional case suffix.
parseNumberToken :: KipParser Text -- ^ Parsed numeric token.
parseNumberToken = do
  sign <- optional (char '-')
  digits <- takeWhile1P (Just "sayı") isDigit
  frac <- optional (try (char '.' *> takeWhile1P (Just "sayı") isDigit))
  suffix <- optional (char '\'' *> takeWhile1P (Just "ek") isLetter)
  let prefix = maybe "" T.singleton sign
      fracPart = maybe "" (T.cons '.') frac
      suff = maybe "" (T.cons '\'') suffix
  return (prefix <> digits <> fracPart <> suff)

-- | Infer the grammatical case for a numeric token.
numberCase :: Text -- ^ Numeric token.
           -> KipParser Case -- ^ Inferred case.
numberCase token = do
  let (surface, suffix) = T.breakOn "'" token
      base = T.filter isDigit surface
  if T.null suffix
    then do
      analyses <- upsCached base
      let cases = mapMaybe (fmap snd . getPossibleCase) analyses
          inflected = filter (/= Nom) cases
      return $
        case inflected of
          c:_ -> c
          [] ->
            case cases of
              c:_ -> c
              [] -> Nom
    else return (stringCaseFromSuffix (T.drop 1 suffix))

-- | Parse a numeric token into an integer value.
parseNumberValue :: Text -- ^ Numeric token.
                 -> Integer -- ^ Parsed integer value.
parseNumberValue token =
  let stripped = T.filter (\c -> c /= '\'' && (isDigit c || c == '-')) token
  in fromMaybe 0 (readMaybe (T.unpack stripped))

-- | Check whether a numeric token contains a fractional part.
isFloatToken :: Text -- ^ Numeric token.
             -> Bool -- ^ True when the token is a float.
isFloatToken token = "." `T.isInfixOf` token

-- | Parse a numeric token into a floating-point value.
parseNumberValueFloat :: Text -- ^ Numeric token.
                      -> Double -- ^ Parsed floating value.
parseNumberValueFloat token =
  let stripped = T.filter (\c -> c /= '\'' && (isDigit c || c == '-' || c == '.')) token
  in fromMaybe 0 (readMaybe (T.unpack stripped))

-- | Parse a quoted string with an optional case suffix.
parseStringToken :: KipParser (Text, Case) -- ^ Parsed string and case.
parseStringToken = do
  txt <- parseQuotedString
  mSuffix <- optional $ do
    _ <- optional (char '\'')
    takeWhile1P (Just "ek") isLetter
  let cas = maybe Nom stringCaseFromSuffix mSuffix
  return (txt, cas)

-- | Map a case suffix to a grammatical case.
stringCaseFromSuffix :: Text -- ^ Suffix string.
                     -> Case -- ^ Case enum.
stringCaseFromSuffix suff =
  case T.toLower suff of
    s
      | s `elem` accSuffixes -> Acc
      | s `elem` datSuffixes -> Dat
      | s `elem` locSuffixes -> Loc
      | s `elem` ablSuffixes -> Abl
      | s `elem` genSuffixes -> Gen
      | s `elem` insSuffixes -> Ins
      | s `elem` condSuffixes -> Cond
      | otherwise -> Nom
  where
    -- | Accusative suffixes.
    accSuffixes = ["i","ı","u","ü","yi","yı","yu","yü"]
    -- | Dative suffixes.
    datSuffixes = ["e","a","ye","ya"]
    -- | Locative suffixes.
    locSuffixes = ["de","da","te","ta"]
    -- | Ablative suffixes.
    ablSuffixes = ["den","dan","ten","tan"]
    -- | Genitive suffixes.
    genSuffixes = ["in","ın","un","ün","nin","nın","nun","nün"]
    -- | Instrumental suffixes.
    insSuffixes = ["le","la","yle","yla"]
    -- | Conditional suffixes.
    condSuffixes = ["se","sa"]

-- | Check whether an identifier names the integer type.
isIntType :: Identifier -- ^ Identifier to inspect.
          -> Bool -- ^ True when identifier names the integer type.
isIntType (xs, x) = xs == [T.pack "tam"] && x == T.pack "sayı"

-- | Check whether an identifier names the floating-point type.
isFloatType :: Identifier -- ^ Identifier to inspect.
            -> Bool -- ^ True when identifier names the floating-point type.
isFloatType (xs, x) = xs == [T.pack "ondalık"] && x == T.pack "sayı"

-- | Check whether an identifier names the string type.
isStringType :: Identifier -- ^ Identifier to inspect.
             -> Bool -- ^ True when identifier names the string type.
isStringType (xs, x) = null xs && x == T.pack "dizge"

-- | Convert an expression into a pattern.
expToPat :: Bool -- ^ Whether to allow scrutinee expressions.
         -> [Identifier] -- ^ Bound pattern names.
         -> Exp Ann -- ^ Expression to convert.
         -> KipParser (Pat Ann) -- ^ Parsed pattern.
expToPat allowScrutinee argNames e = do
  MkParserState{parserCtx} <- getP
  case e of
    Var _ _ candidates ->
      case selectCondName parserCtx candidates of
        Nothing -> customFailure ErrPatternExpected
        Just ctorName -> return (PCtor ctorName [])
    App _ (Var _ _ candidates) es -> do
      ctorName <- case selectCondName parserCtx candidates of
        Nothing -> customFailure ErrPatternExpected
        Just n -> return n
      -- Filter out scrutinee expressions before converting to pattern vars
      es' <- if allowScrutinee then dropScrutineeExp argNames es else return es
      vars <- mapM expToPatVar es'
      vars' <- dropScrutinee allowScrutinee argNames vars
      return (PCtor ctorName vars')
    _ -> customFailure ErrPatternExpected
  where
    -- | Detect whether this is a match-expression context.
    -- Drop the first element if it's a valid scrutinee (Var matching argName) or
    -- a complex expression when in match expression context.
    -- For function clauses, complex expressions should fail.
    -- Match expression context is detected by argNames containing the dummy name "_".
    isMatchExprContext :: [Identifier] -- ^ Bound pattern names.
                       -> Bool -- ^ True when parsing a match expression.
    isMatchExprContext args = ([], T.pack "_") `elem` args
    -- | Drop a scrutinee expression when allowed.
    dropScrutineeExp :: [Identifier] -- ^ Bound pattern names.
                     -> [Exp Ann] -- ^ Expression list.
                     -> KipParser [Exp Ann] -- ^ Filtered expressions.
    dropScrutineeExp _ [] = return []
    dropScrutineeExp _ (v@(Var {}):rest) =
      return (v:rest)
    dropScrutineeExp args (_:rest)
      | isMatchExprContext args = return rest  -- Match expression: OK to drop complex expressions
      | otherwise = customFailure ErrPatternComplexExpr -- Function clause: fail

-- | Select a conditional constructor name from candidates.
selectCondName :: [Identifier] -- ^ Context identifiers.
               -> [(Identifier, Case)] -- ^ Candidate identifiers.
               -> Maybe Identifier -- ^ Selected constructor.
selectCondName ctx candidates =
  case [name | (name, cas) <- candidates, cas == Cond, name `elem` ctx] of
    n:_ -> Just n
    [] ->
      case [name | (name, _) <- candidates, name `elem` ctx] of
        n:_ -> Just n
        [] ->
          case strippedCondMatches of
            n:_ -> Just n
            [] ->
              case [name | (name, cas) <- candidates, cas == Cond] of
                n:_ -> Just n
                [] ->
                  case candidates of
                    (n, _):_ -> Just n
                    [] -> Nothing
  where
    strippedCondMatches =
      [ base
      | (name, _) <- candidates
      , Just base <- [stripCondSuffixIdent name]
      , base `elem` ctx
      ]
    stripCondSuffixIdent (mods, word) = do
      base <- stripCondSuffix word
      return (mods, base)
    stripCondSuffix txt =
      let suffixes = ["ysa", "yse"]
          match = find (`T.isSuffixOf` txt) suffixes
      in case match of
           Nothing -> Nothing
           Just suff ->
             let len = T.length suff
             in if T.length txt > len
                  then Just (T.take (T.length txt - len) txt)
                  else Nothing

-- | Convert an expression into a pattern variable.
expToPatVar :: Exp Ann -- ^ Expression to convert.
            -> KipParser (Identifier, Ann) -- ^ Pattern variable and annotation.
expToPatVar expItem =
  case expItem of
    Var _ _ candidates ->
      case preferInflected candidates of
        (n, c):_ -> return (n, mkAnn c NoSpan)
        _ -> customFailure ErrPatternAmbiguousName
    _ -> customFailure ErrPatternOnlyNames

-- | Drop a scrutinee variable from pattern variables when allowed.
dropScrutinee :: Bool -- ^ Whether to allow scrutinee expressions.
              -> [Identifier] -- ^ Bound pattern names.
              -> [(Identifier, Ann)] -- ^ Pattern variables.
              -> KipParser [(Identifier, Ann)] -- ^ Filtered pattern variables.
dropScrutinee allowScrutinee argNames vars =
  case vars of
    ((n, ann):rest) | annCase ann == Nom && n `elem` argNames ->
      if allowScrutinee
        then return rest
        else customFailure ErrPatternArgNameRepeated
    _ -> return vars

-- | Strip nested block comments from source text.
removeComments :: Text -- ^ Raw source text.
               -> Text -- ^ Source without comments.
removeComments = TL.toStrict . TB.toLazyText . go 0
  where
    -- | Recursive comment stripper with nesting depth.
    go :: Int -- ^ Current nesting depth.
       -> Text -- ^ Remaining input.
       -> TB.Builder -- ^ Output builder.
    go n txt =
      case T.uncons txt of
        Nothing -> mempty
        Just (c, rest) ->
          case T.uncons rest of
            Just ('*', rest')
              | c == '(' -> go (n + 1) rest'
            _ ->
              if c == '*' then
                case T.uncons rest of
                  Just (')', rest')
                    | n > 0 -> go (n - 1) rest'
                  _ -> step n c rest
              else step n c rest
    -- | Emit or skip characters based on nesting depth.
    step :: Int -- ^ Current nesting depth.
         -> Char -- ^ Current character.
         -> Text -- ^ Remaining input.
         -> TB.Builder -- ^ Output builder.
    step n c rest
      | n < 0 = error "Açılışı olmayan yorum kapanışı."
      | n == 0 = TB.singleton c <> go n rest
      | otherwise = go n rest

-- | Parse a full statement from the REPL input.
parseFromRepl :: ParserState -- ^ Initial parser state.
              -> Text
              -- ^ REPL input buffer.
              -> Outer (Either (ParseErrorBundle Text ParserError) (Stmt Ann, ParserState)) -- ^ Parsed statement and state.
parseFromRepl st input = do
  (res, st') <- runStateT (runParserT parseStmt "Kip" (removeComments input)) st
  return (fmap (, st') res)

-- | Parse an expression from REPL input.
parseExpFromRepl :: ParserState -- ^ Initial parser state.
                 -> Text
                 -- ^ REPL input buffer.
                 -> Outer (Either (ParseErrorBundle Text ParserError) (Exp Ann)) -- ^ Parsed expression.
parseExpFromRepl st input = do
  (res, _) <- runStateT (runParserT p "Kip" (removeComments input)) st
  return res
  where
    -- | Parser entry for REPL expressions.
    p :: KipParser (Exp Ann) -- ^ Parsed expression.
    p = do
      ws
      e <- parseExp
      ws
      eof
      return e

-- | Parse a full file into statements.
parseFromFile :: ParserState -- ^ Initial parser state.
              -> Text
              -- ^ File contents.
              -> Outer (Either (ParseErrorBundle Text ParserError) ([Stmt Ann], ParserState)) -- ^ Parsed statements and state.
parseFromFile st input = do
  (res, st') <- runStateT (runParserT p "Kip" (removeComments input)) st
  return (fmap (, st') res)
  where
    -- | Parser entry for file contents.
    p :: KipParser [Stmt Ann] -- ^ Parsed statements.
    p = do
      ws
      stmts <- many (parseStmt <* ws)
      eof
      return stmts
