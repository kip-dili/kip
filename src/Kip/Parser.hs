{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{- |
Parser and morphology-aware utilities for Kip syntax.

= Overview

Kip is a Turkish programming language with natural language-like syntax. The parser
must handle Turkish morphology (grammatical cases, possessive forms) to resolve
identifiers and types correctly.

= Grammar Structure

The Kip grammar consists of:

* __Statements__: Top-level declarations
    * Type declarations: @Bir (type-name) ... olabilir.@
    * Function declarations: @(args) function-name, body.@
    * Load statements: @module-name'i yükle.@

* __Expressions__: Values and computations
    * Literals: numbers, strings, booleans
    * Function calls: @(argument'un) function-name'i@
    * Match expressions: @value pattern-ise result1, pattern2-ise result2@
    * Let bindings: @name olarak value@

* __Types__: Type annotations
    * Primitive: @tam-sayı@, @doğruluk@, @dizge@
    * Type constructors: @(öğe listesi)@, @(a b çifti)@
    * Function types: implicit, inferred from usage

= Turkish Morphology Handling

Turkish is an agglutinative language where grammatical information is expressed
through suffixes. The parser uses TRmorph (a finite-state morphological analyzer)
to handle:

* __Grammatical Cases__:
    * Nominative (yalın): @elma@ (apple)
    * Accusative (-i): @elmayı@ (the apple, as object)
    * Genitive (-in): @elmanın@ (of the apple)
    * Dative (-e): @elmaya@ (to the apple)
    * Locative (-de): @elmada@ (at/in the apple)
    * Ablative (-den): @elmadan@ (from the apple)
    * Instrumental (-le): @elmayla@ (with the apple)

* __Possessive Forms__: @elmanın@ can mean "of the apple" or "its apple"

* __Challenges__:
    * Same base word appears in multiple forms: @b@, @b'den@, @b'yle@, @b'nin@
    * Type variables must resolve consistently despite surface form differences
    * Single-letter identifiers (like @a@, @b@) are not recognized by TRmorph

= Parsing Strategy

1. __Lexical Analysis__: Tokenize into identifiers, keywords, operators
2. __Morphological Analysis__: Use TRmorph to generate candidate base forms
3. __Context Resolution__: Match candidates against names/types in scope
4. __AST Construction__: Build typed AST with case annotations

The parser maintains state including:
* @parserCtx@: Identifiers in scope (functions, variables, constructors)
* @parserTyParams@: Type parameters in scope (e.g., @a@, @b@ in polymorphic types)
* @parserTyCons@: Type constructors and their arities
* FSM caches for morphology lookups

= Example

Given: @(bu tam-sayının) faktöriyeli@

1. Lex: @(@, @bu@, @tam-sayının@, @)@, @faktöriyeli@
2. Morphology: @tam-sayının@ → candidates: @(tam-sayı, Gen)@, @(tam-sayının, Nom)@
3. Resolve: @tam-sayı@ is a type in scope, choose @(tam-sayı, Gen)@
4. AST: Function argument @bu@ of type @TyInd "tam-sayı"@
-}
module Kip.Parser where

import Data.List
import Data.Maybe (maybeToList, mapMaybe, isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as M
import Control.Applicative (optional)
import Control.Monad (forM, forM_, guard, unless, when)
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
    ErrInternal msg -> msg

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
    ErrInternal msg -> msg

{- | Hash table type alias for morphology caches.

TRmorph lookups are expensive, so we cache:
* __Ups cache__: surface form → [base forms]
* __Downs cache__: base form → [surface forms]
-}
type MorphCache = HT.BasicHashTable Text [Text]

{- | Parser state tracking context and morphology caches.

The parser state is updated as we parse to track:

* __Scope__: What identifiers are available
* __Type context__: What type constructors and parameters are in scope
* __Morphology caches__: Memoized TRmorph results for performance

= Fields

* @fsm@: The TRmorph finite-state machine for Turkish morphology analysis

* @parserCtx@: Identifiers currently in scope (functions, variables, constructors).
  Updated as we parse declarations. Used to resolve identifier references.
  Example: @["topla", "çarp", "doğru", "yanlış"]@

* @parserTyParams@: Type parameters in scope when parsing a polymorphic type definition.
  Example: When parsing @Bir (a b çifti)@, contains @[([], "a"), ([], "b")]@
  These may have case suffixes if defined with them: @[([], "a'yla"), ([], "b'den")]@

* @parserTyCons@: Type constructors with their arities (number of type parameters).
  Example: @[("liste", 1), ("çift", 2), ("biri", 2)]@
  Used to check type applications have correct number of arguments.

* @parserTyMods@: Type modifiers (prefixes) that expand to full type names.
  Example: @[("tam-sayı", [["yerleşik"]])]@ means @yerleşik tam-sayı@ → @tam-sayı@

* @parserPrimTypes@: Primitive (built-in) type names.
  Example: @["tam-sayı", "ondalık-sayı", "dizge"]@
  These don't need to be declared before use.

* @parserUpsCache@: Morphology analysis cache. Maps surface forms to base forms.
  Example: @"elmaların"@ → @["elma", "elmalar"]@

* @parserDownsCache@: Morphology generation cache. Maps base+case to surface forms.
  Example: @"elma+Gen"@ → @["elmanın"]@
-}
data ParserState =
  MkParserState
    { fsm :: FSM
    , parserCtx :: [Identifier]
    , parserTyParams :: [Identifier]
    , parserTyCons :: [(Identifier, Int)]
    , parserTyMods :: [(Identifier, [Identifier])]
    , parserPrimTypes :: [Identifier]
    , parserUpsCache :: !MorphCache
    , parserDownsCache :: !MorphCache
    }

-- | Parser monad uses IO for morphology lookups.
type Outer = IO
-- | Parser type for Kip with state and IO.
type KipParser = ParsecT ParserError Text (StateT ParserState Outer)

-- | Turkish case suffixes for apostrophe-marked identifiers (type variables).
-- Used for parsing and stripping case markers in multi-parameter types.
turkishCaseSuffixes :: [(String, Case)]
turkishCaseSuffixes =
  [ ("'yla", Ins), ("'yle", Ins), ("'la", Ins), ("'le", Ins)
  , ("'den", Abl), ("'dan", Abl), ("'ten", Abl), ("'tan", Abl)
  , ("'nin", Gen), ("'nın", Gen), ("'nun", Gen), ("'nün", Gen)
  , ("'yi", Acc), ("'yı", Acc), ("'yu", Acc), ("'yü", Acc)
  , ("'ye", Dat), ("'ya", Dat)
  , ("'de", Loc), ("'da", Loc), ("'te", Loc), ("'ta", Loc)
  ]

-- | Pre-populate a morphology cache with common Turkish demonstrative pronouns.
-- These are pattern variables that TRmorph may not analyze correctly.
-- Entries are stored in TRmorph format: "base<case_tag>"
populateDemonstrativeCache :: MorphCache -- ^ Cache to populate.
                           -> IO () -- ^ Populates the cache.
populateDemonstrativeCache cache = do
  -- Turkish demonstrative pronouns: bu (this), şu (that-near), o (that-far)
  -- Each entry maps surface form to TRmorph-style analysis
  let entries =
        [ -- bu (this)
          ("bu", ["bu<nom>"])
        , ("bunu", ["bu<acc>"])
        , ("buna", ["bu<dat>"])
        , ("bunda", ["bu<loc>"])
        , ("bundan", ["bu<abl>"])
        , ("bunun", ["bu<gen>"])
        , ("bunla", ["bu<ins>"])
        , ("bununla", ["bu<ins>"])
          -- şu (that, near listener)
        , ("şu", ["şu<nom>"])
        , ("şunu", ["şu<acc>"])
        , ("şuna", ["şu<dat>"])
        , ("şunda", ["şu<loc>"])
        , ("şundan", ["şu<abl>"])
        , ("şunun", ["şu<gen>"])
        , ("şunla", ["şu<ins>"])
        , ("şununla", ["şu<ins>"])
          -- o (that, away from both)
        , ("o", ["o<nom>"])
        , ("onu", ["o<acc>"])
        , ("ona", ["o<dat>"])
        , ("onda", ["o<loc>"])
        , ("ondan", ["o<abl>"])
        , ("onun", ["o<gen>"])
        , ("onunla", ["o<ins>"])
        ]
  mapM_ (uncurry (HT.insert cache)) entries

-- | Create a new empty parser state with fresh caches.
newParserState :: FSM -- ^ Morphology FSM.
               -> IO ParserState -- ^ Fresh parser state.
newParserState fsm' = do
  upsCache <- HT.new
  populateDemonstrativeCache upsCache
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
  populateDemonstrativeCache upsCache
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

-- | Execute a parser with pattern variables temporarily added to context.
withPatVars :: [Identifier] -- ^ Pattern variable names to add.
            -> KipParser a -- ^ Inner parser.
            -> KipParser a -- ^ Parsed result.
withPatVars patVars p = do
  MkParserState{parserCtx} <- getP
  modifyP (\s -> s{parserCtx = patVars ++ parserCtx})
  result <- p
  modifyP (\s -> s{parserCtx = parserCtx})
  return result

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
  if null ss && s `elem` ["ya", "var", "olarak", "dersek"]
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
{- | Estimate candidate base forms and cases for a surface identifier.

= Purpose

This is the CORE morphology resolution function. Given a Turkish surface form
(e.g., @"kitaplarından"@ - "from their books"), it generates possible base forms
and grammatical cases (e.g., @[("kitap", Abl), ("kitaplar", Abl)]@).

= Algorithm Overview

1. __Fast path__: If @useCtx@ is true and the identifier is already in scope,
   return it immediately as nominative. Avoids expensive morphology lookups.

2. __Morphology analysis__: Use TRmorph (via @upsCached@) to analyze the surface form.
   The FSM returns morphological parses showing base + inflections.

3. __Copula handling__: Turkish copulas (like "dir", "dır") attach to words.
   Try stripping them and analyzing separately.

4. __Context filtering__: If @useCtx@ is true, prefer candidates that are in @parserCtx@.

5. __Special cases__:
   * IP converb roots (gerunds ending in -ip/-ıp/-up/-üp)
   * Possessive forms
   * Conditional forms

6. __Inflection matching__: For identifiers in context, try to match inflected forms
   using TRmorph's "downs" (generation) capability.

= Parameters

* @useCtx@: If True, prefer identifiers in @parserCtx@. Use True when resolving
  variable/function references, False when parsing new names.

* @(ss, s)@: The surface identifier. @ss@ is modifiers (hyphenated prefix parts),
  @s@ is the main word.

= Return Value

A list of @(Identifier, Case)@ pairs representing possible interpretations:
* @Identifier@: The base form (nominative, without inflections)
* @Case@: The grammatical case of the surface form

= Example

Input: @estimateCandidates True ([], "kitaplarından")@

1. Not in context → do morphology
2. TRmorph: "kitap" (book) + Pl + P3p + Abl
3. Candidates: @[("kitap", Abl), ("kitaplar", Abl)]@
4. If "kitap" is in context, filter to: @[("kitap", Abl)]@

= Performance

Uses caching heavily:
* @upsCached@: Memoizes morphology analysis (surface → parses)
* @downsCached@: Memoizes morphology generation (base+case → surfaces)
-}
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

{- | Resolve a type candidate, preferring names in scope.

= Purpose

This function resolves a surface-form type identifier (e.g., @"listesinin"@) to its
base form and grammatical case (e.g., @("liste", Gen)@).

= Type Resolution Strategy

1. __Direct match__: If the identifier exactly matches a type in scope, return it
   with nominative case. This is the fast path.

2. __Morphology analysis__: Use TRmorph to generate candidate base forms and cases.

3. __Filter by context__: Prefer candidates that are known types in scope
   (type constructors, type parameters, or primitive types).

4. __Inflection preference__: Choose inflected forms over base forms when available.

= Example

Given input @"listesinin"@ (genitive of "liste"):

1. Not in @tyNames@ directly → proceed to morphology
2. TRmorph generates: @[("liste", Gen), ("listesi", Nom)]@
3. Filter: @"liste"@ is in @parserTyCons@ → @[("liste", Gen)]@
4. Return: @("liste", Gen)@

= Turkish Morphology Challenges

Type parameters with case suffixes create problems:
* If @parserTyParams = [("b'den")]@ (ablative)
* And we see @"b'yle"@ (instrumental)
* They don't match exactly, even though both are variants of @"b"@

Current approach: Try exact match, then morphology with base-form matching.
-}
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
  if null mods
    then normalizePossessive ident
    else return ident

-- | Resolve a type candidate without requiring it to be in scope.
resolveTypeCandidateLoose :: Identifier -- ^ Surface type identifier.
                          -> KipParser (Identifier, Case) -- ^ Resolved type identifier and case.
resolveTypeCandidateLoose ident = do
  MkParserState{parserTyCons, parserTyParams, parserPrimTypes} <- getP
  let tyNames = map fst parserTyCons ++ parserTyParams ++ parserPrimTypes
      -- Check if identifier matches a primitive type pattern
      isPrimType base = isIntType base || isFloatType base || isStringType base
  -- First try morphology analysis to extract case if present
  mCandidates <- optional (try (estimateCandidates False ident))
  case mCandidates of
    Just candidates -> do
      -- Check if any candidate's base form is in tyNames or matches a primitive pattern
      let tyMatchesWithCase = [(base, cas) | (base, cas) <- candidates,
                                              base `elem` tyNames || isPrimType base]
      case preferInflected tyMatchesWithCase of
        x:_ -> return x  -- Prefer inflected forms
        [] -> case tyMatchesWithCase of
                (base, cas):_ -> return (base, cas)  -- Take first match with its case
                [] -> case preferInflected candidates of
                        x:_ -> return x
                        [] -> return (ident, Nom)
    Nothing ->
      -- No morphology candidates, return nominative
      return (ident, Nom)

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
  letExp <|> seqExp
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
    -- | Parse a let expression with "dersek".
    letExp :: KipParser (Exp Ann) -- ^ Parsed let expression.
    letExp = try $ do
      items <- some (lexeme atom)
      let (exprItems, nameItem) =
            case reverse items of
              name:revExpr -> (reverse revExpr, name)
              [] -> error "let expression needs at least two items"
      (rawName, nameSpan) <- case nameItem of
        Var annExp n _ -> return (n, annSpan annExp)
        _ -> customFailure ErrDefinitionName
      name <- normalizePossessive rawName
      if null exprItems
        then customFailure ErrDefinitionBodyMissing
        else do
          val <- buildAppFrom exprItems
          let bindSpan = mergeSpan (annSpan (annExp val)) nameSpan
              bindAnn = mkAnn (annCase (annExp val)) bindSpan
              bindExp = Bind bindAnn name val
          lexeme (string "dersek")
          lexeme (char ',')
          st <- getP
          putP st { parserCtx = name : parserCtx st }
          body <- parseExpWithCtx useCtx
          putP st
          let ann = mkAnn (annCase (annExp body)) (mergeSpan bindSpan (annSpan (annExp body)))
          return (Seq ann bindExp body)
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
{- | Parse a top-level statement.

= Statement Grammar

Kip has five types of statements:

1. __Load statement__: Import a module
   @
   module-name'i yükle.
   @
   The module name must be in accusative case (-i).

2. __Primitive type__: Declare a built-in type
   @
   Bir yerleşik type-name olsun.
   @
   Example: @Bir yerleşik tam-sayı olsun.@

3. __Type declaration__: Define an algebraic data type
   @
   Bir (params type-name)
   ya constructor1
   ya constructor2
   ya da constructor3
   olabilir.
   @
   Example:
   @
   Bir (öğe listesi)
   ya boş
   ya da bir öğenin bir öğe listesine eki
   olabilir.
   @

4. __Function declaration__: Define a function
   @
   (arg1) (arg2) function-name, body.
   @
   or with match clauses:
   @
   (arg) function-name,
     pattern1-ise, result1,
     pattern2-ise, result2.
   @

5. __Expression statement__: Evaluate and print
   @
   expression.
   @

= Parsing Strategy

We try parsers in order with backtracking:

1. Load statements (must start with identifier + "yükle")
2. Primitive types (must start with "Bir yerleşik")
3. Type declarations (must start with "Bir")
4. Function declarations (start with arguments or identifier)
5. Expression statements (catch-all)
-}
parseStmt :: KipParser (Stmt Ann) -- ^ Parsed statement.
parseStmt = try loadStmt <|> try primTy <|> ty <|> try func <|> expFirst
  where
    -- | Parse a module load statement.
    loadStmt :: KipParser (Stmt Ann) -- ^ Parsed load statement.
    loadStmt = do
      rawName <- identifier
      _ <- lexeme (string "yükle")
      period
      name <- resolveLoadCandidate rawName
      return (Load name)
    {- | Parse constructor identifiers with case annotation.

    Constructors can be in either Nom (nominative) or P3s (3rd person possessive) case.
    We use TRmorph to detect the actual case instead of forcing nominative.

    Examples:
    * @doğru@ → Nom
    * @hata@ → Nom
    * @başarı@ → Nom
    * @eki@ → P3s (possessive form)
    -}
    ctorIdent :: KipParser (Identifier, Ann) -- ^ Parsed constructor identifier with annotation.
    ctorIdent = do
      rawIdent <- identifier
      (_, sp) <- withSpan (return ())
      -- Constructor names should use their surface form without normalization
      -- Only detect the grammatical case for annotation purposes
      candidates <- estimateCandidates False rawIdent
      let cas = case candidates of
                  (_,c):_ -> c
                  [] -> Nom
      return (rawIdent, mkAnn cas sp)
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
    {- | Parse a type declaration.

    = Type Declaration Grammar

    Types are declared with the keyword "Bir" (a/one):

    @
    Bir type-head
    [ya constructor]*
    ya da constructor
    olabilir.
    @

    Or for empty types (like Void):
    @
    Bir type-name var olamaz.
    @

    The type head can be:

    * Parenthesized with parameters: @(a b type-name)@
    * Simple: @type-name@
    * With modifier: @modifier type-name@

    = Examples

    Simple type:
    @
    Bir doğruluk ya doğru ya da yanlış olabilir.
    @

    Parameterized type:
    @
    Bir (öğe listesi)
    ya boş
    ya da bir öğenin bir öğe listesine eki
    olabilir.
    @

    Multi-parameter type:
    @
    Bir (a b çifti)
    ya bir a bir b çifti
    olabilir.
    @

    = Type Parameters and Turkish Morphology

    Type parameters may be written with Turkish case suffixes:
    * @(a'yla b'den biri)@ - "a" with instrumental, "b" with ablative

    When storing in @parserTyParams@, we strip case suffixes and store base forms.
    For example, @a'yla@ is stored as @a@. This allows constructor arguments with
    different case suffixes (like @a'yle@) to match the same type parameter.

    = State Updates

    After parsing the type head, we update parser state:
    1. Add type name to @parserCtx@
    2. Add parameters to @parserTyParams@ (scoped to this type definition)
    3. Record arity in @parserTyCons@
    4. Store any modifiers in @parserTyMods@

    After parsing constructors:
    5. Add constructor names to @parserCtx@
    -}
    ty :: KipParser (Stmt Ann) -- ^ Parsed type declaration.
    ty = do
      lexeme (string "Bir")
      (n, params, mods) <- typeHead
      -- Extract parameter identifiers from TyVar nodes for parser state
      let paramIdents = map (\(TyVar _ ident) -> ident) params
      modifyP (\ps -> ps { parserCtx = n : paramIdents ++ parserCtx ps
                             , parserTyParams = paramIdents ++ parserTyParams ps
                             , parserTyCons = (n, length params) : parserTyCons ps
                             , parserTyMods =
                                 case mods of
                                   [] -> parserTyMods ps
                                   _ -> (n, mods) : parserTyMods ps
                             })
      ctors <- try (lexeme (string "var olamaz") $> [])
           <|> (parseCtors <* lexeme (string "olabilir"))
      period
      -- Validate that no constructor name would normalize to the same name as the type
      -- This check prevents ambiguity in collectArgsLoop where "bir a teklisi" could be
      -- interpreted as type application if "teklisi" normalizes to type "tekli"
      let ctorNames = map (fst . fst) ctors
      forM_ ctorNames $ \ctorName -> do
        normalizedCtorName <- normalizeTypeHead ctorName
        when (normalizedCtorName == n) $
          customFailure (ErrInternal (T.concat
            ["Constructor '", snd ctorName, "' normalizes to '", snd normalizedCtorName,
             "' which matches type name '", snd n, "' and creates parsing ambiguity"]))
      -- Extract constructor names (now from ((Identifier, Ann), [Ty Ann]))
      modifyP (\ps -> ps {parserCtx = n : ctorNames ++ parserCtx ps})
      return (NewType n params ctors)
    -- | Parse a type head (name and parameters).
    typeHead :: KipParser (Identifier, [Ty Ann], [Identifier]) -- ^ Type head parts.
    typeHead = try typeHeadParens <|> typeHeadInline
    {- | Parse a type parameter, handling Turkish case suffixes.

    Type parameters can be written with case suffixes: a'yla, b'nin, etc.
    We need to:
    1. Strip the case suffix to get the base identifier (a, b)
    2. Detect the grammatical case from the suffix
    3. Store both in a TyVar node
    -}
    parseTypeParam :: Identifier -> KipParser (Ty Ann)
    parseTypeParam rawIdent@(mods, word) = do
      (_, sp) <- withSpan (return ())
      let tryStripSuffix [] = Nothing
          tryStripSuffix ((suff, cas):rest) =
            case T.stripSuffix (T.pack suff) word of
              Just base | T.length base > 0 -> Just (base, cas)
              _ -> tryStripSuffix rest
      case tryStripSuffix turkishCaseSuffixes of
        Just (base, cas) ->
          -- Found a case suffix, use base identifier with detected case
          return (TyVar (mkAnn cas sp) (mods, base))
        Nothing -> do
          -- No case suffix found, try regular resolution
          (ident, cas) <- resolveCandidatePreferNom rawIdent
          return (TyVar (mkAnn cas sp) ident)

    -- | Parse a parenthesized type head.
    typeHeadParens :: KipParser (Identifier, [Ty Ann], [Identifier]) -- ^ Parsed type head.
    typeHeadParens = parens $ do
      parts <- identifier `sepBy1` ws
      let (paramIdents, nameIdent) =
            case parts of
              [] -> ([], ([], T.pack ""))
              [_] -> ([], ([], T.pack ""))
              _ -> (init parts, last parts)
      when (length parts < 2) empty
      -- Parse type parameters with case detection
      params <- mapM parseTypeParam paramIdents
      rawName <- fst <$> resolveCandidatePreferNom nameIdent
      name <- normalizeTypeHead rawName
      return (name, params, [])
    -- | Parse a type head without parentheses.
    typeHeadInline :: KipParser (Identifier, [Ty Ann], [Identifier]) -- ^ Parsed type head.
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
      nameWithAnn <- ctorIdent
      return (nameWithAnn, args)
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
      let isDefnCandidate = null args && not isGerund && isNothing mRetTy
      if isDefnCandidate
        then do
          clauses <- parseBodyOnly
          case clauses of
            [Clause PWildcard body] -> do
              modifyP (\ps -> ps {parserCtx = fname : parserCtx ps})
              return (Defn fname (TyString (mkAnn Nom NoSpan)) body)
            _ -> customFailure ErrDefinitionBodyMissing
        else do
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
        rest <- many (try (ws *> withSpan identifier))
        case rest of
          [] -> typeFromIdentLoose firstIdent sp1
          _ -> do
            let allIdents = (firstIdent, sp1) : rest
                argIdents = init allIdents
                (ctorIdent, ctorSp) = last allIdents
            argTys <- mapM (uncurry typeFromIdentLoose) argIdents
            (ctorName, cas) <- resolveTypeCandidateLoose ctorIdent
            let annApp = mkAnn cas (mergeSpan sp1 ctorSp)
                ctorAnn = mkAnn cas ctorSp
            return (TyApp annApp (TyInd ctorAnn ctorName) argTys)
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
      -- Check for wildcard pattern first
      mWildcard <- optional (try (lexeme (string "değilse")))
      case mWildcard of
        Just _ -> do
          lexeme (char ',')
          Clause PWildcard <$> parseExp
        Nothing -> do
          -- Parse pattern expression first to extract pattern variables
          patExp <- parseExpAny
          let patVarNames = extractPatVarNames patExp
          pat <- expToPat allowScrutinee argNames patExp
          lexeme (char ',')
          -- Add pattern variables to context when parsing body
          body <- withPatVars patVarNames parseExp
          return (Clause pat body)
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
    {- | Parse a type with grammatical case information.

    = Type Grammar

    Types in Kip can appear in various forms:

    1. __Simple__: @tam-sayı@, @doğruluk@
    2. __Parenthesized__: @(tam-sayı)@
    3. __Type application__: @(öğe listesi)@, @(a b çifti)@
    4. __Modified__: @yerleşik tam-sayı@ (prefix modifier)

    Types must be "in scope" - declared before use via type declarations or
    available as type parameters.

    = Type Application

    For parameterized types, we collect arguments left-to-right:
    @
    arg1 arg2 ... argN type-constructor
    @

    Example: @(tam-sayı doğruluk çifti)@
    * Arguments: [@tam-sayı@, @doğruluk@]
    * Constructor: @çift@
    * Arity check: @çift@ expects 2 arguments ✓

    = Turkish Morphology Challenges

    Type parameters may be written with different case suffixes:

    * Type defined as: @Bir (a'yla b'den biri)@
    * Case suffixes are stripped during parsing
    * @parserTyParams@ contains base forms: @[([], "a"), ([], "b")]@
    * Constructor arguments with any case suffix can reference these parameters

    == Solution

    The @findMatchingTypeParam@ and @parseTypeParam@ helpers strip case suffixes:
    * @"a'yla"@ → @"a"@ (strip @'yla@, store base form)
    * @"b'den"@ → @"b"@ (strip @'den@, store base form)
    * Constructor argument @"b'yle"@ → @"b"@ → matches parameter @"b"@

    This allows type parameters to be referenced with any case suffix.

    = Implementation Notes

    Helper functions:
    * @requireInCtx@: Validates that a type identifier is in scope
    * @argTy@: Resolves a type argument identifier to a type node
    * @collectArgsLoop@: Greedily collects type arguments with arity checking
    * @findMatchingTypeParam@: Matches identifiers to type params by base form
    -}
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
                else case findMatchingTypeParam name of
                  Just _ -> return ()  -- Base-form matches a type parameter
                  Nothing -> customFailure ErrTypeNotFound
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
            {- | Strip Turkish case suffixes to get base form of an identifier.

            This is a simple heuristic for matching type parameters that appear
            with different case suffixes.

            = Stripped Suffixes

            Covers common Turkish case markers:
            * Instrumental: @'yla@, @'yle@, @'la@, @'le@
            * Ablative: @'den@, @'dan@, @'ten@, @'tan@
            * Genitive: @'nin@, @'nın@, @'nun@, @'nün@
            * Accusative: @'yi@, @'yı@, @'yu@, @'yü@
            * Dative: @'ye@, @'ya@
            * Locative: @'de@, @'da@, @'te@, @'ta@

            = Examples

            * @"b'yle"@ → @"b"@
            * @"b'den"@ → @"b"@
            * @"elmanın"@ → @"elmanın"@ (no apostrophe, unchanged)
            * @"liste'den"@ → @"liste"@

            = Limitations

            * Only handles apostrophe-marked suffixes (for type variables)
            * Doesn't handle full Turkish morphology (use TRmorph for that)
            * Assumes single-letter type variables or simple identifiers
            -}
            stripCaseSuffix :: Identifier -> Identifier
            stripCaseSuffix (mods, word) =
              let lowerWord = T.toLower word
                  suffixes = map fst turkishCaseSuffixes
                  tryStrip [] = word
                  tryStrip (suff:rest) =
                    if T.pack suff `T.isSuffixOf` lowerWord
                      then let len = length suff
                           in if T.length word > len
                                then T.take (T.length word - len) word
                                else tryStrip rest
                      else tryStrip rest
              in (mods, tryStrip suffixes)

            {- | Find a type parameter that matches the identifier by base form.

            = Purpose

            Type parameters are stored in base form (without case suffixes):
            * Type: @Bir (a'yla b'den biri)@
            * @parserTyParams = [([], "a"), ([], "b")]@ (suffixes stripped)

            Constructor arguments may appear with case suffixes:
            * Constructor: @bir b'yle sağı@
            * Surface form: @"b'yle"@ (instrumental)

            = Solution

            Match by stripping case suffixes from the input:
            1. Strip from input: @"b'yle"@ → @"b"@
            2. Look in params for: @"b"@
            3. Found: @"b"@ matches parameter @"b"@ ✓
            4. Return the parameter: @"b"@

            This ensures all references to the same type variable use the
            same base identifier for type checking.

            = Return Value

            * @Just param@: Found a matching parameter (returns original form)
            * @Nothing@: No match found
            -}
            findMatchingTypeParam :: Identifier -- ^ Surface identifier.
                                  -> Maybe Identifier -- ^ Matching type parameter.
            findMatchingTypeParam ident =
              let identBase = stripCaseSuffix ident
                  matchesByBase = filter (\param -> stripCaseSuffix param == identBase) parserTyParams
              in case matchesByBase of
                   param:_ -> Just param
                   [] -> Nothing
            -- | Build a type node for an identifier.
            argTy :: Identifier -- ^ Identifier to convert.
                  -> KipParser (Ty Ann) -- ^ Type node.
            argTy ident = do
              -- First, try base-form matching with type parameters
              case findMatchingTypeParam ident of
                Just paramName -> return (TyVar (mkAnn Nom NoSpan) paramName)
                Nothing -> do
                  -- If no type param match, try normal resolution
                  mResolved <- optional (try (resolveTypeCandidatePreferCtx ident))
                  let pickTy name cas
                        | name `elem` primNames && isIntType name = TyInt (mkAnn cas NoSpan)
                        | name `elem` primNames && isFloatType name = TyFloat (mkAnn cas NoSpan)
                        | name `elem` primNames && isStringType name = TyString (mkAnn cas NoSpan)
                        | name `elem` primNames = TyInd (mkAnn cas NoSpan) name  -- Other primitives like boolean
                        | name `elem` tyNames = TyInd (mkAnn cas NoSpan) name
                        | name `elem` parserTyParams = TyVar (mkAnn cas NoSpan) name
                        | otherwise = TyVar (mkAnn cas NoSpan) name
                  case mResolved of
                    Just (name, cas) ->
                      if name `elem` parserTyParams
                        then return (TyVar (mkAnn cas NoSpan) name)
                        else return (pickTy name cas)
                    Nothing ->
                      return (pickTy ident Nom)
            -- | Check if an identifier refers to a type in scope.
            isTypeIdent :: Identifier -- ^ Surface identifier.
                        -> KipParser Bool -- ^ True when identifier resolves to a type.
            isTypeIdent ident = do
              m <- optional (try (resolveTypeCandidatePreferCtx ident))
              case m of
                Just (name, _) ->
                  return (name `elem` tyNames || name `elem` parserTyParams || name `elem` primNames)
                Nothing -> return False
        let -- Collect all type identifiers greedily, validating arity at the end
            collectArgsLoop soFar = do
              mNext <- optional (try (do
                arg <- identifier
                ws
                nextIdent <- lookAhead identifier
                mNextResolved <- optional (try (resolveTypeCandidatePreferCtx nextIdent))
                case mNextResolved of
                  Just (nextName, _) -> do
                    let isNextType = nextName `elem` tyNames || nextName `elem` parserTyParams || nextName `elem` primNames
                    guard isNextType
                    return arg
                  Nothing -> empty
                ))
              case mNext of
                Just arg -> collectArgsLoop (soFar ++ [arg])
                Nothing -> return soFar
        -- Try to parse as a type application, with arity validation inside try
        mTypeApp <- optional (try (do
          collected <- collectArgsLoop []
          guard (not (null collected))
          (rawIdent, sp) <- withSpan identifier
          (name, cas) <- resolveTypeCandidatePreferCtx rawIdent
          requireInCtx name
          -- Validate arity: check if this type constructor accepts the right number of arguments
          let numArgs = length collected
              mArity = lookup name parserTyCons
              arityMatches = case mArity of
                Just expectedArity -> numArgs == expectedArity
                Nothing ->
                  -- Primitives have arity 0, type params shouldn't appear as constructors here
                  (name `elem` primNames) && (numArgs == 0)
          guard arityMatches  -- Allow backtracking if arity doesn't match
          let cas' = preferSurfaceCase rawIdent cas
              ann = mkAnn cas' sp
          if name `elem` primNames && isIntType name
            then return (TyInt ann)
            else if name `elem` primNames && isFloatType name
              then return (TyFloat ann)
              else if name `elem` primNames && isStringType name
                then return (TyString ann)
                else do
                  argTys <- mapM argTy collected
                  return (TyApp ann (TyInd (mkAnn Nom NoSpan) name) argTys)
          ))
        case mTypeApp of
          Just ty -> return ty
          Nothing -> do
            (rawIdent, sp) <- withSpan identifier
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
                        argTy' <- argTy argIdent
                        return (TyApp ann (TyInd (mkAnn Nom NoSpan) baseName) [argTy'])
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

-- | Extract pattern variable names from a pattern expression.
extractPatVarNames :: Exp Ann -- ^ Pattern expression.
                   -> [Identifier] -- ^ Pattern variable names.
extractPatVarNames e =
  case e of
    Var {} -> []  -- Constructor with no arguments
    App _ (Var {}) es -> concatMap getVarName es
    _ -> []
  where
    getVarName (Var _ varName candidates) =
      case preferInflected candidates of
        (n, _):_ -> [n]
        -- If no inflected candidates, try all candidates
        _ -> case candidates of
               (n, _):_ -> [n]
               _ -> [varName]  -- Fallback to surface form
    getVarName _ = []

-- | Parse a match expression with optional context filtering.
parseMatchExpr :: Bool -- ^ Whether to use context when resolving names.
               -> KipParser (Exp Ann) -- ^ Parsed match expression.
parseMatchExpr useCtx = do
  (patExp, scrutVar, scrutName) <- parseFirstPattern
  let argNames = [scrutName]
      patVarNames = extractPatVarNames patExp
  pat <- expToPat True argNames patExp
  lexeme (char ',')
  -- Add pattern variables to context when parsing body
  body <- withPatVars patVarNames (parseExpWithCtx useCtx)
  let clause1 = Clause pat body
  clauses <- parseMoreClauses argNames patVarNames
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
                     -> [Identifier] -- ^ Pattern variable names from first clause.
                     -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseMoreClauses argNames _ = do
      mcomma <- optional (try (lexeme (char ',')))
      case mcomma of
        Nothing -> return []
        Just _ -> do
          patExp <- parseWildcardOrExp
          let clausePatVars = maybe [] extractPatVarNames patExp
          pat <- case patExp of
                   Nothing -> return PWildcard
                   Just e -> expToPat False argNames e
          lexeme (char ',')
          body <- withPatVars clausePatVars (parseExpWithCtx useCtx)
          rest <- parseMoreClauses argNames clausePatVars
          return (Clause pat body : rest)
    -- | Parse either a wildcard pattern or a pattern expression.
    parseWildcardOrExp :: KipParser (Maybe (Exp Ann)) -- ^ Nothing for wildcard, Just exp otherwise.
    parseWildcardOrExp =
      (lexeme (string "değilse") $> Nothing) <|>
      (Just <$> parseExpAny)
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
