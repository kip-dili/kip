{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
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
    * Load statements: @dir\/module-name'i yükle.@

* __Expressions__: Values and computations
    * Literals: numbers, strings, booleans
    * Function calls: @(argument'un) function-name'i@
    * Match expressions: @value pattern-ise result1, pattern2-ise result2@
    * Let bindings: @name için value@

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
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList, mapMaybe, listToMaybe, isJust, isNothing, fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Control.Applicative (optional)
import Control.Monad (foldM, forM, forM_, guard, unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put, modify, runStateT)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Char (isLetter, isDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import Data.Void (Void)
import Data.Hashable (Hashable)
import qualified Data.HashTable.IO as HT
import Data.Functor (($>))
import Text.Read (readMaybe)
import System.IO.Unsafe (unsafePerformIO)

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


import Language.Foma
import Kip.AST

-- | Parser-specific error categories for human-friendly rendering.
data ParserError
  = ErrKeywordAsIdent
  | ErrNoMatchingNominative
  | ErrUnrecognizedTurkishWord Text Span [Text]
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
  | ErrPatternBinderRepeated Identifier Span
  | ErrAmbiguousBareApplication Span
  | ErrAmbiguousBareApplicationOverload Identifier [Int] Span
  | ErrYaDaInvalid
  | ErrInternal Text
  deriving (Eq, Ord, Show)

-- | Render parser errors in Turkish.
renderParserErrorTr :: ParserError -> Text
renderParserErrorTr err =
  case err of
    ErrKeywordAsIdent -> "Anahtar kelime isim yerine kullanılamaz."
    ErrNoMatchingNominative -> "Buraya uyan yalın halde bir isim bulunamadı."
    ErrUnrecognizedTurkishWord w _ suggestions ->
      let base = "'" <> w <> "' Türkçe bir kelime olarak tanınmadığı için burada kullanılamaz."
      in case suggestions of
           [] -> base
           _ ->
             base <> " Şunlardan birini mi demek istediniz: "
               <> T.intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"
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
    ErrPatternBinderRepeated ident _ ->
      "Örüntüde aynı bağlayıcı isim tekrar edilemez: " <> identText ident
    ErrAmbiguousBareApplication _ ->
      "Parantezsiz çağrı burada belirsiz. İç çağrıları parantezleyin."
    ErrAmbiguousBareApplicationOverload ident arities _ ->
      if null arities
        then "'" <> identText ident
             <> "' işlevi farklı argüman sayılarıyla aşırı yüklenmiş olabilir. "
             <> "Parantezsiz çağrı bu yüzden belirsiz; alt ifadeleri parantezleyin."
        else
          let arityTxt = T.intercalate ", " (map (T.pack . show) arities)
          in "'" <> identText ident
             <> "' işlevinin farklı argüman sayılarıyla aşırı yüklenmiş sürümleri var ("
             <> arityTxt
             <> "). Parantezsiz çağrı bu yüzden belirsiz; alt ifadeleri parantezleyin."
    ErrYaDaInvalid -> "\"ya da\" yalnızca çok yapkılı tiplerin son yapkısında kullanılabilir."
    ErrInternal msg -> msg

-- | Render parser errors in English.
renderParserErrorEn :: ParserError -> Text
renderParserErrorEn err =
  case err of
    ErrKeywordAsIdent -> "A keyword cannot be used as an identifier."
    ErrNoMatchingNominative -> "No matching nominative identifier found here."
    ErrUnrecognizedTurkishWord w _ suggestions ->
      let base = "'" <> w <> "' is not recognized as a Turkish word, so it cannot be used here."
      in case suggestions of
           [] -> base
           _ ->
             base <> " Did you mean one of: "
               <> T.intercalate ", " (map (\s -> "'" <> s <> "'") suggestions) <> "?"
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
    ErrPatternBinderRepeated ident _ ->
      "The same binder name cannot be repeated in a pattern: " <> identText ident
    ErrAmbiguousBareApplication _ ->
      "Parenthesis-free call is ambiguous here. Parenthesize inner calls."
    ErrAmbiguousBareApplicationOverload ident arities _ ->
      if null arities
        then "'" <> identText ident
             <> "' may be overloaded with different arities. "
             <> "This parenthesis-free call is ambiguous; parenthesize subexpressions."
        else
          let arityTxt = T.intercalate ", " (map (T.pack . show) arities)
          in "'" <> identText ident
             <> "' is overloaded with multiple arities ("
             <> arityTxt
             <> "). This parenthesis-free call is ambiguous; parenthesize subexpressions."
    ErrYaDaInvalid -> "\"ya da\" can only appear before the last constructor."
    ErrInternal msg -> msg

{- | Hash table type alias for morphology caches.

TRmorph lookups are expensive, so we cache:
* __Ups cache__: surface form → [base forms]
* __Downs cache__: base form → [surface forms]
-}
type MorphCache = HT.BasicHashTable Text [Text]

{-# INLINE stripSuffixAny #-}
stripSuffixAny :: [Text] -> Text -> Maybe Text
stripSuffixAny suffixes txt = go suffixes
  where
    go [] = Nothing
    go (s:ss) =
      case T.stripSuffix s txt of
        Just base | not (T.null base) -> Just base
        _ -> go ss

condSuffixesTxt :: [Text]
condSuffixesTxt = ["ysa", "yse", "sa", "se"]

ipSuffixesTxt :: [Text]
ipSuffixesTxt = ["ip", "ıp", "up", "üp"]

copulaSuffixesTxt :: [Text]
copulaSuffixesTxt = ["dir","dır","dur","dür","tir","tır","tur","tür"]

genSuffixesNoApostropheTxt :: [Text]
genSuffixesNoApostropheTxt = ["nın", "nin", "nun", "nün", "ın", "in", "un", "ün"]

{-# INLINE hasCondSuffix #-}
hasCondSuffix :: Text -> Bool
hasCondSuffix = isJust . stripSuffixAny condSuffixesTxt

{-# INLINE identScore #-}
identScore :: Identifier -> Int
identScore (mods, word) = sum (map T.length mods) + T.length word

-- Keep only the first candidate with maximal identifier size.
chooseLongestCandidate :: [(Identifier, Case)] -> [(Identifier, Case)]
chooseLongestCandidate [] = []
chooseLongestCandidate (x:xs) =
  let (!best, !_bestScore) = foldl' step (x, identScore (fst x)) xs
  in [best]
  where
    step (!best, !bestScore) cur =
      let !curScore = identScore (fst cur)
      in if curScore > bestScore
           then (cur, curScore)
           else (best, bestScore)

-- | Memoized possessive normalization roots for the current process.
--
-- ==== Performance note (Optimization: morphology normalization memo)
-- @normalizePossessive@ is hit repeatedly while loading and resolving stdlib
-- names. Keeping a process-local cache avoids repeating the same
-- morphology-based normalization work for identical surface words.
{-# NOINLINE normalizePossessiveCache #-}
normalizePossessiveCache :: IORef (M.Map Text Text)
normalizePossessiveCache = unsafePerformIO (newIORef M.empty)

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
    -- ==== Performance note (Optimization: strict parser state spine)
    -- These strict fields keep parser context/index updates in WHNF as the
    -- state is threaded through large files and stdlib bootstrap.
    { fsm :: FSM
    , parserCtx :: !(Set.Set Identifier)
    , parserCtors :: ![Identifier]
    , parserTyParams :: ![Identifier]
    , parserTyCons :: ![(Identifier, Int)]
    -- | Cached type-constructor names to avoid rebuilding @map fst parserTyCons@
    -- on hot lookup paths.
    , parserTyConsNames :: ![Identifier]
    , parserTyMods :: ![(Identifier, [Identifier])]
    , parserPrimTypes :: ![Identifier]
    , parserFuncArities :: !(M.Map Identifier (Set.Set Int))
    -- | Secondary index used by hot-path overload checks.
    --
    -- ==== Performance note (Optimization: parser overload lookup)
    -- Ambiguity detection in REPL used to scan the whole
    -- 'parserFuncArities' map to gather all entries with matching surface
    -- names. Keeping this precomputed name-based map makes those lookups
    -- O(log n) and allocation-light.
    , parserFuncAritiesByName :: !(M.Map Text (Set.Set Int))
    , parserDefSpans :: !(M.Map Identifier [Span])
    , parserFilePath :: !(Maybe FilePath)
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

turkishCaseSuffixesTxt :: [Text]
turkishCaseSuffixesTxt = map (T.pack . fst) turkishCaseSuffixes

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
-- newParserState :: FSM -- ^ Morphology FSM.
--                -> IO ParserState -- ^ Fresh parser state.
-- newParserState fsm' = do
--   upsCache <- HT.new
--   populateDemonstrativeCache upsCache
--   MkParserState fsm' Set.empty [] [] [] [] [] M.empty Nothing upsCache <$> HT.new

-- | Create a parser state with a given context and fresh caches.
newParserStateWithCtx :: FSM -- ^ Morphology FSM.
                      -> Set.Set Identifier -- ^ Initial identifier context.
                      -> [Identifier] -- ^ Initial constructor identifiers.
                      -> [Identifier] -- ^ Initial type parameters.
                      -> [(Identifier, Int)] -- ^ Type constructor arities.
                      -> [(Identifier, [Identifier])] -- ^ Type modifiers.
                      -> [Identifier] -- ^ Primitive types.
                      -> Maybe FilePath -- ^ Optional file path for span tracking.
                      -> IO ParserState -- ^ Fresh parser state.
newParserStateWithCtx fsm' ctx ctors tyParams tyCons tyMods primTypes mFilePath = do
  upsCache <- HT.new
  populateDemonstrativeCache upsCache
  let !tyConsNames = map fst tyCons
  MkParserState fsm' ctx ctors tyParams tyCons tyConsNames tyMods primTypes M.empty M.empty M.empty mFilePath upsCache <$> HT.new

-- | Create a parser state with shared caches (for parse/render reuse).
newParserStateWithCaches :: FSM -- ^ Morphology FSM.
                         -> Maybe FilePath -- ^ Optional file path for span tracking.
                         -> MorphCache -- ^ Shared ups cache.
                         -> MorphCache -- ^ Shared downs cache.
                         -> ParserState -- ^ Parser state.
newParserStateWithCaches fsm' =
  MkParserState fsm' Set.empty [] [] [] [] [] [] M.empty M.empty M.empty

-- | Create a parser state with context and shared caches.
--
-- ==== Performance note (Optimization: parser state indexing)
-- We compute 'parserFuncAritiesByName' eagerly here so all call sites
-- (fresh states and cache-restored states) get the same fast-path lookup.
newParserStateWithCtxAndCaches :: FSM -- ^ Morphology FSM.
                               -> Set.Set Identifier -- ^ Initial identifier context.
                               -> [Identifier] -- ^ Initial constructor identifiers.
                               -> [Identifier] -- ^ Initial type parameters.
                               -> [(Identifier, Int)] -- ^ Type constructor arities.
                               -> [(Identifier, [Identifier])] -- ^ Type modifiers.
                               -> [Identifier] -- ^ Primitive types.
                               -> M.Map Identifier (Set.Set Int) -- ^ Known function arities.
                               -> M.Map Identifier [Span] -- ^ Definition spans.
                               -> Maybe FilePath -- ^ Optional file path for span tracking.
                               -> MorphCache -- ^ Shared ups cache.
                               -> MorphCache -- ^ Shared downs cache.
                               -> ParserState -- ^ Parser state.
newParserStateWithCtxAndCaches fsm' ctx ctors tyParams tyCons tyMods primTypes funcArities =
  let !tyConsNames = map fst tyCons
      !arityByName = funcAritiesNameIndex funcArities
  in MkParserState
    fsm'
    ctx
    ctors
    tyParams
    tyCons
    tyConsNames
    tyMods
    primTypes
    funcArities
    arityByName

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

-- | Record a definition span for later lookup (prefers the first occurrence).
recordDefSpan :: Identifier -> Span -> KipParser ()
recordDefSpan ident sp =
  modifyP (\ps -> ps { parserDefSpans = M.insertWith (flip (<>)) ident [sp] (parserDefSpans ps) })

-- | Track declared arities for functions seen by the parser.
--
-- ==== Performance note (Optimization: incremental index maintenance)
-- Updates both arity maps in one state transition so the secondary name
-- index stays in sync without requiring periodic rebuilds.
registerFuncArity :: Identifier -> Int -> KipParser ()
registerFuncArity ident arity =
  modifyP (\ps ->
    ps
      { parserFuncArities =
          M.insertWith Set.union ident (Set.singleton arity) (parserFuncArities ps)
      , parserFuncAritiesByName =
          M.insertWith Set.union (snd ident) (Set.singleton arity) (parserFuncAritiesByName ps)
      })

-- | Build a secondary arity index keyed only by function surface name.
--
-- This is used by parenthesis-free ambiguity checks that operate on
-- inflected surface forms where only the final word is needed for
-- overload grouping.
funcAritiesNameIndex :: M.Map Identifier (Set.Set Int) -> M.Map Text (Set.Set Int)
funcAritiesNameIndex =
  M.foldlWithKey'
    (\acc (_, name) arities -> M.insertWith Set.union name arities acc)
    M.empty

-- | Parse an item and return it with a span.
withSpan :: KipParser a -- ^ Parser to wrap.
         -> KipParser (a, Span) -- ^ Parsed value with span.
withSpan p = do
  start <- getSourcePos
  x <- p
  end <- getSourcePos
  pState <- getP
  return (x, Span start end (parserFilePath pState))

-- | Execute a parser with pattern variables temporarily added to context.
withPatVars :: [Identifier] -- ^ Pattern variable names to add.
            -> KipParser a -- ^ Inner parser.
            -> KipParser a -- ^ Parsed result.
withPatVars patVars p = do
  MkParserState{parserCtx} <- getP
  modifyP (\s -> s{parserCtx = Set.fromList patVars `Set.union` parserCtx})
  result <- p
  modifyP (\s -> s{parserCtx = parserCtx})
  return result

-- | O(n log n) duplicate removal preserving first-occurrence order.
ordNub :: Ord a => [a] -> [a]
ordNub xs = reverse uniquesRev
  where
    ( _, uniquesRev) = foldl' step (Set.empty, []) xs
    step (!seen, acc) x
      | x `Set.member` seen = (seen, acc)
      | otherwise =
          let !seen' = Set.insert x seen
          in (seen', x : acc)

-- | Append items to an already deduplicated list, keeping first occurrence.
--
-- ==== Performance note (Optimization 12)
-- This avoids re-running 'ordNub' over the entire left list when only a
-- small right list is appended.
ordNubAppend :: Ord a => [a] -> [a] -> [a]
ordNubAppend xs ys = xs ++ reverse extrasRev
  where
    -- Build only the new unique suffix in reverse order, then append once.
    -- This keeps complexity linear in both lists.
    (_, extrasRev) = foldl' step (Set.fromList xs, []) ys
    step (!seen, acc) z
      | z `Set.member` seen = (seen, acc)
      | otherwise =
          let !seen' = Set.insert z seen
          in (seen', z : acc)

-- | Whitespace parser.
ws :: KipParser () -- ^ No result.
ws = L.space space1 empty empty <?> "boşluk"

-- | Parse a period token with surrounding whitespace.
period :: KipParser () -- ^ No result.
period = ws >> string "." >> ws

-- | Parse a clause separator between match clauses.
clauseSep :: KipParser ()
clauseSep = lexeme (char ';') $> ()

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
  if null ss && s `elem` ["ya", "var", "için", "olarak", "dersek"]
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
   * IP converb roots (infinitives ending in -ip/-ıp/-up/-üp)
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
      mBareCase = stripBareCaseSuffix (ss, s)
  -- Fast path when the surface form is already in scope: exact context hits
  -- must win over heuristic bare-suffix stripping (e.g. "eki" must not be
  -- reinterpreted as "ek" + Acc).
  if useCtx && directIdent `Set.member` parserCtx
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
      let isIpSurface = isJust (stripIpSuffix s)
          ipFallbackMatch =
            if not useCtx
              then Nothing
              else do
                stripped <- stripIpSuffix s
                let stripBufferY txt =
                      case T.unsnoc txt of
                        Just (pref, 'y') | not (T.null pref) -> Just pref
                        _ -> Nothing
                    fallbackRoots = ordNub (stripped : maybeToList (stripBufferY stripped))
                    matches =
                      [ ((ss, root), Nom)
                      | root <- fallbackRoots
                      , (ss, root) `Set.member` parserCtx
                      ]
                if null matches then Nothing else Just (chooseLongestCandidate matches)
      case ipFallbackMatch of
        Just match -> return match
        Nothing -> do
              possBase <- normalizePossessive (ss, s)
              candidates0 <- candidatesForWithAnalyses s parserCtx sAnalyses
              mBarePossBase <-
                case mBareCase of
                  Just (base, _) -> Just <$> normalizePossessive base
                  Nothing -> return Nothing
              copulaCtxHits <-
                case mCopula of
                  Just stripped | useCtx -> do
                    let strippedIdent = (ss, stripped)
                        -- For hyphenated identifiers, also try the joined form:
                        -- (["boş"], "sözlük") → ([], "boş-sözlük")
                        joinedIdent
                          | null ss   = Nothing
                          | otherwise = Just ([], T.intercalate "-" (ss ++ [stripped]))
                    strippedBase <- normalizePossessive strippedIdent
                    return
                      ( [ (strippedIdent, Nom)
                        | strippedIdent `Set.member` parserCtx
                        ]
                        ++
                        [ (strippedBase, Nom)
                        | strippedBase `Set.member` parserCtx
                        , strippedBase /= strippedIdent
                        ]
                        ++
                        [ (joined, Nom)
                        | Just joined <- [joinedIdent]
                        , joined `Set.member` parserCtx
                        ]
                      )
                  _ -> return []
              -- | For bare suffix forms (no apostrophe), we may synthesize
              -- candidates from surface clues to keep P3s/Acc disambiguation
              -- workable (for example, forms like @varlığı@).
              --
              -- IMPORTANT: synthesis is guarded by TRmorph availability
              -- (`null sAnalyses -> []`). This prevents accepting words that
              -- morphology cannot analyze.
              let bareCaseCandidates =
                    case mBareCase of
                      _ | null sAnalyses -> []
                      Just (base, Acc) | not useCtx || base `Set.member` parserCtx ->
                        if hasBufferedAccSuffix s
                          then [(base, Acc)]
                          else
                            (base, Acc) :
                            [(possBase, P3s) | possBase /= (ss, s) && (not useCtx || possBase `Set.member` parserCtx)] ++
                            [(base, P3s)]
                      Just (base, cas) ->
                        let direct = [(base, cas) | not useCtx || base `Set.member` parserCtx]
                            poss =
                              case cas of
                                Ins ->
                                  case mBarePossBase of
                                    Just possBase' | possBase' /= base && (not useCtx || possBase' `Set.member` parserCtx) ->
                                      [(possBase', cas)]
                                    _ -> []
                                _ -> []
                        in ordNubAppend direct poss
                      _ -> []
                  -- Also allow a candidate inferred from the surface suffix
                  -- when morphology doesn't provide a case.
                  candidates0' = addSurfaceCaseCandidate s candidates0
                  candidates0'' = ordNubAppend (ordNub candidates0') bareCaseCandidates
              -- Conditional (-sa/-se) gets special casing to avoid losing
              -- constructor matches when morphology is ambiguous.
              condExtra0 <- condCandidatesM s
              let candidates = ordNubAppend candidates0'' condExtra0
                  filtered0 = filter (\(ident, _) -> ident `Set.member` parserCtx) candidates
                  filtered =
                    if any (\(_, cas) -> cas == Cond) candidates
                      then ordNubAppend (ordNub filtered0) (filter (\(_, cas) -> cas == Cond) candidates)
                      else filtered0
                  filteredWithCopula = ordNubAppend (ordNub filtered) copulaCtxHits
              if useCtx && not isIpSurface && not (null filteredWithCopula)
                then return filteredWithCopula
                else do
                  -- Fast path for ip-converb roots already in scope.
                  -- Prefer TRmorph analyses; keep suffix stripping only as a
                  -- fallback when analyses are missing.
                  let ipRootsFromAnalyses =
                        ordNub
                          [ T.takeWhile (/= '<') analysis
                          | analysis <- sAnalyses
                          , "<cv:ip>" `T.isInfixOf` analysis
                          ]
                      ipCandidatesFromAnalyses =
                        [ ((ss, root), Nom)
                        | root <- ipRootsFromAnalyses
                        , (ss, root) `Set.member` parserCtx
                        ]
                      ipMatch = do
                        stripped <- stripIpSuffix s
                        let stripBufferY txt =
                              case T.unsnoc txt of
                                Just (pref, 'y') | not (T.null pref) -> Just pref
                                _ -> Nothing
                            fallbackRoots = ordNub (stripped : maybeToList (stripBufferY stripped))
                            fallbackMatches =
                              [ ((ss, root), Nom)
                              | root <- fallbackRoots
                              , (ss, root) `Set.member` parserCtx
                              ]
                            allMatches = ordNubAppend (ordNub ipCandidatesFromAnalyses) fallbackMatches
                        if useCtx && not (null allMatches)
                          then Just (chooseLongestCandidate allMatches)
                          else Nothing
                  case ipMatch of
                    Just match -> return match
                    Nothing ->
                      case mCopula of
                        Just stripped -> do
                          baseIdent <- normalizePossessive (ss, stripped)
                          let strippedIdent = (ss, stripped)
                              joinedIdent
                                | null ss   = Nothing
                                | otherwise = Just ([], T.intercalate "-" (ss ++ [stripped]))
                              copulaCtxHits =
                                [ (strippedIdent, Nom)
                                | strippedIdent `Set.member` parserCtx
                                ]
                                ++
                                [ (baseIdent, Nom)
                                | baseIdent `Set.member` parserCtx
                                , baseIdent /= strippedIdent
                                ]
                                ++
                                [ (joined, Nom)
                                | Just joined <- [joinedIdent]
                                , joined `Set.member` parserCtx
                                ]
                          if useCtx && not (null copulaCtxHits)
                            then return copulaCtxHits
                            else do
                              strippedAnalyses <- case mCopulaAnalyses of
                                Just (_, analyses) -> return analyses
                                Nothing -> upsCached stripped
                              possBase1 <- normalizePossessive (ss, stripped)
                              candidates1 <- candidatesForWithAnalyses stripped parserCtx strippedAnalyses
                              -- | Same synthesis rule for copula-stripped forms.
                              -- As above, this is only enabled when TRmorph
                              -- produced analyses for the stripped surface.
                              let bareCaseCandidates1 =
                                    case stripBareCaseSuffix (ss, stripped) of
                                      _ | null strippedAnalyses -> []
                                      Just (base, Acc) | not useCtx || base `Set.member` parserCtx ->
                                        if hasBufferedAccSuffix stripped
                                          then [(base, Acc)]
                                          else
                                            (base, Acc) :
                                            [(possBase1, P3s) | possBase1 /= (ss, stripped) && (not useCtx || possBase1 `Set.member` parserCtx)] ++
                                            [(base, P3s)]
                                      Just (base, cas) | not useCtx || base `Set.member` parserCtx -> [(base, cas)]
                                      _ -> []
                                  candidates1' = addSurfaceCaseCandidate stripped candidates1
                                  candidates1'' = ordNubAppend (ordNub candidates1') bareCaseCandidates1
                              condExtra1 <- condCandidatesM stripped
                              let candidates' = ordNubAppend candidates1'' condExtra1
                                  filtered1 = filter (\(ident, _) -> ident `Set.member` parserCtx) candidates'
                                  filtered' =
                                    if any (\(_, cas) -> cas == Cond) candidates'
                                      then ordNubAppend (ordNub filtered1) (filter (\(_, cas) -> cas == Cond) candidates')
                                      else filtered1
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
                     -> Set.Set Identifier -- ^ Context identifiers.
                     -> KipParser (Maybe (Identifier, Case)) -- ^ Unique context match, if any.
    findCtxCandidate mods surfaceRoot ctx = do
      analyses <- upsCached surfaceRoot
      findCtxCandidateWithAnalyses mods surfaceRoot ctx analyses

    -- | Find a matching identifier in context using pre-fetched analyses.
    findCtxCandidateWithAnalyses :: [Text] -- ^ Identifier modifiers.
                                 -> Text -- ^ Surface root.
                                 -> Set.Set Identifier -- ^ Context identifiers.
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
        collectAllMatches xs = do
          (_, revAcc) <- foldM collectOne (Set.empty, []) xs
          return (reverse revAcc)
          where
            addUnique :: (Set.Set (Identifier, Case), [(Identifier, Case)])
                      -> (Identifier, Case)
                      -> (Set.Set (Identifier, Case), [(Identifier, Case)])
            addUnique (!seen, revAcc) candidate
              | candidate `Set.member` seen = (seen, revAcc)
              | otherwise =
                  let !seen' = Set.insert candidate seen
                  in (seen', candidate : revAcc)
            addMany :: (Set.Set (Identifier, Case), [(Identifier, Case)])
                    -> [(Identifier, Case)]
                    -> (Set.Set (Identifier, Case), [(Identifier, Case)])
            addMany = foldl' addUnique
            collectOne :: (Set.Set (Identifier, Case), [(Identifier, Case)])
                       -> Text
                       -> KipParser (Set.Set (Identifier, Case), [(Identifier, Case)])
            collectOne acc analysis =
              case getPossibleCase analysis of
                Nothing -> return acc
                Just (baseRoot, cas) -> do
                  let direct = (mods, baseRoot)
                  matches <-
                    if direct `Set.member` ctx
                      then return [(direct, cas)]
                      else do
                        let stem = stripCaseTags analysis
                        forms <- downsCached stem
                        p3sForms <-
                          if "<p3s>" `T.isInfixOf` analysis && cas /= P3s
                            then downsCached (stem <> "<p3s>")
                            else return []
                        let roots = ordNub (forms ++ p3sForms ++ [baseRoot])
                            rootsInCtx = filter (`Set.member` ctx) [(mods, root) | root <- roots]
                        return [(rootInCtx, cas) | rootInCtx <- rootsInCtx]
                  return $! addMany acc matches
    allCases = [Nom, Acc, Dat, Gen, Loc, Abl, Ins, Cond, P3s]
    candidatesForWithAnalyses :: Text -- ^ Surface form.
                              -> Set.Set Identifier -- ^ Context identifiers.
                              -> [Text] -- ^ Morphology analyses.
                              -> KipParser [(Identifier, Case)] -- ^ Candidate identifiers.
    candidatesForWithAnalyses _surface ctx morphAnalyses = do
      let stems = map stripCaseTags morphAnalyses
          uniqueStems = ordNub stems
          -- Collect stems that need a possessive form so we can batch downs.
          p3sStems = ordNub
            [ stem <> "<p3s>"
            | (analysis, stem) <- zip morphAnalyses stems
            , "<p3s>" `T.isInfixOf` analysis
            ]
      -- Batch morphology generation to amortize Foma handle setup.
      stemForms <- downsCachedBatch uniqueStems
      p3sForms <- downsCachedBatch p3sStems
      let stemMap = M.fromList (zip uniqueStems stemForms)
          p3sMap = M.fromList (zip p3sStems p3sForms)
          hasCompoundP3sAcc =
            any (\analysis -> "<p3s>" `T.isInfixOf` analysis && "<acc>" `T.isInfixOf` analysis) morphAnalyses
          addUnique :: Ord a => (Set.Set a, [a]) -> a -> (Set.Set a, [a])
          addUnique (!seen, revAcc) x
            | x `Set.member` seen = (seen, revAcc)
            | otherwise =
                let !seen' = Set.insert x seen
                in (seen', x : revAcc)
          addMany :: Ord a => (Set.Set a, [a]) -> [a] -> (Set.Set a, [a])
          addMany = foldl' addUnique
      -- Build and deduplicate candidates incrementally via a Set-backed
      -- builder, avoiding repeated left-list appends.
      (_, candidatesRev) <- foldM
        (\candState (y, stem) ->
          case getPossibleCase y of
            Nothing -> return candState
            Just (_, cas) -> do
              let baseRoot = T.takeWhile (/= '<') y
                  direct = (ss, baseRoot)
              -- Fast path: if the base root is already in context, avoid costly downs.
              if direct `Set.member` ctx && not ("<lik>" `T.isInfixOf` y)
                then return $! addUnique candState (direct, cas)
                else do
                  let forms = fromMaybe [] (M.lookup stem stemMap)
                      p3sFormsFor =
                        if "<p3s>" `T.isInfixOf` y
                          then fromMaybe [] (M.lookup (stem <> "<p3s>") p3sMap)
                          else []
                      includeBaseRoot = not ("<lik>" `T.isInfixOf` y)
                      roots
                        | null forms && null p3sFormsFor = [baseRoot]
                        | otherwise =
                            let (rootSeen0, rootRev0) = addMany (Set.empty, []) forms
                                (rootSeen1, rootRev1) = addMany (rootSeen0, rootRev0) p3sFormsFor
                                (_rootSeen2, rootRev2) =
                                  if includeBaseRoot
                                    then addUnique (rootSeen1, rootRev1) baseRoot
                                    else (rootSeen1, rootRev1)
                            in reverse rootRev2
                      newCandidates = [((ss, root), cas) | root <- roots]
                  return $! addMany candState newCandidates
        )
        (Set.empty, [])
        (zip morphAnalyses stems)
      let candidatesDedup = reverse candidatesRev
          candidates =
            if not hasCompoundP3sAcc
              then
                let (p3s, rest) = partition (\(_, cas) -> cas == P3s) candidatesDedup
                in if null p3s then candidatesDedup else p3s ++ rest
              else candidatesDedup
      return candidates
    -- | Conditional suffix candidates are generated outside morphology to
    -- keep "ise" variants available even when analyses are missing.
    condCandidatesM :: Text -- ^ Surface form.
                    -> KipParser [(Identifier, Case)] -- ^ Candidate conditional forms.
    condCandidatesM surface =
      case stripCondSuffix surface of
        Just base -> return [((ss, base), Cond)]
        Nothing -> return []
    -- | Inject a candidate derived purely from the surface suffix when
    -- morphology doesn't give us a case but context suggests one.
    addSurfaceCaseCandidate :: Text -- ^ Surface form.
                            -> [(Identifier, Case)] -- ^ Candidates.
                            -> [(Identifier, Case)] -- ^ Candidates with surface-case hint.
    addSurfaceCaseCandidate surface candidates =
      case surfaceCaseCandidate surface of
        Just cand | cand `notElem` candidates -> cand : candidates
        _ -> candidates
    -- | Infer a simple case from the written suffix (currently only Gen).
    surfaceCaseCandidate :: Text -- ^ Surface form.
                         -> Maybe (Identifier, Case) -- ^ Candidate inferred from suffix.
    surfaceCaseCandidate surface = do
      (base, cas) <- surfaceCaseFromSuffix surface
      return ((ss, base), cas)
    -- | Surface-only case heuristic. We allow this only when:
    --   1) an apostrophe is present, or
    --   2) the stem is very short (to avoid over-stripping longer words).
    surfaceCaseFromSuffix :: Text -- ^ Surface form.
                          -> Maybe (Text, Case) -- ^ Base and case.
    surfaceCaseFromSuffix surface =
      let stripGen = do
            base0 <- stripSuffixAny genSuffixesNoApostropheTxt surface
            let base = T.dropWhileEnd (== '\'') base0
            if T.null base then Nothing else Just (base, Gen)
          allow =
            T.any (== '\'') surface ||
            maybe False ((<= 2) . T.length . fst) stripGen
      in if allow then stripGen else Nothing
    stripCondSuffix :: Text -- ^ Surface form.
                    -> Maybe Text -- ^ Stripped stem.
    stripCondSuffix = stripSuffixAny condSuffixesTxt
    -- | Strip ip-converb suffixes like \"-ip\" from surface forms.
    stripIpSuffix :: Text -- ^ Surface form.
                  -> Maybe Text -- ^ Stripped stem.
    stripIpSuffix = stripSuffixAny ipSuffixesTxt
    -- | Accusative with explicit buffer consonant is usually unambiguous;
    -- avoid injecting P3s alternatives for these surfaces.
    hasBufferedAccSuffix :: Text -> Bool
    hasBufferedAccSuffix txt =
      let lower = T.toLower txt
      in any (`T.isSuffixOf` lower) ["yi","yı","yu","yü","ni","nı","nu","nü"]

-- | Strip copula suffixes like \"-dir\" to recover the base noun.
{-# INLINE stripCopulaSuffix #-}
stripCopulaSuffix :: Text -- ^ Surface form.
                  -> Maybe Text -- ^ Stem without copula suffix.
stripCopulaSuffix txt =
  stripSuffixAny copulaSuffixesTxt (T.toLower txt)

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
      mRoot = infinitiveRoot ident <|> (copulaStripped >>= infinitiveRoot)
  case mRoot of
    Just root ->
      case find (\(cand, _) -> cand == root) candidates of
        Just match -> return match
        Nothing ->
          if root `Set.member` parserCtx
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
  let filtered = filter (\(ident', _) -> ident' `Set.member` parserCtx) candidates
  case preferInflected filtered of
    x:_ -> return x
    [] ->
      case preferInflected candidates of
        x:_ -> return x
        [] -> do
          base <- normalizePossessive ident
          if base /= ident
            then return (base, Nom)
            else customFailure ErrNoMatchingNominative

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
            [] -> do
              base <- normalizePossessive ident
              if base /= ident
                then return (base, Nom)
                else customFailure ErrNoMatchingNominative

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
  MkParserState{parserTyConsNames, parserTyParams, parserPrimTypes} <- getP
  let tyNames = parserTyConsNames ++ parserTyParams ++ parserPrimTypes
  let normalizeCandidate (name, cas) = do
        base <- normalizeTypeHead name
        if base `elem` tyNames
          then return (base, cas)
          else return (name, cas)
  case stripBareCaseSuffix ident of
    Just (base, cas) | base `elem` tyNames -> return (base, cas)
    _ ->
      if ident `elem` tyNames
        then return (ident, Nom)
        else do
          candidates <- estimateCandidates False ident
          let filtered = filter (\(ident', _) -> ident' `elem` tyNames) candidates
          case preferInflected filtered of
            x:_ -> normalizeCandidate x
            [] -> do
              mMatch <- matchCtxByInflection (Set.fromList tyNames) ident candidates
              case mMatch of
                Just matched -> normalizeCandidate matched
                Nothing -> resolveCandidatePreferCtx ident

-- | Normalize a possessive surface form to its nominative root.
normalizePossessive :: Identifier -- ^ Surface identifier.
                    -> KipParser Identifier -- ^ Normalized identifier.
normalizePossessive (mods, word) = do
  cached <- liftIO (M.lookup word <$> readIORef normalizePossessiveCache)
  base <- case cached of
    Just hit -> return hit
    Nothing -> do
      analyses <- upsCached word
      resolved <-
        case find (\a -> "<p3s>" `T.isInfixOf` a) analyses of
          Just analysis -> do
            let baseRoot = T.takeWhile (/= '<') analysis
            forms <- downsCached (stripCaseTags analysis)
            return $
              case forms of
                (x:_) ->
                  if x == word
                    then fromMaybe x (stripPossessiveSuffix word)
                    else x
                [] ->
                  if baseRoot == word
                    then fromMaybe baseRoot (stripPossessiveSuffix word)
                    else baseRoot
          Nothing ->
            return (fromMaybe word (stripPossessiveSuffix word))
      liftIO (modifyIORef' normalizePossessiveCache (M.insert word resolved))
      return resolved
  return (mods, base)
  where
    stripPossessiveSuffix txt =
      case T.stripSuffix "si" txt
        <|> T.stripSuffix "sı" txt
        <|> T.stripSuffix "su" txt
        <|> T.stripSuffix "sü" txt of
        Just base | not (T.null base) -> Just base
        _ -> Nothing

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
  MkParserState{parserTyConsNames, parserTyParams, parserPrimTypes} <- getP
  let tyNames = parserTyConsNames ++ parserTyParams ++ parserPrimTypes
      -- Check if identifier matches a primitive type pattern
      isPrimType base = isIntType base || isFloatType base || isStringType base || isCharType base
  -- First try morphology analysis to extract case if present
  mCandidates <- optional (estimateCandidates False ident)
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

-- | Extract an infinitive root from a verb identifier if present.
infinitiveRoot :: Identifier -- ^ Surface identifier.
           -> Maybe Identifier -- ^ Root without infinitive suffix.
infinitiveRoot (xs, x)
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
--
-- ==== Performance note (Optimization 12)
-- Uses a single pass to build case order and per-case identifier sets,
-- replacing repeated @ordNub@ and per-case list scans.
findAmbiguousCase :: [(Identifier, Case)] -> Maybe Case
findAmbiguousCase candidates =
  let (caseOrderRev, grouped, _) =
        foldl'
          (\(order, groups, seenCases) (ident, cas) ->
              let groups' = M.insertWith Set.union cas (Set.singleton ident) groups
                  seen = Set.member cas seenCases
                  order' = if seen then order else cas : order
                  seenCases' = if seen then seenCases else Set.insert cas seenCases
              in (order', groups', seenCases'))
          ([], M.empty, Set.empty)
          candidates
      caseOrder = reverse caseOrderRev
      ambiguousCases = [cas | cas <- caseOrder, maybe False (\idents -> Set.size idents > 1) (M.lookup cas grouped)]
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
--
-- ==== Performance note (Optimization 12)
-- Builds the candidate case list in one pass while preserving first-seen
-- order, reducing repeated dedup work in this hot matching path.
matchCtxByInflection :: Set.Set Identifier -- ^ Context identifiers.
                     -> Identifier -- ^ Surface identifier.
                     -> [(Identifier, Case)] -- ^ Candidate identifiers.
                     -> KipParser (Maybe (Identifier, Case)) -- ^ Matched candidate, if any.
matchCtxByInflection ctx ident candidates = do
  let (mods, surfaceRoot) = ident
      cases = reverse (snd (foldl' (\(seen, acc) (_, cas) ->
                  if cas `Set.member` seen
                    then (seen, acc)
                    else (Set.insert cas seen, cas : acc)
                ) (Set.empty, []) candidates))
      ctxFiltered =
        if null mods
          then ctx
          else Set.filter (\(ms, _) -> ms == mods) ctx
  if ident `Set.member` ctxFiltered
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
               -> Set.Set Identifier -- ^ Context identifiers.
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
              if direct `Set.member` ctxFiltered && not ("<lik>" `T.isInfixOf` analysis)
                -- Fast path: analysis root is already a context identifier.
                then return (Just (direct, cas))
                else do
                  let stem = stripCaseTags analysis
                  forms <- downsCached stem
                  p3sForms <-
                    if "<p3s>" `T.isInfixOf` analysis && cas /= P3s
                      then downsCached (stem <> "<p3s>")
                      else return []
                  let includeBaseRoot = not ("<lik>" `T.isInfixOf` analysis)
                      roots =
                        case forms ++ p3sForms of
                          [] -> [baseRoot]
                          xs -> ordNub (if includeBaseRoot then xs ++ [baseRoot] else xs)
                      matches = filter (`Set.member` ctxFiltered) [(mods, root) | root <- roots]
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

-- | Parse a pattern expression without match or sequence parsing.
-- This is a documentation marker for the pattern parser path; the actual
-- implementation lives in 'parsePatExp' near clause parsing.

-- | Parse an expression with optional context filtering.
parseExpWithCtx :: Bool -- ^ Whether to use context when resolving names.
                -> KipParser (Exp Ann) -- ^ Parsed expression.
parseExpWithCtx useCtx = parseExpWithCtx' useCtx True

-- | Parse an expression with optional context filtering and match-expression support.
parseExpWithCtx' :: Bool -- ^ Whether to use context when resolving names.
                 -> Bool -- ^ Whether to allow match expressions.
                 -> KipParser (Exp Ann) -- ^ Parsed expression.
parseExpWithCtx' useCtx allowMatch =
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
    -- | Parse a character literal with case.
    charLiteral :: KipParser (Exp Ann) -- ^ Parsed character literal.
    charLiteral = do
      ((c, cas), sp) <- withSpan parseCharToken
      return (CharLit (mkAnn cas sp) c)
    -- | Parse a string literal with case.
    stringLiteral :: KipParser (Exp Ann) -- ^ Parsed string literal.
    stringLiteral = do
      ((txt, cas), sp) <- withSpan parseStringToken
      return (StrLit (mkAnn cas sp) txt)
    -- | Parse a variable reference.
    var :: KipParser (Exp Ann) -- ^ Parsed variable.
    var = do
      (name, sp) <- withSpan identifierNotKeyword
      let word = snd name
      when useCtx $ do
        MkParserState{parserCtx, fsm} <- getP
        let inScopeExact = name `Set.member` parserCtx
        unless inScopeExact $ do
          analyses <- upsCached word
          when (null analyses) $ do
            near <- liftIO (suggestContextLike fsm word)
            let suggestions = take 5 near
            unless (null suggestions) $
              customFailure (ErrUnrecognizedTurkishWord word sp suggestions)
      candidates <- estimateCandidates useCtx name
      -- If we are using context, treat conditional forms as Cond to
      -- keep clause parsing consistent (otherwise fall back to the
      -- best candidate case).
      let cas =
            if useCtx
              then case find (\(_, c) -> c == Cond) candidates of
                     Just _ -> Cond
                     Nothing -> pickCase True candidates
              else pickCase True candidates
      return (Var (mkAnn cas sp) name candidates)
    -- | Parse an atomic expression.
    atom :: KipParser (Exp Ann) -- ^ Parsed atomic expression.
    atom =
      (if allowMatch then try matchExpr else empty)
      <|> listLiteral
      <|> charLiteral
      <|> stringLiteral
      <|> numberLiteral
      <|> try var
      <|> parens (parseExpWithCtx' useCtx allowMatch)
    -- | Parse a list literal like [1, 2, 3]'ü.
    -- The optional suffix is interpreted as a case on the *whole list*,
    -- but morphologically it is treated as if it attached to the last element.
    listLiteral :: KipParser (Exp Ann) -- ^ Parsed list literal expression.
    listLiteral = do
      ((elems, cas), sp) <- withSpan parseListLiteral
      buildListLiteral elems cas sp
      where
        -- Parse "[...]" plus optional suffix (with or without apostrophe).
        parseListLiteral :: KipParser ([Exp Ann], Case)
        parseListLiteral = do
          _ <- char '['
          ws
          elems <- sepBy (parseExpWithCtx' useCtx allowMatch) (lexeme (char ','))
          ws
          _ <- char ']'
          mSuffix <- optional $ do
            _ <- optional (char '\'')
            takeWhile1P (Just "ek") isLetter
          let cas = maybe Nom stringCaseFromSuffix mSuffix
          return (elems, cas)
        -- Lower list literals into nested constructor applications:
        --   [a, b, c]  ->  a'nın (b'nin (c'nin boşa ekine) ekine) eki
        -- and then apply the case to the full list expression.
        buildListLiteral :: [Exp Ann] -> Case -> Span -> KipParser (Exp Ann)
        buildListLiteral elems cas sp = do
          let mkCtorVar name ctorCase =
                let ident = ([], name)
                    ann = mkAnn ctorCase sp
                in Var ann ident [(ident, ctorCase)]
              setExpCasePrefer expCase expItem =
                let updateAnn ann = setAnnCase ann expCase
                in case expItem of
                  Var ann name cands ->
                    let filtered = filter ((== expCase) . snd) cands
                        cands' =
                          case filtered of
                            [] ->
                              case cands of
                                (ident, _):_ -> (ident, expCase) : cands
                                [] -> cands
                            _ -> filtered
                    in Var (updateAnn ann) name cands'
                  App ann fn args -> App (updateAnn ann) fn args
                  StrLit ann txt -> StrLit (updateAnn ann) txt
                  IntLit ann n -> IntLit (updateAnn ann) n
                  FloatLit ann n -> FloatLit (updateAnn ann) n
                  Bind ann name nameAnn body -> Bind (updateAnn ann) name (updateAnn nameAnn) body
                  Seq ann a b -> Seq (updateAnn ann) a b
                  Match ann scrut clauses -> Match (updateAnn ann) scrut clauses
                  Let ann name body -> Let (updateAnn ann) name body
              -- We keep both Nom and Dat variants so "ek" can attach
              -- in the same way as the written syntax.
              emptyNom = mkCtorVar (T.pack "boş") Nom
              emptyDat = setExpCasePrefer Dat emptyNom
              ekiVar = mkCtorVar (T.pack "eki") P3s
              cons elemExp (tailNom, tailDat) =
                let elemGen = setExpCasePrefer Gen elemExp
                    consNom = App (mkAnn Nom sp) ekiVar [elemGen, tailDat]
                    consDat = setExpCasePrefer Dat consNom
                in (consNom, consDat)
              (listNom, _) = foldr cons (emptyNom, emptyDat) elems
          return (setExpCasePrefer cas listNom)
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
    -- | Parse binding expressions with "için".
    bindExp :: KipParser (Exp Ann) -- ^ Parsed binding expression.
    bindExp = try $ do
      (name, sp) <- withSpan identifierNotKeyword
      lexeme (string "için")
      action <- try ascribeInner <|> app <|> atom
      let ann = mkAnn (annCase (annExp action)) (mergeSpan sp (annSpan (annExp action)))
          nameAnn = mkAnn Nom sp
      return (Bind ann name nameAnn action)
      where
        -- | Parse type ascription without consuming from seqExp.
        ascribeInner :: KipParser (Exp Ann)
        ascribeInner = do
          (ty, sp') <- withSpan parseTypeForAscription
          lexeme (string "olarak")
          expr <- atom
          let ann' = mkAnn (annCase (annExp expr)) (mergeSpan sp' (annSpan (annExp expr)))
          return (Ascribe ann' ty expr)
        parseTypeForAscription :: KipParser (Ty Ann)
        parseTypeForAscription = do
          (rawIdent, sp'') <- withSpan identifierNotKeyword
          candidates <- estimateCandidates False rawIdent
          let ann'' = mkAnn (pickCase False candidates) sp''
          MkParserState{parserPrimTypes, parserTyConsNames} <- getP
          let tyNames = parserTyConsNames
              primSet = Set.fromList parserPrimTypes
              tySet = Set.fromList tyNames
              isPrim ident = ident `Set.member` primSet
              knownCandidate = find (`Set.member` tySet) (map fst candidates)
              resolvedName = fromMaybe rawIdent knownCandidate
          case resolvedName of
            ident
              | isPrim ident && isIntType ident -> return (TyInt ann'')
              | isPrim ident && isFloatType ident -> return (TyFloat ann'')
              | isPrim ident && isStringType ident -> return (TyString ann'')
              | isPrim ident && isCharType ident -> return (TyChar ann'')
              | ident `elem` tyNames -> return (TyInd ann'' ident)
              | otherwise -> return (TyInd ann'' ident)
    -- | Parse a let expression with "dersek".
    letExp :: KipParser (Exp Ann) -- ^ Parsed let expression.
    letExp = try $ do
      items <- some (lexeme atom)
      -- some guarantees non-empty list, so reverse is also non-empty
      let (name:revExpr) = reverse items
          exprItems = reverse revExpr
          nameItem = name
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
              -- Mark "dersek" binds with Dat on the binder annotation so the
              -- typechecker can enforce the dative requirement without
              -- affecting regular "için" bindings.
              nameAnn = mkAnn Dat nameSpan
              bindExp = Bind bindAnn name nameAnn val
          lexeme (string "dersek")
          lexeme (char ',')
          st <- getP
          putP st { parserCtx = Set.insert name (parserCtx st) }
          body <- parseExpWithCtx' useCtx allowMatch
          putP st
          let ann = mkAnn (annCase (annExp body)) (mergeSpan bindSpan (annSpan (annExp body)))
          return (Seq ann bindExp body)
    -- | Parse type ascription expressions with "olarak".
    ascribeExp :: KipParser (Exp Ann) -- ^ Parsed type ascription expression.
    ascribeExp = try $ do
      (ty, sp) <- withSpan parseTypeForAscription
      lexeme (string "olarak")
      expr <- app
      let ann = mkAnn (annCase (annExp expr)) (mergeSpan sp (annSpan (annExp expr)))
      return (Ascribe ann ty expr)
      where
        -- | Parse a type for ascription (inline version of parseTypeLoose).
        parseTypeForAscription :: KipParser (Ty Ann)
        parseTypeForAscription = do
          (rawIdent, sp') <- withSpan identifierNotKeyword
          candidates <- estimateCandidates False rawIdent
          let ann' = mkAnn (pickCase False candidates) sp'
          MkParserState{parserPrimTypes, parserTyConsNames} <- getP
          let tyNames = parserTyConsNames
              primSet = Set.fromList parserPrimTypes
              tySet = Set.fromList tyNames
              isPrim ident = ident `Set.member` primSet
              knownCandidate = find (`Set.member` tySet) (map fst candidates)
              resolvedName = fromMaybe rawIdent knownCandidate
          case resolvedName of
            ident
              | isPrim ident && isIntType ident -> return (TyInt ann')
              | isPrim ident && isFloatType ident -> return (TyFloat ann')
              | isPrim ident && isStringType ident -> return (TyString ann')
              | isPrim ident && isCharType ident -> return (TyChar ann')
              | ident `Set.member` tySet -> return (TyInd ann' ident)
              | otherwise -> return (TyInd ann' ident)
    -- | Parse comma-separated sequence expressions.
    seqExp :: KipParser (Exp Ann) -- ^ Parsed sequence expression.
    seqExp = do
      e1 <- ascribeExp <|> bindExp <|> app <|> atom
      mcomma <- optional (lookAhead (lexeme (char ',')))
      case mcomma of
        Nothing -> do
          failOnUnknownWordAfterExpr
          return e1
        Just _ ->
          if allowMatch && useCtx
            then do
              let shouldTryMatch =
                    case e1 of
                      App _ fnExp (_:_) ->
                        case fnExp of
                          Var _ (_, name) _ -> isJust (stripSuffixAny condSuffixesTxt name)
                          _ -> False
                      _ -> False
              mMatch <-
                if shouldTryMatch
                  then optional (try (parseMatchFromApp e1))
                  else return Nothing
              case mMatch of
                Just match -> return match
                Nothing -> parseSeqOrReturn e1
            else parseSeqOrReturn e1
      where
        parseSeqOrReturn e1' = do
          ok <- isIpConverbExp e1'
          if ok
            then do
              lexeme (char ',')
              e2 <- case e1' of
                Bind {bindName} -> do
                  st <- getP
                  putP st { parserCtx = Set.insert bindName (parserCtx st) }
                  res <- parseExpWithCtx' useCtx allowMatch
                  putP st
                  return res
                _ -> parseExpWithCtx' useCtx allowMatch
              let ann = mkAnn (annCase (annExp e2)) (mergeSpan (annSpan (annExp e1')) (annSpan (annExp e2)))
              return (Seq ann e1' e2)
            else do
              failOnUnknownWordAfterExpr
              return e1'

        failOnUnknownWordAfterExpr :: KipParser ()
        failOnUnknownWordAfterExpr = do
          mWord <- optional (lookAhead parseWordAfterExpr)
          case mWord of
            Nothing -> return ()
            Just (w, sp) -> do
              analyses <- upsCached w
              when (null analyses) $ do
                MkParserState{fsm} <- getP
                -- This branch is a use-site (after an expression), so favor
                -- context-like morphology suggestions over raw edit-distance.
                near <- liftIO (suggestContextLike fsm w)
                let suggestions = take 5 near
                customFailure (ErrUnrecognizedTurkishWord w sp suggestions)

        parseWordAfterExpr :: KipParser (Text, Span)
        parseWordAfterExpr = do
          ws
          (w, sp) <- withSpan word
          return (w, sp)

        parseMatchFromApp :: Exp Ann -> KipParser (Exp Ann)
        parseMatchFromApp expItem =
          case expItem of
            App _ fnExp args
              | Just base <- appCondBase fnExp
              , (scrutExp:patArgs) <- args -> do
                  (scrutVar, scrutName) <- inferScrutineeFromExp scrutExp
                  let argNames = [scrutName]
                  pats <- mapM expToPatArg patArgs
                  let pat = PCtor (dropCondSuffixName fnExp base, annFromExp fnExp) pats
                  lexeme (char ',')
                  let patVarNames = extractPatVars pat
                  body <- withPatVars patVarNames (parseExpWithCtx' useCtx allowMatch)
                  let clause1 = Clause pat body
                  clauses <- parseMoreClausesCont argNames
                  let allClauses = clause1 : clauses
                      start = annSpan (annExp scrutVar)
                      end =
                        case reverse allClauses of
                          Clause _ lastBody:_ -> annSpan (annExp lastBody)
                          [] -> annSpan (annExp scrutVar)
                      ann = mkAnn (annCase (annExp scrutVar)) (mergeSpan start end)
                  return (Match ann scrutVar allClauses)
            _ -> customFailure ErrPatternExpected
          where
            appCondBase :: Exp Ann -> Maybe Text
            appCondBase expF =
              case expF of
                Var _ (_, name) _ -> stripCondSuffix name
                _ -> Nothing
            stripCondSuffix = stripSuffixAny condSuffixesTxt
            dropCondSuffixName :: Exp Ann -> Text -> Identifier
            dropCondSuffixName expF base =
              case expF of
                Var _ (mods, _) _ -> (mods, base)
                _ -> ([], base)
            annFromExp :: Exp Ann -> Ann
            annFromExp expF =
              case expF of
                Var ann _ _ -> ann
                _ -> mkAnn Nom NoSpan
    -- | Parse additional clauses for a match continuation.
    parseMoreClausesCont :: [Identifier] -- ^ Bound pattern names.
                         -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseMoreClausesCont argNames = do
      msep <- optional (try (clauseSep <|> (lexeme (char ',') $> ())))
      case msep of
        Nothing -> return []
        Just _ -> do
          pat <- parsePatternCont True argNames
          lexeme (char ',')
          let patVarNames = extractPatVars pat
          body <- withPatVars patVarNames (parseExpWithCtx' useCtx allowMatch)
          rest <- parseMoreClausesCont argNames
          return (Clause pat body : rest)
    -- | Parse a pattern in continuation form.
    parsePatternCont :: Bool -- ^ Whether to allow scrutinee.
                     -> [Identifier] -- ^ Bound pattern names.
                     -> KipParser (Pat Ann) -- ^ Parsed pattern.
    parsePatternCont allowScrutinee argNames =
      (lexeme (string "değilse") $> PWildcard (Nom, NoSpan)) <|>
      (parseExpAny >>= expToPat allowScrutinee argNames)
    -- | Infer the scrutinee expression and name from an explicit scrutinee.
    inferScrutineeFromExp :: Exp Ann -- ^ Scrutinee expression.
                          -> KipParser (Exp Ann, Identifier) -- ^ Scrutinee and identifier.
    inferScrutineeFromExp expItem =
      case expItem of
        v@Var{varCandidates} -> do
          scrutName <- pickScrutineeNameCont v
          let hasNom = any (\(_, cas) -> cas == Nom) varCandidates
              v' =
                if annCase (annExp v) == Nom || hasNom
                  then v {annExp = setAnnCase (annExp v) Nom}
                  else v
          return (v', scrutName)
        _ -> return (expItem, ([], T.pack "_"))
    -- | Pick a scrutinee name based on candidates and context.
    pickScrutineeNameCont :: Exp Ann -- ^ Scrutinee expression.
                           -> KipParser Identifier -- ^ Chosen scrutinee name.
    pickScrutineeNameCont Var{varName, varCandidates} = do
      MkParserState{parserCtx} <- getP
      let inScope = [ident | (ident, _) <- varCandidates, ident `Set.member` parserCtx]
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
        [] -> customFailure (ErrInternal "Internal error: buildAppFrom called with empty list")
        [x] -> return x
        _ -> do
          mStrict <- buildStrictApp xs
          case mStrict of
            Just (Left ambErr) -> customFailure ambErr
            Just (Right expStrict) -> normalizeCopulaCallableHead expStrict
            Nothing -> buildAppFromFallback xs >>= normalizeCopulaCallableHead
      where
        normalizeCopulaCallableHead :: Exp Ann -> KipParser (Exp Ann)
        normalizeCopulaCallableHead expItem =
          case expItem of
            App ann fn args -> do
              fn' <- promoteCopulaCallable fn args
              let appCase = annCase (annExp fn')
              return (App (setAnnCase ann appCase) fn' args)
            _ -> return expItem

        promoteCopulaCallable :: Exp Ann -> [Exp Ann] -> KipParser (Exp Ann)
        promoteCopulaCallable fnExp fnArgs =
          case fnExp of
            Var ann name candidates
              | not (null fnArgs)
              , annCase ann /= Gen
              , isJust (stripCopulaSuffix (snd name)) -> do
                  MkParserState{parserCtx, parserCtors, parserFuncArities} <- getP
                  let localBoundIds =
                        [ ident
                        | (ident, _) <- candidates
                        , ident `Set.member` parserCtx
                        , ident `notElem` parserCtors
                        , not (M.member ident parserFuncArities)
                        ]
                  if null localBoundIds
                    then return fnExp
                    else do
                      let injected = [(ident, Gen) | ident <- localBoundIds]
                          candidates' = ordNub (injected ++ candidates)
                      return (Var (setAnnCase ann Gen) name candidates')
            _ -> return fnExp

        buildAppFromFallback ys =
          case ys of
            [] -> customFailure (ErrInternal "Internal error: buildAppFromFallback called with empty list")
            [x] -> return x
            first:_ ->
              case reverse ys of
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
                [] -> customFailure (ErrInternal "Internal error: buildAppFromFallback reverse returned empty")

        -- | Try strict application reconstruction from token chains.
        --
        -- Strict reconstruction enumerates parses using known callable
        -- arities and succeeds only when there is a single normalized result.
        -- If there are zero or multiple strict parses, we return `Nothing`
        -- and let the existing fallback parser preserve legacy behavior.
        buildStrictApp :: [Exp Ann] -> KipParser (Maybe (Either ParserError (Exp Ann)))
        buildStrictApp ys = do
          ps <- getP
          let arityMap = parserFuncArities ps
              arityNameMap = parserFuncAritiesByName ps
          results <- parseStrict arityMap arityNameMap ys
          let uniq = nub results
          case uniq of
            [e] -> return (Just (Right e))
            [] -> return Nothing
            _ -> return Nothing

        parseStrict :: M.Map Identifier (Set.Set Int)
                    -> M.Map Text (Set.Set Int)
                    -> [Exp Ann]
                    -> KipParser [Exp Ann]
        parseStrict _ _ [e] = return [e]
        parseStrict arityMap arityNameMap ts =
          case reverse ts of
            fnExp:revRest -> do
              arities <- knownArities arityMap arityNameMap fnExp
              let rest = reverse revRest
              concat <$>
                mapM
                  (\arity ->
                    if arity > 0
                      then fmap (map (mkApp ts fnExp)) (splitArgs arityMap arityNameMap arity rest)
                      else return []
                  )
                  arities
            [] -> return []

        splitArgs :: M.Map Identifier (Set.Set Int)
                  -> M.Map Text (Set.Set Int)
                  -> Int
                  -> [Exp Ann]
                  -> KipParser [[Exp Ann]]
        --
        -- ==== Performance note (Optimization: reverse argument builders)
        -- Candidate argument vectors are built with cons and reversed once to
        -- avoid nested @prevArgs ++ [arg]@ allocations in split enumeration.
        splitArgs arityMap arityNameMap arity tokens
          | arity <= 0 = return []
          | length tokens < arity = return []
          | otherwise = map reverse <$> goRev arity tokens
          where
            goRev n pool
              | n == 0 =
                  return ([[] | null pool])
              | length pool < n = return []
              | otherwise =
                  concat <$>
                    mapM
                      (\splitAtIx -> do
                          let prefix = take splitAtIx pool
                              chunk = drop splitAtIx pool
                          argExps <- parseStrict arityMap arityNameMap chunk
                          prevExpSetsRev <- goRev (n - 1) prefix
                          return [arg : prevArgsRev | arg <- argExps, prevArgsRev <- prevExpSetsRev]
                      )
                      [n - 1 .. length pool - 1]

        -- | Collect callable arities for a function head candidate.
        --
        -- We query both exact identifiers and name-indexed arities so
        -- inflected surface forms still contribute to ambiguity checks.
        knownArities :: M.Map Identifier (Set.Set Int)
                     -> M.Map Text (Set.Set Int)
                     -> Exp Ann
                     -> KipParser [Int]
        knownArities arityMap arityNameMap expItem =
          case expItem of
            Var {varName, varCandidates} -> do
              aritySets <- mapM lookupCandidateArities (varName : map fst varCandidates)
              let arities = foldl' Set.union Set.empty aritySets
              return (Set.toList arities)
            _ -> return []
          where
            lookupCandidateArities ident = do
              let byNameFor i =
                    if null (fst i)
                      then fromMaybe Set.empty (M.lookup (snd i) arityNameMap)
                      else Set.empty
                  exact = fromMaybe Set.empty (M.lookup ident arityMap)
                  strippedBase =
                    case stripBareCaseSuffix ident of
                      Just (base, _) -> fromMaybe Set.empty (M.lookup base arityMap)
                      Nothing -> Set.empty
                  byNameArities = byNameFor ident
              normalized <- normalizePossessive ident
              let normalizedArities = fromMaybe Set.empty (M.lookup normalized arityMap)
                  normalizedByName = byNameFor normalized
              normalizedFromStripped <-
                case stripBareCaseSuffix ident of
                  Just (base, _) -> do
                    normalizedBase <- normalizePossessive base
                    let exactBase = fromMaybe Set.empty (M.lookup normalizedBase arityMap)
                        byNameBase = byNameFor normalizedBase
                    return (Set.union exactBase byNameBase)
                  Nothing -> return Set.empty
              return (foldl' Set.union Set.empty [exact, strippedBase, byNameArities, normalizedArities, normalizedByName, normalizedFromStripped])

        mkApp :: [Exp Ann] -> Exp Ann -> [Exp Ann] -> Exp Ann
        mkApp ts fnExp args =
          case ts of
            first:_ ->
              let ann =
                    mkAnn
                      (annCase (annExp fnExp))
                      (mergeSpan (annSpan (annExp first)) (annSpan (annExp fnExp)))
              in App ann fnExp args
            [] ->
              let ann = mkAnn (annCase (annExp fnExp)) (annSpan (annExp fnExp))
              in App ann fnExp args

    -- | Attempt to reinterpret application as a type cast.
    tryApplyTypeCase :: Exp Ann -- ^ Candidate expression.
                     -> Exp Ann -- ^ Candidate type expression.
                     -> KipParser (Maybe (Exp Ann)) -- ^ Casted expression if possible.
    tryApplyTypeCase x y = do
      MkParserState{parserTyParams, parserTyConsNames, parserPrimTypes, parserCtx} <- getP
      let tyNames = parserTyConsNames ++ parserTyParams ++ parserPrimTypes
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
          let isFuncCandidate = any (\(ident, _) -> ident `Set.member` parserCtx && ident `notElem` tyNames) candidates
          -- Don't apply type cast if inflected OR if it's a function candidate
          if isInflected || isFuncCandidate
            then return Nothing  -- Don't apply type casting if it could be a function call
            else do
              mResolved <- optional (resolveTypeCandidatePreferCtx name)
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
   dir\/module-name'i yükle.
   @
   The module name must be in accusative case (-i).
   An optional directory path can precede the module name, separated by @\/@.

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
    -- Supports optional directory path: @dir\/module-name'i yükle.@
    loadStmt :: KipParser (Stmt Ann) -- ^ Parsed load statement.
    loadStmt = do
      segments <- sepBy1 identifierNotKeyword (char '/')
      _ <- lexeme (string "yükle")
      period
      let (dirSegments, rawName) = (init segments, last segments)
          dirPath = map identText dirSegments
      name <- resolveLoadCandidate rawName
      return (Load dirPath name)
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
      (rawIdent, sp) <- withSpan identifierNotKeyword
      -- Constructor names should use their surface form without normalization
      -- Only detect the grammatical case for annotation purposes
      candidates <- estimateCandidates False rawIdent
      let cas = case candidates of
                  (_,c):_ -> c
                  [] -> Nom
      recordDefSpan rawIdent sp
      return (rawIdent, mkAnn cas sp)
    -- | Parse constructors without arguments.
    ctor :: KipParser (Ctor Ann) -- ^ Parsed constructor.
    ctor = (, []) <$> ctorIdent
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
      (n, nameSpan, params, _mods) <- try typeHeadParens <|> primTyInline
      recordDefSpan n nameSpan
      let paramIdents = [ident | TyVar _ ident <- params]
      lexeme (string "olsun")
      period
      modifyP (\ps -> ps { parserCtx = Set.insert n (Set.fromList paramIdents `Set.union` parserCtx ps)
                         , parserTyParams = paramIdents ++ parserTyParams ps
                         , parserTyCons = (n, length params) : parserTyCons ps
                         , parserTyConsNames = n : parserTyConsNames ps
                         , parserPrimTypes = n : parserPrimTypes ps
                         })
      return (PrimType n params)
    -- | Parse inline type head for primTy, stopping before "olsun".
    primTyInline :: KipParser (Identifier, Span, [Ty Ann], [Identifier])
    primTyInline = do
      first <- withSpan identifierNotKeyword
      rest <- many (try (ws *> notFollowedBy (string "olsun") *> withSpan identifierNotKeyword))
      case rest of
        [] -> do
          rawName <- fst <$> resolveCandidatePreferNom (fst first)
          name <- normalizeTypeHead rawName
          return (name, snd first, [], [])
        _ -> do
          let paramIdents = first : init rest
              nameIdent = last rest
          params <- mapM (parseTypeParam . fst) paramIdents
          rawName <- fst <$> resolveCandidatePreferNom (fst nameIdent)
          name <- normalizeTypeHead rawName
          return (name, snd nameIdent, params, [])
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
      (n, nameSpan, params, mods) <- typeHead
      recordDefSpan n nameSpan
      -- Extract parameter identifiers from TyVar nodes for parser state
      let paramIdents = map (\(TyVar _ ident) -> ident) params
      modifyP (\ps -> ps { parserCtx = Set.insert n (Set.fromList paramIdents `Set.union` parserCtx ps)
                             , parserTyParams = paramIdents ++ parserTyParams ps
                             , parserTyCons = (n, length params) : parserTyCons ps
                             , parserTyConsNames = n : parserTyConsNames ps
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
      modifyP (\ps -> ps { parserCtx = Set.insert n (Set.fromList ctorNames `Set.union` parserCtx ps)
                         , parserCtors = ctorNames ++ parserCtors ps
                         })
      return (NewType n params ctors)
    -- | Parse a type head (name and parameters).
    typeHead :: KipParser (Identifier, Span, [Ty Ann], [Identifier]) -- ^ Type head parts.
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
          -- Found apostrophe case suffix, use base identifier with detected case
          return (TyVar (mkAnn cas sp) (mods, base))
        Nothing -> do
          -- Try morphological analysis to get base form and case
          analyses <- upsCached word
          let -- Extract base form and case from morphological analysis
              extractBaseAndCase analysis =
                let base = T.takeWhile (/= '<') analysis
                    -- Try to extract case from analysis tags
                    mCase = getPossibleCase analysis
                in case mCase of
                     Just (_, cas) -> Just (base, cas)
                     Nothing -> Nothing
              -- Try all analyses, prefer the first successful extraction
              baseAndCase = listToMaybe (mapMaybe extractBaseAndCase analyses)
          case baseAndCase of
            Just (base, cas) | base /= word ->
              -- Found a base form different from surface form
              return (TyVar (mkAnn cas sp) (mods, base))
            _ -> do
              -- Fallback to regular resolution
              (ident, cas) <- resolveCandidatePreferNom rawIdent
              return (TyVar (mkAnn cas sp) ident)

    -- | Parse a parenthesized type head.
    typeHeadParens :: KipParser (Identifier, Span, [Ty Ann], [Identifier]) -- ^ Parsed type head.
    typeHeadParens = parens $ do
      parts <- withSpan identifierNotKeyword `sepBy1` ws
      let (paramIdents, nameIdent) =
            case parts of
              [] -> ([], (([], T.pack ""), NoSpan))
              [_] -> ([], (([], T.pack ""), NoSpan))
              _ -> (init parts, last parts)
      when (length parts < 2) empty
      -- Parse type parameters with case detection
      params <- mapM (parseTypeParam . fst) paramIdents
      rawName <- fst <$> resolveCandidatePreferNom (fst nameIdent)
      name <- normalizeTypeHead rawName
      return (name, snd nameIdent, params, [])
    -- | Parse a type head without parentheses.
    typeHeadInline :: KipParser (Identifier, Span, [Ty Ann], [Identifier]) -- ^ Parsed type head.
    typeHeadInline = do
      first <- withSpan identifierNotKeyword
      rest <- many (try (ws *> withSpan identifierNotKeyword))
      case rest of
        [] -> do
          -- Single identifier: just the type name
          rawName <- fst <$> resolveCandidatePreferNom (fst first)
          name <- normalizeTypeHead rawName
          return (name, snd first, [], [])
        _ -> do
          -- Two or more identifiers: all but last are params, last is type name
          let paramIdents = first : init rest
              nameIdent = last rest
          params <- mapM (parseTypeParam . fst) paramIdents
          rawName <- fst <$> resolveCandidatePreferNom (fst nameIdent)
          name <- normalizeTypeHead rawName
          return (name, snd nameIdent, params, [])
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
          reverse <$> parseMore [firstCtor]
      where
        -- | Parse additional constructors separated by "ya" (reverse accumulator).
        parseMore :: [Ctor Ann] -- ^ Reverse-accumulated constructors.
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
                    Nothing -> return (ctor : acc)
                    Just _ -> customFailure yaDaErrorMsg
                else parseMore (ctor : acc)
        -- | Parse a constructor separator and track "ya da".
        parseSep :: KipParser Bool -- ^ True when the separator is "ya da".
        parseSep = do
          _ <- ya
          mda <- optional (lookAhead (string "da"))
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
      _ <- try (lexeme (string "bir") *> lookAhead (identifierNotKeyword <|> (char '(' $> ([], T.pack ""))))
      parseTypeWithCase
    -- | Parse a function argument declaration.
    parseArg :: KipParser ((Identifier, Ann), Ty Ann) -- ^ Parsed argument declaration.
    parseArg =
      try parseEffectfulHigherOrderArg <|> do
        (argName, argSpan) <- withSpan identifierNotKeyword
        ws
        ty <- try parseHigherOrderArgType <|> try parseTypeWithParenArgs <|> try parseTypeWithCase <|> parseTypeLoose
        let argAnn = mkAnn Nom argSpan
        return ((argName, argAnn), ty)
      where
        -- Parse nested effectful higher-order argument syntax:
        --   (a'yı (işlemek b'siyle))
        -- The bound callback name is the infinitive root (işle), and the type
        -- becomes a function arrow from domain to image type.
        parseEffectfulHigherOrderArg :: KipParser ((Identifier, Ann), Ty Ann)
        parseEffectfulHigherOrderArg = do
          domTy <- try parseTypeWithCase <|> parseTypeLoose
          ws
          (infNameRaw, infSpan, imgTy) <- parens $ do
            (rawName, rawSpan) <- withSpan identifierNotKeyword
            ws
            ty <- try parseTypeWithCase <|> parseTypeLoose
            return (rawName, rawSpan, ty)
          infName <- maybe empty return (infinitiveRoot infNameRaw)
          let imgCase = annCase (annTy imgTy)
          let spanMerged = mergeSpan (annSpan (annTy domTy)) (annSpan (annTy imgTy))
              argAnn = mkAnn Nom infSpan
              arrTy = Arr (mkAnn imgCase spanMerged) domTy imgTy
          return ((infName, argAnn), arrTy)

        parseHigherOrderArgType :: KipParser (Ty Ann)
        parseHigherOrderArgType = do
          firstTy <- parseOneTy
          ws
          secondTy <- parseOneTy
          restTys <- many (try (ws *> parseOneTy))
          notFollowedBy (ws *> identifierNotKeyword)
          let allTys = firstTy : secondTy : restTys
              allCases = map (annCase . annTy) allTys
              argCases = init allCases
              imgCase = last allCases
              argsLookArrow = all (== Gen) argCases
              imgLooksArrow = imgCase == Ins || imgCase == P3s
          guard argsLookArrow
          guard imgLooksArrow
          return (foldr1 mkArrow allTys)
          where
            parseOneTy = try parseTypeWithCase <|> parseTypeLoose
            mkArrow domTy imgTy =
              let spanMerged = mergeSpan (annSpan (annTy domTy)) (annSpan (annTy imgTy))
              in Arr (mkAnn (annCase (annTy imgTy)) spanMerged) domTy imgTy
    -- | Parse a type without requiring it to be in scope.
    parseTypeLoose :: KipParser (Ty Ann) -- ^ Parsed type.
    parseTypeLoose = do
      (rawIdent@(mods, word), sp) <- withSpan identifierNotKeyword
      candidates <- estimateCandidates False rawIdent
      let ann = mkAnn (pickCase False candidates) sp
          nameForTy =
            case candidates of
              (ident, _):_ -> ident
              [] -> rawIdent
      MkParserState{parserPrimTypes, parserTyConsNames} <- getP
      let tyNames = parserTyConsNames
      -- Check if it's a known type directly
      case candidates of
        (ident, _):_
          | ident `elem` parserPrimTypes && isIntType ident -> return (TyInt ann)
        (ident, _):_
          | ident `elem` parserPrimTypes && isFloatType ident -> return (TyFloat ann)
        (ident, _):_
          | ident `elem` parserPrimTypes && isStringType ident -> return (TyString ann)
        (ident, _):_
          | ident `elem` parserPrimTypes && isCharType ident -> return (TyChar ann)
        _ -> do
          -- If not a known type, try extracting base form with TRmorph for P3s support
          let tryAsTyName name = name `elem` tyNames || name `elem` parserPrimTypes
          if tryAsTyName nameForTy
            then return (TyInd ann nameForTy)
            else do
              -- Use TRmorph to extract base form (for P3s support)
              analyses <- upsCached word
              let extractBase = T.takeWhile (/= '<')
                  bases = map extractBase analyses
                  validBase = listToMaybe [base | base <- bases, tryAsTyName (mods, base)]
              case validBase of
                Just base -> return (TyInd ann (mods, base))
                Nothing -> return (TyVar ann nameForTy)
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
      args <- many (try (lexeme parseArgGroup <* notFollowedBy (lexeme (char ','))))
      (rawName, nameSpan, mRetTy) <- parseFuncHeader
      validateDefinitionName rawName nameSpan
      let isInfinitive = isJust (infinitiveRoot rawName)
          baseName = fromMaybe rawName (infinitiveRoot rawName)
      (fname, _) <-
        if isInfinitive
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
                      Nothing -> return (baseName, Nom)
      recordDefSpan fname nameSpan
      lexeme (char ',')
      let isDefnCandidate = null args && not isInfinitive && isNothing mRetTy
      if isDefnCandidate
        then do
          clauses <- parseBodyOnly []
          case clauses of
            [Clause (PWildcard _) body] -> do
              modifyP (\ps -> ps {parserCtx = Set.insert fname (parserCtx ps)})
              registerFuncArity fname 0
              return (Defn fname (TyString (mkAnn Nom NoSpan)) body)
            _ -> customFailure ErrDefinitionBodyMissing
        else do
          st <- getP
          let argNames = map argIdent args
          putP (st {parserCtx = Set.insert fname (Set.fromList argNames `Set.union` parserCtx st)})
          hasPrim <- option False (lookAhead (lexeme (string "yerleşiktir") $> True))
          prim <- if hasPrim
            then Just <$> (lexeme (string "yerleşiktir") *> period)
            else return Nothing
          let retTy = fromMaybe (TyString (mkAnn Nom NoSpan)) mRetTy
          case prim of
            Just _ -> do
              st' <- getP
              putP (st' {parserCtx = Set.insert fname (parserCtx st)})
              registerFuncArity fname (length args)
              return (PrimFunc fname args retTy isInfinitive)
            Nothing -> do
              isBindStart <- option False (try bindStartLookahead)
              clauses <-
                if isBindStart
                  then parseBodyOnly argNames
                  else try (parseBodyOnly argNames) <|> parseClauses argNames
              st' <- getP
              putP (st' {parserCtx = Set.insert fname (parserCtx st)})
              registerFuncArity fname (length args)
              return (Function fname args retTy clauses isInfinitive)
    -- | Ensure newly defined names are recognized Turkish surface forms.
    -- For hyphenated identifiers, morphology applies to the head (last segment).
    validateDefinitionName :: Identifier -> Span -> KipParser ()
    validateDefinitionName (_mods, word) nameSpan = do
      analyses <- upsCached word
      when (null analyses) $ do
        MkParserState{fsm} <- getP
        near <- liftIO (suggestEditDistance1 fsm word)
        let suggestions = take 15 near
        customFailure (ErrUnrecognizedTurkishWord word nameSpan suggestions)
    -- | Parse a single parenthesized function argument group.
    --
    -- This also supports nested effectful higher-order argument syntax:
    --   (a'yı (işlemek b'siyle))
    parseArgGroup :: KipParser ((Identifier, Ann), Ty Ann)
    parseArgGroup =
      try (parens parseNestedEffectfulArg) <|> parens parseArg
      where
        parseNestedEffectfulArg :: KipParser ((Identifier, Ann), Ty Ann)
        parseNestedEffectfulArg = do
          domTy <- try parseTypeWithCase <|> parseTypeLoose
          ws
          (infNameRaw, infSpan, imgTy) <- parens $ do
            (rawName, rawSpan) <- withSpan identifierNotKeyword
            ws
            ty <- try parseTypeWithCase <|> parseTypeLoose
            return (rawName, rawSpan, ty)
          infName <- maybe empty return (infinitiveRoot infNameRaw)
          let imgCase = annCase (annTy imgTy)
              spanMerged = mergeSpan (annSpan (annTy domTy)) (annSpan (annTy imgTy))
              argAnn = mkAnn Nom infSpan
              arrTy = Arr (mkAnn imgCase spanMerged) domTy imgTy
          return ((infName, argAnn), arrTy)
    -- | Parse function header with optional return type.
    parseFuncHeader :: KipParser (Identifier, Span, Maybe (Ty Ann)) -- ^ Function name, span, and optional return type.
    parseFuncHeader = do
      mHeader <- optional (try (parens (do
        (name, sp) <- withSpan identifierNotKeyword
        ty <- ws *> parseReturnType
        return (name, sp, Just ty))))
      case mHeader of
        Just (name, sp, ty) -> return (name, sp, ty)
        Nothing -> do
          (name, sp) <- withSpan identifierNotKeyword
          return (name, sp, Nothing)
    -- | Parse a return type with a lenient fallback for plain identifiers.
    parseReturnType :: KipParser (Ty Ann) -- ^ Parsed return type.
    parseReturnType =
      try parseTypeWithParenArgs <|> try parseTypeWithCase <|> parseReturnTypeLoose
    -- | Parse a return type whose first argument is parenthesized:
    -- @(anahtarla değer çifti) listesi@.
    parseTypeWithParenArgs :: KipParser (Ty Ann) -- ^ Parsed type.
    parseTypeWithParenArgs = do
      firstArg <- parens parseTypeWithCase
      restArgs <- many (try (ws *> parens parseTypeWithCase))
      ws
      (ctorIdent, ctorSp) <- withSpan identifierNotKeyword
      (ctorName, cas) <- resolveTypeCandidateLoose ctorIdent
      let ctorAnn = mkAnn cas ctorSp
      return (TyApp ctorAnn (TyInd ctorAnn ctorName) (firstArg : restArgs))
    -- | Parse a return type without requiring it to be in scope.
    parseReturnTypeLoose :: KipParser (Ty Ann) -- ^ Parsed return type.
    parseReturnTypeLoose =
      try (parens parseReturnTypeLoose) <|> do
        (firstIdent, sp1) <- withSpan identifierNotKeyword
        rest <- many (try (ws *> withSpan identifierNotKeyword))
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
            else if name `elem` parserPrimTypes && isCharType name
              then return (TyChar ann)
              else return (TyVar ann name)
    -- | Parse a function body without explicit clauses.
    parseBodyOnly :: [Identifier] -- ^ Function argument names.
                  -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseBodyOnly argNames = do
      body <- parseExp
      unless (null argNames) $ do
        ambiguous <- isAmbiguousMatchExpr argNames body
        guard (not ambiguous)
      period
      return [Clause (PWildcard (Nom, NoSpan)) body]
    -- | Reject match expressions that likely represent function clauses.
    isAmbiguousMatchExpr :: [Identifier] -> Exp Ann -> KipParser Bool
    isAmbiguousMatchExpr argNames expItem =
      case expItem of
        Match _ scrutinee _ ->
          case scrutinee of
            Var {varName, varCandidates} -> do
              MkParserState{parserCtx, parserCtors} <- getP
              let candidateNames = map fst varCandidates
                  isArg = any (`elem` argNames) (varName : candidateNames)
                  isCtor = any (`elem` parserCtors) candidateNames
                  inScope = any (`Set.member` parserCtx) candidateNames
              return (isArg || (isCtor && inScope))
            _ -> return False
        _ -> return False
    -- | Parse match clauses for a function.
    parseClauses :: [Identifier] -- ^ Function argument names.
                 -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseClauses argNames = do
      c <- parseClause True argNames
      (period >> return [c]) <|> do
        clauseSep
        (c :) <$> parseClausesRest argNames
    -- | Parse the remaining clauses after a comma.
    parseClausesRest :: [Identifier] -- ^ Function argument names.
                     -> KipParser [Clause Ann] -- ^ Parsed clauses.
    parseClausesRest argNames = do
      checkRepeatedArgClauseStart argNames
      c <- try (parseClause False argNames) <|> repeatedArgClauseError argNames
      (period >> return [c]) <|> do
        clauseSep
        (c :) <$> parseClausesRest argNames
    -- | Parse a single clause.
    parseClause :: Bool -- ^ Whether to allow scrutinee expressions.
                -> [Identifier] -- ^ Function argument names.
                -> KipParser (Clause Ann) -- ^ Parsed clause.
    parseClause allowScrutinee argNames = do
      unless allowScrutinee $ do
        checkRepeatedArgClauseStart argNames
      -- Check for wildcard pattern first
      mWildcard <- optional (try (lexeme (string "değilse")))
      case mWildcard of
        Just _ -> do
          lexeme (char ',')
          Clause (PWildcard (Nom, NoSpan)) <$> parseExp
        Nothing -> do
          -- Parse pattern expression and convert to pattern
          patExp <- parsePatExp
          when (not allowScrutinee && hasRepeatedArgPattern argNames patExp) $
            customFailure ErrPatternArgNameRepeated
          mClause <- try (parseClauseAfterScrutinee allowScrutinee argNames patExp)
          case mClause of
            Just clause -> return clause
            Nothing -> do
              pat <- expToPat allowScrutinee argNames patExp
              -- Extract pattern variables from the converted pattern
              let patVarNames = extractPatVars pat
              lexeme (char ',')
              -- Add pattern variables to context when parsing body
              body <- withPatVars patVarNames parseExp
              return (Clause pat body)
      where
        hasRepeatedArgPattern :: [Identifier] -> Exp Ann -> Bool
        hasRepeatedArgPattern names expItem =
          let namesSet = Set.fromList (argNameVariants names)
              inNames ident = ident `Set.member` namesSet
          in case expItem of
            Var _ (mods, name) _ ->
              (inNames (mods, name) || any (\m -> ([], m) `Set.member` namesSet) mods) &&
              hasCondSuffix name
            App _ fn (Var _ n _ : _) | isCondFn fn -> inNames n
            _ -> False
        isCondFn :: Exp Ann -> Bool
        isCondFn expItem =
          case expItem of
            Var _ (_, name) _ ->
              hasCondSuffix name
            _ -> False
    -- | Parse a clause of the form "scrutinee, pattern, body".
    parseClauseAfterScrutinee :: Bool -- ^ Whether to allow scrutinee expressions.
                              -> [Identifier] -- ^ Function argument names.
                              -> Exp Ann -- ^ Parsed scrutinee expression.
                              -> KipParser (Maybe (Clause Ann))
    parseClauseAfterScrutinee allowScrutinee argNames scrutExp =
      case scrutExp of
        Var {varName, varCandidates} -> do
          let candidateNames = map fst varCandidates
              matchesArgs = any (`elem` argNames) (varName : candidateNames)
          if not matchesArgs
            then return Nothing
            else do
              clause <- try $ do
                lexeme (char ',')
                patExp <- parsePatExp
                pat <- expToPat allowScrutinee argNames patExp
                lexeme (char ',')
                let patVarNames = extractPatVars pat
                body <- withPatVars patVarNames parseExp
                return (Clause pat body)
              return (Just clause)
        _ -> return Nothing
    -- | Expand argument names to include their modifier tokens.
    argNameVariants :: [Identifier] -- ^ Declared argument names.
                    -> [Identifier] -- ^ Arguments plus modifier tokens.
    argNameVariants names =
      ordNub (names ++ [([], m) | (mods, _) <- names, m <- mods])
    -- | Shared worker for repeated-argument clause-head detection.
    --
    -- This keeps both clause parsers on the same fast-path checks and avoids
    -- duplicating parser setup/allocation for each call site.
    checkRepeatedArgClauseStart :: [Identifier] -> KipParser ()
    checkRepeatedArgClauseStart names = do
      let !nameVariants = argNameVariants names
          !nameVariantSet = Set.fromList nameVariants
          !nameTokenSet = Set.fromList (map (T.toLower . identText) nameVariants)
      startsRepeated <- clauseStartsWithRepeatedArg nameTokenSet
      when startsRepeated (customFailure ErrPatternArgNameRepeated)
      mRepeated <- optional (try (lookAhead (repeatedArgPattern nameVariantSet)))
      when (isJust mRepeated) (customFailure ErrPatternArgNameRepeated)
      mFirst <- optional (try (lookAhead (lexeme identifier)))
      when (maybe False (`Set.member` nameVariantSet) mFirst) (customFailure ErrPatternArgNameRepeated)
    -- | Check if the next clause starts with a repeated argument name.
    -- This is a lightweight textual lookahead to catch "bu yanlışsa"
    -- style patterns before the main expression parser backtracks.
    clauseStartsWithRepeatedArg :: Set.Set Text -> KipParser Bool
    clauseStartsWithRepeatedArg nameTokens = do
      input <- getInput
      let tokens = take 2 (T.words (T.dropWhile isSpace input))
          stripPunct = T.dropWhileEnd (\c -> c == ',' || c == ';' || c == '.')
      case tokens of
        (t1:t2:_) ->
          let t1' = T.toLower (stripPunct t1)
              t2' = T.toLower (stripPunct t2)
          in return (t1' `Set.member` nameTokens && hasCondSuffix t2')
        _ -> return False
    -- | Detect repeated argument names in conditional patterns.
    -- This works on parsed identifiers, so it is robust to whitespace
    -- and punctuation variations in clause heads.
    repeatedArgPattern :: Set.Set Identifier -> KipParser ()
    repeatedArgPattern nameSet = do
      ws
      (firstIdent, _) <- withSpan identifierNotKeyword
      guard (firstIdent `Set.member` nameSet)
      ws
      (secondIdent, _) <- withSpan identifierNotKeyword
      let (_, secondName) = secondIdent
      guard (hasCondSuffix secondName)
    -- | Fail fast on repeated argument names in clause heads.
    repeatedArgClauseError :: [Identifier] -> KipParser a
    repeatedArgClauseError names = do
      _ <- repeatedArgPattern (Set.fromList (argNameVariants names))
      customFailure ErrPatternArgNameRepeated
    -- | Parse an expression for pattern contexts (no match parsing).
    -- We disable both match and sequence parsing so patterns don't
    -- accidentally consume clause separators.
    parsePatExp :: KipParser (Exp Ann) -- ^ Parsed expression.
    parsePatExp = parseExpWithCtx' False False
    -- | Parse a pattern, optionally allowing a scrutinee expression.
    parsePattern :: Bool -- ^ Whether to allow scrutinee expressions.
                 -> [Identifier] -- ^ Function argument names.
                 -> KipParser (Pat Ann) -- ^ Parsed pattern.
    parsePattern allowScrutinee argNames =
      (lexeme (string "değilse") $> PWildcard (Nom, NoSpan)) <|>
      try (parsePatExp >>= expToPat allowScrutinee argNames)
    -- | Look ahead for a binding expression start.
    bindStartLookahead :: KipParser Bool -- ^ True when a binding start is found.
    bindStartLookahead = do
      lookAhead $ do
        _ <- identifierNotKeyword
        lexeme (string "için")
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
        MkParserState{parserTyParams, parserTyCons, parserTyConsNames, parserPrimTypes} <- getP
        let tyNames = parserTyConsNames
            primNames = parserPrimTypes
            typeScope = tyNames ++ parserTyParams ++ primNames
            !tyNamesSet = Set.fromList tyNames
            !tyParamsSet = Set.fromList parserTyParams
            !primNamesSet = Set.fromList primNames
            !typeScopeSet = Set.fromList typeScope
            !tyArityMap = M.fromList parserTyCons
        let
            -- | Require a type identifier to be in scope.
            requireInCtx :: Identifier -- ^ Type identifier to check.
                         -> KipParser () -- ^ No result.
            requireInCtx name =
              if name `Set.member` typeScopeSet
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
              fmap (mods,) (stripSuffixAny genSuffixesNoApostropheTxt word)
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
                  mBase = stripSuffixAny turkishCaseSuffixesTxt lowerWord
                  stripped =
                    case mBase of
                      Just baseLower ->
                        let len = T.length baseLower
                        in if T.length word > len
                             then T.take len word
                             else word
                      Nothing -> word
              in (mods, stripped)

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
                Just paramName -> do
                  (_, resolvedCase) <- resolveTypeCandidateLoose ident
                  let cas = preferSurfaceCase ident resolvedCase
                  return (TyVar (mkAnn cas NoSpan) paramName)
                Nothing -> do
                  -- If no type param match, try normal resolution
                  mResolved <- optional (resolveTypeCandidatePreferCtx ident)
                  let pickTy name cas
                        | name `Set.member` primNamesSet && isIntType name = TyInt (mkAnn cas NoSpan)
                        | name `Set.member` primNamesSet && isFloatType name = TyFloat (mkAnn cas NoSpan)
                        | name `Set.member` primNamesSet && isStringType name = TyString (mkAnn cas NoSpan)
                        | name `Set.member` primNamesSet && isCharType name = TyChar (mkAnn cas NoSpan)
                        | name `Set.member` primNamesSet = TyInd (mkAnn cas NoSpan) name  -- Other primitives like boolean
                        | name `Set.member` tyNamesSet = TyInd (mkAnn cas NoSpan) name
                        | name `Set.member` tyParamsSet = TyVar (mkAnn cas NoSpan) name
                        | otherwise = TyVar (mkAnn cas NoSpan) name
                  case mResolved of
                    Just (name, cas) ->
                      if name `Set.member` tyParamsSet
                        then return (TyVar (mkAnn cas NoSpan) name)
                        else return (pickTy name cas)
                    Nothing ->
                      return (pickTy ident Nom)
            -- | Check if an identifier refers to a type in scope.
            isTypeIdent :: Identifier -- ^ Surface identifier.
                        -> KipParser Bool -- ^ True when identifier resolves to a type.
            isTypeIdent ident = do
              m <- optional (resolveTypeCandidatePreferCtx ident)
              case m of
                Just (name, _) ->
                  return (name `Set.member` typeScopeSet)
                Nothing -> return False
        let -- Collect all type identifiers greedily, validating arity at the end
            collectArgsLoopRev soFarRev = do
              mNext <- optional (try (do
                arg <- identifierNotKeyword
                ws
                nextIdent <- lookAhead identifierNotKeyword
                mNextResolved <- optional (resolveTypeCandidatePreferCtx nextIdent)
                case mNextResolved of
                  Just (nextName, _) -> do
                    let isNextType = nextName `Set.member` typeScopeSet
                    guard isNextType
                    return arg
                  Nothing -> empty
                ))
              case mNext of
                Just arg -> collectArgsLoopRev (arg : soFarRev)
                Nothing -> return (reverse soFarRev)
        -- Try to parse as a type application, with arity validation inside try
        mTypeApp <- optional . try $ do
              collected <- collectArgsLoopRev []
              guard (not (null collected))
              (rawIdent, sp) <- withSpan identifierNotKeyword
              (name, cas) <- resolveTypeCandidatePreferCtx rawIdent
              requireInCtx name
              -- Validate arity: check if this type constructor accepts the right number of arguments
              let numArgs = length collected
                  mArity = lookup name parserTyCons
                  mArityFast = M.lookup name tyArityMap
                  arityMatches = case mArity of
                    Just expectedArity -> numArgs == expectedArity
                    Nothing ->
                      -- Primitives have arity 0, type params shouldn't appear as constructors here
                      (name `Set.member` primNamesSet) && (numArgs == 0)
                  arityMatchesFast = case mArityFast of
                    Just expectedArity -> numArgs == expectedArity
                    Nothing -> (name `Set.member` primNamesSet) && (numArgs == 0)
              guard (arityMatches && arityMatchesFast)  -- Allow backtracking if arity doesn't match
              let cas' = preferSurfaceCase rawIdent cas
                  ann = mkAnn cas' sp
              if name `Set.member` primNamesSet && isIntType name
                then return (TyInt ann)
                else if name `Set.member` primNamesSet && isFloatType name
                  then return (TyFloat ann)
                else if name `Set.member` primNamesSet && isStringType name
                    then return (TyString ann)
                else if name `Set.member` primNamesSet && isCharType name
                    then return (TyChar ann)
                    else do
                      argTys <- mapM argTy collected
                      return (TyApp ann (TyInd (mkAnn Nom NoSpan) name) argTys)
        case mTypeApp of
          Just ty -> return ty
          Nothing -> do
            (rawIdent, sp) <- withSpan identifierNotKeyword
            case rawIdent of
              (xs, xraw) | not (null xs) -> do
                (baseName, cas) <- resolveTypeCandidatePreferCtx ([], xraw)
                if baseName `Set.member` tyNamesSet
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
                    if name `Set.member` primNamesSet && isIntType name
                      then return (TyInt (mkAnn cas'' sp))
                      else if name `Set.member` primNamesSet && isFloatType name
                        then return (TyFloat (mkAnn cas'' sp))
                        else if name `Set.member` primNamesSet && isStringType name
                          then return (TyString (mkAnn cas'' sp))
                          else if name `Set.member` primNamesSet && isCharType name
                            then return (TyChar (mkAnn cas'' sp))
                            else if name `Set.member` tyParamsSet
                              then return (TyVar (mkAnn cas'' sp) name)
                              else return (TyInd (mkAnn cas'' sp) name)
              _ -> do
                (name, cas) <- resolveTypeCandidatePreferCtx rawIdent
                requireInCtx name
                let cas' = preferSurfaceCase rawIdent cas
                if name `Set.member` primNamesSet && isIntType name
                  then return (TyInt (mkAnn cas' sp))
                  else if name `Set.member` primNamesSet && isFloatType name
                    then return (TyFloat (mkAnn cas' sp))
                    else if name `Set.member` primNamesSet && isStringType name
                      then return (TyString (mkAnn cas' sp))
                      else if name `Set.member` primNamesSet && isCharType name
                        then return (TyChar (mkAnn cas' sp))
                        else if name `Set.member` tyParamsSet
                          then return (TyVar (mkAnn cas' sp) name)
                          else return (TyInd (mkAnn cas' sp) name)
    -- | Parse a type name with a modifier prefix.
    parseModifiedType :: KipParser (Ty Ann) -- ^ Parsed type.
    parseModifiedType = do
      (firstIdent, sp1) <- withSpan identifierNotKeyword
      ws
      (secondIdent, sp2) <- withSpan identifierNotKeyword
      MkParserState{parserTyConsNames} <- getP
      let tyNames = parserTyConsNames
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
-- This must check if identifiers are constructors to distinguish variables.
extractPatVarNamesInContext :: [Identifier] -- ^ Constructor identifiers.
                            -> Exp Ann -- ^ Pattern expression.
                            -> [Identifier] -- ^ Pattern variable names.
extractPatVarNamesInContext ctx e =
  case e of
    Var _ _ candidates ->
      -- If this matches a constructor in context, it's not a variable
      case selectCondNameInCtors ctx candidates of
        Nothing ->
          -- Not a constructor, treat as variable
          case preferInflected candidates of
            (n, _):_ -> [n]
            _ -> []
        Just _ -> []  -- Constructor with no arguments
    App _ (Var {}) es -> concatMap (extractPatVarNamesInContext ctx) es
    _ -> []

-- | Legacy version that doesn't check context (returns empty for safety).
extractPatVarNames :: Exp Ann -- ^ Pattern expression.
                   -> [Identifier] -- ^ Pattern variable names.
extractPatVarNames _ = []

-- | Extract variable names bound by a pattern.
extractPatVars :: Pat Ann -- ^ Pattern.
               -> [Identifier] -- ^ Bound variable names.
extractPatVars pat =
  case pat of
    PWildcard _ -> []
    PVar n _ -> [n]
    PCtor _ pats -> concatMap extractPatVars pats
    PIntLit _ _ -> []
    PFloatLit _ _ -> []
    PStrLit _ _ -> []
    PCharLit _ _ -> []
    PListLit pats -> concatMap extractPatVars pats

-- | Extract bound variable names with source spans from a pattern.
extractPatVarsWithSpans :: Pat Ann -- ^ Pattern.
                        -> [(Identifier, Span)] -- ^ Bound variable names and spans.
extractPatVarsWithSpans pat =
  case pat of
    PWildcard _ -> []
    PVar n ann -> [(n, annSpan ann)]
    PCtor _ pats -> concatMap extractPatVarsWithSpans pats
    PIntLit _ _ -> []
    PFloatLit _ _ -> []
    PStrLit _ _ -> []
    PCharLit _ _ -> []
    PListLit pats -> concatMap extractPatVarsWithSpans pats

-- | Parse a match expression with optional context filtering.
parseMatchExpr :: Bool -- ^ Whether to use context when resolving names.
               -> KipParser (Exp Ann) -- ^ Parsed match expression.
parseMatchExpr useCtx = do
  (patExp, scrutVar, scrutName) <- parseFirstPattern
  let argNames = [scrutName]
  pat <- expToPat True argNames patExp
  let patVarNames = extractPatVars pat
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
      msep <- optional (try clauseSep)
      case msep of
        Nothing -> return []
        Just _ -> do
          patExp <- parseWildcardOrExp
          pat <- case patExp of
                   Nothing -> return (PWildcard (Nom, NoSpan))
                   Just e -> expToPat False argNames e
          let clausePatVars = extractPatVars pat
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
      (lexeme (string "değilse") $> PWildcard (Nom, NoSpan)) <|>
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
      let inScope = [ident | (ident, _) <- varCandidates, ident `Set.member` parserCtx]
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

-- | Parse a non-escaped character in a character literal.
nonEscapeChar :: KipParser Char -- ^ Parsed character.
nonEscapeChar = satisfy (\c -> c /= '\\' && c /= '\'' && c /= '\0' && c /= '\n' && c /= '\r')

-- | Parse an escape sequence in a character literal.
escapeChar :: KipParser Char -- ^ Parsed escaped character.
escapeChar = do
  _ <- char '\\'
  c <- satisfy (`elem` ("\\\\'0nrvtbf" :: String))
  return $ case c of
    '\\' -> '\\'
    '\'' -> '\''
    '0' -> '\0'
    'n' -> '\n'
    'r' -> '\r'
    'v' -> '\v'
    't' -> '\t'
    'b' -> '\b'
    'f' -> '\f'
    _ -> c

-- | Parse a character literal token with optional case suffix.
parseCharToken :: KipParser (Char, Case) -- ^ Parsed character and case.
parseCharToken = do
  _ <- char '\''
  c <- escapeChar <|> nonEscapeChar
  _ <- char '\''
  mSuffix <- optional $ do
    _ <- optional (char '\'')
    takeWhile1P (Just "ek") isLetter
  let cas = maybe Nom stringCaseFromSuffix mSuffix
  return (c, cas)

-- | Map a case suffix to a grammatical case.
{-# INLINE stringCaseFromSuffix #-}
stringCaseFromSuffix :: Text -- ^ Suffix string.
                     -> Case -- ^ Case enum.
stringCaseFromSuffix suff =
  case T.toLower suff of
    s
      | s `elem` accSuffixesTxt -> Acc
      | s `elem` datSuffixesTxt -> Dat
      | s `elem` locSuffixesTxt -> Loc
      | s `elem` ablSuffixesTxt -> Abl
      | s `elem` genSuffixesTxt -> Gen
      | s `elem` insSuffixesTxt -> Ins
      | s `elem` condCaseSuffixesTxt -> Cond
      | otherwise -> Nom

accSuffixesTxt, datSuffixesTxt, locSuffixesTxt, ablSuffixesTxt, genSuffixesTxt, insSuffixesTxt, condCaseSuffixesTxt :: [Text]
accSuffixesTxt = ["ni","nı","nu","nü","yi","yı","yu","yü","i","ı","u","ü"]
datSuffixesTxt = ["ne","na","ye","ya","e","a"]
locSuffixesTxt = ["nde","nda","nte","nta","de","da","te","ta"]
ablSuffixesTxt = ["nden","ndan","nten","ntan","den","dan","ten","tan"]
genSuffixesTxt = ["in","ın","un","ün","nin","nın","nun","nün"]
insSuffixesTxt = ["le","la","yle","yla"]
condCaseSuffixesTxt = ["se","sa"]

-- | Prefer surface genitive over nominative when inflection is explicit.
preferSurfaceCase :: Identifier -- ^ Surface identifier.
                  -> Case -- ^ Candidate case.
                  -> Case -- ^ Preferred case.
preferSurfaceCase ident cas =
  case surfaceCaseHint ident of
    Just hinted | cas == Nom -> hinted
    _ -> cas

-- | Infer a case hint from a surface identifier.
{-# INLINE surfaceCaseHint #-}
surfaceCaseHint :: Identifier -- ^ Surface identifier.
                -> Maybe Case -- ^ Suggested case.
surfaceCaseHint (_, word) =
  if isJust (stripSuffixAny genSuffixesNoApostropheTxt (T.toLower word))
    then Just Gen
    else Nothing

-- | Strip a bare case suffix (no apostrophe) from an identifier.
-- This is used to detect surface-only case clues in ambiguous forms
-- and to seed candidate lists before morphology is consulted.
stripBareCaseSuffix :: Identifier -- ^ Surface identifier.
                    -> Maybe (Identifier, Case) -- ^ Base identifier and detected case.
stripBareCaseSuffix (mods, word) =
  let suffixes =
        [ ("nın", Gen), ("nin", Gen), ("nun", Gen), ("nün", Gen)
        , ("ın", Gen), ("in", Gen), ("un", Gen), ("ün", Gen)
        , ("yla", Ins), ("yle", Ins), ("la", Ins), ("le", Ins)
        , ("nden", Abl), ("ndan", Abl), ("nten", Abl), ("ntan", Abl)
        , ("den", Abl), ("dan", Abl), ("ten", Abl), ("tan", Abl)
        , ("nde", Loc), ("nda", Loc), ("nte", Loc), ("nta", Loc)
        , ("de", Loc), ("da", Loc), ("te", Loc), ("ta", Loc)
        , ("ne", Dat), ("na", Dat), ("ye", Dat), ("ya", Dat), ("e", Dat), ("a", Dat)
        , ("ni", Acc), ("nı", Acc), ("nu", Acc), ("nü", Acc)
        , ("yi", Acc), ("yı", Acc), ("yu", Acc), ("yü", Acc)
        , ("i", Acc), ("ı", Acc), ("u", Acc), ("ü", Acc)
        ]
      tryStrip (suf, cas) =
        case T.stripSuffix suf word of
          Just base | T.length base > 1 -> Just ((mods, base), cas)
          _ -> Nothing
  in foldr (\s acc -> tryStrip s <|> acc) Nothing suffixes

-- | Check whether an identifier names the integer type.
{-# INLINE isIntType #-}
isIntType :: Identifier -- ^ Identifier to inspect.
          -> Bool -- ^ True when identifier names the integer type.
isIntType (xs, x) = xs == [T.pack "tam"] && x == T.pack "sayı"

-- | Check whether an identifier names the floating-point type.
{-# INLINE isFloatType #-}
isFloatType :: Identifier -- ^ Identifier to inspect.
            -> Bool -- ^ True when identifier names the floating-point type.
isFloatType (xs, x) = xs == [T.pack "ondalık"] && x == T.pack "sayı"

-- | Check whether an identifier names the string type.
{-# INLINE isStringType #-}
isStringType :: Identifier -- ^ Identifier to inspect.
             -> Bool -- ^ True when identifier names the string type.
isStringType (xs, x) = null xs && x == T.pack "dizge"

-- | Check whether an identifier names the character type.
{-# INLINE isCharType #-}
isCharType :: Identifier -- ^ Identifier to inspect.
           -> Bool -- ^ True when identifier names the character type.
isCharType (xs, x) = null xs && x == T.pack "karakter"

-- | Convert an expression into a pattern.
expToPat :: Bool -- ^ Whether to allow scrutinee expressions.
         -> [Identifier] -- ^ Bound pattern names.
         -> Exp Ann -- ^ Expression to convert.
         -> KipParser (Pat Ann) -- ^ Parsed pattern.
expToPat allowScrutinee argNames e = do
  MkParserState{parserCtors} <- getP
  mCondPat <- condSurfaceToPat allowScrutinee argNames e
  case mCondPat of
    Just pat -> return pat
    Nothing ->
      case e of
        Var ann name candidates ->
          case selectCondNameInCtors parserCtors candidates of
            Nothing -> do
              mCondCtor <- condCtorFallback parserCtors candidates
              case mCondCtor of
                Just ctorName -> return (PCtor (ctorName, ann) [])
                Nothing ->
                  -- Not a constructor, treat as variable pattern
                  case preferInflected candidates of
                    (n, c):_ -> return (PVar n (mkAnn (preferSurfaceCase name c) (annSpan ann)))
                    _ -> return (PVar name (mkAnn (annCase ann) (annSpan ann)))
            Just ctorName -> return (PCtor (ctorName, ann) [])
        App _ (Var ann _ candidates) es -> do
          ctorName <- case selectCondNameInCtors parserCtors candidates of
            Just n -> return n
            Nothing -> do
              mCondCtor <- condCtorFallback parserCtors candidates
              case mCondCtor of
                Just n -> return n
                Nothing -> customFailure ErrPatternExpected
          -- Filter out scrutinee expressions before converting to patterns
          es' <- if allowScrutinee then dropScrutineeExp allowScrutinee argNames es else return es
          pats <- mapM expToPatArg es'
          pats' <- dropScrutineePat allowScrutinee argNames pats
          return (PCtor (ctorName, ann) pats')
        IntLit ann n -> return (PIntLit n ann)
        FloatLit ann n -> return (PFloatLit n ann)
        StrLit ann s -> return (PStrLit s ann)
        CharLit ann c -> return (PCharLit c ann)
        _ -> do
          -- Try to parse as list literal pattern
          mListPat <- tryParseListPattern e
          case mListPat of
            Just pat -> return pat
            Nothing -> customFailure ErrPatternExpected
  where
    condSurfaceToPat :: Bool -> [Identifier] -> Exp Ann -> KipParser (Maybe (Pat Ann))
    condSurfaceToPat allowScrutinee' argNames' expItem =
      case expItem of
        Var ann (mods, name) _ ->
          case stripCondSuffix name of
            Just base -> do
              ctorName <- resolveCondCtorName (mods, base)
              return (Just (PCtor (ctorName, ann) []))
            Nothing -> return Nothing
        App _ (Var ann (mods, name) _) es ->
          case stripCondSuffix name of
            Just base -> do
              ctorName <- resolveCondCtorName (mods, base)
              es' <- if allowScrutinee' then dropScrutineeExp allowScrutinee' argNames' es else return es
              pats' <-
                case malformedCtorBinderPats es' of
                  Just recovered -> return recovered
                  Nothing -> do
                    pats <- mapM expToPatArg es'
                    dropScrutineePat allowScrutinee' argNames' pats
              return (Just (PCtor (ctorName, ann) pats'))
            Nothing -> return Nothing
        IntLit ann n ->
          if annCase ann == Cond
            then return (Just (PIntLit n ann))
            else return Nothing
        FloatLit ann n ->
          if annCase ann == Cond
            then return (Just (PFloatLit n ann))
            else return Nothing
        StrLit ann s ->
          if annCase ann == Cond
            then return (Just (PStrLit s ann))
            else return Nothing
        CharLit ann c ->
          if annCase ann == Cond
            then return (Just (PCharLit c ann))
            else return Nothing
        _ -> do
          -- Try to parse as list literal pattern
          mListPat <- tryParseListPattern expItem
          case mListPat of
            Just pat -> return (Just pat)
            Nothing -> return Nothing
    stripCondSuffix = stripSuffixAny condSuffixesTxt
    malformedCtorBinderPats :: [Exp Ann] -> Maybe [Pat Ann]
    malformedCtorBinderPats es' =
      case es' of
        [headExp, midExp, tailExp]
          | expCase tailExp == Dat -> do
              (headName, headSpan) <- simpleVar headExp
              (tailName, _) <- tailBinder midExp
              tailSpan <- finalSpan tailExp
              return
                [ PVar headName (mkAnn Gen headSpan)
                , PVar tailName (mkAnn Dat tailSpan)
                ]
        _ -> Nothing
    simpleVar :: Exp Ann -> Maybe (Identifier, Span)
    simpleVar expItem =
      case expItem of
        Var ann name candidates ->
          case preferInflected candidates of
            (n, _):_ -> Just (n, annSpan ann)
            _ -> Just (name, annSpan ann)
        _ -> Nothing
    tailBinder :: Exp Ann -> Maybe (Identifier, Span)
    tailBinder expItem =
      case expItem of
        App _ _ args ->
          case reverse args of
            lastArg:_ -> simpleVar lastArg
            [] -> Nothing
        _ -> simpleVar expItem
    finalSpan :: Exp Ann -> Maybe Span
    finalSpan expItem =
      case expItem of
        Var ann _ _ -> Just (annSpan ann)
        App ann _ _ -> Just (annSpan ann)
        IntLit ann _ -> Just (annSpan ann)
        FloatLit ann _ -> Just (annSpan ann)
        StrLit ann _ -> Just (annSpan ann)
        CharLit ann _ -> Just (annSpan ann)
        Bind ann _ _ _ -> Just (annSpan ann)
        Seq ann _ _ -> Just (annSpan ann)
        Match ann _ _ -> Just (annSpan ann)
        Let ann _ _ -> Just (annSpan ann)
        Ascribe ann _ _ -> Just (annSpan ann)
    expCase :: Exp Ann -> Case
    expCase expItem =
      case expItem of
        Var ann _ _ -> annCase ann
        App ann _ _ -> annCase ann
        IntLit ann _ -> annCase ann
        FloatLit ann _ -> annCase ann
        StrLit ann _ -> annCase ann
        CharLit ann _ -> annCase ann
        Bind ann _ _ _ -> annCase ann
        Seq ann _ _ -> annCase ann
        Match ann _ _ -> annCase ann
        Let ann _ _ -> annCase ann
        Ascribe ann _ _ -> annCase ann
    resolveCondCtorName :: Identifier -> KipParser Identifier
    resolveCondCtorName ident@(mods, surface) = do
      MkParserState{parserCtors} <- getP
      candidates <- estimateCandidates False ident
      case selectCondNameInCtors parserCtors candidates of
        Just ctorName -> return ctorName
        Nothing -> do
          -- Secondary pass: inspect raw TRmorph analyses directly and try
          -- constructor roots from those analyses.
          analyses <- upsCached surface
          let morphCandidates =
                [ ((mods, root), cas)
                | analysis <- analyses
                , Just (root, cas) <- [getPossibleCase analysis]
                ]
          case selectCondNameInCtors parserCtors morphCandidates of
            Just ctorName -> return ctorName
            Nothing -> do
              mFallback <- condCtorFallback parserCtors (candidates ++ morphCandidates)
              case mFallback of
                Just ctorName -> return ctorName
                Nothing -> return ident
    condCtorFallback :: [Identifier] -> [(Identifier, Case)] -> KipParser (Maybe Identifier)
    condCtorFallback ctors candidates = do
      let stripped = mapMaybe (stripCondSuffixIdent . fst) candidates
      case stripped of
        (ident:_) -> do
          let direct = if ident `elem` ctors then Just ident else Nothing
          case direct of
            Just _ -> return direct
            Nothing -> do
              ident' <- normalizePossessive ident
              return (if ident' `elem` ctors then Just ident' else Nothing)
        [] -> return Nothing
    stripCondSuffixIdent (mods, word) = do
      base <- stripCondSuffix word
      return (mods, base)
    -- | Detect whether this is a match-expression context.
    -- Drop the first element if it's a valid scrutinee (Var matching argName) or
    -- a complex expression when in match expression context.
    -- For function clauses, complex expressions should fail.
    -- Match expression context is detected by argNames containing the dummy name "_".
    isMatchExprContext :: [Identifier] -- ^ Bound pattern names.
                       -> Bool -- ^ True when parsing a match expression.
    isMatchExprContext args = ([], T.pack "_") `elem` args
    -- | Drop a scrutinee expression when allowed.
    dropScrutineeExp :: Bool -- ^ Whether to allow scrutinee.
                     -> [Identifier] -- ^ Bound pattern names.
                     -> [Exp Ann] -- ^ Expression list.
                     -> KipParser [Exp Ann] -- ^ Filtered expressions.
    dropScrutineeExp _ _ [] = return []
    dropScrutineeExp _ args (v@(Var _ n _):rest)
      | n `elem` args = return rest
      | otherwise = return (v:rest)
    dropScrutineeExp _ args (_:rest)
      | isMatchExprContext args = return rest  -- Match expression: OK to drop complex expressions
      | otherwise = customFailure ErrPatternComplexExpr -- Function clause: fail
    -- | Drop a scrutinee pattern from pattern list when allowed.
    dropScrutineePat :: Bool -- ^ Whether to allow scrutinee.
                     -> [Identifier] -- ^ Bound pattern names.
                     -> [Pat Ann] -- ^ Patterns.
                     -> KipParser [Pat Ann] -- ^ Filtered patterns.
    dropScrutineePat allowScrutinee argNames pats =
      case pats of
        (PVar n _ :rest) | n `elem` argNames && null rest ->
          if allowScrutinee
            then return pats
            else customFailure ErrPatternArgNameRepeated
        _ -> return pats
    -- | Try to parse a list literal pattern from an expression.
    tryParseListPattern :: Exp Ann -> KipParser (Maybe (Pat Ann))
    tryParseListPattern exp =
      case extractListElements exp of
        Just elems -> do
          elemPats <- mapM (expToPat allowScrutinee argNames) elems
          return (Just (PListLit elemPats))
        Nothing -> return Nothing
    -- | Extract list elements from a list expression structure.
    extractListElements :: Exp Ann -> Maybe [Exp Ann]
    extractListElements (Var _ ([], name) _)
      | name == T.pack "boş" = Just []
    extractListElements (App _ (Var _ ([], name) _) [elem, rest])
      | name == T.pack "eki" = do
          restElems <- extractListElements rest
          return (elem : restElems)
    extractListElements _ = Nothing

-- | Convert a pattern argument expression into a pattern.
-- Unlike expToPat, this treats bare Var nodes as variables without checking if they're constructors.
-- This is correct for pattern arguments where "x'in y'ye ekiyse" should bind x and y as variables.
expToPatArg :: Exp Ann -- ^ Expression to convert.
            -> KipParser (Pat Ann) -- ^ Parsed pattern.
expToPatArg e = do
  MkParserState{parserCtors} <- getP
  case e of
    Var ann name candidates ->
      case selectCondNameInCtors parserCtors candidates of
        Just ctorName -> return (PCtor (ctorName, ann) [])
        Nothing ->
          case preferInflected candidates of
            (n, c):_ -> return (PVar n (mkAnn (preferSurfaceCase name c) (annSpan ann)))
            _ -> return (PVar name (mkAnn (annCase ann) (annSpan ann)))
    App _ (Var ann _ candidates) es -> do
      -- Nested constructor pattern - check if the function is a constructor
      case selectCondNameInCtors parserCtors candidates of
        Just ctorName -> do
          pats <- mapM expToPatArg es
          return (PCtor (ctorName, ann) pats)
        Nothing ->
          -- Treat non-constructor applications as annotated variables (e.g., x öğe listesi)
          case leftmostVarCandidate e of
            Just (n, sp) -> return (PVar n (mkAnn (annCase ann) sp))
            Nothing -> customFailure ErrPatternOnlyNames
    IntLit ann n -> return (PIntLit n ann)
    FloatLit ann n -> return (PFloatLit n ann)
    StrLit ann s -> return (PStrLit s ann)
    CharLit ann c -> return (PCharLit c ann)
    _ -> do
      -- Try to parse as list literal pattern
      mListPat <- tryParseListPatternArg e
      case mListPat of
        Just pat -> return pat
        Nothing -> customFailure ErrPatternOnlyNames
  where
    -- | Try to parse a list literal pattern from an expression.
    tryParseListPatternArg :: Exp Ann -> KipParser (Maybe (Pat Ann))
    tryParseListPatternArg exp =
      case extractListElements exp of
        Just elems -> do
          elemPats <- mapM expToPatArg elems
          return (Just (PListLit elemPats))
        Nothing -> return Nothing
    -- | Extract list elements from a list expression structure.
    extractListElements :: Exp Ann -> Maybe [Exp Ann]
    extractListElements (Var _ ([], name) _)
      | name == T.pack "boş" = Just []
    extractListElements (App _ (Var _ ([], name) _) [elem, rest])
      | name == T.pack "eki" = do
          restElems <- extractListElements rest
          return (elem : restElems)
    extractListElements _ = Nothing

    -- | Recover the user-written binder from an annotated variable phrase.
    -- Examples:
    --   "ilk öğenin" -> ilk
    --   "devam öğe listesine" -> devam
    leftmostVarCandidate :: Exp Ann -> Maybe (Identifier, Span)
    leftmostVarCandidate exp0 =
      case flattenApplied exp0 of
        (_, args0) -> firstVar args0
      where
        firstVar [] = Nothing
        firstVar (x:xs) =
          case x of
            Var varAnn name varCandidates ->
              case preferInflected varCandidates of
                (n, _):_ -> Just (n, annSpan varAnn)
                _ -> Just (name, annSpan varAnn)
            App {} ->
              case leftmostVarCandidate x of
                Just v -> Just v
                Nothing -> firstVar xs
            _ -> firstVar xs

-- | Select a constructor name only when it is in scope.
selectCondNameInCtors :: [Identifier] -- ^ Constructor identifiers.
                      -> [(Identifier, Case)] -- ^ Candidate identifiers.
                      -> Maybe Identifier -- ^ Selected constructor.
selectCondNameInCtors ctors candidates =
  case [name | (name, cas) <- candidates, cas == Cond, name `Set.member` ctorSet] of
    n:_ -> Just n
    [] ->
      case [name | (name, _) <- candidates, name `Set.member` ctorSet] of
        n:_ -> Just n
        [] ->
          case strippedCondMatches of
            n:_ -> Just n
            [] -> Nothing
  where
    ctorSet = Set.fromList ctors
    strippedCondMatches =
      [ base
      | (name, _) <- candidates
      , Just base <- [stripCondSuffixIdent name]
      , base `Set.member` ctorSet
      ]
    stripCondSuffixIdent (mods, word) = do
      base <- stripCondSuffix word
      return (mods, base)
    stripCondSuffix = stripSuffixAny condSuffixesTxt

-- | Select a conditional constructor name from candidates.
selectCondName :: Set.Set Identifier -- ^ Context identifiers.
               -> [(Identifier, Case)] -- ^ Candidate identifiers.
               -> Maybe Identifier -- ^ Selected constructor.
selectCondName ctx candidates =
  case [name | (name, cas) <- candidates, cas == Cond, name `Set.member` ctx] of
    n:_ -> Just n
    [] ->
      case [name | (name, _) <- candidates, name `Set.member` ctx] of
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
      , base `Set.member` ctx
      ]
    stripCondSuffixIdent (mods, word) = do
      base <- stripCondSuffix word
      return (mods, base)
    stripCondSuffix = stripSuffixAny condSuffixesTxt

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
    -- Invariant: n >= 0 (starts at 0, only decrements when n > 0)
    step :: Int -- ^ Current nesting depth (non-negative).
         -> Char -- ^ Current character.
         -> Text -- ^ Remaining input.
         -> TB.Builder -- ^ Output builder.
    step n c rest
      | n == 0 = TB.singleton c <> go n rest
      | otherwise = go n rest -- n > 0: skip character (inside comment)

-- | Parse a full statement from the REPL input.
parseFromRepl :: ParserState -- ^ Initial parser state.
              -> Text
              -- ^ REPL input buffer.
              -> Outer (Either (ParseErrorBundle Text ParserError) (Stmt Ann, ParserState)) -- ^ Parsed statement and state.
parseFromRepl st input = do
  let stripped = removeComments input
  (res, st') <- runStateT (runParserT parseStmt "Kip" stripped) st
  case res of
    Right stmt@(ExpStmt e)
      | Just ambErr <- ambiguousBareReplError st' stripped e ->
          case runParser (customFailure ambErr) "Kip" stripped of
            Left err -> return (Left err)
            Right _ -> return (Right (stmt, st'))
    _ -> return (fmap (, st') res)

-- | Parse an expression from REPL input.
parseExpFromRepl :: ParserState -- ^ Initial parser state.
                 -> Text
                 -- ^ REPL input buffer.
                 -> Outer (Either (ParseErrorBundle Text ParserError) (Exp Ann)) -- ^ Parsed expression.
parseExpFromRepl st input = do
  let stripped = removeComments input
  (res, st') <- runStateT (runParserT p "Kip" stripped) st
  case res of
    Right e
      | Just ambErr <- ambiguousBareReplError st' stripped e ->
          case runParser (customFailure ambErr) "Kip" stripped of
            Left err -> return (Left err)
            Right _ -> return res
      | otherwise -> return res
    _ -> return res
  where
    -- | Parser entry for REPL expressions.
    p :: KipParser (Exp Ann) -- ^ Parsed expression.
    p = do
      ws
      e <- parseExp
      ws
      eof
      return e

-- | Parse for debugging - returns AST with any remaining input (no eof required).
-- For statements, tries to parse without requiring eof.
parseForDebug :: ParserState -> Text
              -> Outer (Either (ParseErrorBundle Text ParserError) (Stmt Ann, Text))
parseForDebug st input = do
  (res, _) <- runStateT (runParserT p "Kip" (removeComments input)) st
  return res
  where
    p = do
      ws
      stmt <- parseStmt
      ws
      remaining <- getInput
      return (stmt, remaining)

-- | Parse expression for debugging.
parseExpForDebug :: ParserState -> Text
                 -> Outer (Either (ParseErrorBundle Text ParserError) (Exp Ann, Text))
parseExpForDebug st input = do
  let stripped = removeComments input
  (res, st') <- runStateT (runParserT p "Kip" stripped) st
  case res of
    Right (e, remaining)
      | Just ambErr <- ambiguousBareReplError st' stripped e ->
          case runParser (customFailure ambErr) "Kip" stripped of
            Left err -> return (Left err)
            Right _ -> return res
      | otherwise -> return res
    _ -> return res
  where
    p = do
      ws
      e <- parseExp
      ws
      remaining <- getInput
      return (e, remaining)

-- | Detect unresolved parenthesis-free REPL applications and build a custom error.
--
-- This check is intentionally heuristic and REPL-only:
-- * It catches chains where overloaded heads are likely to be parsed in an
--   unintended way without parentheses.
-- * It does not hard-code function names; decisions are derived from arity
--   information plus source-form cues.
ambiguousBareReplError :: ParserState -> Text -> Exp Ann -> Maybe ParserError
ambiguousBareReplError st sourceText expItem =
  case expItem of
    App _ fnExp args ->
      let arities = headArities st fnExp
          tooManyArgs =
            case arities of
              [] -> False
              _ -> length args > maximum arities
          suspiciousChain = length args >= 4 && inflectedVarCount args >= 2
          -- Nested-call ambiguity heuristic:
          -- if an overloaded head is applied to arguments that are themselves
          -- calls with strictly smaller callable arity ceilings, the chain is
          -- likely a parenthesis-free misparse.
          overloadedNestedSmallerArgs =
            case arities of
              [] -> False
              _ ->
                let headMaxArity = maximum arities
                in length arities > 1
                     && not (null args)
                     && all (isNestedSmallerHead headMaxArity) args
          -- Surface-form ambiguity heuristic:
          -- in parenthesis-free input, repeated use of the same overloaded
          -- head name is treated as ambiguous to match user-facing diagnostics.
          repeatedOverloadedHeadSurface =
            case normalizedHeadIdent fnExp of
              Just headIdent ->
                let headName = snd headIdent
                    byName = fromMaybe Set.empty (M.lookup headName (parserFuncAritiesByName st))
                    noParens = not (T.any (\c -> c == '(' || c == ')') sourceText)
                in Set.size byName > 1
                     && noParens
                     && T.count headName sourceText >= 2
              Nothing -> False
      in if tooManyArgs || suspiciousChain || repeatedOverloadedHeadSurface || overloadedNestedSmallerArgs
           then Just (mkAmbiguousBareError expItem fnExp arities)
           else Nothing
    _ -> Nothing
  where
    inflectedVarCount =
      length . filter isInflectedVar
    isInflectedVar e =
      case e of
        Var ann _ _ -> annCase ann /= Nom
        _ -> False

    isNestedSmallerHead :: Int -> Exp Ann -> Bool
    isNestedSmallerHead headMaxArity e =
      case e of
        App _ nestedFn _ ->
          let nestedArities = headArities st nestedFn
          in case nestedArities of
               [] -> False
               _ -> maximum nestedArities < headMaxArity
        _ -> False

    mkAmbiguousBareError :: Exp Ann -> Exp Ann -> [Int] -> ParserError
    mkAmbiguousBareError fullExp fnExp arities =
      let sp = annSpan (annExp fullExp)
      in case functionIdent fnExp of
           Just ident ->
             let displayIdent =
                   case stripBareCaseSuffix ident of
                     Just (base, _) -> base
                     Nothing -> ident
             in if length arities > 1
                  then ErrAmbiguousBareApplicationOverload displayIdent arities sp
                  else ErrAmbiguousBareApplicationOverload displayIdent [] sp
           Nothing -> ErrAmbiguousBareApplication sp

    functionIdent :: Exp Ann -> Maybe Identifier
    functionIdent e =
      case e of
        Var _ varName _ -> Just varName
        _ -> Nothing

    normalizedHeadIdent :: Exp Ann -> Maybe Identifier
    normalizedHeadIdent e =
      case functionIdent e of
        Just ident ->
          case stripBareCaseSuffix ident of
            Just (base, _) -> Just base
            Nothing -> Just ident
        Nothing -> Nothing

    headArities :: ParserState -> Exp Ann -> [Int]
    headArities pst fnExp =
      case fnExp of
        Var _ varName varCandidates ->
          let arityMap = parserFuncArities pst
              arityByName = parserFuncAritiesByName pst
              directIds = varName : map fst varCandidates
              strippedIds =
                mapMaybe
                  (\ident ->
                    case stripBareCaseSuffix ident of
                      Just (base, _) -> Just base
                      Nothing -> Nothing
                  )
                  directIds
              ids = nub (directIds ++ strippedIds)
          in aritiesForIds arityMap arityByName ids
        _ -> []

    -- | Gather candidate arities by exact identifier and by surface name.
    --
    -- ==== Performance note (Optimization: ambiguity arity lookup)
    -- Name-based collection uses the prebuilt 'parserFuncAritiesByName'
    -- index, avoiding full scans of all known arities on each REPL parse.
    aritiesForIds :: M.Map Identifier (Set.Set Int) -> M.Map Text (Set.Set Int) -> [Identifier] -> [Int]
    aritiesForIds arityMap arityByName ids =
      let byIdent =
            foldl'
              Set.union
              Set.empty
              [ fromMaybe Set.empty (M.lookup ident arityMap)
              | ident <- ids
              ]
          byName =
            foldl'
              (\acc keyName -> Set.union acc (fromMaybe Set.empty (M.lookup keyName arityByName)))
              Set.empty
              (ordNub (map snd ids))
      in sort (Set.toList (Set.union byIdent byName))

-- | Parse a full file into statements.
parseFromFile :: ParserState -- ^ Initial parser state.
              -> Text
              -- ^ File contents.
              -> Outer (Either (ParseErrorBundle Text ParserError) ([Stmt Ann], ParserState)) -- ^ Parsed statements and state.
parseFromFile st input = do
  let stripped = removeComments input
  (res, st') <- runStateT (runParserT p "Kip" stripped) st
  case res of
    Right stmts ->
      case findRepeatedPatternBinderText (parserFilePath st) stripped of
        Just (ident, sp) ->
          case runParser (customFailure (ErrPatternBinderRepeated ident sp)) "Kip" stripped of
            Left err' -> return (Left err')
            Right _ -> return (Right (stmts, st'))
        Nothing -> return (Right (stmts, st'))
    Left err ->
      -- When a repeated-arg pattern slips into a branch that produces a
      -- generic syntax error, we prefer a targeted custom error so tests
      -- (and users) see the intended diagnostic.
      if hasCustomParserError err
        then return (Left err)
        else
          case findRepeatedPatternBinderText (parserFilePath st) stripped of
            Just (ident, sp) ->
              case runParser (customFailure (ErrPatternBinderRepeated ident sp)) "Kip" stripped of
                Left err' -> return (Left err')
                Right _ -> return (Left err)
            Nothing ->
              if hasRepeatedArgPatternText stripped
                then
                  -- We re-run a tiny parser that always fails with the custom error
                  -- so Megaparsec constructs a bundle at the correct location.
                  case runParser (customFailure ErrPatternArgNameRepeated) "Kip" stripped of
                    Left err' -> return (Left err')
                    Right _ -> return (Left err)
                else return (Left err)
  where
    -- | Parser entry for file contents.
    p :: KipParser [Stmt Ann] -- ^ Parsed statements.
    p = do
      ws
      stmts <- many (parseStmt <* ws)
      eof
      return stmts
    hasCustomParserError :: ParseErrorBundle Text ParserError -> Bool
    hasCustomParserError (ParseErrorBundle errs _) = any isCustom (NE.toList errs)
    isCustom parseErr =
      case parseErr of
        FancyError _ xs -> any isErrorCustom (Set.toList xs)
        _ -> False
    isErrorCustom fancy =
      case fancy of
        ErrorCustom _ -> True
        _ -> False

-- | Detect repeated argument names in function clause heads from raw text.
-- This is a coarse, line-oriented heuristic used only as a fallback
-- when full parsing fails; it avoids tying the detection to a specific
-- AST shape while still surfacing the intended error.
hasRepeatedArgPatternText :: Text -- ^ Source text.
                          -> Bool -- ^ True when a repeated arg pattern is found.
hasRepeatedArgPatternText src =
  let ls = T.lines src
      stripPunct = T.dropWhileEnd (\c -> c == ',' || c == ';')
      isCondSuffix txt = any (`T.isSuffixOf` txt) ["ysa", "yse", "sa", "se"]
      wordChars c = isLetter c || c == '\'' || c == '-'
      takeWord = T.takeWhile wordChars
      dropToNextParen t =
        case T.breakOn (T.pack "(") t of
          (_, rest) | T.null rest -> Nothing
          (_, rest) -> Just (T.drop 1 rest)
      collectArgs line =
        if not (T.isSuffixOf (T.pack ",") (T.strip line))
          then []
          else go line []
        where
          go t acc =
            case dropToNextParen t of
              Nothing -> acc
              Just rest ->
                let rest' = T.dropWhile isSpace rest
                    name = takeWord rest'
                    next = T.dropWhile (/= ')') rest'
                in if T.null name
                     then acc
                     else go next (name : acc)
      startsWithRepeatedArg args line =
        case T.words (T.dropWhile isSpace line) of
          (t1:t2:_) ->
            let t1' = stripPunct t1
                t2' = stripPunct t2
            in t1' `elem` args && isCondSuffix t2'
          _ -> False
      scan [] _ = False
      scan (l:rest) args
        | null args =
            let args' = collectArgs l
            in if null args' then scan rest [] else scan rest args'
        | startsWithRepeatedArg args l = True
        | T.isSuffixOf (T.pack ".") (T.strip l) = scan rest []
        | otherwise = scan rest args
  in scan ls []

-- | Find repeated binders in the same pattern surface (heuristic fallback).
--
-- This catches surface forms such as @x'in x'e ekiyse@ and points to the
-- later repeated binder occurrence.
findRepeatedPatternBinderText :: Maybe FilePath -- ^ Optional source path.
                              -> Text -- ^ Source text.
                              -> Maybe (Identifier, Span) -- ^ Repeated binder and span.
findRepeatedPatternBinderText mPath src =
  listToMaybe (mapMaybe lineRepeat (zip [1 :: Int ..] (T.lines src)))
  where
    suffixes =
      [ "nın", "nin", "nun", "nün"
      , "ın", "in", "un", "ün"
      , "dan", "den", "tan", "ten"
      , "da", "de", "ta", "te"
      , "yla", "yle", "la", "le"
      , "yı", "yi", "yu", "yü"
      , "ı", "i", "u", "ü"
      , "ya", "ye", "a", "e"
      ]
    normalize :: Text -> Text
    normalize = stripCaseSuffix . stripApostropheSuffix . T.toLower
    stripApostropheSuffix txt =
      case T.breakOn "'" txt of
        (base, rest)
          | T.null rest -> txt
          | otherwise -> base
    stripCaseSuffix txt =
      fromMaybe txt (stripSuffixAny suffixes txt)
    isWordChar c = isLetter c || c == '\'' || c == '-'
    wordsWithCols :: Text -> [(Text, Int)]
    wordsWithCols line = go 0 (T.unpack line)
      where
        go _ [] = []
        go col (c:cs)
          | isWordChar c =
              let w = c : takeWhile isWordChar cs
                  rest = dropWhile isWordChar cs
                  wLen = length w
              in (T.pack w, col + 1) : go (col + wLen) rest
          | otherwise = go (col + 1) cs
    isCondWord w = any (`T.isSuffixOf` T.toLower w) ["ekiyse", "ikilisiyse"]
    mkSpan :: Int -> Int -> Text -> Span
    mkSpan lineNo col txt =
      let mkP l c = SourcePos "Kip" (mkPos l) (mkPos c)
          start = mkP lineNo col
          end = mkP lineNo (col + max 0 (T.length txt - 1))
      in Span start end mPath
    lineRepeat (lineNo, lineText) =
      let ws = wordsWithCols lineText
          triples = zip3 ws (drop 1 ws) (drop 2 ws)
      in listToMaybe
           [ (([], bNorm), mkSpan lineNo bCol bWord)
           | ((aWord, _), (bWord, bCol), (cWord, _)) <- triples
           , isCondWord cWord
           , ("'" `T.isInfixOf` aWord) || ("'" `T.isInfixOf` bWord)
           , let aNorm = normalize aWord
           , let bNorm = normalize bWord
           , not (T.null aNorm)
           , aNorm == bNorm
           ]
