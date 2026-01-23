{-# LANGUAGE NamedFieldPuns #-}
-- | Rendering utilities for identifiers, types, and values.
module Kip.Render
  ( prettyIdent
  , applyTyMods
  , RenderCache
  , newRenderCache
  , mkRenderCache
  , renderIdentWithCases
  , renderIdentWithCase
  , renderTy
  , renderTyNom
  , renderTyParts
  , renderArg
  , renderArgParts
  , renderFunctionSignature
  , renderFunctionSignatureParts
  , renderSig
  , renderTyText
  , renderTyNomText
  , renderSigText
  , renderExpValue
  , renderExpWithCase
  , renderExpNom
  ) where

import Data.Char (isLetter, isLower, isDigit, isSpace)
import Data.List (intercalate, maximumBy, find, isInfixOf, isSuffixOf, isPrefixOf, intersect, nub)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashTable.IO as HT

import Language.Foma
import Kip.AST
import Kip.Eval (EvalState, evalCtors)

-- | Cache for morphology calls.
type MorphCache = HT.BasicHashTable Text [Text]

-- | Render cache containing morphology caches.
data RenderCache = RenderCache
  { rcUpsCache :: !MorphCache -- ^ Cache for analysis results.
  , rcDownsCache :: !MorphCache -- ^ Cache for generation results.
  }

-- | Create a new empty render cache.
newRenderCache :: IO RenderCache -- ^ Fresh render cache.
newRenderCache = do
  upsCache <- HT.new
  RenderCache upsCache <$> HT.new

-- | Create a render cache from existing hash tables (for sharing with parser).
mkRenderCache :: MorphCache -- ^ Shared ups cache.
              -> MorphCache -- ^ Shared downs cache.
              -> RenderCache -- ^ Render cache.
mkRenderCache = RenderCache

-- | Cached version of 'ups'.
upsCached :: RenderCache -- ^ Render cache.
          -> FSM -- ^ Morphology FSM.
          -> Text -- ^ Surface form.
          -> IO [Text] -- ^ Morphology analyses.
upsCached RenderCache{rcUpsCache} fsm s = do
  cached <- HT.lookup rcUpsCache s
  case cached of
    Just res -> return res
    Nothing -> do
      res <- ups fsm s
      HT.insert rcUpsCache s res
      return res

-- | Cached version of 'downs'.
downsCached :: RenderCache -- ^ Render cache.
            -> FSM -- ^ Morphology FSM.
            -> Text -- ^ Analysis string.
            -> IO [Text] -- ^ Surface forms.
downsCached RenderCache{rcDownsCache} fsm s = do
  cached <- HT.lookup rcDownsCache s
  case cached of
    Just res -> return res
    Nothing -> do
      res <- downs fsm s
      HT.insert rcDownsCache s res
      return res

-- | Cached batch morphology generation lookup.
-- Uses batch FFI when multiple stems are missing to avoid repeated handle setup.
downsCachedBatch :: RenderCache -- ^ Render cache.
                -> FSM -- ^ Morphology FSM.
                -> [Text] -- ^ Morphology stems.
                -> IO [[Text]] -- ^ Generated surface forms per stem.
downsCachedBatch _ _ [] = return []
downsCachedBatch RenderCache{rcDownsCache} fsm stems = do
  cached <- mapM (HT.lookup rcDownsCache) stems
  let missing = [s | (s, Nothing) <- zip stems cached]
  fetched <- if null missing then return [] else downsBatch fsm missing
  let fetchedMap = M.fromList (zip missing fetched)
  mapM_ (uncurry (HT.insert rcDownsCache)) (zip missing fetched)
  let resolve s = fromMaybe (fromMaybe [] (M.lookup s fetchedMap))
  return (zipWith resolve stems cached)

-- | Render a dotted identifier to a single dash-separated string.
prettyIdent :: Identifier -- ^ Identifier to render.
            -> String -- ^ Rendered string.
prettyIdent (xs, x) = T.unpack (T.intercalate (T.pack "-") (xs ++ [x]))

-- | Apply type modifier expansions to a type name.
applyTyMods :: [(Identifier, [Identifier])] -- ^ Type modifier expansions.
            -> Identifier -- ^ Base type identifier.
            -> Identifier -- ^ Expanded identifier.
applyTyMods tyMods name =
  case lookup name tyMods of
    Nothing -> name
    Just mods ->
      let parts = concatMap (\(xs, x) -> xs ++ [x]) mods
          (xs, x) = name
      in (parts ++ xs, x)

-- | Render the case tag for morphology lookups.
caseTag :: Case -- ^ Case to encode.
        -> String -- ^ Morphology case tag.
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

-- | Pick the best surface form from candidate forms.
pickDownForm :: [String] -- ^ Candidate surface forms.
             -> Maybe String -- ^ Selected form.
pickDownForm = pickDownFormWithStem Nothing

-- | Pick the best surface form, preferring candidates close to a stem.
pickDownFormWithStem :: Maybe String -- ^ Preferred stem.
                     -> [String] -- ^ Candidate surface forms.
                     -> Maybe String -- ^ Selected form.
pickDownFormWithStem mStem forms =
  let normalizedForms = map normalizeQuotes forms
  in case filter isPlain normalizedForms of
    [] -> case normalizedForms of
      f:_ -> Just f
      [] -> Nothing
    fs ->
      let preferred = case mStem of
            Just stem -> filter (doesNotDoubleConsonant stem) fs
            Nothing -> fs
          candidates = if null preferred then fs else preferred
      in Just (maximumBy (\a b -> compare (length a) (length b)) candidates)
  where
    -- | Normalize quoting to a single ASCII quote.
    normalizeQuotes :: String -- ^ Input string.
                    -> String -- ^ Normalized string.
    normalizeQuotes = collapseQuotes . map normalizeQuote
    -- | Normalize any quote-like character to a single quote.
    normalizeQuote :: Char -- ^ Input character.
                   -> Char -- ^ Normalized character.
    normalizeQuote c = if isQuoteLike c then '\'' else c
    -- | Collapse repeated quote characters.
    collapseQuotes :: String -- ^ Input string.
                   -> String -- ^ Normalized string.
    collapseQuotes [] = []
    collapseQuotes [x] = [x]
    collapseQuotes (x:y:xs)
      | x == '\'' && y == '\'' = collapseQuotes ('\'' : xs)
      | otherwise = x : collapseQuotes (y : xs)
    -- | Check whether a character should be treated as a quote.
    isQuoteLike :: Char -- ^ Character to inspect.
                -> Bool -- ^ True when character is quote-like.
    isQuoteLike c = c `elem` ("'´`'ʼ" :: String)
    -- | Check whether a form is "plain" (letters and hyphens only).
    isPlain :: String -- ^ Surface form.
            -> Bool -- ^ True when the form is plain.
    isPlain =
      all (\c -> (isLetter c && isLower c) || c == '-')
    -- | Avoid forms that double the last consonant of a stem.
    doesNotDoubleConsonant :: String -- ^ Stem to compare against.
                           -> String -- ^ Candidate form.
                           -> Bool -- ^ True when the form does not double consonants.
    doesNotDoubleConsonant stem form =
      case reverse stem of
        c:_ | isConsonant c -> not (hasDoubledConsonant c form stem)
        _ -> True
    -- | Check if a character is a consonant for Turkish morphology heuristics.
    isConsonant :: Char -- ^ Character to inspect.
                -> Bool -- ^ True when character is a consonant.
    isConsonant c = isLetter c && c `notElem` ("aeiouıöü" :: String)
    -- | Detect whether a candidate form doubles a stem-final consonant.
    hasDoubledConsonant :: Char -- ^ Stem-final consonant.
                        -> String -- ^ Candidate form.
                        -> String -- ^ Original stem.
                        -> Bool -- ^ True when candidate doubles consonant.
    hasDoubledConsonant c form stem =
      let stemLen = length stem
      in length form > stemLen + 1 &&
         take stemLen form == stem &&
         form !! stemLen == c

-- | Apply a minimal suffix heuristic when morphology yields no forms.
fallbackInflect :: String -- ^ Surface form.
                -> [Case] -- ^ Desired cases.
                -> String -- ^ Inflected form.
fallbackInflect s cases =
  case cases of
    [P3s] -> addP3sSuffix s
    [Gen] -> addGenSuffix s
    [Acc] -> addAccSuffix s
    [Dat] -> addDatSuffix s
    [Loc] -> addLocSuffix s
    [Abl] -> addAblSuffix s
    _ -> s

-- | Add the genitive suffix for a simple fallback inflection.
addGenSuffix :: String -- ^ Surface form.
             -> String -- ^ Inflected form.
addGenSuffix s
  | null s = s
  | maybe False isDigit (lastChar s) = s ++ "'nin"
  | otherwise =
      case lastVowel s of
        Nothing -> s ++ "in"
        Just v ->
          let suffixV = genVowel v
              connector = if endsWithVowel s then "n" else ""
          in s ++ connector ++ [suffixV] ++ "n"
  where
    genVowel v
      | v `elem` "aı" = 'ı'
      | v `elem` "ou" = 'u'
      | v `elem` "ei" = 'i'
      | v `elem` "öü" = 'ü'
      | otherwise = 'i'

-- | Add the accusative suffix for a simple fallback inflection.
addAccSuffix :: String -- ^ Surface form.
             -> String -- ^ Inflected form.
addAccSuffix s =
  case lastVowel s of
    Nothing -> s ++ "'i"
    Just v ->
      let suffixV = accVowel v
          separator = if maybe False isDigit (lastChar s) then "'" else ""
          connector = if endsWithVowel s then "y" else ""
      in s ++ separator ++ connector ++ [suffixV]
  where
    accVowel v
      | v `elem` "aı" = 'ı'
      | v `elem` "ou" = 'u'
      | v `elem` "ei" = 'i'
      | v `elem` "öü" = 'ü'
      | otherwise = 'i'

-- | Add the dative suffix for a simple fallback inflection.
addDatSuffix :: String -- ^ Surface form.
             -> String -- ^ Inflected form.
addDatSuffix s =
  case lastVowel s of
    Nothing -> s ++ "'e"
    Just v ->
      let suffixV = if v `elem` "eiöü" then 'e' else 'a'
          separator = if maybe False isDigit (lastChar s) then "'" else ""
          connector = if endsWithVowel s then "y" else ""
      in s ++ separator ++ connector ++ [suffixV]

-- | Add the locative suffix for a simple fallback inflection.
addLocSuffix :: String -- ^ Surface form.
             -> String -- ^ Inflected form.
addLocSuffix s =
  case lastVowel s of
    Nothing -> s ++ "'de"
    Just v ->
      let suffixV = if v `elem` "eiöü" then 'e' else 'a'
          separator = if maybe False isDigit (lastChar s) then "'" else ""
      in s ++ separator ++ "d" ++ [suffixV]

-- | Add the ablative suffix for a simple fallback inflection.
addAblSuffix :: String -- ^ Surface form.
             -> String -- ^ Inflected form.
addAblSuffix s =
  case lastVowel s of
    Nothing -> s ++ "'den"
    Just v ->
      let suffixV = if v `elem` "eiöü" then 'e' else 'a'
          separator = if maybe False isDigit (lastChar s) then "'" else ""
      in s ++ separator ++ "d" ++ [suffixV] ++ "n"

-- | Add the 3rd person possessive suffix for a simple fallback inflection.
addP3sSuffix :: String -- ^ Surface form.
             -> String -- ^ Inflected form.
addP3sSuffix s =
  case lastVowel s of
    Nothing -> s
    Just v ->
      let suffixV = p3sVowel v
      in if endsWithVowel s
           then s ++ "s" ++ [suffixV]
           else s ++ [suffixV]

-- | Find the last vowel in a word.
lastVowel :: String -- ^ Input word.
          -> Maybe Char -- ^ Last vowel when present.
lastVowel =
  foldl (\acc c -> if isVowel c then Just c else acc) Nothing

-- | Get the last character of a string.
lastChar :: String -- ^ Input string.
         -> Maybe Char -- ^ Last character when present.
lastChar s =
  case reverse s of
    c:_ -> Just c
    [] -> Nothing

-- | Check whether a word ends with a vowel.
endsWithVowel :: String -- ^ Input word.
              -> Bool -- ^ True when the word ends with a vowel.
endsWithVowel s =
  case reverse s of
    c:_ -> isVowel c
    [] -> False

-- | Vowel predicate for Turkish vowels.
isVowel :: Char -- ^ Character to inspect.
        -> Bool -- ^ True when character is a vowel.
isVowel c = c `elem` ("aıoueiöü" :: String)

-- | Pick the possessive suffix vowel for a given last vowel.
p3sVowel :: Char -- ^ Last vowel.
         -> Char -- ^ Possessive suffix vowel.
p3sVowel v
  | v `elem` "aı" = 'ı'
  | v `elem` "ou" = 'u'
  | v `elem` "ei" = 'i'
  | v `elem` "öü" = 'ü'
  | otherwise = 'ı'

-- | Render an identifier with one or more cases applied.
renderIdentWithCases :: RenderCache -- ^ Render cache.
                     -> FSM -- ^ Morphology FSM.
                     -> Identifier -- ^ Identifier to render.
                     -> [Case] -- ^ Cases to apply.
                     -> IO String -- ^ Rendered identifier.
renderIdentWithCases cache fsm (xs, x) cases = do
  let stem = T.unpack x
      tagged = T.pack (stem ++ "<N>" ++ concatMap caseTag (filter (/= Nom) cases))
  forms <- map T.unpack <$> downsCached cache fsm tagged
  forms' <- if null forms
    then deriveInflectedForms cache fsm stem (concatMap caseTag (filter (/= Nom) cases))
    else return forms
  let root = fromMaybe (fallbackInflect stem cases) (pickDownFormWithStem (Just stem) forms)
      root' = fromMaybe root (pickDownFormWithStem (Just stem) forms')
  return (T.unpack (T.intercalate (T.pack "-") (xs ++ [T.pack root'])))

-- | Render an identifier with a single case applied.
renderIdentWithCase :: RenderCache -- ^ Render cache.
                    -> FSM -- ^ Morphology FSM.
                    -> Identifier -- ^ Identifier to render.
                    -> Case -- ^ Case to apply.
                    -> IO String -- ^ Rendered identifier.
renderIdentWithCase cache fsm name cas =
  renderIdentWithCases cache fsm name [cas]

-- | Derive inflected forms by analyzing and then generating with tags.
deriveInflectedForms :: RenderCache -- ^ Render cache.
                     -> FSM -- ^ Morphology FSM.
                     -> String -- ^ Root surface form.
                     -> String -- ^ Morphology tags to apply.
                     -> IO [String] -- ^ Derived surface forms.
deriveInflectedForms cache fsm root tags = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack root)
  let nounAnalyses =
        filter (\a -> "<N>" `isInfixOf` a && not ("<V>" `isInfixOf` a)) analyses
      baseAnalyses =
        if null nounAnalyses then analyses else nounAnalyses
      stemAnalyses = map stripCaseTags baseAnalyses
      taggedStems = map (\stem -> T.pack (stem ++ tags)) stemAnalyses
  -- Batch morphology generation to amortize Foma handle setup.
  forms <- downsCachedBatch cache fsm taggedStems
  return (concatMap (map T.unpack) forms)

-- | Strip case tags from a morphology analysis string.
stripCaseTags :: String -- ^ Analysis string.
              -> String -- ^ Stem without case tags.
stripCaseTags = go
  where
    -- | List of case tags recognized in analyses.
    tags :: [String] -- ^ Known case tags.
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
    -- | Remove case tags iteratively from the end.
    go :: String -- ^ Analysis string.
       -> String -- ^ Stripped stem.
    go str =
      case find (`isSuffixOf` str) tags of
        Nothing -> str
        Just tag -> go (take (length str - length tag) str)

-- | Render a type into surface syntax with case inflection.
renderTy :: RenderCache -- ^ Render cache.
         -> FSM -- ^ Morphology FSM.
         -> [Identifier] -- ^ Type parameters to render with P3s.
         -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
         -> Ty Ann -- ^ Type to render.
         -> IO String -- ^ Rendered type.
renderTy cache fsm paramTyCons tyMods ty =
  case ty of
    TyInd ann name ->
      renderIdentWithCase cache fsm (applyTyMods tyMods name) (annCase ann)
    TyVar ann name ->
      renderIdentWithCase cache fsm name (annCase ann)
    TySkolem ann name ->
      renderIdentWithCase cache fsm name (annCase ann)
    TyApp ann (TyInd _ name) [argTy] -> do
      argStr <- renderTy cache fsm paramTyCons tyMods argTy
      let nameCases =
            if name `elem` paramTyCons
              then if annCase ann == Nom then [P3s] else [P3s, annCase ann]
              else [annCase ann]
      nameStr <- renderIdentWithCases cache fsm name nameCases
      return (argStr ++ " " ++ nameStr)
    TyApp ann ctor _ -> do
      ctorStr <- renderTy cache fsm paramTyCons tyMods ctor
      return (ctorStr ++ caseTag (annCase ann))
    TyInt ann ->
      renderIdentWithCase cache fsm ([T.pack "tam"], T.pack "sayı") (annCase ann)
    TyFloat ann ->
      renderIdentWithCase cache fsm ([T.pack "ondalık"], T.pack "sayı") (annCase ann)
    TyString ann ->
      renderIdentWithCase cache fsm ([], T.pack "dizge") (annCase ann)
    Arr {} ->
      return "işlev"

-- | Render a type in nominative case (for error messages).
renderTyNom :: RenderCache -- ^ Render cache.
            -> FSM -- ^ Morphology FSM.
            -> [Identifier] -- ^ Type parameters to render with P3s.
            -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
            -> Ty Ann -- ^ Type to render.
            -> IO String -- ^ Rendered type.
renderTyNom cache fsm paramTyCons tyMods ty =
  case ty of
    TyInd _ name ->
      renderIdentWithCase cache fsm (applyTyMods tyMods name) Nom
    TyVar _ name ->
      renderIdentWithCase cache fsm name Nom
    TySkolem _ name ->
      renderIdentWithCase cache fsm name Nom
    TyApp _ (TyInd _ name) [argTy] -> do
      argStr <- renderTyNom cache fsm paramTyCons tyMods argTy
      let nameCases = if name `elem` paramTyCons then [P3s] else [Nom]
      nameStr <- renderIdentWithCases cache fsm name nameCases
      return (argStr ++ " " ++ nameStr)
    TyApp _ ctor _ ->
      renderTyNom cache fsm paramTyCons tyMods ctor
    TyInt _ ->
      renderIdentWithCase cache fsm ([T.pack "tam"], T.pack "sayı") Nom
    TyFloat _ ->
      renderIdentWithCase cache fsm ([T.pack "ondalık"], T.pack "sayı") Nom
    TyString _ ->
      renderIdentWithCase cache fsm ([], T.pack "dizge") Nom
    Arr {} ->
      return "işlev"

-- | Render a type as parts with a flag indicating type variables.
renderTyParts :: RenderCache -- ^ Render cache.
              -> FSM -- ^ Morphology FSM.
              -> [Identifier] -- ^ Type parameters to render with P3s.
              -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
              -> Ty Ann -- ^ Type to render.
              -> IO [(String, Bool)] -- ^ Rendered type parts and type-var flags.
renderTyParts cache fsm paramTyCons tyMods ty =
  case ty of
    TyInd ann name -> do
      s <- renderIdentWithCase cache fsm (applyTyMods tyMods name) (annCase ann)
      return [(s, False)]
    TyVar ann name -> do
      s <- renderIdentWithCase cache fsm name (annCase ann)
      return [(s, True)]
    TySkolem ann name -> do
      s <- renderIdentWithCase cache fsm name (annCase ann)
      return [(s, True)]
    TyApp ann (TyInd _ name) [argTy] -> do
      argParts <- renderTyParts cache fsm paramTyCons tyMods argTy
      let nameCases =
            if name `elem` paramTyCons
              then if annCase ann == Nom then [P3s] else [P3s, annCase ann]
              else [annCase ann]
      nameStr <- renderIdentWithCases cache fsm name nameCases
      return (argParts ++ [(" ", False), (nameStr, False)])
    TyApp ann ctor _ -> do
      ctorStr <- renderTy cache fsm paramTyCons tyMods ctor
      return [(ctorStr ++ caseTag (annCase ann), False)]
    TyInt ann -> do
      s <- renderIdentWithCase cache fsm ([T.pack "tam"], T.pack "sayı") (annCase ann)
      return [(s, False)]
    TyFloat ann -> do
      s <- renderIdentWithCase cache fsm ([T.pack "ondalık"], T.pack "sayı") (annCase ann)
      return [(s, False)]
    TyString ann -> do
      s <- renderIdentWithCase cache fsm ([], T.pack "dizge") (annCase ann)
      return [(s, False)]
    Arr {} ->
      return [("işlev", False)]

-- | Render a typed argument as a single string.
renderArg :: RenderCache -- ^ Render cache.
          -> FSM -- ^ Morphology FSM.
          -> [Identifier] -- ^ Type parameters to render with P3s.
          -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
          -> Arg Ann -- ^ Argument to render.
          -> IO String -- ^ Rendered argument.
renderArg cache fsm paramTyCons tyMods (argName, ty) = do
  argStr <- renderIdentWithCase cache fsm argName Nom
  tyStr <-
    if shouldPossessiveArg argName
      then renderTyPossessive cache fsm paramTyCons tyMods ty
      else renderTy cache fsm paramTyCons tyMods ty
  return ("(" ++ argStr ++ " " ++ tyStr ++ ")")

-- | Render a typed argument into name and type parts.
renderArgParts :: RenderCache -- ^ Render cache.
               -> FSM -- ^ Morphology FSM.
               -> [Identifier] -- ^ Type parameters to render with P3s.
               -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
               -> Arg Ann -- ^ Argument to render.
               -> IO (String, [(String, Bool)]) -- ^ Rendered name and type parts.
renderArgParts cache fsm paramTyCons tyMods (argName, ty) = do
  argStr <- renderIdentWithCase cache fsm argName Nom
  tyParts <-
    if shouldPossessiveArg argName
      then renderTyPartsPossessive cache fsm paramTyCons tyMods ty
      else renderTyParts cache fsm paramTyCons tyMods ty
  return (argStr, tyParts)

-- | Decide whether a typed argument should render its type with possessive case.
shouldPossessiveArg :: Identifier -- ^ Argument name.
                    -> Bool -- ^ True when possessive rendering is needed.
shouldPossessiveArg ident = not (isDemonstrative ident)

-- | Check for Turkish demonstrative pronouns used as bare argument names.
isDemonstrative :: Identifier -- ^ Identifier to inspect.
                -> Bool -- ^ True when the identifier is a demonstrative pronoun.
isDemonstrative (mods, name) =
  null mods && name `elem` [T.pack "bu", T.pack "şu", T.pack "o"]

-- | Render a type with a possessive suffix before its grammatical case.
renderTyPossessive :: RenderCache -- ^ Render cache.
                   -> FSM -- ^ Morphology FSM.
                   -> [Identifier] -- ^ Type parameters to render with P3s.
                   -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                   -> Ty Ann -- ^ Type to render.
                   -> IO String -- ^ Rendered type.
renderTyPossessive cache fsm paramTyCons tyMods ty =
  case ty of
    TyInd ann name ->
      renderIdentWithCases cache fsm (applyTyMods tyMods name) (possessiveCases (annCase ann))
    TyVar ann name ->
      renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
    TySkolem ann name ->
      renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
    TyApp ann (TyInd _ name) [argTy] -> do
      argStr <- renderTy cache fsm paramTyCons tyMods argTy
      nameStr <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return (argStr ++ " " ++ nameStr)
    TyApp ann ctor _ -> do
      ctorStr <- renderTy cache fsm paramTyCons tyMods ctor
      return (ctorStr ++ caseTag (annCase ann))
    TyInt ann ->
      renderIdentWithCases cache fsm ([T.pack "tam"], T.pack "sayı") (possessiveCases (annCase ann))
    TyFloat ann ->
      renderIdentWithCases cache fsm ([T.pack "ondalık"], T.pack "sayı") (possessiveCases (annCase ann))
    TyString ann ->
      renderIdentWithCases cache fsm ([], T.pack "dizge") (possessiveCases (annCase ann))
    Arr ann _ _ ->
      return ("işlev" ++ caseTag (annCase ann))

-- | Render type parts with possessive suffixes before grammatical case.
renderTyPartsPossessive :: RenderCache -- ^ Render cache.
                        -> FSM -- ^ Morphology FSM.
                        -> [Identifier] -- ^ Type parameters to render with P3s.
                        -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                        -> Ty Ann -- ^ Type to render.
                        -> IO [(String, Bool)] -- ^ Rendered parts and type-var flags.
renderTyPartsPossessive cache fsm paramTyCons tyMods ty =
  case ty of
    TyInd ann name -> do
      s <- renderIdentWithCases cache fsm (applyTyMods tyMods name) (possessiveCases (annCase ann))
      return [(s, False)]
    TyVar ann name -> do
      s <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return [(s, True)]
    TySkolem ann name -> do
      s <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return [(s, True)]
    TyApp ann (TyInd _ name) [argTy] -> do
      argParts <- renderTyParts cache fsm paramTyCons tyMods argTy
      nameStr <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return (argParts ++ [(" ", False), (nameStr, False)])
    TyApp ann ctor _ -> do
      ctorStr <- renderTy cache fsm paramTyCons tyMods ctor
      return [(ctorStr ++ caseTag (annCase ann), False)]
    TyInt ann -> do
      s <- renderIdentWithCases cache fsm ([T.pack "tam"], T.pack "sayı") (possessiveCases (annCase ann))
      return [(s, False)]
    TyFloat ann -> do
      s <- renderIdentWithCases cache fsm ([T.pack "ondalık"], T.pack "sayı") (possessiveCases (annCase ann))
      return [(s, False)]
    TyString ann -> do
      s <- renderIdentWithCases cache fsm ([], T.pack "dizge") (possessiveCases (annCase ann))
      return [(s, False)]
    Arr ann _ _ ->
      return [("işlev" ++ caseTag (annCase ann), False)]

-- | Build a possessive-then-case sequence.
possessiveCases :: Case -- ^ Target case.
                -> [Case] -- ^ P3s plus the target case when needed.
possessiveCases cas =
  case cas of
    Nom -> [P3s]
    _ -> [P3s, cas]

-- | Render a function signature into argument strings and name.
renderFunctionSignature :: RenderCache -- ^ Render cache.
                        -> FSM -- ^ Morphology FSM.
                        -> [Identifier] -- ^ Type parameters to render with P3s.
                        -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                        -> Identifier -- ^ Function name.
                        -> [Arg Ann] -- ^ Argument types.
                        -> IO ([String], String) -- ^ Rendered arguments and name.
renderFunctionSignature cache fsm paramTyCons tyMods name args = do
  let args' = normalizeSigArgs args
  argsStrs <- mapM (renderArg cache fsm paramTyCons tyMods) args'
  nameStr <- renderIdentWithCase cache fsm name P3s
  return (argsStrs, nameStr)

-- | Render a function signature into colored parts for diagnostics.
renderFunctionSignatureParts :: RenderCache -- ^ Render cache.
                             -> FSM -- ^ Morphology FSM.
                             -> [Identifier] -- ^ Type parameters to render with P3s.
                             -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                             -> Bool -- ^ Whether the function is a gerund.
                             -> Identifier -- ^ Function name.
                             -> [Arg Ann] -- ^ Argument types.
                             -> IO ([(String, [(String, Bool)])], String) -- ^ Rendered parts and name.
renderFunctionSignatureParts cache fsm paramTyCons tyMods isGerund name args = do
  let args' = normalizeSigArgs args
  argsParts <- mapM (renderArgParts cache fsm paramTyCons tyMods) args'
  nameStr <-
    if isGerund
      then renderGerundName cache fsm name
      else renderIdentWithCase cache fsm name (if null args then Nom else P3s)
  return (argsParts, nameStr)

-- | Normalize argument type cases for signature display.
-- Use genitive when a signature includes locative-like cases to keep output consistent.
normalizeSigArgs :: [Arg Ann] -- ^ Argument list to normalize.
                 -> [Arg Ann] -- ^ Normalized arguments.
normalizeSigArgs args =
  if any (needsGen . snd) args
    then map normalizeArg args
    else args
  where
    normalizeArg :: Arg Ann -- ^ Argument to normalize.
                 -> Arg Ann -- ^ Normalized argument.
    normalizeArg (n, ty) = (n, forceGen ty)
    needsGen :: Ty Ann -- ^ Type to inspect.
             -> Bool -- ^ True when the type needs genitive normalization.
    needsGen ty =
      case ty of
        TyInd ann _ -> annCase ann == Loc
        TyVar ann _ -> annCase ann == Loc
        TySkolem ann _ -> annCase ann == Loc
        TyApp ann _ _ -> annCase ann == Loc
        TyInt ann -> annCase ann == Loc
        TyFloat ann -> annCase ann == Loc
        TyString ann -> annCase ann == Loc
        Arr ann _ _ -> annCase ann == Loc
    forceGen :: Ty Ann -- ^ Type to rewrite.
             -> Ty Ann -- ^ Genitive-normalized type.
    forceGen ty =
      case ty of
        TyInd ann name -> TyInd (setAnnCase ann Gen) name
        TyVar ann name -> TyVar (setAnnCase ann Gen) name
        TySkolem ann name -> TySkolem (setAnnCase ann Gen) name
        TyInt ann -> TyInt (setAnnCase ann Gen)
        TyFloat ann -> TyFloat (setAnnCase ann Gen)
        TyString ann -> TyString (setAnnCase ann Gen)
        Arr ann d i -> Arr (setAnnCase ann Gen) (forceGen d) (forceGen i)
        TyApp ann ctor args -> TyApp (setAnnCase ann Gen) ctor args

-- | Render a function name in its gerund form.
renderGerundName :: RenderCache -- ^ Render cache.
                 -> FSM -- ^ Morphology FSM.
                 -> Identifier -- ^ Function identifier.
                 -> IO String -- ^ Rendered gerund name.
renderGerundName cache fsm (xs, x) = do
  let tagged = T.pack (T.unpack x ++ "<V><vn:inf><N>")
  forms <- map T.unpack <$> downsCached cache fsm tagged
  let base = T.unpack x
      root = case pickDownForm forms of
        Just f | '\'' `notElem` f && base `isPrefixOf` f -> f
        _ -> fallbackGerund base
  return (T.unpack (T.intercalate (T.pack "-") (xs ++ [T.pack root])))

-- | Fallback gerund formation without morphology.
fallbackGerund :: String -- ^ Base verb stem.
               -> String -- ^ Gerund form.
fallbackGerund base =
  base ++ if isFrontVowel (lastVowel base) then "mek" else "mak"

-- | Check if a vowel is front (e,i,o,u variants).
isFrontVowel :: Maybe Char -- ^ Last vowel.
             -> Bool -- ^ True when the vowel is front.
isFrontVowel mv =
  case mv of
    Just c -> c `elem` ("eiöü" :: String)
    Nothing -> False

-- | Render a signature from a name and arguments.
renderSig :: RenderCache -- ^ Render cache.
          -> FSM -- ^ Morphology FSM.
          -> [Identifier] -- ^ Type parameters to render with P3s.
          -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
          -> (Identifier, [Arg Ann]) -- ^ Function name and arguments.
          -> IO String -- ^ Rendered signature.
renderSig cache fsm paramTyCons tyMods (name, args) = do
  tyStrs <- mapM (renderTy cache fsm paramTyCons tyMods . snd) args
  nameStr <- renderIdentWithCase cache fsm name P3s
  let parts = map (\t -> "(" ++ t ++ ")") tyStrs
  return (unwords (parts ++ [nameStr]))

-- | Render a type into Text.
renderTyText :: RenderCache -- ^ Render cache.
             -> FSM -- ^ Morphology FSM.
             -> [Identifier] -- ^ Type parameters to render with P3s.
             -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
             -> Ty Ann -- ^ Type to render.
             -> IO Text -- ^ Rendered type.
renderTyText cache fsm paramTyCons tyMods ty =
  T.pack <$> renderTy cache fsm paramTyCons tyMods ty

-- | Render a type in nominative case into Text.
renderTyNomText :: RenderCache -- ^ Render cache.
                -> FSM -- ^ Morphology FSM.
                -> [Identifier] -- ^ Type parameters to render with P3s.
                -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
                -> Ty Ann -- ^ Type to render.
                -> IO Text -- ^ Rendered type.
renderTyNomText cache fsm paramTyCons tyMods ty =
  T.pack <$> renderTyNom cache fsm paramTyCons tyMods ty

-- | Render a signature into Text.
renderSigText :: RenderCache -- ^ Render cache.
              -> FSM -- ^ Morphology FSM.
              -> [Identifier] -- ^ Type parameters to render with P3s.
              -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
              -> (Identifier, [Arg Ann]) -- ^ Function name and arguments.
              -> IO Text -- ^ Rendered signature.
renderSigText cache fsm paramTyCons tyMods sig =
  T.pack <$> renderSig cache fsm paramTyCons tyMods sig

-- | Render an evaluated expression for user output.
renderExpValue :: RenderCache -- ^ Render cache.
               -> FSM -- ^ Morphology FSM.
               -> EvalState -- ^ Evaluator state.
               -> Exp Ann -- ^ Expression to render.
               -> IO String -- ^ Rendered output.
renderExpValue cache fsm evalSt = renderExpWithCase cache fsm evalSt Nom

-- | Render an expression with a requested grammatical case.
renderExpWithCase :: RenderCache -- ^ Render cache.
                  -> FSM -- ^ Morphology FSM.
                  -> EvalState -- ^ Evaluator state.
                  -> Case -- ^ Target case.
                  -> Exp Ann -- ^ Expression to render.
                  -> IO String -- ^ Rendered output.
renderExpWithCase cache fsm evalSt cas exp =
  case exp of
    IntLit {intVal} ->
      renderIntWithCase cache fsm cas intVal
    FloatLit {floatVal} ->
      renderFloatWithCase cache fsm cas floatVal
    Var {annExp, varName, varCandidates} ->
      renderVarWithCase cache fsm varName annExp varCandidates cas
    App {fn = Var {varCandidates}, args} ->
      case lookupCtorSig (evalCtors evalSt) varCandidates of
        Just (ctorName, (argTys, _))
          | length argTys == length args -> do
              argStrs <- sequence
                [ renderExpWithCase cache fsm evalSt (selectArgCase ty arg) arg
                | (ty, arg) <- zip argTys args
                ]
              let argStrs' = map wrapIfNeeded argStrs
                  fnCases
                    | null args = [cas]
                    | cas == Nom = [P3s]
                    | otherwise = [P3s, cas]
              fnStr <- renderIdentWithCases cache fsm ctorName fnCases
              return (unwords (argStrs' ++ [fnStr]))
        _ -> renderExpWithCaseFallback cache fsm evalSt cas exp
    _ -> renderExpWithCaseFallback cache fsm evalSt cas exp

-- | Fallback rendering when special forms do not match.
renderExpWithCaseFallback :: RenderCache -- ^ Render cache.
                          -> FSM -- ^ Morphology FSM.
                          -> EvalState -- ^ Evaluator state.
                          -> Case -- ^ Target case.
                          -> Exp Ann -- ^ Expression to render.
                          -> IO String -- ^ Rendered output.
renderExpWithCaseFallback cache fsm evalSt cas exp = do
  base <- renderExpNom cache fsm evalSt exp
  if cas == Nom
    then return base
    else applyCaseToLastWord cache fsm cas base

-- | Render an expression in nominative case.
renderExpNom :: RenderCache -- ^ Render cache.
             -> FSM -- ^ Morphology FSM.
             -> EvalState -- ^ Evaluator state.
             -> Exp Ann -- ^ Expression to render.
             -> IO String -- ^ Rendered output.
renderExpNom cache fsm evalSt exp =
  case exp of
    IntLit {intVal} ->
      renderIntWithCase cache fsm Nom intVal
    FloatLit {floatVal} ->
      renderFloatWithCase cache fsm Nom floatVal
    Var {annExp, varName, varCandidates} ->
      renderVarWithCase cache fsm varName annExp varCandidates Nom
    App {fn = Var {varCandidates}, args} ->
      case lookupCtorSig (evalCtors evalSt) varCandidates of
        Just (ctorName, (argTys, _))
          | length argTys == length args -> do
              argStrs <- sequence
                [ renderExpWithCase cache fsm evalSt (selectArgCase ty arg) arg
                | (ty, arg) <- zip argTys args
                ]
              let argStrs' = map wrapIfNeeded argStrs
              fnStr <- renderIdentWithCases cache fsm ctorName [P3s]
              return (unwords (argStrs' ++ [fnStr]))
        _ -> renderFallback cache fsm evalSt exp
    _ -> renderFallback cache fsm evalSt exp

-- | Render expressions without special-case handling.
renderFallback :: RenderCache -- ^ Render cache.
               -> FSM -- ^ Morphology FSM.
               -> EvalState -- ^ Evaluator state.
               -> Exp Ann -- ^ Expression to render.
               -> IO String -- ^ Rendered output.
renderFallback cache fsm evalSt exp =
  case exp of
    App {fn, args} -> do
      fnStr <- renderExpNom cache fsm evalSt fn
      argStrs <- mapM (renderExpNom cache fsm evalSt) args
      let argStrs' = map wrapIfNeeded argStrs
      return (unwords (argStrs' ++ [fnStr]))
    Var {annExp, varName, varCandidates} ->
      renderVarWithCase cache fsm varName annExp varCandidates Nom
    StrLit {lit} ->
      return ("\"" ++ T.unpack lit ++ "\"")
    IntLit {intVal} ->
      renderIntWithCase cache fsm Nom intVal
    FloatLit {floatVal} ->
      renderFloatWithCase cache fsm Nom floatVal
    Seq {} ->
      return (prettyExp exp)
    Bind {} ->
      return (prettyExp exp)
    Let {} ->
      return (prettyExp exp)
    Match {} ->
      return (prettyExp exp)

-- | Prefer explicit argument case when provided.
selectArgCase :: Ty Ann -- ^ Argument type.
              -> Exp Ann -- ^ Argument expression.
              -> Case -- ^ Selected case.
selectArgCase ty _ =
  annCase (annTy ty)

-- | Apply a case suffix to the last word in a phrase.
applyCaseToLastWord :: RenderCache -- ^ Render cache.
                    -> FSM -- ^ Morphology FSM.
                    -> Case -- ^ Target case.
                    -> String -- ^ Input phrase.
                    -> IO String -- ^ Updated phrase.
applyCaseToLastWord cache fsm cas s =
  case splitLastWord s of
    Nothing -> return s
    Just (prefix, word, suffix) -> do
      inflected <- renderIdentWithCases cache fsm ([], T.pack word) [cas]
      return (prefix ++ inflected ++ suffix)
  where
    -- | Split a string into prefix, last word, and trailing whitespace.
    splitLastWord :: String -- ^ Input phrase.
                  -> Maybe (String, String, String) -- ^ Prefix, last word, and suffix.
    splitLastWord input =
      let (revSuffix, revBody) = span isSpace (reverse input)
          (revWord, revPrefix) = break isSpace revBody
      in if null revWord
        then Nothing
        else Just (reverse revPrefix, reverse revWord, reverse revSuffix)

-- | Wrap a string in parentheses when it contains whitespace.
wrapIfNeeded :: String -- ^ Input string.
             -> String -- ^ Wrapped string when needed.
wrapIfNeeded s =
  if any isSpace s then "(" ++ s ++ ")" else s

-- | Render an integer, applying case when requested.
renderIntWithCase :: RenderCache -- ^ Render cache.
                  -> FSM -- ^ Morphology FSM.
                  -> Case -- ^ Target case.
                  -> Integer -- ^ Integer value.
                  -> IO String -- ^ Rendered integer.
renderIntWithCase cache fsm cas n = do
  let base = show (abs n)
      prefix = if n < 0 then "-" else ""
  if cas == Nom
    then return (prefix ++ base)
    else do
      inflected <- renderIdentWithCases cache fsm ([], T.pack base) [cas]
      return (prefix ++ inflected)

-- | Render a floating-point number, applying case when requested.
renderFloatWithCase :: RenderCache -- ^ Render cache.
                    -> FSM -- ^ Morphology FSM.
                    -> Case -- ^ Target case.
                    -> Double -- ^ Floating-point value.
                    -> IO String -- ^ Rendered floating-point number.
renderFloatWithCase cache fsm cas n = do
  let base = show (abs n)
      prefix = if n < 0 then "-" else ""
  if cas == Nom
    then return (prefix ++ base)
    else do
      inflected <- renderIdentWithCases cache fsm ([], T.pack base) [cas]
      return (prefix ++ inflected)

-- | Render a variable with the requested case, using candidates if present.
renderVarWithCase :: RenderCache -- ^ Render cache.
                  -> FSM -- ^ Morphology FSM.
                  -> Identifier -- ^ Identifier to render.
                  -> Ann -- ^ Original annotation.
                  -> [(Identifier, Case)] -- ^ Candidate identifiers.
                  -> Case -- ^ Target case.
                  -> IO String -- ^ Rendered identifier.
renderVarWithCase cache fsm name annExp candidates targetCase =
  case find (\(_, cas) -> cas == targetCase) candidates of
    Just (ident, _) -> renderIdentWithCases cache fsm ident [targetCase]
    Nothing ->
      if annCase annExp == targetCase
        then return (prettyIdent name)
        else case candidates of
          (ident, _):_ -> renderIdentWithCases cache fsm ident [targetCase]
          [] -> renderIdentWithCases cache fsm name [targetCase]

-- | Look up a constructor signature by candidate identifiers.
lookupCtorSig :: [(Identifier, ([Ty Ann], Ty Ann))] -- ^ Constructor signatures.
              -> [(Identifier, Case)] -- ^ Candidate identifiers.
              -> Maybe (Identifier, ([Ty Ann], Ty Ann)) -- ^ Matching constructor signature.
lookupCtorSig ctors candidates =
  let names = map fst candidates
  in case go names of
       Just hit -> Just hit
       Nothing -> goFuzzy names
  where
    -- | Find the first candidate that appears in the constructor table.
    go :: [Identifier] -- ^ Remaining candidate names.
       -> Maybe (Identifier, ([Ty Ann], Ty Ann)) -- ^ Matching constructor signature.
    go [] = Nothing
    go (n:ns) =
      case lookup n ctors of
        Just sig -> Just (n, sig)
        Nothing -> go ns
    -- | Fuzzy match constructor identifiers with possessive normalization.
    goFuzzy :: [Identifier] -- ^ Candidate names to match.
            -> Maybe (Identifier, ([Ty Ann], Ty Ann)) -- ^ Matching constructor signature.
    goFuzzy [] = Nothing
    goFuzzy (n:ns) =
      case find (\(ctorName, _) -> identMatchesPoss ctorName n) ctors of
        Just (ctorName, sig) -> Just (ctorName, sig)
        Nothing -> goFuzzy ns
    -- | Compare identifiers with possessive/root normalization.
    identMatchesPoss :: Identifier -- ^ Constructor identifier.
                     -> Identifier -- ^ Candidate identifier.
                     -> Bool -- ^ True when identifiers match loosely.
    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2)
      && not (null (roots x1 `intersect` roots x2))
    -- | Build candidate roots for possessive normalization.
    roots :: Text -- ^ Identifier root.
          -> [Text] -- ^ Candidate roots.
    roots txt =
      nub (catMaybes [Just txt, dropTrailingVowel txt >>= dropTrailingSoftG])
    -- | Drop a trailing Turkish vowel.
    dropTrailingVowel :: Text -- ^ Input text.
                      -> Maybe Text -- ^ Text without trailing vowel.
    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing
    -- | Replace trailing soft g with k.
    dropTrailingSoftG :: Text -- ^ Input text.
                      -> Maybe Text -- ^ Normalized text.
    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing
