{-# LANGUAGE NamedFieldPuns #-}
-- | Rendering utilities for identifiers, types, and values.
module Kip.Render
  ( prettyIdent
  , applyTyMods
  , RenderCache
  , newRenderCache
  , renderIdentWithCases
  , renderIdentWithCase
  , renderTy
  , renderTyNom
  , renderTyParts
  , renderTyPartsPossessive
  , renderTyPossessive
  , renderArg
  , renderArgParts
  , renderFunctionSignature
  , renderFunctionSignatureParts
  , renderInfinitiveName
  , renderSig
  , renderTyText
  , renderTyNomText
  , renderSigText
  , renderExpValue
  , renderExpWithCase
  , renderExpNom
  , renderExpPreservingCase
  , upsCached
  , upsCachedBatch
  , downsCached
  , pickDownForm
  ) where

import Data.Char (isLetter, isLower, isDigit, isSpace, isAlphaNum)
import Data.List (intercalate, maximumBy, minimumBy, find, isInfixOf, isSuffixOf, isPrefixOf, stripPrefix, intersect, nub)
import qualified Data.Bifunctor as B
import Data.Maybe (fromMaybe, catMaybes, listToMaybe, maybeToList)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

import Language.Foma
import Kip.AST
import Kip.Eval (EvalState, evalCtors)
import qualified Kip.MorphCache as MC

-- | The morphology caches used while rendering.
type RenderCache = MC.MorphCaches

-- | Create a new empty render cache.
newRenderCache :: IO RenderCache -- ^ Fresh render cache.
newRenderCache = MC.newMorphCaches

-- | Cached version of 'ups'.
upsCached :: RenderCache -- ^ Render cache.
          -> FSM -- ^ Morphology FSM.
          -> Text -- ^ Surface form.
          -> IO [Text] -- ^ Morphology analyses.
upsCached cache = MC.upsCached (MC.morphUpsCache cache)
{-# INLINE upsCached #-}

-- | Cached batch morphology analysis lookup.
-- Uses batch FFI when multiple words are missing to avoid repeated handle setup.
upsCachedBatch :: RenderCache -- ^ Render cache.
               -> FSM -- ^ Morphology FSM.
               -> [Text] -- ^ Surface forms.
               -> IO [[Text]] -- ^ Morphology analyses per surface form.
upsCachedBatch cache = MC.upsCachedBatch (MC.morphUpsCache cache)
{-# INLINE upsCachedBatch #-}

-- | Cached version of 'downs'.
downsCached :: RenderCache -- ^ Render cache.
            -> FSM -- ^ Morphology FSM.
            -> Text -- ^ Analysis string.
            -> IO [Text] -- ^ Surface forms.
downsCached cache = MC.downsCached (MC.morphDownsCache cache)
{-# INLINE downsCached #-}

-- | Cached batch morphology generation lookup.
-- Uses batch FFI when multiple stems are missing to avoid repeated handle setup.
downsCachedBatch :: RenderCache -- ^ Render cache.
                -> FSM -- ^ Morphology FSM.
                -> [Text] -- ^ Morphology stems.
                -> IO [[Text]] -- ^ Generated surface forms per stem.
downsCachedBatch cache = MC.downsCachedBatch (MC.morphDownsCache cache)
{-# INLINE downsCachedBatch #-}

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

-- | Find the last vowel in a word.
lastVowel :: String -- ^ Input word.
          -> Maybe Char -- ^ Last vowel when present.
lastVowel =
  foldl (\acc c -> if isVowel c then Just c else acc) Nothing

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

-- | Add possessive suffix to a stem (minimal fallback for P3s case).
addP3sSuffix :: String -- ^ Input stem.
             -> String -- ^ Stem with P3s suffix.
addP3sSuffix stem =
  case lastVowel stem of
    Nothing -> stem ++ "ı"  -- No vowel, default to ı
    Just v ->
      let suffixVowel = p3sVowel v
      in if endsWithVowel stem
           then stem ++ ['s', suffixVowel]  -- After vowel: add 's' + vowel
           else stem ++ [suffixVowel]       -- After consonant: just add vowel

-- | Add conditional suffix to a stem (minimal fallback for Cond case).
addCondSuffix :: String -- ^ Input stem.
              -> String -- ^ Stem with conditional suffix.
addCondSuffix stem =
  case lastVowel stem of
    Nothing -> stem ++ "sa"  -- No vowel, default to sa
    Just v ->
      let suffixVowel = if v `elem` ("aıou" :: String) then 'a' else 'e'
      in if endsWithVowel stem
           then stem ++ ['y', 's', suffixVowel]  -- After vowel: add 'y' + 's' + vowel
           else stem ++ ['s', suffixVowel]        -- After consonant: 's' + vowel

-- | Fallback inflection for opaque stems when TRmorph cannot generate forms.
--
-- Opaque stems are tokens where apostrophe usage is expected in surface text
-- (single letters, numerals, and similar non-word tokens). For regular words,
-- we keep the bare stem when morphology generation fails.
fallbackOpaqueCase :: String -- ^ Input stem.
                   -> Case -- ^ Case to apply.
                   -> Maybe String -- ^ Inflected fallback when applicable.
fallbackOpaqueCase stem cas
  | not (needsApostrophe stem) = Nothing
  | otherwise =
      case cas of
        Gen -> Just (stem ++ "'" ++ genSuffix stem)
        Acc -> Just (stem ++ "'" ++ accSuffix stem)
        Dat -> Just (stem ++ "'" ++ datSuffix stem)
        Loc -> Just (stem ++ "'" ++ locSuffix stem)
        Abl -> Just (stem ++ "'" ++ ablSuffix stem)
        Ins -> Just (stem ++ "'" ++ insSuffix stem)
        _ -> Nothing
  where
    needsApostrophe s = length s == 1 || any isDigit s || not (all isLetter s)
    backVowel v = v `elem` ("aıou" :: String)
    harmony4 s =
      case lastVowel s of
        Just v | v `elem` ("aı" :: String) -> "ı"
        Just v | v `elem` ("ou" :: String) -> "u"
        Just v | v `elem` ("ei" :: String) -> "i"
        Just v | v `elem` ("öü" :: String) -> "ü"
        _ -> "i"
    harmony2 s =
      case lastVowel s of
        Just v -> if backVowel v then "a" else "e"
        _ -> "e"
    genSuffix s = (if endsWithVowel s then "n" else "") ++ harmony4 s ++ "n"
    accSuffix s = (if endsWithVowel s then "y" else "") ++ harmony4 s
    datSuffix s = (if endsWithVowel s then "y" else "") ++ harmony2 s
    locSuffix s = "d" ++ harmony2 s
    ablSuffix s = "d" ++ harmony2 s ++ "n"
    insSuffix s = (if endsWithVowel s then "y" else "") ++ "l" ++ harmony2 s

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
    then do
      derived <- deriveInflectedForms cache fsm stem (concatMap caseTag (filter (/= Nom) cases))
      adjAdvForms <-
        if cases == [Dat]
          then map T.unpack <$> downsCached cache fsm (T.pack (stem ++ "<Adj><adv>"))
          else return []
      return (derived ++ adjAdvForms)
    else return forms
  -- Use FSM-derived forms, or minimal fallback if FSM fails.
  -- P3s (possessive) and Cond (conditional) fallbacks are kept since they are
  -- regular and needed for types/patterns; TRmorph does not cover Cond.
  let minimalFallback = case cases of
        [cas] | Just inflected <- fallbackOpaqueCase stem cas -> inflected
        [P3s] -> addP3sSuffix stem
        [Cond] -> addCondSuffix stem
        _ -> stem
      root = fromMaybe minimalFallback (pickDownFormWithStem (Just stem) forms)
      root' = fromMaybe root (pickDownFormWithStem (Just stem) forms')
  datAdjRoot <-
    if cases == [Dat] && root' == stem
      then do
        adjForms <- map T.unpack <$> downsCached cache fsm (T.pack (stem ++ "<Adj><0><N><dat>"))
        advForms <- map T.unpack <$> downsCached cache fsm (T.pack (stem ++ "<Adv><0><N><dat>"))
        let datForms = adjForms ++ advForms
        case pickDownFormWithStem (Just stem) datForms of
          Just hit -> return (Just hit)
          Nothing -> inferDatSurfaceViaUps cache fsm stem
      else return Nothing
  let finalRoot = fromMaybe root' datAdjRoot
  return (T.unpack (T.intercalate (T.pack "-") (xs ++ [T.pack finalRoot])))

-- | Infer a dative surface from TRmorph analyses when downs generation is empty.
inferDatSurfaceViaUps :: RenderCache -> FSM -> String -> IO (Maybe String)
inferDatSurfaceViaUps cache fsm stem = do
  let probes = map (stem ++) ["a", "e", "ya", "ye"]
  analysesByProbe <- upsCachedBatch cache fsm (map T.pack probes)
  let hasDatRoot =
        any (\an -> T.pack "<dat>" `T.isInfixOf` an && T.takeWhile (/= '<') an == T.pack stem)
      hasAnyDat = any (T.pack "<dat>" `T.isInfixOf`)
      hasAny analyses = not (null analyses)
      exact = [p | (p, analyses) <- zip probes analysesByProbe, hasDatRoot analyses]
      loose = [p | (p, analyses) <- zip probes analysesByProbe, hasAnyDat analyses]
      anyHit = [p | (p, analyses) <- zip probes analysesByProbe, hasAny analyses]
  return $
    case exact of
      p:_ -> Just p
      [] -> case loose of
        p:_ -> Just p
        [] -> case anyHit of
          p:_ -> Just p
          [] -> Nothing

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
  analyses <- upsCached cache fsm (T.pack root)
  let nounTag = T.pack "<N>"
      verbTag = T.pack "<V>"
      nounAnalyses =
        filter (\a -> nounTag `T.isInfixOf` a && not (verbTag `T.isInfixOf` a)) analyses
      baseAnalyses =
        if null nounAnalyses then analyses else nounAnalyses
      stemAnalyses = map stripCaseTags baseAnalyses
      taggedStems = map (<> T.pack tags) stemAnalyses
  -- Batch morphology generation to amortize Foma handle setup.
  forms <- downsCachedBatch cache fsm taggedStems
  return (concatMap (map T.unpack) forms)

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
    TyApp ann (TyInd _ name) args -> do
      argStrs <- mapM (renderTy cache fsm paramTyCons tyMods) args
      let nonEmptyArgStrs = filter (not . null) argStrs
      let nameCases =
            if name `elem` paramTyCons
              then if annCase ann == Nom then [P3s] else [P3s, annCase ann]
              else [annCase ann]
      nameStr <- renderIdentWithCases cache fsm name nameCases
      return (unwords (nonEmptyArgStrs ++ [nameStr]))
    TyApp ann ctor _ -> do
      ctorStr <- renderTy cache fsm paramTyCons tyMods ctor
      return (ctorStr ++ caseTag (annCase ann))
    TyInt ann ->
      renderIdentWithCase cache fsm ([T.pack "tam"], T.pack "sayı") (annCase ann)
    TyFloat ann ->
      renderIdentWithCase cache fsm ([T.pack "ondalık"], T.pack "sayı") (annCase ann)
    TyString ann ->
      renderIdentWithCase cache fsm ([], T.pack "dizge") (annCase ann)
    TyChar ann ->
      renderIdentWithCase cache fsm ([], T.pack "karakter") (annCase ann)
    Arr ann d i -> do
      let d' = setTyCases Gen d
          i' = setTyCases Nom i
      dStr <- renderTy cache fsm paramTyCons tyMods d'
      iBase <- renderTyPossessive cache fsm paramTyCons tyMods i'
      iStr <-
        if annCase ann == Nom
          then return iBase
          else applyCaseToLastWord cache fsm (annCase ann) iBase
      return (dStr ++ " " ++ normalizePossIns iStr)

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
    TyApp _ (TyInd _ name) args -> do
      argStrs <- mapM (renderTyNom cache fsm paramTyCons tyMods) args
      let nonEmptyArgStrs = filter (not . null) argStrs
      let nameCases = if name `elem` paramTyCons then [P3s] else [Nom]
      nameStr <- renderIdentWithCases cache fsm name nameCases
      return (unwords (nonEmptyArgStrs ++ [nameStr]))
    TyApp _ ctor _ ->
      renderTyNom cache fsm paramTyCons tyMods ctor
    TyInt _ ->
      renderIdentWithCase cache fsm ([T.pack "tam"], T.pack "sayı") Nom
    TyFloat _ ->
      renderIdentWithCase cache fsm ([T.pack "ondalık"], T.pack "sayı") Nom
    TyString _ ->
      renderIdentWithCase cache fsm ([], T.pack "dizge") Nom
    TyChar _ ->
      renderIdentWithCase cache fsm ([], T.pack "karakter") Nom
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
      return [(s, isLikelyTypeVar name)]
    TyVar ann name -> do
      s <- renderIdentWithCase cache fsm name (annCase ann)
      return [(s, True)]
    TySkolem ann name -> do
      s <- renderIdentWithCase cache fsm name (annCase ann)
      return [(s, True)]
    TyApp ann (TyInd _ name) args -> do
      argPartsList <- mapM (renderTyParts cache fsm paramTyCons tyMods) args
      let nonEmptyArgParts = filter (not . null) argPartsList
      let nameCases =
            if name `elem` paramTyCons
              then if annCase ann == Nom then [P3s] else [P3s, annCase ann]
              else [annCase ann]
      nameStr <- renderIdentWithCases cache fsm name nameCases
      if null nonEmptyArgParts
        then return [(nameStr, False)]
        else do
          let argParts = intercalate [(" ", False)] nonEmptyArgParts
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
    TyChar ann -> do
      s <- renderIdentWithCase cache fsm ([], T.pack "karakter") (annCase ann)
      return [(s, False)]
    Arr ann d i -> do
      let d' = setTyCases Gen d
          i' = setTyCases Nom i
      domParts <- renderTyParts cache fsm paramTyCons tyMods d'
      imgBaseParts <- renderTyPartsPossessive cache fsm paramTyCons tyMods i'
      imgParts <-
        if annCase ann == Nom
          then return imgBaseParts
          else inflectLastPartCase cache fsm (annCase ann) imgBaseParts
      let normalizedImgParts = map (B.first normalizePossIns) imgParts
      return (domParts ++ [(" ", False)] ++ normalizedImgParts)

-- | Render a typed argument as a single string.
renderArg :: RenderCache -- ^ Render cache.
          -> FSM -- ^ Morphology FSM.
          -> [Identifier] -- ^ Type parameters to render with P3s.
          -> [(Identifier, [Identifier])] -- ^ Type modifier expansions.
          -> Arg Ann -- ^ Argument to render.
          -> IO String -- ^ Rendered argument.
renderArg cache fsm paramTyCons tyMods ((argName, _), ty) = do
  case ty of
    Arr _ domTy imgTy | annCase (annTy domTy) /= Gen -> do
      domStr <- renderTy cache fsm paramTyCons tyMods domTy
      fnInf <- renderInfinitiveName cache fsm argName
      imgStr <- renderTyPossessive cache fsm paramTyCons tyMods imgTy
      let domStr' = T.unpack (T.strip (T.pack domStr))
      return ("(" ++ domStr' ++ " (" ++ fnInf ++ " " ++ normalizePossIns imgStr ++ "))")
    _ -> do
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
renderArgParts cache fsm paramTyCons tyMods ((argName, _), ty) = do
  case ty of
    Arr _ domTy imgTy | annCase (annTy domTy) /= Gen -> do
      domParts <- renderTyParts cache fsm paramTyCons tyMods domTy
      fnInf <- renderInfinitiveName cache fsm argName
      imgParts <- renderTyPartsPossessive cache fsm paramTyCons tyMods imgTy
      let normalizedImgParts = map (B.first normalizePossIns) imgParts
          tyParts = domParts ++ [(" (", False), (fnInf, False), (" ", False)] ++ normalizedImgParts ++ [(")", False)]
      return ("", tyParts)
    _ -> do
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
    TyApp ann (TyInd _ name) args -> do
      argStrs <- mapM (renderTy cache fsm paramTyCons tyMods) args
      let nonEmptyArgStrs = filter (not . null) argStrs
      nameStr <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return (unwords (nonEmptyArgStrs ++ [nameStr]))
    TyApp ann ctor _ -> do
      ctorStr <- renderTy cache fsm paramTyCons tyMods ctor
      return (ctorStr ++ caseTag (annCase ann))
    TyInt ann ->
      renderIdentWithCases cache fsm ([T.pack "tam"], T.pack "sayı") (possessiveCases (annCase ann))
    TyFloat ann ->
      renderIdentWithCases cache fsm ([T.pack "ondalık"], T.pack "sayı") (possessiveCases (annCase ann))
    TyString ann ->
      renderIdentWithCases cache fsm ([], T.pack "dizge") (possessiveCases (annCase ann))
    TyChar ann ->
      renderIdentWithCases cache fsm ([], T.pack "karakter") (possessiveCases (annCase ann))
    Arr ann d i -> do
      let d' = setTyCases Gen d
          i' = setTyCases Nom i
      dStr <- renderTy cache fsm paramTyCons tyMods d'
      iBase <- renderTyPossessive cache fsm paramTyCons tyMods i'
      iStr <-
        if annCase ann == Nom
          then return iBase
          else applyCaseToLastWord cache fsm (annCase ann) iBase
      return (dStr ++ " " ++ normalizePossIns iStr)

-- | Normalize possessive+instrumental spellings like "b'si'yle" to "b'siyle".
normalizePossIns :: String -> String
normalizePossIns s =
  let t = normalizeQuoteChars (T.pack s)
      t1 = T.replace (T.pack "'si'yle") (T.pack "'siyle") t
      t2 = T.replace (T.pack "'sı'yla") (T.pack "'sıyla") t1
      t3 = T.replace (T.pack "'su'yla") (T.pack "'suyla") t2
      t4 = T.replace (T.pack "'sü'yle") (T.pack "'süyle") t3
  in T.unpack t4

-- | Normalize quote-like apostrophes to plain ASCII apostrophe.
normalizeQuoteChars :: Text -> Text
normalizeQuoteChars =
  T.map (\c -> if c `elem` ("'´`ʼ" :: String) then '\'' else c)

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
      return [(s, isLikelyTypeVar name)]
    TyVar ann name -> do
      s <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return [(s, True)]
    TySkolem ann name -> do
      s <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      return [(s, True)]
    TyApp ann (TyInd _ name) args -> do
      argPartsList <- mapM (renderTyParts cache fsm paramTyCons tyMods) args
      let nonEmptyArgParts = filter (not . null) argPartsList
      nameStr <- renderIdentWithCases cache fsm name (possessiveCases (annCase ann))
      if null nonEmptyArgParts
        then return [(nameStr, False)]
        else do
          let argParts = intercalate [(" ", False)] nonEmptyArgParts
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
    TyChar ann -> do
      s <- renderIdentWithCases cache fsm ([], T.pack "karakter") (possessiveCases (annCase ann))
      return [(s, False)]
    Arr ann d i -> do
      let d' = setTyCases Gen d
          i' = setTyCases Nom i
      domParts <- renderTyParts cache fsm paramTyCons tyMods d'
      imgBaseParts <- renderTyPartsPossessive cache fsm paramTyCons tyMods i'
      imgParts <-
        if annCase ann == Nom
          then return imgBaseParts
          else inflectLastPartCase cache fsm (annCase ann) imgBaseParts
      let normalizedImgParts = map (B.first normalizePossIns) imgParts
      return (domParts ++ [(" ", False)] ++ normalizedImgParts)

-- | Apply a grammatical case to the last rendered type part.
inflectLastPartCase :: RenderCache
                    -> FSM
                    -> Case
                    -> [(String, Bool)]
                    -> IO [(String, Bool)]
inflectLastPartCase cache fsm cas parts =
  case reverse parts of
    [] -> return []
    (lastTxt, isVar):restRev -> do
      inflected <- applyCaseToLastWord cache fsm cas lastTxt
      return (reverse ((inflected, isVar) : restRev))

-- | Heuristic: treat single-letter identifiers as type variables for coloring.
isLikelyTypeVar :: Identifier -> Bool
isLikelyTypeVar (mods, name) =
  null mods
    && T.length name == 1
    && T.all (\c -> isLetter c && isLower c) name

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
                             -> Bool -- ^ Whether the function is an infinitive.
                             -> Identifier -- ^ Function name.
                             -> [Arg Ann] -- ^ Argument types.
                             -> IO ([(String, [(String, Bool)])], String) -- ^ Rendered parts and name.
renderFunctionSignatureParts cache fsm paramTyCons tyMods isInfinitive name args = do
  let args' = normalizeSigArgs args
  argsParts <- mapM (renderArgParts cache fsm paramTyCons tyMods) args'
  nameStr <-
    if isInfinitive
      then renderInfinitiveName cache fsm name
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
        TyChar ann -> annCase ann == Loc
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
        TyChar ann -> TyChar (setAnnCase ann Gen)
        Arr ann d i -> Arr (setAnnCase ann Gen) (forceGen d) (forceGen i)
        TyApp ann ctor args -> TyApp (setAnnCase ann Gen) ctor args

-- | Render a function name in its infinitive form.
renderInfinitiveName :: RenderCache -- ^ Render cache.
                 -> FSM -- ^ Morphology FSM.
                 -> Identifier -- ^ Function identifier.
                 -> IO String -- ^ Rendered infinitive name.
renderInfinitiveName cache fsm (xs, x) = do
  let tagged = T.pack (T.unpack x ++ "<V><vn:inf><N>")
  forms <- map T.unpack <$> downsCached cache fsm tagged
  let base = T.unpack x
      preferred = filter isInfinitiveForm forms
      pickFrom candidates =
        case pickDownForm candidates of
          Just f | '\'' `notElem` f && base `isPrefixOf` f -> Just f
          _ -> Nothing
      root = fromMaybe (fallbackInfinitive base) (pickFrom preferred)
  return (T.unpack (T.intercalate (T.pack "-") (xs ++ [T.pack root])))
  where
    isInfinitiveForm :: String -> Bool
    isInfinitiveForm f = "mek" `isSuffixOf` f || "mak" `isSuffixOf` f

-- | Fallback infinitive formation without morphology.
fallbackInfinitive :: String -- ^ Base verb stem.
               -> String -- ^ Infinitive form.
fallbackInfinitive base =
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

-- | Render an expression preserving the case annotations on each sub-expression.
-- Used for trace output where intermediate expressions carry their original cases.
renderExpPreservingCase :: RenderCache -- ^ Render cache.
                        -> FSM -- ^ Morphology FSM.
                        -> EvalState -- ^ Evaluator state.
                        -> Exp Ann -- ^ Expression to render.
                        -> IO String -- ^ Rendered output.
renderExpPreservingCase cache fsm evalSt expr =
  case expr of
    IntLit {annExp, intVal} ->
      renderIntWithCase cache fsm (annCase annExp) intVal
    FloatLit {annExp, floatVal} ->
      renderFloatWithCase cache fsm (annCase annExp) floatVal
    StrLit {annExp, lit} ->
      renderStrLitWithCase cache fsm (annCase annExp) lit
    CharLit {annExp, charVal} ->
      renderCharLitWithCase cache fsm (annCase annExp) charVal
    Var {annExp, varName, varCandidates} ->
      renderVarWithCase cache fsm varName annExp varCandidates (annCase annExp)
    App {annExp, fn, args} ->
      case fn of
        Var {varCandidates} ->
          case lookupCtorSig (M.toList (evalCtors evalSt)) varCandidates of
            Just (ctorName, (argTys, _))
              | length argTys == length args -> do
                argStrs <- sequence
                  [ renderExpWithCase cache fsm evalSt (selectCtorArgCase ctorName idx ty arg) arg
                  | (idx, (ty, arg)) <- zip [0 :: Int ..] (zip argTys args)
                  ]
                let argStrs' = map wrapIfNeeded argStrs
                    topCase = annCase annExp
                    fnCases
                      | null args = [topCase]
                      | topCase == Nom = [P3s]
                      | otherwise = [P3s, topCase]
                fnStr <- renderIdentWithCases cache fsm ctorName fnCases
                return (unwords (argStrs' ++ [fnStr]))
            _ -> renderAppPC cache fsm evalSt annExp fn args
        _ -> renderAppPC cache fsm evalSt annExp fn args
    Match {annExp, scrutinee, clauses} -> do
      -- If scrutinee is a value (IntLit, FloatLit, StrLit, or Var that's a constructor),
      -- only render the selected clause body
      case selectMatchingClause scrutinee clauses of
        Just clauseBody -> renderExpPreservingCase cache fsm evalSt clauseBody
        Nothing -> do
          -- Scrutinee not yet evaluated, show full match
          scrutStrRaw <- renderExpPreservingCase cache fsm evalSt scrutinee
          let shouldParenthesizeScrutinee exp' =
                case exp' of
                  Var {} -> False
                  App {fn = Var {varCandidates}, args} ->
                    case lookupCtorSig (M.toList (evalCtors evalSt)) varCandidates of
                      Just (_, (argTys, _)) -> length argTys == length args
                      Nothing -> True
                  _ -> True
              scrutStr = if shouldParenthesizeScrutinee scrutinee
                           then "(" ++ scrutStrRaw ++ ")"
                           else scrutStrRaw
          clauseStrs <- mapM (renderClausePC cache fsm evalSt (annCase annExp) scrutStr) clauses
          return (intercalate "; " clauseStrs)
    Seq {first, second} -> do
      firstStr <- renderSeqFirstExp cache fsm evalSt first
      secondStr <- renderExpPreservingCase cache fsm evalSt second
      return (firstStr ++ ", " ++ secondStr)
    Bind {bindName, bindExp} -> do
      expStr <- renderSeqFirstExp cache fsm evalSt bindExp
      return (prettyIdent bindName ++ " için " ++ expStr)
    Let {body} ->
      renderExpPreservingCase cache fsm evalSt body
    Ascribe {ascExp} ->
      renderExpPreservingCase cache fsm evalSt ascExp

-- | Render the first expression of a sequence in converb style when possible.
renderSeqFirstExp :: RenderCache -> FSM -> EvalState -> Exp Ann -> IO String
renderSeqFirstExp cache fsm evalSt exp' =
  case exp' of
    Bind {bindName, bindExp} -> do
      expStr <- renderSeqFirstExp cache fsm evalSt bindExp
      return (prettyIdent bindName ++ " için " ++ expStr)
    App {fn = Var {varName, varCandidates}, args} ->
      case lookupCtorSig (M.toList (evalCtors evalSt)) varCandidates of
        Just (_, (argTys, _))
          | length argTys == length args -> do
              argStrs <- sequence
                [ renderExpWithCase cache fsm evalSt (selectCtorArgCase varName idx ty arg) arg
                | (idx, (ty, arg)) <- zip [0 :: Int ..] (zip argTys args)
                ]
              renderSeqConverbApp argStrs
        _ -> do
          fnLemma <- pickLemmaIdentifier cache fsm varName varCandidates
          let isWritePrimName (mods, name) = null mods && name == T.pack "yaz"
              hasWritePrim = isWritePrimName fnLemma
          fnVerb <- hasVerbAnalysis cache fsm fnLemma
          surfaceVerb <- hasVerbAnalysis cache fsm varName
          let forceVerbArgs = (hasWritePrim || fnVerb || surfaceVerb) && not (null args)
              renderAccArg arg =
                case arg of
                  App {} -> renderExpPreservingCase cache fsm evalSt arg >>= applyCaseToLastWord cache fsm Acc
                  StrLit {} -> do
                    s <- renderExpPreservingCase cache fsm evalSt arg
                    if hasAccSuffix s then return s else applyCaseToLastWord cache fsm Acc s
                  _ -> renderExpWithCase cache fsm evalSt Acc arg
          argStrs <-
            if forceVerbArgs
              then mapM renderAccArg args
              else mapM (renderExpPreservingCase cache fsm evalSt) args
          renderSeqConverbApp argStrs
      where
        renderSeqConverbApp argStrs = do
          fnStr <- renderVerbAsConverb cache fsm varName varCandidates
          return (unwords (map wrapIfNeeded argStrs ++ [fnStr]))
    Var {varName, varCandidates} ->
      renderVerbAsConverb cache fsm varName varCandidates
    _ -> renderExpPreservingCase cache fsm evalSt exp'

-- | Render a function application preserving case annotations.
renderAppPC :: RenderCache -> FSM -> EvalState -> Ann -> Exp Ann -> [Exp Ann] -> IO String
renderAppPC cache fsm evalSt appAnn fn' args' = do
  let topCase = annCase appAnn
  (argStrs, fnStr) <- case fn' of
    Var {annExp = fnAnn, varName, varCandidates} -> do
      preserveSurface <- shouldPreserveSurfaceForm cache fsm varName
      fnName <- pickLemmaIdentifier cache fsm varName varCandidates
      surfaceImperative <- hasImperativeAnalysis cache fsm varName
      lemmaImperative <- hasImperativeAnalysis cache fsm fnName
      let isWritePrimName (mods, name) = null mods && name == T.pack "yaz"
          hasWritePrim = isWritePrimName fnName
          forceVerbArgs = (hasWritePrim || surfaceImperative || lemmaImperative) && not (null args')
          isEkiLike =
            let s = T.unpack (snd fnName)
            in s == "ek" || s == "eki" || "ek" `isInfixOf` s
          renderAccArg arg =
            case arg of
              App {} -> renderExpPreservingCase cache fsm evalSt arg >>= applyCaseToLastWord cache fsm Acc
              StrLit {} -> do
                s <- renderExpPreservingCase cache fsm evalSt arg
                if hasAccSuffix s then return s else applyCaseToLastWord cache fsm Acc s
              _ -> renderExpWithCase cache fsm evalSt Acc arg
          fnCases
            | preserveSurface = [Nom]
            | hasWritePrim && not (null args') = [Nom]
            | null args' = [topCase]
            | topCase == Nom || topCase == P3s = [P3s]
            | otherwise = [P3s, topCase]
      argStrs <-
        if forceVerbArgs
          then mapM renderAccArg args'
          else if isEkiLike && length args' == 2
            then do
              let [a1, a2] = args'
              s1 <- renderExpWithCase cache fsm evalSt Gen a1
              s2 <- renderExpWithCase cache fsm evalSt Dat a2
              return [s1, s2]
          else mapM (renderExpPreservingCase cache fsm evalSt) args'
      if surfaceImperative || lemmaImperative
        then do
          renderedVerb <- renderVerbAsImperative cache fsm varName fnName varCandidates
          return (argStrs, renderedVerb)
        else if preserveSurface
        then return (argStrs, prettyIdent varName)
        else do
          fnStr <- renderIdentWithCases cache fsm fnName fnCases
          return (argStrs, fnStr)
    _ -> do
      argStrs <- mapM (renderExpPreservingCase cache fsm evalSt) args'
      fnStr <- renderExpPreservingCase cache fsm evalSt fn'
      return (argStrs, fnStr)
  let argStrs' = map wrapIfNeeded argStrs
  return (unwords (argStrs' ++ [fnStr]))

-- | Decide whether to keep the exact surface form for a function token.
-- Uses TRmorph analyses rather than suffix heuristics.
shouldPreserveSurfaceForm :: RenderCache -> FSM -> Identifier -> IO Bool
shouldPreserveSurfaceForm cache fsm ident = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack (prettyIdent ident))
  let hasMarker a = "<adv>" `isInfixOf` a
  return (any hasMarker analyses)

-- | Keep exact copula surface so :steps can strip only the copula suffix.
shouldPreserveCopulaSurface :: RenderCache -> FSM -> Identifier -> IO Bool
shouldPreserveCopulaSurface cache fsm ident = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack (prettyIdent ident))
  return (any ("<0><V><cpl:" `isInfixOf`) analyses)

-- | Check whether an identifier has a verbal analysis.
isVerbLike :: RenderCache -> FSM -> Identifier -> IO Bool
isVerbLike cache fsm ident = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack (prettyIdent ident))
  let hasVerb = any ("<V>" `isInfixOf`) analyses
      hasNoun = any ("<N>" `isInfixOf`) analyses
  return (hasVerb && not hasNoun)

-- | Check whether an identifier has any verbal analysis.
hasVerbAnalysis :: RenderCache -> FSM -> Identifier -> IO Bool
hasVerbAnalysis cache fsm ident = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack (prettyIdent ident))
  return (any ("<V>" `isInfixOf`) analyses)

-- | Check whether an identifier has a 2nd-person imperative verbal analysis.
hasImperativeAnalysis :: RenderCache -> FSM -> Identifier -> IO Bool
hasImperativeAnalysis cache fsm ident = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack (prettyIdent ident))
  return (any ("<V>" `isInfixOf`) analyses && any ("<imp><2s>" `isInfixOf`) analyses)

-- | Pick a lemma-like candidate, preferring nominative analyses.
pickLemmaCandidate :: Identifier -> [(Identifier, Case)] -> Identifier
pickLemmaCandidate fallback candidates =
  case find (\(_, cas) -> cas == Nom) candidates of
    Just (ident, _) -> ident
    Nothing ->
      case candidates of
        (ident, _):_ -> ident
        [] -> fallback

-- | Pick a lemma identifier via TRmorph analyses, then fall back to candidates.
pickLemmaIdentifier :: RenderCache -> FSM -> Identifier -> [(Identifier, Case)] -> IO Identifier
pickLemmaIdentifier cache fsm fallback@(mods, _) candidates = do
  case candidates of
    _:_ ->
      let cand = pickLemmaCandidate fallback candidates
      in if cand /= fallback
           then return cand
           else do
            analyses <- upsCached cache fsm (T.pack (prettyIdent fallback))
            let roots = [T.takeWhile (/= '<') a | a <- analyses, not (T.null a)]
                shortestRoot =
                  case roots of
                    [] -> Nothing
                    _ -> Just (minimumBy (\a b -> compare (T.length a) (T.length b)) roots)
            case shortestRoot of
              Just root | not (T.null root) -> return (mods, root)
              _ -> return fallback
    [] -> do
      analyses <- upsCached cache fsm (T.pack (prettyIdent fallback))
      let roots = [T.takeWhile (/= '<') a | a <- analyses, not (T.null a)]
          shortestRoot =
            case roots of
              [] -> Nothing
              _ -> Just (minimumBy (\a b -> compare (T.length a) (T.length b)) roots)
      case shortestRoot of
        Just root | not (T.null root) -> return (mods, root)
        _ -> return fallback

-- | Render a verb in ip-converb form using TRmorph.
renderVerbAsConverb :: RenderCache -> FSM -> Identifier -> [(Identifier, Case)] -> IO String
renderVerbAsConverb cache fsm name candidates = do
  lemma <- pickLemmaIdentifier cache fsm name candidates
  let stem = prettyIdent lemma
      tagged = T.pack (stem ++ "<V><adv>")
  forms <- map T.unpack <$> downsCached cache fsm tagged
  return (fromMaybe (prettyIdent name) (pickDownForm forms))

-- | Render a verb in imperative form by preferring concise TRmorph surfaces.
renderVerbAsImperative :: RenderCache -> FSM -> Identifier -> Identifier -> [(Identifier, Case)] -> IO String
renderVerbAsImperative cache fsm surface lemma candidates = do
  analyses <- map T.unpack <$> upsCached cache fsm (T.pack (prettyIdent surface))
  let verbAnalyses = filter ("<V>" `isInfixOf`) analyses
      positiveImperatives =
        [ an
        | an <- verbAnalyses
        , "<imp><2s>" `isInfixOf` an
        , not ("<neg>" `isInfixOf` an)
        ]
      derivationTags an =
        concat
          [ t
          | t <- ["<caus>", "<pass>", "<reflex>", "<rcp>"]
          , t `isInfixOf` an
          ]
      imperativeTagFromAnalysis an =
        let root = takeWhile (/= '<') an
        in root ++ "<V>" ++ derivationTags an ++ "<imp><2s>"
      derivedImperativeTags =
        nub [imperativeTagFromAnalysis an | an <- verbAnalyses, not (null an)]
      chooseShortest forms =
        case filter isPlainLower forms of
          [] ->
            case forms of
              f:_ -> Just f
              [] -> Nothing
          xs -> Just (minimumBy (\a b -> compare (length a) (length b)) xs)
  fromPositiveImperatives <-
    if null positiveImperatives
      then return Nothing
      else do
        formsByAnalysis <- map (map T.unpack) <$> downsCachedBatch cache fsm (map T.pack positiveImperatives)
        return (chooseShortest (concat formsByAnalysis))
  fromDerivedImperatives <-
    if null derivedImperativeTags
      then return Nothing
      else do
        formsByAnalysis <- map (map T.unpack) <$> downsCachedBatch cache fsm (map T.pack derivedImperativeTags)
        return (chooseShortest (concat formsByAnalysis))
  case fromPositiveImperatives of
    Just surfaceForm -> return surfaceForm
    Nothing ->
      case fromDerivedImperatives of
        Just surfaceForm -> return surfaceForm
        Nothing -> do
          normalizedLemma <- pickLemmaIdentifier cache fsm lemma candidates
          let tagged = T.pack (prettyIdent normalizedLemma ++ "<V><imp><2s>")
          forms <- map T.unpack <$> downsCached cache fsm tagged
          return (fromMaybe (prettyIdent surface) (chooseShortest forms))
  where
    isPlainLower = all (\ch -> (isLetter ch && isLower ch) || ch == '-')

-- | Check whether rendered text already carries an accusative suffix.
hasAccSuffix :: String -> Bool
hasAccSuffix s =
  any (`isSuffixOf` s)
    [ "'ı", "'i", "'u", "'ü"
    , "'yı", "'yi", "'yu", "'yü"
    ]

-- | Render a clause preserving case annotations.
renderClausePC :: RenderCache -> FSM -> EvalState -> Case -> String -> Clause Ann -> IO String
renderClausePC cache fsm evalSt matchCase scrutStr (Clause pat body) = do
  patStr <- renderPatPC cache fsm scrutStr pat
  let bodyForRender =
        case body of
          IntLit {} -> body
          FloatLit {} -> body
          StrLit {} -> body
          CharLit {} -> body
          _ -> body { annExp = setAnnCase (annExp body) matchCase }
  bodyStr <- renderExpPreservingCase cache fsm evalSt bodyForRender
  return (patStr ++ ", " ++ bodyStr)

-- | Try to select the matching clause if the scrutinee is a value.
-- Returns Just clauseBody if a clause matches, Nothing if scrutinee needs evaluation.
selectMatchingClause :: Exp Ann -> [Clause Ann] -> Maybe (Exp Ann)
selectMatchingClause scrut clauses =
  case scrut of
    IntLit {} -> findMatchingClauseBody scrut clauses
    FloatLit {} -> findMatchingClauseBody scrut clauses
    StrLit {} -> findMatchingClauseBody scrut clauses
    CharLit {} -> findMatchingClauseBody scrut clauses
    _ -> Nothing
  where
    findMatchingClauseBody :: Exp Ann -> [Clause Ann] -> Maybe (Exp Ann)
    findMatchingClauseBody _ [] = Nothing
    findMatchingClauseBody s (Clause pat body : rest) =
      if matchesPattern s pat
        then Just body
        else findMatchingClauseBody s rest

    matchesPattern :: Exp Ann -> Pat Ann -> Bool
    matchesPattern _ (PWildcard _) = True
    matchesPattern _ (PVar _ _) = True
    matchesPattern (IntLit _ n1) (PIntLit n2 _) = n1 == n2
    matchesPattern (FloatLit _ n1) (PFloatLit n2 _) = n1 == n2
    matchesPattern (StrLit _ s1) (PStrLit s2 _) = s1 == s2
    matchesPattern (CharLit _ c1) (PCharLit c2 _) = c1 == c2
    matchesPattern (Var _ _ cands) (PCtor (ctorName, _) pats) =
      any (\(ident, _) -> ident == ctorName) cands && null pats
    matchesPattern (App _ (Var _ _ cands) args) (PCtor (ctorName, _) pats) =
      any (\(ident, _) -> ident == ctorName) cands && length args == length pats
    matchesPattern _ _ = False


-- | Render a pattern preserving case annotations.
renderPatPC :: RenderCache -> FSM -> String -> Pat Ann -> IO String
renderPatPC cache fsm scrutStr pat =
  case pat of
    PWildcard _ -> return "değilse"
    PVar name ann -> renderIdentWithCases cache fsm name [annCase ann]
    PCtor (ctor, ann) pats -> do
      subPats <- mapM (renderPatPC cache fsm "") pats
      let argStrs = (if null scrutStr then id else (scrutStr :)) subPats
      ctorStr <- renderIdentWithCases cache fsm ctor [annCase ann]
      return (unwords (argStrs ++ [ctorStr]))
    PIntLit n ann -> renderIntWithCase cache fsm (annCase ann) n
    PFloatLit n ann -> renderFloatWithCase cache fsm (annCase ann) n
    PStrLit s _ -> return ("\"" ++ T.unpack s ++ "\"")
    PCharLit c _ -> return ("'" ++ [c] ++ "'")
    PListLit pats -> do
      patStrs <- mapM (renderPatPC cache fsm "") pats
      return ("[" ++ intercalate ", " patStrs ++ "]")

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
    CharLit {charVal} ->
      renderCharLitWithCase cache fsm cas charVal
    Var {annExp, varName, varCandidates} ->
      renderVarWithCase cache fsm varName annExp varCandidates cas
    App {fn = Var {varCandidates}, args} ->
      case lookupCtorSig (M.toList (evalCtors evalSt)) varCandidates of
        Just (ctorName, (argTys, _))
          | length argTys == length args -> do
              argStrs <- sequence
                [ renderExpWithCase cache fsm evalSt (selectCtorArgCase ctorName idx ty arg) arg
                | (idx, (ty, arg)) <- zip [0 :: Int ..] (zip argTys args)
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
      case lookupCtorSig (M.toList (evalCtors evalSt)) varCandidates of
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
    CharLit {charVal} ->
      return ("'" ++ [charVal] ++ "'")
    IntLit {intVal} ->
      renderIntWithCase cache fsm Nom intVal
    FloatLit {floatVal} ->
      renderFloatWithCase cache fsm Nom floatVal
    SetLit {} ->
      return (prettyExp exp)
    MapLit {} ->
      return (prettyExp exp)
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

-- | Pick argument case with constructor-specific rendering rules.
selectCtorArgCase :: Identifier -- ^ Constructor/function identifier.
                  -> Int -- ^ Argument index.
                  -> Ty Ann -- ^ Argument type.
                  -> Exp Ann -- ^ Argument expression.
                  -> Case -- ^ Selected case.
selectCtorArgCase ident idx ty arg
  | isEkiCtor ident && idx == 1 = Dat
  | otherwise = selectArgCase ty arg
  where
    isEkiCtor (_, n) =
      let s = T.unpack n
      in s == "ek" || s == "eki" || "ek" `isInfixOf` s

-- | Apply a case suffix to the last word in a phrase.
applyCaseToLastWord :: RenderCache -- ^ Render cache.
                    -> FSM -- ^ Morphology FSM.
                    -> Case -- ^ Target case.
                    -> String -- ^ Input phrase.
                    -> IO String -- ^ Updated phrase.
applyCaseToLastWord cache fsm cas s =
  case splitLastWord s of
    Nothing -> return s
    Just (prefix, punctPrefix, word, punctSuffix, suffix) ->
      if null word
        then return s
        else do
          inflected <-
            if cas == Acc
              then do
                analyses <- map T.unpack <$> upsCached cache fsm (T.pack word)
                case find ("<p3s>" `isInfixOf`) analyses of
                  Just an -> do
                    let root = T.takeWhile (/= '<') (T.pack an)
                    renderIdentWithCases cache fsm ([], root) [P3s, Acc]
                  Nothing -> renderIdentWithCases cache fsm ([], T.pack word) [cas]
              else renderIdentWithCases cache fsm ([], T.pack word) [cas]
          prefix' <-
            if cas == Acc && "eki" `isPrefixOf` inflected
              then promotePreviousWordDat prefix
              else return prefix
          return (prefix' ++ punctPrefix ++ inflected ++ punctSuffix ++ suffix)
  where
    -- | Split a string into prefix, punctuation, last word, punctuation, and trailing whitespace.
    splitLastWord :: String -- ^ Input phrase.
                  -> Maybe (String, String, String, String, String)
    splitLastWord input =
      let (revSuffix, revBody) = span isSpace (reverse input)
          (revPunctSuffix, revBody1) = break isAlphaNum revBody
          (revWord, revBody2) = span isAlphaNum revBody1
          (revPunctPrefix, revPrefix) = break isSpace revBody2
      in if null revWord
        then Nothing
        else
          Just
            ( reverse revPrefix
            , reverse revPunctPrefix
            , reverse revWord
            , reverse revPunctSuffix
            , reverse revSuffix
            )

    promotePreviousWordDat :: String -> IO String
    promotePreviousWordDat pref =
      case splitPrevWord pref of
        Nothing -> return pref
        Just (pref0, w, suff) -> do
          mDat <- inferDatSurfaceViaUps cache fsm w
          case mDat of
            Just dat -> return (pref0 ++ dat ++ suff)
            Nothing -> return pref

    splitPrevWord :: String -> Maybe (String, String, String)
    splitPrevWord input =
      let (revSuff, revBody) = span isSpace (reverse input)
          (revWord, revPref) = break isSpace revBody
      in if null revWord
           then Nothing
           else Just (reverse revPref, reverse revWord, reverse revSuff)

-- | Render a quoted string literal with a case suffix (if any).
renderStrLitWithCase :: RenderCache -> FSM -> Case -> Text -> IO String
renderStrLitWithCase cache fsm cas litText = do
  let bare = T.unpack litText
      quoted = "\"" ++ bare ++ "\""
  if cas == Nom
    then return quoted
    else do
      inflected <- renderIdentWithCases cache fsm ([], litText) [cas]
      let suffix = maybe "'i" quoteSuffix (stripPrefix bare inflected)
      return (quoted ++ suffix)
  where
    quoteSuffix "" = ""
    quoteSuffix s@('\'':_) = s
    quoteSuffix s = '\'' : s

-- | Render a character literal with a case suffix (if any).
renderCharLitWithCase :: RenderCache -> FSM -> Case -> Char -> IO String
renderCharLitWithCase cache fsm cas c = do
  let quoted = "'" ++ [c] ++ "'"
  if cas == Nom
    then return quoted
    else do
      inflected <- renderIdentWithCases cache fsm ([], T.singleton c) [cas]
      let suffix = maybe "'i" quoteSuffix (stripPrefix [c] inflected)
      return (quoted ++ suffix)
  where
    quoteSuffix "" = ""
    quoteSuffix s@('\'':_) = s
    quoteSuffix s = '\'' : s

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
      commaBase = map (\c -> if c == '.' then ',' else c) base
      suffixFrom inflected =
        listToMaybe
          [ s
          | b <- [base, commaBase]
          , s <- maybeToList (stripPrefix b inflected)
          ]
      withQuoteSuffix "" = ""
      withQuoteSuffix s@('\'':_) = s
      withQuoteSuffix s = '\'' : s
  if cas == Nom || cas == P3s
    then return (prefix ++ base)
    else do
      inflected <- renderIdentWithCases cache fsm ([], T.pack base) [cas]
      let rendered =
            case suffixFrom inflected of
              Just suf -> base ++ withQuoteSuffix suf
              Nothing -> inflected
      return (prefix ++ rendered)

-- | Render a variable with the requested case, using candidates if present.
renderVarWithCase :: RenderCache -- ^ Render cache.
                  -> FSM -- ^ Morphology FSM.
                  -> Identifier -- ^ Identifier to render.
                  -> Ann -- ^ Original annotation.
                  -> [(Identifier, Case)] -- ^ Candidate identifiers.
                  -> Case -- ^ Target case.
                  -> IO String -- ^ Rendered identifier.
renderVarWithCase cache fsm name annExp candidates targetCase = do
  preserveSurface <- shouldPreserveSurfaceForm cache fsm name
  preserveCopula <- shouldPreserveCopulaSurface cache fsm name
  verbByMorph <- isVerbLike cache fsm name
  let verbByCase = any (\(_, cas) -> cas == Cond) candidates
      isVerb = verbByMorph || verbByCase
  lemma <-
    if isVerb
      then pickLemmaIdentifier cache fsm name candidates
      else return (pickLemmaCandidate name candidates)
  let renderBase = renderIdentWithCases cache fsm lemma [targetCase]
  if annCase annExp == targetCase
    then
      if preserveSurface || (preserveCopula && not isVerb)
        then return (prettyIdent name)
        else if isVerb
        then
          renderBase
        else
          if targetCase == Dat
            then do
              mDat <- inferDatSurfaceViaUps cache fsm (prettyIdent name)
              return (fromMaybe (prettyIdent name) mDat)
            else return (prettyIdent name)
    else renderBase

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
