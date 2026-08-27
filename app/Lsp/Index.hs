{-# LANGUAGE OverloadedStrings #-}

-- | Positional AST and resolved-symbol indices used by the LSP server.
module Lsp.Index
  ( BinderInfo(..)
  , SpanInfo(..)
  , SpanIndex
  , ExpIndex
  , VarIndex
  , PatVarIndex
  , CtorIndex
  , MatchClauseIndex
  , FuncClauseIndex
  , DocIndices(..)
  , buildSpanIndex
  , spanInfoForSpan
  , spanInfoAtPosition
  , rangeSizeForSort
  , lookupByPosition
  , buildDocIndices
  , mergeSpanAll
  , patRootSpan
  , posInSpan
  , spanSizeForSort
  ) where

import Control.Applicative ((<|>))
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Language.LSP.Protocol.Types (Position(..), Range(..), positionInRange)
import Text.Megaparsec.Pos (SourcePos(..), sourceColumn, sourceLine, unPos)

import Kip.AST

-- | A bound identifier, its declaration range, and its lexical scope.
data BinderInfo = BinderInfo
  { biIdent :: !Identifier
    -- ^ The bound name.
  , biRange :: !Range
    -- ^ Range of the binding occurrence itself.
  , biScope :: !Span
    -- ^ Span over which the binding is visible.
  }

-- | How one index entry is located: by Kip source span or by LSP range.
data SpanKey
  = SpanKey Span
  -- ^ Keyed by a span from the Kip AST.
  | RangeKey Range
  -- ^ Keyed by an LSP range, used for binder declarations.
  deriving (Eq, Ord, Show)

-- | Resolved information attached to one source span or LSP range.
data SpanInfo = SpanInfo
  { siSpan :: Maybe Span
    -- ^ Source span this information came from.
  , siRange :: Maybe Range
    -- ^ LSP range this information came from.
  , siIdent :: Maybe Identifier
    -- ^ Name the typechecker resolved here.
  , siSig :: Maybe (Identifier, [Ty Ann])
    -- ^ Function and argument types resolved for a call here.
  , siType :: Maybe (Ty Ann)
    -- ^ Type inferred for the expression here.
  , siBinder :: Maybe BinderInfo
    -- ^ Binder declared here, when this entry is a declaration.
  }

-- | Exact hash lookup plus entries for smallest-enclosing-span lookup.
data SpanIndex = SpanIndex
  { siByKey :: HM.HashMap Text SpanInfo
    -- ^ Entries keyed by their serialized span or range, for exact lookup.
  , siEntries :: [(SpanKey, SpanInfo)]
    -- ^ The same entries as a list, scanned to find the innermost match for a position.
  }

-- | Values attached to source spans, looked up by the innermost span containing
-- a cursor position.
newtype PosIndex a = PosIndex
  { piEntries :: [(Span, a)]
    -- ^ Each indexed value with the span it covers.
  }

-- | Expressions by the span they occupy.
type ExpIndex = PosIndex (Exp Ann)

-- | Variable occurrences, each with the name as written and its candidate readings.
type VarIndex = PosIndex (Identifier, [(Identifier, Case)])

-- | Variables bound by patterns, by the span of the binding occurrence.
type PatVarIndex = PosIndex Identifier

-- | Constructor patterns: the constructor, its annotation, its argument
-- patterns, and the scrutinee it matches against when known.
type CtorIndex = PosIndex (Identifier, Ann, [Pat Ann], Maybe (Exp Ann))

-- | Clauses of a @Match@ expression, each with its scrutinee and pattern.
type MatchClauseIndex = PosIndex (Exp Ann, Pat Ann)

-- | Clauses of a function definition, each with the function's arguments and
-- the clause's pattern.
type FuncClauseIndex = PosIndex ([Arg Ann], Pat Ann)

-- | Positional lookup indices built together in one AST traversal.
data DocIndices = DocIndices
  { indexedExpressions :: !ExpIndex
    -- ^ Every expression by span.
  , indexedVariables :: !VarIndex
    -- ^ Every variable occurrence by span.
  , indexedPatternVariables :: !PatVarIndex
    -- ^ Every pattern-bound variable by span.
  , indexedConstructors :: !CtorIndex
    -- ^ Every constructor pattern by span.
  , indexedMatchClauses :: !MatchClauseIndex
    -- ^ Every @Match@ clause by span.
  , indexedFunctionClauses :: !FuncClauseIndex
    -- ^ Every function-definition clause by span.
  }

-- | Entries accumulated during the traversal that builds 'DocIndices'.
data DocIndexLists = DocIndexLists
  { expEntries :: [(Span, Exp Ann)]
    -- ^ Expression entries collected so far.
  , varEntries :: [(Span, (Identifier, [(Identifier, Case)]))]
    -- ^ Variable-occurrence entries collected so far.
  , patVarEntries :: [(Span, Identifier)]
    -- ^ Pattern-bound variable entries collected so far.
  , ctorEntries :: [(Span, (Identifier, Ann, [Pat Ann], Maybe (Exp Ann)))]
    -- ^ Constructor-pattern entries collected so far.
  , matchClauseEntries :: [(Span, (Exp Ann, Pat Ann))]
    -- ^ @Match@ clause entries collected so far.
  , funcClauseEntries :: [(Span, ([Arg Ann], Pat Ann))]
    -- ^ Function-clause entries collected so far.
  }

-- | Accumulator with no entries collected yet.
emptyDocIndexLists :: DocIndexLists -- ^ Empty entry lists.
emptyDocIndexLists = DocIndexLists [] [] [] [] [] []

-- | Serialize a span for use as a hash-map key.
spanKeyText :: Span -- ^ Span to serialize.
            -> Text -- ^ Its textual key.
spanKeyText = T.pack . show

-- | Serialize an LSP range for use as a hash-map key.
rangeKeyText :: Range -- ^ Range to serialize.
             -> Text -- ^ Its textual key.
rangeKeyText = T.pack . show

-- | Serialize a span key, prefixed so span and range keys cannot collide.
spanKeyTextForSpanKey :: SpanKey -- ^ Key to serialize.
                      -> Text -- ^ Its textual key, prefixed @S:@ or @R:@.
spanKeyTextForSpanKey key =
  case key of
    SpanKey sp -> "S:" <> spanKeyText sp
    RangeKey range -> "R:" <> rangeKeyText range

-- | Build a positional index from collected entries.
posIndexFromEntries :: [(Span, a)] -- ^ Values with the spans they cover.
                    -> PosIndex a -- ^ Index over those entries.
posIndexFromEntries = PosIndex

-- | Build the unified resolved-symbol index.
buildSpanIndex :: Map.Map Span Identifier -- ^ Name resolved at each span.
               -> Map.Map Span (Identifier, [Ty Ann]) -- ^ Call signature resolved at each span.
               -> Map.Map Span (Ty Ann) -- ^ Type inferred at each span.
               -> [BinderInfo] -- ^ Binders declared in the document.
               -> SpanIndex -- ^ Index merging all four sources per location.
buildSpanIndex resolved resolvedSigs resolvedTypes binders =
  SpanIndex (HM.map snd byKey) (HM.elems byKey)
  where
    entries =
      [ (SpanKey sp, SpanInfo (Just sp) Nothing (Just ident) Nothing Nothing Nothing)
      | (sp, ident) <- Map.toList resolved
      ] ++
      [ (SpanKey sp, SpanInfo (Just sp) Nothing Nothing (Just sig) Nothing Nothing)
      | (sp, sig) <- Map.toList resolvedSigs
      ] ++
      [ (SpanKey sp, SpanInfo (Just sp) Nothing Nothing Nothing (Just ty) Nothing)
      | (sp, ty) <- Map.toList resolvedTypes
      ] ++
      [ (RangeKey (biRange bi), SpanInfo Nothing (Just (biRange bi)) (Just (biIdent bi)) Nothing Nothing (Just bi))
      | bi <- binders
      ]
    byKey = foldl' insertEntry HM.empty entries
    insertEntry acc (key, info) =
      HM.insertWith mergeEntry (spanKeyTextForSpanKey key) (key, info) acc
    mergeEntry new old =
      let (oldKey, oldInfo) = old
          (_, newInfo) = new
      in (oldKey, mergeSpanInfo newInfo oldInfo)
    mergeSpanInfo new old =
      SpanInfo
        { siSpan = siSpan new <|> siSpan old
        , siRange = siRange new <|> siRange old
        , siIdent = siIdent new <|> siIdent old
        , siSig = siSig new <|> siSig old
        , siType = siType new <|> siType old
        , siBinder = siBinder new <|> siBinder old
        }

-- | Look up resolved information by an exact AST span.
spanInfoForSpan :: Span -- ^ Span to look up.
                -> SpanIndex -- ^ Index to search.
                -> Maybe SpanInfo -- ^ Information recorded at exactly that span.
spanInfoForSpan sp idx =
  HM.lookup ("S:" <> spanKeyText sp) (siByKey idx)

-- | Find the smallest indexed span or range containing a position.
spanInfoAtPosition :: Position -- ^ Cursor position.
                   -> SpanIndex -- ^ Index to search.
                   -> Maybe SpanInfo -- ^ Information at the innermost entry containing the position.
spanInfoAtPosition pos idx =
  fmap snd . listToMaybe . sortOn fst $
    [ (spanKeySize key, info)
    | (key, info) <- siEntries idx
    , contains key
    ]
  where
    contains key =
      case key of
        SpanKey sp -> posInSpan pos sp
        RangeKey range -> positionInRange pos range
    spanKeySize key =
      case key of
        SpanKey sp -> spanSizeForSort sp
        RangeKey range -> rangeSizeForSort range

-- | Size key ordering ranges from smallest to largest.
rangeSizeForSort :: Range -- ^ Range to measure.
                 -> (Int, Int) -- ^ Line span, then column span for single-line ranges.
rangeSizeForSort (Range (Position sl sc) (Position el ec)) =
  let lines = fromIntegral el - fromIntegral sl
      cols = if lines == 0 then fromIntegral ec - fromIntegral sc else maxBound :: Int
  in (lines, cols)

-- | Find the value attached to the smallest span containing a position.
lookupByPosition :: Position -- ^ Cursor position.
                 -> PosIndex a -- ^ Index to search.
                 -> Maybe a -- ^ Value at the innermost span containing the position.
lookupByPosition pos idx =
  fmap snd . listToMaybe . sortOn fst $
    [ (spanSizeForSort sp, value)
    | (sp, value) <- piEntries idx
    , posInSpan pos sp
    ]

-- | Build every per-document positional index in one AST traversal.
buildDocIndices :: [Stmt Ann] -- ^ Statements of one document.
                -> DocIndices -- ^ Every positional index for that document.
buildDocIndices stmts =
  let lists = foldl' collectStmt emptyDocIndexLists stmts
  in DocIndices
       { indexedExpressions = posIndexFromEntries (expEntries lists)
       , indexedVariables = posIndexFromEntries (varEntries lists)
       , indexedPatternVariables = posIndexFromEntries (patVarEntries lists)
       , indexedConstructors = posIndexFromEntries (ctorEntries lists)
       , indexedMatchClauses = posIndexFromEntries (matchClauseEntries lists)
       , indexedFunctionClauses = posIndexFromEntries (funcClauseEntries lists)
       }
  where
    addSpanEntry sp val xs =
      case sp of
        NoSpan -> xs
        _ -> (sp, val) : xs
    collectStmt acc stmt =
      case stmt of
        Defn _ _ expr -> collectExp Nothing acc expr
        Function _ args _ clauses _ -> foldl' (collectFunctionClause args) acc clauses
        ExpStmt expr -> collectExp Nothing acc expr
        _ -> acc
    collectFunctionClause args acc (Clause pat body) =
      let bodySpan = annSpan (annExp body)
          scopeSpan = mergeSpanAll [bodySpan, patRootSpan pat]
          acc' = acc
            { funcClauseEntries = addSpanEntry scopeSpan (args, pat) (funcClauseEntries acc) }
          acc'' = collectPat Nothing acc' pat
      in collectExp Nothing acc'' body
    collectExp mScrutinee acc expr =
      let acc' =
            acc
              { expEntries = addSpanEntry (annSpan (annExp expr)) expr (expEntries acc)
              , varEntries =
                  case expr of
                    Var {annExp = ann, varName = name, varCandidates = candidates} ->
                      addSpanEntry (annSpan ann) (name, candidates) (varEntries acc)
                    _ -> varEntries acc
              }
      in case expr of
           App _ fn args -> foldl' (collectExp mScrutinee) (collectExp mScrutinee acc' fn) args
           Bind _ _ _ body -> collectExp mScrutinee acc' body
           Seq _ first second -> collectExp mScrutinee (collectExp mScrutinee acc' first) second
           Match _ scrutinee clauses ->
             foldl' (collectMatchClause scrutinee) (collectExp mScrutinee acc' scrutinee) clauses
           Let _ _ body -> collectExp mScrutinee acc' body
           Ascribe _ _ body -> collectExp mScrutinee acc' body
           _ -> acc'
    collectMatchClause scrutinee acc (Clause pat body) =
      let bodySpan = annSpan (annExp body)
          acc' = acc
            { matchClauseEntries = addSpanEntry bodySpan (scrutinee, pat) (matchClauseEntries acc) }
          acc'' = collectPat (Just scrutinee) acc' pat
      in collectExp (Just scrutinee) acc'' body
    collectPat mScrutinee acc pat =
      case pat of
        PVar ident ann ->
          acc { patVarEntries = addSpanEntry (annSpan ann) ident (patVarEntries acc) }
        PCtor (ctor, ann) pats ->
          let ctorSpan = mergeSpanAll (annSpan ann : map patRootSpan pats)
              acc' = acc
                { ctorEntries = addSpanEntry ctorSpan (ctor, ann, pats, mScrutinee) (ctorEntries acc) }
          in foldl' (collectPat mScrutinee) acc' pats
        PListLit pats -> foldl' (collectPat mScrutinee) acc pats
        _ -> acc

-- | Merge spans while preserving the earliest start and latest end.
mergeSpanAll :: [Span] -- ^ Spans to combine.
             -> Span -- ^ Span from the earliest start to the latest end, 'NoSpan' if none are located.
mergeSpanAll spans =
  case [(start, end) | Span start end _ <- spans] of
    [] -> NoSpan
    pairs ->
      let path = listToMaybe [value | Span _ _ (Just value) <- spans]
      in Span (minimum (map fst pairs)) (maximum (map snd pairs)) path

-- | Compute the span covering a pattern and all nested children.
patRootSpan :: Pat Ann -- ^ Pattern to measure.
            -> Span -- ^ Span covering the pattern and all its subpatterns.
patRootSpan pat =
  case pat of
    PWildcard ann -> annSpan ann
    PVar _ ann -> annSpan ann
    PCtor (_, ann) pats -> mergeSpanAll (annSpan ann : map patRootSpan pats)
    PIntLit _ ann -> annSpan ann
    PFloatLit _ ann -> annSpan ann
    PStrLit _ ann -> annSpan ann
    PCharLit _ ann -> annSpan ann
    PListLit pats -> mergeSpanAll (map patRootSpan pats)

-- | Test whether an LSP position lies within a Kip span.
posInSpan :: Position -- ^ Zero-based LSP position.
          -> Span -- ^ One-based Kip source span.
          -> Bool -- ^ 'True' when the position falls inside the span.
posInSpan _ NoSpan = False
posInSpan (Position line column) (Span start end _) =
  let startLine = fromIntegral (unPos (sourceLine start) - 1)
      startColumn = fromIntegral (unPos (sourceColumn start) - 1)
      endLine = fromIntegral (unPos (sourceLine end) - 1)
      endColumn = fromIntegral (unPos (sourceColumn end) - 1)
  in (line > startLine || (line == startLine && column >= startColumn))
       && (line < endLine || (line == endLine && column <= endColumn))

-- | Sort spans from smallest to largest for innermost lookup.
spanSizeForSort :: Span -- ^ Span to measure.
                -> (Int, Int) -- ^ Line span, then column span for single-line spans;
                -- 'NoSpan' sorts last.
spanSizeForSort NoSpan = (maxBound, maxBound)
spanSizeForSort (Span (SourcePos _ startLine startColumn) (SourcePos _ endLine endColumn) _) =
  let lines = unPos endLine - unPos startLine
      columns = if lines == 0 then unPos endColumn - unPos startColumn else maxBound
  in (lines, columns)
