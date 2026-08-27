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
  , biRange :: !Range
  , biScope :: !Span
  }

data SpanKey
  = SpanKey Span
  | RangeKey Range
  deriving (Eq, Ord, Show)

-- | Resolved information attached to one source span or LSP range.
data SpanInfo = SpanInfo
  { siSpan :: Maybe Span
  , siRange :: Maybe Range
  , siIdent :: Maybe Identifier
  , siSig :: Maybe (Identifier, [Ty Ann])
  , siType :: Maybe (Ty Ann)
  , siBinder :: Maybe BinderInfo
  }

-- | Exact hash lookup plus entries for smallest-enclosing-span lookup.
data SpanIndex = SpanIndex
  { siByKey :: HM.HashMap Text SpanInfo
  , siEntries :: [(SpanKey, SpanInfo)]
  }

newtype PosIndex a = PosIndex
  { piEntries :: [(Span, a)]
  }

type ExpIndex = PosIndex (Exp Ann)
type VarIndex = PosIndex (Identifier, [(Identifier, Case)])
type PatVarIndex = PosIndex Identifier
type CtorIndex = PosIndex (Identifier, Ann, [Pat Ann], Maybe (Exp Ann))
type MatchClauseIndex = PosIndex (Exp Ann, Pat Ann)
type FuncClauseIndex = PosIndex ([Arg Ann], Pat Ann)

-- | Positional lookup indices built together in one AST traversal.
data DocIndices = DocIndices
  { indexedExpressions :: !ExpIndex
  , indexedVariables :: !VarIndex
  , indexedPatternVariables :: !PatVarIndex
  , indexedConstructors :: !CtorIndex
  , indexedMatchClauses :: !MatchClauseIndex
  , indexedFunctionClauses :: !FuncClauseIndex
  }

data DocIndexLists = DocIndexLists
  { expEntries :: [(Span, Exp Ann)]
  , varEntries :: [(Span, (Identifier, [(Identifier, Case)]))]
  , patVarEntries :: [(Span, Identifier)]
  , ctorEntries :: [(Span, (Identifier, Ann, [Pat Ann], Maybe (Exp Ann)))]
  , matchClauseEntries :: [(Span, (Exp Ann, Pat Ann))]
  , funcClauseEntries :: [(Span, ([Arg Ann], Pat Ann))]
  }

emptyDocIndexLists :: DocIndexLists
emptyDocIndexLists = DocIndexLists [] [] [] [] [] []

spanKeyText :: Span -> Text
spanKeyText = T.pack . show

rangeKeyText :: Range -> Text
rangeKeyText = T.pack . show

spanKeyTextForSpanKey :: SpanKey -> Text
spanKeyTextForSpanKey key =
  case key of
    SpanKey sp -> "S:" <> spanKeyText sp
    RangeKey range -> "R:" <> rangeKeyText range

posIndexFromEntries :: [(Span, a)] -> PosIndex a
posIndexFromEntries = PosIndex

-- | Build the unified resolved-symbol index.
buildSpanIndex :: Map.Map Span Identifier -> Map.Map Span (Identifier, [Ty Ann]) -> Map.Map Span (Ty Ann) -> [BinderInfo] -> SpanIndex
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
spanInfoForSpan :: Span -> SpanIndex -> Maybe SpanInfo
spanInfoForSpan sp idx =
  HM.lookup ("S:" <> spanKeyText sp) (siByKey idx)

-- | Find the smallest indexed span or range containing a position.
spanInfoAtPosition :: Position -> SpanIndex -> Maybe SpanInfo
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

rangeSizeForSort :: Range -> (Int, Int)
rangeSizeForSort (Range (Position sl sc) (Position el ec)) =
  let lines = fromIntegral el - fromIntegral sl
      cols = if lines == 0 then fromIntegral ec - fromIntegral sc else maxBound :: Int
  in (lines, cols)

-- | Find the value attached to the smallest span containing a position.
lookupByPosition :: Position -> PosIndex a -> Maybe a
lookupByPosition pos idx =
  fmap snd . listToMaybe . sortOn fst $
    [ (spanSizeForSort sp, value)
    | (sp, value) <- piEntries idx
    , posInSpan pos sp
    ]

-- | Build every per-document positional index in one AST traversal.
buildDocIndices :: [Stmt Ann] -> DocIndices
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
mergeSpanAll :: [Span] -> Span
mergeSpanAll spans =
  case [(start, end) | Span start end _ <- spans] of
    [] -> NoSpan
    pairs ->
      let path = listToMaybe [value | Span _ _ (Just value) <- spans]
      in Span (minimum (map fst pairs)) (maximum (map snd pairs)) path

-- | Compute the span covering a pattern and all nested children.
patRootSpan :: Pat Ann -> Span
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
posInSpan :: Position -> Span -> Bool
posInSpan _ NoSpan = False
posInSpan (Position line column) (Span start end _) =
  let startLine = fromIntegral (unPos (sourceLine start) - 1)
      startColumn = fromIntegral (unPos (sourceColumn start) - 1)
      endLine = fromIntegral (unPos (sourceLine end) - 1)
      endColumn = fromIntegral (unPos (sourceColumn end) - 1)
  in (line > startLine || (line == startLine && column >= startColumn))
       && (line < endLine || (line == endLine && column <= endColumn))

-- | Sort spans from smallest to largest for innermost lookup.
spanSizeForSort :: Span -> (Int, Int)
spanSizeForSort NoSpan = (maxBound, maxBound)
spanSizeForSort (Span (SourcePos _ startLine startColumn) (SourcePos _ endLine endColumn) _) =
  let lines = unPos endLine - unPos startLine
      columns = if lines == 0 then unPos endColumn - unPos startColumn else maxBound
  in (lines, columns)
