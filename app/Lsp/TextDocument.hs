{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Pure text operations used by the LSP document handlers.
module Lsp.TextDocument
  ( applyContentChanges
  , documentEndPosition
  , formatText
  ) where

import Control.Lens ((^.))
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.LSP.Protocol.Lens as L
import Language.LSP.Protocol.Types

-- | Apply a sequence of LSP content changes to document text.
--
-- The protocol allows both full-document replacements and ranged edits.
-- Clients may send incremental edits even when full sync is requested, so
-- both forms are handled here.
applyContentChanges :: Text -- ^ Current document text.
                    -> [TextDocumentContentChangeEvent] -- ^ Changes to apply, in order.
                    -> Text -- ^ Document text after every change.
applyContentChanges = foldl' applyContentChange

-- | Apply one LSP content change to document text.
applyContentChange :: Text -- ^ Current document text.
                   -> TextDocumentContentChangeEvent -- ^ A ranged edit or a whole-document replacement.
                   -> Text -- ^ Document text after the change.
applyContentChange oldText (TextDocumentContentChangeEvent change) =
  case change of
    InL (TextDocumentContentChangePartial range _ t) -> applyRangeEdit oldText range t
    InR (TextDocumentContentChangeWholeDocument t) -> t

-- | Apply a ranged text edit to a UTF-16 position-based document.
applyRangeEdit :: Text -- ^ Current document text.
               -> Range -- ^ Range to replace, in UTF-16 code units.
               -> Text -- ^ Replacement text.
               -> Text -- ^ Document text after the edit.
applyRangeEdit txt (Range startPos endPos) replacement =
  let (startOff, endOff) = offsetsAtRange txt startPos endPos
      prefix = T.take startOff txt
      suffix = T.drop endOff txt
  in prefix <> replacement <> suffix

-- | Convert start/end positions to text offsets in one scan.
offsetsAtRange :: Text -- ^ Document text to scan.
               -> Position -- ^ Start position, in UTF-16 code units.
               -> Position -- ^ End position, in UTF-16 code units.
               -> (Int, Int) -- ^ Character offsets of the two positions, in ascending order.
offsetsAtRange txt startPos endPos
  | (startLine, startCol) <= (endLine, endCol) =
      go 0 0 0 Nothing Nothing txt
  | otherwise =
      let (endOff, startOff) = offsetsAtRange txt endPos startPos
      in (startOff, endOff)
  where
    startLine = max 0 (fromIntegral (startPos ^. L.line))
    startCol = max 0 (fromIntegral (startPos ^. L.character))
    endLine = max 0 (fromIntegral (endPos ^. L.line))
    endCol = max 0 (fromIntegral (endPos ^. L.character))

    reached :: Int -> Int -> Int -> Int -> Bool
    reached line col targetL targetC =
      line > targetL || (line == targetL && col >= targetC)

    stepMatch :: Int -> Int -> Int -> Int -> Int -> Maybe Int -> Maybe Int
    stepMatch off line col targetL targetC m =
      case m of
        Just _ -> m
        Nothing ->
          if reached line col targetL targetC
            then Just off
            else Nothing

    finish :: Int -> Maybe Int -> Maybe Int -> (Int, Int)
    finish off mStart mEnd =
      let startOff = fromMaybe off mStart
          endOff = fromMaybe off mEnd
      in (startOff, endOff)

    go :: Int -> Int -> Int -> Maybe Int -> Maybe Int -> Text -> (Int, Int)
    go !off !line !col !mStart !mEnd !restTxt =
      let !mStart' = stepMatch off line col startLine startCol mStart
          !mEnd' = stepMatch off line col endLine endCol mEnd
      in case mStart' of
           Just startOff ->
             case mEnd' of
               Just endOff -> (startOff, endOff)
               Nothing ->
                 case T.uncons restTxt of
                   Nothing -> finish off mStart' mEnd'
                   Just (c, rest1)
                     | c == '\r' ->
                         case T.uncons rest1 of
                           Just ('\n', rest2) ->
                             if line == endLine
                               then (startOff, off)
                               else go (off + 2) (line + 1) 0 mStart' mEnd' rest2
                           _ ->
                             if line == endLine
                               then (startOff, off)
                               else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                     | c == '\n' ->
                         if line == endLine
                           then (startOff, off)
                           else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                     | otherwise ->
                         let !w = utf16Width c
                             !nextCol = col + w
                         in if line == endLine && nextCol > endCol
                              then (startOff, off)
                              else go (off + 1) line nextCol mStart' mEnd' rest1
           Nothing ->
             case T.uncons restTxt of
               Nothing -> finish off mStart' mEnd'
               Just (c, rest1)
                 | c == '\r' ->
                     case T.uncons rest1 of
                       Just ('\n', rest2) ->
                         if line == startLine
                           then finish off mStart' mEnd'
                           else go (off + 2) (line + 1) 0 mStart' mEnd' rest2
                       _ ->
                         if line == startLine
                           then finish off mStart' mEnd'
                           else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                 | c == '\n' ->
                     if line == startLine
                       then finish off mStart' mEnd'
                       else go (off + 1) (line + 1) 0 mStart' mEnd' rest1
                 | otherwise ->
                     let !w = utf16Width c
                         !nextCol = col + w
                     in if line == startLine && nextCol > startCol
                          then finish off mStart' mEnd'
                          else go (off + 1) line nextCol mStart' mEnd' rest1

    utf16Width :: Char -> Int
    utf16Width c = if ord c > 0xFFFF then 2 else 1

-- | Trim trailing whitespace and ensure a trailing newline.
formatText :: Text -- ^ Document text to format.
           -> Text -- ^ Text with trailing whitespace removed and a final newline.
formatText txt =
  let trimmed = T.unlines (map T.stripEnd (T.lines txt))
  in if T.null trimmed || T.last trimmed == '\n' then trimmed else trimmed <> "\n"

-- | Compute the final position in pre-split document lines.
documentEndPosition :: V.Vector Text -- ^ Document lines.
                    -> Position -- ^ Position just past the last character.
documentEndPosition ls =
  if V.null ls
    then Position 0 0
    else Position (fromIntegral (V.length ls - 1)) (fromIntegral (T.length (V.last ls)))
