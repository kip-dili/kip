{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

-- | Formatting for the REPL's @:steps@ command, which replays a recorded
-- evaluation trace and prints each reduction as it happens.
module Repl.Steps
  ( formatStepsStreaming
  , stripStepsCopulaTRmorph
  , shouldSkipInfinitiveSteps
  , setTopCaseNom
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit, toLower)
import qualified Data.Map.Strict as Map
import Data.List (find, findIndex, isInfixOf, isPrefixOf, isSuffixOf, nub, tails)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Foma (FSM)
import System.Console.Chalk (blue, dim)

import Kip.AST
import Kip.Eval (TraceStep(..))
import Kip.Render (RenderCache)
import qualified Kip.Render

-- | Stream trace steps as soon as they are formatted.
--
-- This prints interactive REPL output incrementally instead of building one
-- large intermediate string.
formatStepsStreaming :: Monad m
                     => Bool -- ^ Whether to emit ANSI color.
                     -> (Exp Ann -> m String) -- ^ Preserving-case renderer (for inputs).
                     -> (Exp Ann -> m String) -- ^ Nominative renderer (for outputs).
                     -> Exp Ann               -- ^ Final evaluated expression.
                     -> [TraceStep]           -- ^ Recorded evaluation steps to replay.
                     -> (String -> m ())      -- ^ Output sink (e.g. @outputStrLn@).
                     -> m ()                  -- ^ Writes each formatted step to the sink.
formatStepsStreaming useColor renderInput renderOutput finalExp steps emit = do
  let truncated = length steps >= 1000
  mLastLine <- formatStepsReplayStreaming useColor renderInput renderOutput steps emit
  finalLine <- do
    finalStr <- renderOutput finalExp
    let arrow = if useColor then dim "⇝ " else "⇝ "
    return (arrow ++ finalStr)
  -- Also render with case preserved to check if it matches the last step
  finalLinePreserved <- do
    finalStr <- renderInput finalExp
    let arrow = if useColor then dim "⇝ " else "⇝ "
    return (arrow ++ finalStr)
  let finalPlain = stripAnsiForCompare finalLine
      finalPreservedPlain = stripAnsiForCompare finalLinePreserved
  case mLastLine of
    Nothing ->
      emit finalLine
    Just lastLine -> do
      let lastPlain = stripAnsiForCompare lastLine
      if lastPlain == finalPlain || lastPlain == finalPreservedPlain
        then return ()
        else do
          emit ""
          emit finalLine
  when truncated $
    emit "(1000 adım sınırına ulaşıldı)"

-- | Check whether a :steps expression is an infinitive reference.
-- Such expressions should not be stepped or evaluated in :steps mode.
shouldSkipInfinitiveSteps :: RenderCache -- ^ Memoized morphological renderings.
                          -> FSM -- ^ Morphological analyzer.
                          -> Exp Ann -- ^ Expression given to @:steps@.
                          -> IO Bool -- ^ 'True' when it is a bare infinitive reference.
shouldSkipInfinitiveSteps cache fsm = go
  where
    go exp' =
      case exp' of
        App _ fn _ -> isInfinitiveHead fn
        Var {} -> isInfinitiveHead exp'
        Ascribe _ _ e -> go e
        _ -> return False

    isInfinitiveHead expr =
      case expr of
        Var _ varName varCandidates -> do
          byName <- hasInfinitiveAnalysis varName
          byCandidates <- or <$> mapM (hasInfinitiveAnalysis . fst) varCandidates
          return (byName || byCandidates)
        _ -> return False

    hasInfinitiveAnalysis ident = do
      analyses <- Kip.Render.upsCached cache fsm (T.pack (Kip.Render.prettyIdent ident))
      return (any (T.isInfixOf "<vn:inf><N>") analyses)

-- | Stream replayed trace transitions and return the last emitted line.
formatStepsReplayStreaming :: Monad m
                           => Bool -- ^ Whether to emit ANSI color.
                           -> (Exp Ann -> m String) -- ^ Renderer for intermediate expressions.
                           -> (Exp Ann -> m String) -- ^ Renderer for final output values.
                           -> [TraceStep] -- ^ Recorded evaluation steps to replay.
                           -> (String -> m ()) -- ^ Sink each formatted line is written to.
                           -> m (Maybe String) -- ^ The last line emitted, if any.
formatStepsReplayStreaming _ _ _ [] _ = return Nothing
formatStepsReplayStreaming useColor renderInput renderOutput steps emit = do
  let arrow = if useColor then dim "⇝ " else "⇝ "
      pointerIndent = "  "
      mStartIdx = findIndex (\s -> tsDepth s == 0) steps
      replay expr rest = do
        txt <- renderOutput expr
        let firstLine = arrow ++ txt
        emit firstLine
        (_, _, lastLine) <-
          replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput renderOutput
            (expr, txt, firstLine) rest emit
        return (Just lastLine)
  case mStartIdx of
    Just i ->
      let (before, topAndAfter) = splitAt i steps
      in case topAndAfter of
           [] -> return Nothing
           top:after ->
             let isSeq = case tsInput top of { Seq {} -> True; _ -> False }
             in
             if isSeq && not (null before)
               then replay (tsInput top) (before ++ after)  -- Seq: start from input, replay all
               else if not (null after) then replay (tsOutput top) after    -- expansion (e.g. factorial)
               else if not (null before) then replay (tsInput top) before  -- sub-eval (e.g. nested sum)
               else return Nothing                                          -- simple one-step result
    Nothing -> case steps of
      s : _ -> replay (tsInput s) steps

-- | Remove ANSI escape sequences for robust string comparisons.
stripAnsiForCompare :: String -- ^ Text that may contain ANSI escapes.
                    -> String -- ^ The same text with escape sequences removed.
stripAnsiForCompare [] = []
stripAnsiForCompare ('\ESC':'[':xs) = stripAnsiForCompare (dropAnsiCode xs)
stripAnsiForCompare (x:xs) = x : stripAnsiForCompare xs

-- | Drop one ANSI escape sequence from the front of a string.
dropAnsiCode :: String -- ^ Text beginning with an escape sequence.
             -> String -- ^ Text following that sequence.
dropAnsiCode [] = []
dropAnsiCode (y:ys)
  | isAsciiLetter y = ys
  | otherwise = dropAnsiCode ys
  where
    isAsciiLetter c = isAsciiUpper c || isAsciiLower c

-- | Streaming variant of 'replayUntilFixedPoint'.
--
-- Emits each newly formatted line immediately and keeps only minimal state
-- needed to continue replaying transitions.
replayUntilFixedPointStreaming :: Monad m
                               => Bool -- ^ Whether to emit ANSI color.
                               -> String -- ^ Prefix printed before each emitted line.
                               -> String -- ^ Indentation applied to continuation lines.
                               -> (Exp Ann -> m String) -- ^ Renderer for intermediate expressions.
                               -> (Exp Ann -> m String) -- ^ Renderer for final output values.
                               -> (Exp Ann, String, String)
                               -- ^ Current expression with its rendered and comparison texts.
                               -> [TraceStep] -- ^ Steps still available to apply.
                               -> (String -> m ()) -- ^ Sink each formatted line is written to.
                               -> m (Exp Ann, String, String)
                               -- ^ Final expression with its rendered and comparison texts.
replayUntilFixedPointStreaming _ _ _ _ _ state [] _ = return state
replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput renderOutput (current, currentText, lastLine) steps emit =
  case reduceBooleanMatchFirst current of
    Just (oldSub, newSub, nextTop) -> do
      oldSubText <- renderInput oldSub
      newSubText <- renderOutput newSub
      nextTopText <- renderInput nextTop
      let pointerLines = pointerLinesForColored useColor pointerIndent currentText oldSubText newSubText
          highlightedNext = highlightSubstring useColor newSubText nextTopText
          emitted = pointerLines ++ ["", arrow ++ highlightedNext]
      mapM_ emit emitted
      replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput renderOutput (nextTop, nextTopText, arrow ++ highlightedNext) steps emit
    _ -> case pickStep current steps of
      Nothing -> continueOrFallback (current, currentText, lastLine) steps
      Just (idx, step, next) -> do
        let matchedChild = fromMaybe (tsInput step) (findFirstChild (tsInput step) current)
            rest = removeAt idx steps
            (next', useOutputRender) = collapseSeqAfterStep next
            renderInput' = if useOutputRender then renderOutput else renderInput
        subInput <- renderSubInputForPointer currentText matchedChild
        subOutput <- renderOutput (tsOutput step)
        nextText <- renderInput' next'
        if stripAnsiForCompare nextText == stripAnsiForCompare currentText
          then replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput' renderOutput (next', nextText, lastLine) rest emit
          else do
            let pointerLines = pointerLinesForColored useColor pointerIndent currentText subInput subOutput
                highlightedNext = highlightSubstring useColor subOutput nextText
                emitted = pointerLines ++ ["", arrow ++ highlightedNext]
            mapM_ emit emitted
            replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput' renderOutput (next', nextText, arrow ++ highlightedNext) rest emit
  where
    renderSubInputForPointer curTxt subExpr = do
      base <- renderInput subExpr
      let baseNorm = stripAnsiForCompare base
          curNorm = stripAnsiForCompare curTxt
      let ann = annExp subExpr
          dummyName = ([], T.pack "_")
          bindExpr = Bind ann dummyName ann subExpr
      bindTxt <- renderInput bindExpr
      let marker = "için "
          markerLen = length marker
          alt =
            case findIndex (isPrefixOf marker) (tails bindTxt) of
              Just i -> drop (i + markerLen) bindTxt
              Nothing -> bindTxt
          altNorm = stripAnsiForCompare alt
          baseHit = length baseNorm >= 3 && baseNorm `isInfixOf` curNorm
          altHit = length altNorm >= 3 && altNorm `isInfixOf` curNorm
      case (baseHit, altHit) of
        (True, True) -> return (if length altNorm > length baseNorm then alt else base)
        (False, True) -> return alt
        _ -> return base

    continueOrFallback state@(cur, curText, _) restSteps =
      case findHeadFallback cur restSteps of
        Just (idx, oldSub, newSub, cur') -> do
          oldSubText <- renderInput oldSub
          newSubText <- renderOutput newSub
          curText' <- renderInput cur'
          let pointerLines = pointerLinesForColored useColor pointerIndent curText oldSubText newSubText
              highlightedCur = highlightSubstring useColor newSubText curText'
              emitted = pointerLines ++ ["", arrow ++ highlightedCur]
          mapM_ emit emitted
          replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput renderOutput (cur', curText', arrow ++ highlightedCur) (removeAt idx restSteps) emit
        Nothing -> reduceSeqFallback state restSteps
    findHeadFallback curExpr steps' =
      let candidates =
            [ (i, oldSub, newSub, cur')
            | (i, s) <- zip [0..] steps'
            , tsDepth s > 0
            , Just (oldSub, newSub, cur') <- [substituteFirstByHead (tsInput s) (tsOutput s) curExpr]
            ]
      in case candidates of
          x:_ -> Just x
          [] -> Nothing
    reduceSeqFallback state@(cur, curText, _) restSteps =
      case reduceSeqFirst cur of
        Nothing -> reduceBooleanFallback state restSteps
        Just (oldSub, newSub, cur') -> do
          oldSubText <- renderInput oldSub
          newSubText <- renderOutput newSub
          curText' <- renderInput cur'
          let pointerLines = pointerLinesForColored useColor pointerIndent curText oldSubText newSubText
              highlightedCur = highlightSubstring useColor newSubText curText'
              emitted = pointerLines ++ ["", arrow ++ highlightedCur]
          mapM_ emit emitted
          replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput renderOutput (cur', curText', arrow ++ highlightedCur) restSteps emit
    reduceBooleanFallback state@(cur, curText, _) restSteps =
      case reduceBooleanMatchFirst cur of
        Nothing -> return state
        Just (oldSub, newSub, cur') -> do
          oldSubText <- renderInput oldSub
          newSubText <- renderOutput newSub
          curText' <- renderInput cur'
          let pointerLines = pointerLinesForColored useColor pointerIndent curText oldSubText newSubText
              highlightedCur = highlightSubstring useColor newSubText curText'
              emitted = pointerLines ++ ["", arrow ++ highlightedCur]
          mapM_ emit emitted
          replayUntilFixedPointStreaming useColor arrow pointerIndent renderInput renderOutput (cur', curText', arrow ++ highlightedCur) restSteps emit
-- | Try to substitute by matching the "head" of application expressions.
-- This is a more lenient matching strategy used as a fallback.
--
-- Returns (old sub-expression, new sub-expression, updated parent expression).
substituteFirstByHead :: Exp Ann -- ^ Sub-expression to replace, matched by application head.
                      -> Exp Ann -- ^ Replacement sub-expression.
                      -> Exp Ann -- ^ Parent expression to rewrite.
                      -> Maybe (Exp Ann, Exp Ann, Exp Ann)
                      -- ^ The matched sub-expression, its replacement, and the rewritten parent.
substituteFirstByHead from to = go False
  where
    go allowRoot cur
      | allowRoot && sameHead from cur =
          let to' = copyCase cur to
          in Just (cur, to', to')
      | otherwise =
          case cur of
            App ann f args ->
              case go True f of
                Just (oldSub, newSub, f') -> Just (oldSub, newSub, App ann f' args)
                Nothing -> goArgs ann f [] args
            Match ann scr cls ->
              case go True scr of
                Just (oldSub, newSub, scr') -> Just (oldSub, newSub, Match ann scr' cls)
                Nothing -> goClauses ann scr [] cls
            Seq ann first second ->
              case go True first of
                Just (oldSub, newSub, first') -> Just (oldSub, newSub, Seq ann first' second)
                Nothing ->
                  case go True second of
                    Just (oldSub, newSub, second') -> Just (oldSub, newSub, Seq ann first second')
                    Nothing -> Nothing
            Bind ann n na be ->
              case go True be of
                Just (oldSub, newSub, be') -> Just (oldSub, newSub, Bind ann n na be')
                Nothing -> Nothing
            Let ann n body ->
              case go True body of
                Just (oldSub, newSub, body') -> Just (oldSub, newSub, Let ann n body')
                Nothing -> Nothing
            Ascribe ann ty e ->
              case go True e of
                Just (oldSub, newSub, e') -> Just (oldSub, newSub, Ascribe ann ty e')
                Nothing -> Nothing
            _ -> Nothing

    goArgs _ _ _ [] = Nothing
    goArgs ann f revPref (x:xs) =
      case go True x of
        Just (oldSub, newSub, x') ->
          let pref = reverse revPref
          in Just (oldSub, newSub, App ann f (pref ++ (x' : xs)))
        Nothing -> goArgs ann f (x : revPref) xs

    goClauses _ _ _ [] = Nothing
    goClauses ann scr revPref (Clause p b:rest) =
      case go True b of
        Just (oldSub, newSub, b') ->
          let pref = reverse revPref
          in Just (oldSub, newSub, Match ann scr (pref ++ (Clause p b' : rest)))
        Nothing -> goClauses ann scr (Clause p b : revPref) rest

    sameHead (App _ fromFn fromArgs) (App _ exprFn exprArgs) =
      eqTraceExp fromFn exprFn
        && length fromArgs == length exprArgs
        && (length fromArgs <= 1 || or (zipWith eqTraceExp fromArgs exprArgs))
    sameHead _ _ = False

-- | Find a trace step that matches the current expression.
-- Returns the step index, the step itself, and the expression after applying it.
--
-- Indexes are tracked explicitly with strict recursion to avoid allocation from
-- zipped index lists in the hot replay loop.
pickStep :: Exp Ann -- ^ Current expression.
         -> [TraceStep] -- ^ Steps still available to apply.
         -> Maybe (Int, TraceStep, Exp Ann)
         -- ^ Index of the applicable step, the step, and the expression after it.
pickStep current steps =
  case reduceTopBooleanMatch current of
    Just (_, nextTop) -> findMatchingNextTop 0 nextTop steps
    Nothing -> findFirstApplicable 0 steps
  where
    findFirstApplicable _ [] = Nothing
    findFirstApplicable !i (s:ss) =
      let (changed, next) = substituteFirstChild (tsInput s) (tsOutput s) current
      in if changed
           then Just (i, s, next)
           else findFirstApplicable (i + 1) ss

    findMatchingNextTop _ _ [] = Nothing
    findMatchingNextTop !i nextTop (s:ss) =
      let (changed, next) = substituteFirstChild (tsInput s) (tsOutput s) current
      in if changed && eqTraceExp next nextTop
           then Just (i, s, next)
           else findMatchingNextTop (i + 1) nextTop ss

-- | Remove element at the given index from a list.
--
-- This stays as one `splitAt` + concatenation because replay step lists are
-- typically short; keeping behavior simple here was faster to validate.
removeAt :: Int -- ^ Zero-based index to drop.
         -> [a] -- ^ List to shorten.
         -> [a] -- ^ List without that element.
removeAt idx xs =
  let (pref, rest) = splitAt idx xs
  in case rest of
       [] -> xs
       (_:suff) -> pref ++ suff

-- ============================================================================
-- Boolean Match Reduction
-- ============================================================================
-- Special handling for boolean conditional expressions (doğru/yanlış).
-- These can be reduced directly without trace steps.

-- | Reduce the first boolean conditional match found in pre-order traversal.
-- Returns (old expression, new expression, updated parent expression).
reduceBooleanMatchFirst :: Exp Ann -- ^ Expression to search.
                        -> Maybe (Exp Ann, Exp Ann, Exp Ann)
                        -- ^ The reduced sub-expression, its result, and the rewritten parent.
reduceBooleanMatchFirst expr =
  case expr of
    Match ann scr clauses ->
      case pickBoolClause scr clauses of
        Just body ->
          let body' = copyCase expr body
          in Just (expr, body', body')
        Nothing -> do
          (oldSub, newSub, scr') <- reduceBooleanMatchFirst scr
          return (oldSub, newSub, Match ann scr' clauses)
    App ann fn args ->
      case reduceBooleanMatchFirst fn of
        Just (oldSub, newSub, fn') -> Just (oldSub, newSub, App ann fn' args)
        Nothing -> do
          (oldSub, newSub, args') <- reduceInArgs args
          return (oldSub, newSub, App ann fn args')
    Seq ann first second ->
      case reduceBooleanMatchFirst first of
        Just (oldSub, newSub, first') -> Just (oldSub, newSub, Seq ann first' second)
        Nothing -> do
          (oldSub, newSub, second') <- reduceBooleanMatchFirst second
          return (oldSub, newSub, Seq ann first second')
    Bind ann n na be ->
      do
        (oldSub, newSub, be') <- reduceBooleanMatchFirst be
        return (oldSub, newSub, Bind ann n na be')
    Let ann n body ->
      do
        (oldSub, newSub, body') <- reduceBooleanMatchFirst body
        return (oldSub, newSub, Let ann n body')
    Ascribe ann ty e ->
      do
        (oldSub, newSub, e') <- reduceBooleanMatchFirst e
        return (oldSub, newSub, Ascribe ann ty e')
    _ -> Nothing
  where
    reduceInArgs [] = Nothing
    reduceInArgs (x:xs) =
      case reduceBooleanMatchFirst x of
        Just (oldSub, newSub, x') -> Just (oldSub, newSub, x' : xs)
        Nothing -> do
          (oldSub, newSub, xs') <- reduceInArgs xs
          return (oldSub, newSub, x : xs')

-- | Choose a clause body when scrutinee is a boolean constructor.
-- Matches patterns against doğru (true) or yanlış (false).
pickBoolClause :: Exp Ann -- ^ Scrutinee, expected to be a boolean constructor.
               -> [Clause Ann] -- ^ Clauses, tried in order.
               -> Maybe (Exp Ann) -- ^ Body of the first clause that matches.
pickBoolClause scr clauses = do
  b <- boolValue scr
  let matchClause [] = Nothing
      matchClause (Clause pat body:rest) =
        if patMatchesBool b pat then Just body else matchClause rest
  matchClause clauses

-- | Extract boolean value from an expression if it's a boolean constructor.
boolValue :: Exp Ann -- ^ Expression to classify.
          -> Maybe Bool -- ^ Its boolean value, when it is a boolean constructor.
boolValue exp' =
  case exp' of
    Var _ varName cands ->
      let names = map fst cands
      in if isTrueIdent varName || any isTrueIdent names
           then Just True
           else if isFalseIdent varName || any isFalseIdent names
             then Just False
             else Nothing
    _ -> Nothing

-- | Check if a pattern matches a boolean value.
-- Wildcards and variables match anything; constructor patterns must match.
patMatchesBool :: Bool -- ^ Boolean value being matched.
               -> Pat Ann -- ^ Pattern to test.
               -> Bool -- ^ 'True' when the pattern accepts that value.
patMatchesBool _ (PWildcard _) = True
patMatchesBool _ (PVar _ _) = True
patMatchesBool b (PCtor (ctor, _) _) =
  if b then isTrueIdent ctor else isFalseIdent ctor
patMatchesBool _ _ = False

-- | Check if an identifier is "doğru" (true).
isTrueIdent :: Identifier -- ^ Constructor name to classify.
            -> Bool -- ^ 'True' for the boolean true constructor.
isTrueIdent (_, w) = w == T.pack "doğru"

-- | Check if an identifier is "yanlış" (false).
isFalseIdent :: Identifier -- ^ Constructor name to classify.
             -> Bool -- ^ 'True' for the boolean false constructor.
isFalseIdent (_, w) = w == T.pack "yanlış"

-- | Reduce a boolean match only if the entire expression is a conditional.
-- This is more conservative than reduceBooleanMatchFirst.
reduceTopBooleanMatch :: Exp Ann -- ^ Expression, reduced only if it is itself a conditional.
                      -> Maybe (Exp Ann, Exp Ann) -- ^ The original expression and its result.
reduceTopBooleanMatch exp' =
  case exp' of
    Match _ scr clauses -> do
      body <- pickBoolClause scr clauses
      return (exp', body)
    _ -> Nothing

-- ============================================================================
-- Seq/Bind Reduction
-- ============================================================================
-- Special handling for sequential composition (Seq/Bind) in the replay.
-- When a Bind's expression has been fully evaluated, we can substitute it
-- into the continuation body.

-- | Check if an expression is a fully-evaluated trace value.
isTraceValue :: Exp Ann -- ^ Expression to classify.
             -> Bool -- ^ 'True' when it needs no further reduction.
isTraceValue (IntLit _ _)   = True
isTraceValue (FloatLit _ _) = True
isTraceValue (StrLit _ _)   = True
isTraceValue (CharLit _ _)  = True
isTraceValue Var {}          = True
isTraceValue _              = False

-- | Substitute a single bind variable in an expression.
-- Replaces occurrences of @name@ with @value@, preserving the variable's
-- case annotation on the substituted value.
substituteBindVar :: Identifier -- ^ Bound name to replace.
                  -> Exp Ann -- ^ Value substituted for it.
                  -> Exp Ann -- ^ Expression to rewrite.
                  -> Exp Ann -- ^ Expression with the name replaced, keeping each site's case.
substituteBindVar name value = go
  where
    go expr = case expr of
      Var _ vName candidates
        | vName == name || any (\(c, _) -> c == name) candidates ->
            copyCase expr value
        | otherwise -> expr
      App ann f args     -> App ann (go f) (map go args)
      Match ann scr cls  -> Match ann (go scr) (map goClause cls)
      Seq ann f s        -> Seq ann (go f) (go s)
      Bind ann nm na be  -> Bind ann nm na (go be)
      Let ann nm b       -> Let ann nm (go b)
      Ascribe ann ty e   -> Ascribe ann ty (go e)
      _                  -> expr
    goClause (Clause p e) = Clause p (go e)

-- | Reduce the first reducible Seq found in pre-order traversal.
-- Returns (old expression, new expression, updated parent expression).
--
-- Two reduction rules:
--   1. @Seq (Bind x value) body@ where @value@ is a trace value
--      → substitute @x → value@ in @body@
--   2. @Seq first second@ where @first@ is a trace value (e.g. bitimlik)
--      → @second@
reduceSeqFirst :: Exp Ann -- ^ Expression to search.
               -> Maybe (Exp Ann, Exp Ann, Exp Ann)
               -- ^ The reduced sequence, its result, and the rewritten parent.
reduceSeqFirst expr =
  case expr of
    Seq _ (Bind _ bName _ bExp) second
      | isTraceValue bExp ->
          let result = copyCase expr (substituteBindVar bName bExp second)
          in Just (expr, result, result)
    Seq _ f second
      | isTraceValue f ->
          let result = second
          in Just (expr, result, result)
    -- Recurse into sub-expressions
    App ann fn args ->
      case reduceSeqFirst fn of
        Just (oldSub, newSub, fn') -> Just (oldSub, newSub, App ann fn' args)
        Nothing -> do
          (oldSub, newSub, args') <- reduceInArgs args
          return (oldSub, newSub, App ann fn args')
    Match ann scr clauses ->
      case reduceSeqFirst scr of
        Just (oldSub, newSub, scr') -> Just (oldSub, newSub, Match ann scr' clauses)
        Nothing -> Nothing
    Seq ann f s ->
      case reduceSeqFirst f of
        Just (oldSub, newSub, f') -> Just (oldSub, newSub, Seq ann f' s)
        Nothing -> do
          (oldSub, newSub, s') <- reduceSeqFirst s
          return (oldSub, newSub, Seq ann f s')
    Bind ann nm na be -> do
      (oldSub, newSub, be') <- reduceSeqFirst be
      return (oldSub, newSub, Bind ann nm na be')
    Let ann nm b -> do
      (oldSub, newSub, b') <- reduceSeqFirst b
      return (oldSub, newSub, Let ann nm b')
    Ascribe ann ty e -> do
      (oldSub, newSub, e') <- reduceSeqFirst e
      return (oldSub, newSub, Ascribe ann ty e')
    _ -> Nothing
  where
    reduceInArgs [] = Nothing
    reduceInArgs (x:xs) =
      case reduceSeqFirst x of
        Just (oldSub, newSub, x') -> Just (oldSub, newSub, x' : xs)
        Nothing -> do
          (oldSub, newSub, xs') <- reduceInArgs xs
          return (oldSub, newSub, x : xs')

-- | Collapse a top-level sequence once after a regular step replay.
-- Returns (collapsed expression, render collapsed step with output renderer).
collapseSeqAfterStep :: Exp Ann -- ^ Expression after a replayed step.
                     -> (Exp Ann, Bool) -- ^ The collapsed expression, and whether it should be
                     -- rendered with the output renderer.
collapseSeqAfterStep expr =
  case expr of
    Seq _ (Bind _ _ _ bindExp) _ | isTraceValue bindExp ->
      case reduceSeqFirst expr of
        Just (_, _, collapsed) -> (collapsed, False)
        Nothing -> (expr, False)
    Seq _ first _ | isTraceValue first ->
      case reduceSeqFirst expr of
        Just (_, _, collapsed) -> (collapsed, True)
        Nothing -> (expr, False)
    _ -> (expr, False)


-- | Strip Turkish copula suffixes from rendered trace text using TRmorph.
-- Uses morphological analysis to identify copulas, then strips them manually.
-- Used only by :steps output.
--
-- Example: "toplamıdır" → "toplamı"
--   Analysis: toplam<N><p3s><0><V><cpl:pres><3s><dir>
--   Contains copula: <0><V><cpl:...> → strip suffix
--
-- Optimized with batching and caching:
-- - Collects all words and makes a single upsCachedBatch call
-- - Checks cache first, only fetches uncached words
-- - Works with String, only converts to Text at TRmorph boundary
stripStepsCopulaTRmorph :: RenderCache -- ^ Memoized morphological analyses.
                        -> FSM -- ^ Morphological analyzer.
                        -> String -- ^ Rendered trace line.
                        -> IO String -- ^ The same line with copula suffixes removed.
stripStepsCopulaTRmorph cache fsm s = do
  let segments = segmentTextStr s
      words = [w | (True, w) <- segments]

  -- Batch fetch with caching (hits cache, batches misses)
  allAnalyses <- Kip.Render.upsCachedBatch cache fsm (map T.pack words)

  -- Build analysis map
  let analysisMap = Map.fromList (zip words allAnalyses)

  -- Process segments with analysis map
  concat <$> mapM (processSegment analysisMap) segments
  where
    -- Segment string into words and non-words (String version)
    segmentTextStr :: String -> [(Bool, String)]
    segmentTextStr [] = []
    segmentTextStr str =
      let (word, rest) = span isWordCharTR str
      in if null word
           then let (nonWord, rest') = break isWordCharTR str
                in (False, nonWord) : segmentTextStr rest'
           else (True, word) : segmentTextStr rest
      where
        -- Note: Using U+2019 (right single quotation mark) same as original
        isWordCharTR ch = isAlpha ch || isDigit ch || ch == '\'' || ch == '\x2019'

    processSegment :: Map.Map String [Text] -> (Bool, String) -> IO String
    processSegment _ (False, nonWord) = return nonWord
    processSegment analysisMap (True, word) = do
      let analyses = Map.findWithDefault [] word analysisMap
          hasCopula = any (T.isInfixOf "<0><V><cpl:") analyses
          hasLexicalVerb =
            any (\a -> T.isInfixOf "<V>" a && not (T.isInfixOf "<0><V><cpl:" a)) analyses
      return $ if hasCopula && not hasLexicalVerb
               then stripCopulaSuffixManual word
               else word

    -- Manual copula suffix stripping (String version)
    stripCopulaSuffixManual :: String -> String
    stripCopulaSuffixManual w =
      let suffixes = ["dır", "dir", "dur", "dür", "tır", "tir", "tur", "tür"]
          wLower = map toLower w
          match = find (`isSuffixOf` wLower) suffixes
      in case match of
           Just suf -> take (length w - length suf) w
           Nothing -> w


-- ============================================================================
-- String Utilities
-- ============================================================================

-- | Find the starting position of a substring in a string.
-- Returns Nothing if the substring is not found.
findSubstring :: String -- ^ Substring to search for.
              -> String -- ^ String to search in.
              -> Maybe Int -- ^ Zero-based index of the first occurrence.
findSubstring sub = go 0
  where
    go _ [] = Nothing
    go idx s@(_:rest)
      | sub `isPrefixOf` s = Just idx
      | otherwise = go (idx + 1) rest

-- | Build underline/result lines for a highlighted sub-expression.
-- Creates visual pointers using Unicode box-drawing characters (└─┘)
-- to show which part of a larger expression is being evaluated.
--
-- If the rendered sub-expression is wrapped in one outer parenthesis pair,
-- prefer highlighting its inner text.
--
-- When color is enabled:
--   - Underline characters (└─┘) are rendered in dim gray
--   - Result text is rendered in blue
pointerLinesForColored :: Bool -- ^ Whether to emit ANSI color.
                       -> String -- ^ Prefix printed before each line.
                       -> String -- ^ Full rendered expression.
                       -> String -- ^ Rendered sub-expression to underline.
                       -> String -- ^ Rendered result of reducing it.
                       -> [String] -- ^ Underline line and result line.
pointerLinesForColored useColor pointerIndent wholeText subText resultText =
  case findNeedlePosition of
    Nothing -> []
    Just (ix, needle) ->
      let subLen = length needle
      in if ix == 0 && subLen == length wholeText
           then []  -- Don't show pointer if the whole expression is highlighted
           else if subLen >= 3
                  then buildLongPointer ix subLen
                  else buildShortPointer ix
  where
    -- Find the position of the sub-expression in the whole text
    findNeedlePosition =
      let stripped = stripOuterParens subText
          candidates = nub [stripped, subText]
          withPos = mapMaybe (\cand -> fmap (, cand) (findSubstring cand wholeText)) candidates
      in listToMaybe withPos

    stripOuterParens ('(':rest) =
      case reverse rest of
        ')':middleRev -> reverse middleRev
        _ -> '(' : rest
    stripOuterParens s = s

    -- Build pointer for sub-expressions of length >= 3
    buildLongPointer ix subLen =
      let boxDrawing = "└" ++ replicate (subLen - 2) '─' ++ "┘"
          underline = pointerIndent ++ replicate ix ' ' ++ applyColor boxDrawing dim
          resultStart =
            let gap = subLen - length resultText
                centered = if gap <= 2 then 0 else gap `div` 2
            in ix + max 0 centered
          result = pointerIndent ++ replicate resultStart ' ' ++ applyColor resultText blue
      in [underline, result]

    -- Build pointer for short sub-expressions
    buildShortPointer ix =
      [pointerIndent ++ replicate ix ' ' ++ applyColor resultText blue]

    applyColor text colorFn = if useColor then colorFn text else text

-- | Highlight a substring in blue within a larger text, if found.
-- Used to keep evaluated sub-expressions highlighted when they appear
-- in the next evaluation step.
highlightSubstring :: Bool -- ^ Whether to emit ANSI color.
                   -> String -- ^ Substring to highlight.
                   -> String -- ^ Text to highlight within.
                   -> String -- ^ Text with the first occurrence colored, if present.
highlightSubstring useColor needle haystack
  | not useColor = haystack
  | otherwise =
      case findSubstring needle haystack of
        Nothing -> haystack
        Just ix ->
          let (before, rest) = splitAt ix haystack
              (match, after) = splitAt (length needle) rest
          in before ++ blue match ++ after



-- ============================================================================
-- Expression Substitution and Equality
-- ============================================================================

-- | Replace only the first matching sub-expression in pre-order traversal.
-- Returns (changed flag, updated expression).
--
-- The 'changed' flag indicates whether a substitution was made.
substituteFirstChild :: Exp Ann -- ^ Sub-expression to replace.
                     -> Exp Ann -- ^ Replacement sub-expression.
                     -> Exp Ann -- ^ Expression to rewrite.
                     -> (Bool, Exp Ann) -- ^ Whether a replacement happened, and the result.
substituteFirstChild from to expr
  | eqTraceExp from expr = (True, copyCase expr to)
  | otherwise =
      case expr of
        App ann fn args ->
          let (cf, fn') = substituteFirstChild from to fn
          in if cf
               then (True, App ann fn' args)
               else
                 let (ca, args') = substArgs args
                 in (ca, App ann fn args')
        Match ann scr cls ->
          let (cs, scr') = substituteFirstChild from to scr
          in if cs
               then (True, Match ann scr' cls)
               else
                 let (cc, cls') = substClauses cls
                 in (cc, Match ann scr cls')
        Seq ann first second ->
          let (c1, first') = substituteFirstChild from to first
          in if c1
               then (True, Seq ann first' second)
               else
                 let (c2, second') = substituteFirstChild from to second
                 in (c2, Seq ann first second')
        Bind ann n na be ->
          let (c, be') = substituteFirstChild from to be
          in (c, Bind ann n na be')
        Let ann n body ->
          let (c, body') = substituteFirstChild from to body
          in (c, Let ann n body')
        Ascribe ann ty e ->
          let (c, e') = substituteFirstChild from to e
          in (c, Ascribe ann ty e')
        _ -> (False, expr)
  where
    substArgs [] = (False, [])
    substArgs (x:xs) =
      let (c, x') = substituteFirstChild from to x
      in if c
           then (True, x' : xs)
           else
             let (cr, xs') = substArgs xs
             in (cr, x : xs')

    substClauses [] = (False, [])
    substClauses (Clause p b:xs) =
      let (c, b') = substituteFirstChild from to b
      in if c
           then (True, Clause p b' : xs)
           else
             let (cr, xs') = substClauses xs
             in (cr, Clause p b : xs')

-- | Find the first child expression matching via 'eqTraceExp'.
-- Returns the child from the parent tree so its case annotation is preserved.
findFirstChild :: Exp Ann -- ^ Sub-expression to look for.
               -> Exp Ann -- ^ Expression to search.
               -> Maybe (Exp Ann) -- ^ The matching child as it appears in the parent.
findFirstChild from expr
  | eqTraceExp from expr = Just expr
  | otherwise = case expr of
      App _ fn args ->
        findFirstChild from fn <|>
        foldr ((<|>) . findFirstChild from) Nothing args
      Match _ scr cls ->
        findFirstChild from scr <|>
        foldr ((<|>) . findInClause) Nothing cls
      Seq _ a b -> findFirstChild from a <|> findFirstChild from b
      Bind _ _ _ e -> findFirstChild from e
      Let _ _ e -> findFirstChild from e
      Ascribe _ _ e -> findFirstChild from e
      _ -> Nothing
  where
    findInClause (Clause _ body) = findFirstChild from body

-- | Structural equality for trace expressions.
-- More lenient than standard equality - ignores annotations and handles
-- Turkish case variants of the same variable.
--
-- This is important because the same logical expression may appear with
-- different case markings (nominative, accusative, etc.) in different contexts.
eqTraceExp :: Exp Ann -- ^ First expression.
           -> Exp Ann -- ^ Second expression.
           -> Bool -- ^ 'True' when they are equal ignoring annotations and case.
eqTraceExp a b =
  case (a, b) of
    (Var _ n1 c1, Var _ n2 c2) ->
      n1 == n2
        || n1 `elem` map fst c2
        || n2 `elem` map fst c1
        || any (\(cand, _) -> cand `elem` map fst c2) c1
    (App _ f1 a1, App _ f2 a2) ->
      eqTraceExp f1 f2 && length a1 == length a2 && and (zipWith eqTraceExp a1 a2)
    (IntLit _ n1, IntLit _ n2) -> n1 == n2
    (FloatLit _ n1, FloatLit _ n2) -> n1 == n2
    (StrLit _ s1, StrLit _ s2) -> s1 == s2
    (CharLit _ c1, CharLit _ c2) -> c1 == c2
    (Bind _ n1 _ e1, Bind _ n2 _ e2) -> n1 == n2 && eqTraceExp e1 e2
    (Seq _ f1 s1, Seq _ f2 s2) -> eqTraceExp f1 f2 && eqTraceExp s1 s2
    (Match _ sc1 cl1, Match _ sc2 cl2) ->
      eqTraceExp sc1 sc2
        && length cl1 == length cl2
        && and (zipWith eqClause cl1 cl2)
    (Let _ n1 b1, Let _ n2 b2) -> n1 == n2 && eqTraceExp b1 b2
    (Ascribe _ t1 e1, Ascribe _ t2 e2) -> t1 == t2 && eqTraceExp e1 e2
    _ -> False
  where
    eqClause (Clause p1 e1) (Clause p2 e2) = p1 == p2 && eqTraceExp e1 e2

-- ============================================================================
-- Case Annotation Helpers
-- ============================================================================

-- | Reset the outermost expression annotation to nominative case.
-- This prevents evaluated results from carrying stale case annotations.
--
-- Example: if evaluating "f x" in instrumental case produces "5",
-- the result should be "5" in nominative, not "5" in instrumental.
setTopCaseNom :: Exp Ann -- ^ Expression whose outermost annotation is reset.
              -> Exp Ann -- ^ The same expression in the nominative case.
setTopCaseNom e = case e of
  Var ann n c       -> Var (setAnnCase ann Nom) n c
  App ann f a       -> App (setAnnCase ann Nom) f a
  IntLit ann n      -> IntLit (setAnnCase ann Nom) n
  FloatLit ann n    -> FloatLit (setAnnCase ann Nom) n
  StrLit ann s      -> StrLit (setAnnCase ann Nom) s
  CharLit ann c     -> CharLit (setAnnCase ann Nom) c
  Bind ann n na e'  -> Bind (setAnnCase ann Nom) n na e'
  Seq ann f s       -> Seq (setAnnCase ann Nom) f s
  Match ann sc cl   -> Match (setAnnCase ann Nom) sc cl
  Let ann n b       -> Let (setAnnCase ann Nom) n b
  Ascribe ann t e'  -> Ascribe (setAnnCase ann Nom) t e'

-- | Copy the case annotation from one expression to another.
-- Preserves the grammatical case context when substituting expressions.
copyCase :: Exp Ann -- ^ Expression supplying the case.
         -> Exp Ann -- ^ Expression to re-annotate.
         -> Exp Ann -- ^ The second expression carrying the first one's case.
copyCase from to =
  let cas = annCase (annExp from)
  in case to of
    Var ann n c       -> Var (setAnnCase ann cas) n c
    App ann f a       -> App (setAnnCase ann cas) f a
    IntLit ann n      -> IntLit (setAnnCase ann cas) n
    FloatLit ann n    -> FloatLit (setAnnCase ann cas) n
    StrLit ann s      -> StrLit (setAnnCase ann cas) s
    CharLit ann c     -> CharLit (setAnnCase ann cas) c
    Bind ann n na e'  -> Bind (setAnnCase ann cas) n na e'
    Seq ann f s       -> Seq (setAnnCase ann cas) f s
    Match ann sc cl   -> Match (setAnnCase ann cas) sc cl
    Let ann n b       -> Let (setAnnCase ann cas) n b
    Ascribe ann t e'  -> Ascribe (setAnnCase ann cas) t e'

-- | Render a message or fall back to a generic error.
