{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Repl.Steps
  ( formatSteps
  , formatStepsStreaming
  , stripStepsCopulaTRmorph
  , shouldSkipInfinitiveSteps
  , setTopCaseNom
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Char (isAlpha, isAsciiLower, isAsciiUpper, isDigit, toLower)
import qualified Data.Map.Strict as Map
import Data.List (find, findIndex, foldl', intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, splitAt, tails)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Language.Foma (FSM)
import System.Console.Chalk (blue, dim)

import Kip.AST
import Kip.Eval (TraceStep(..))
import Kip.Render (RenderCache)
import qualified Kip.Render

-- | Format trace steps for display using Unicode box drawing.
-- This is the top-level entry point for the :steps command output.
--
-- Takes a list of trace steps and formats them to show the evaluation
-- process with visual pointers (└─┘) indicating which sub-expressions
-- are being evaluated at each step.
formatSteps :: Bool
            -> (Exp Ann -> IO String) -- ^ Preserving-case renderer (for inputs).
            -> (Exp Ann -> IO String) -- ^ Nominative renderer (for outputs).
            -> Exp Ann                -- ^ Final evaluated expression.
            -> [TraceStep] -> IO String
formatSteps useColor renderInput renderOutput finalExp steps = do
  outRef <- newIORef []
  formatStepsStreaming useColor renderInput renderOutput finalExp steps (\line ->
    modifyIORef' outRef (line :))
  rendered <- readIORef outRef
  return (intercalate "\n" (reverse rendered))

-- | Stream trace steps as soon as they are formatted.
--
-- Unlike 'formatSteps', this avoids building one large intermediate string
-- before printing. It is intended for interactive REPL output where users
-- should see step rendering progress incrementally.
formatStepsStreaming :: Monad m
                     => Bool
                     -> (Exp Ann -> m String) -- ^ Preserving-case renderer (for inputs).
                     -> (Exp Ann -> m String) -- ^ Nominative renderer (for outputs).
                     -> Exp Ann               -- ^ Final evaluated expression.
                     -> [TraceStep]
                     -> (String -> m ())      -- ^ Output sink (e.g. @outputStrLn@).
                     -> m ()
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
shouldSkipInfinitiveSteps :: RenderCache -> FSM -> Exp Ann -> IO Bool
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

-- | Format steps by replaying trace transitions.
-- Finds the starting expression and then replays each evaluation step,
-- showing how sub-expressions are reduced.
formatStepsReplay :: Bool
                  -> (Exp Ann -> IO String)
                  -> (Exp Ann -> IO String)
                  -> [TraceStep]
                  -> IO String
formatStepsReplay useColor renderInput renderOutput steps = do
  outRef <- newIORef []
  _ <- formatStepsReplayStreaming useColor renderInput renderOutput steps (\line ->
    modifyIORef' outRef (line :))
  rendered <- readIORef outRef
  return (intercalate "\n" (reverse rendered))

-- | Stream replayed trace transitions and return the last emitted line.
formatStepsReplayStreaming :: Monad m
                           => Bool
                           -> (Exp Ann -> m String)
                           -> (Exp Ann -> m String)
                           -> [TraceStep]
                           -> (String -> m ())
                           -> m (Maybe String)
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
stripAnsiForCompare :: String -> String
stripAnsiForCompare [] = []
stripAnsiForCompare ('\ESC':'[':xs) = stripAnsiForCompare (dropAnsiCode xs)
stripAnsiForCompare (x:xs) = x : stripAnsiForCompare xs

dropAnsiCode :: String -> String
dropAnsiCode [] = []
dropAnsiCode (y:ys)
  | isAsciiLetter y = ys
  | otherwise = dropAnsiCode ys
  where
    isAsciiLetter c = isAsciiUpper c || isAsciiLower c

-- | Repeatedly apply the shallowest matching step until no step applies.
-- This is the core algorithm that reconstructs the evaluation trace.
--
-- The algorithm:
-- 1. Try to reduce boolean matches directly in the current expression
-- 2. Otherwise, find a matching trace step and apply it
-- 3. Show visual pointers indicating what changed
-- 4. Recursively process remaining steps
replayUntilFixedPoint :: Bool
                      -> String
                      -> String
                      -> (Exp Ann -> IO String)
                      -> (Exp Ann -> IO String)
                      -> (Exp Ann, String, [String])
                      -> [TraceStep]
                      -> IO (Exp Ann, String, [String])
replayUntilFixedPoint _ _ _ _ _ state [] = return state
replayUntilFixedPoint useColor arrow pointerIndent renderInput renderOutput (current, currentText, accLines) steps =
  case reduceBooleanMatchFirst current of
    Just (oldSub, newSub, nextTop) -> do
      oldSubText <- renderInput oldSub
      newSubText <- renderOutput newSub
      nextTopText <- renderInput nextTop
      let pointerLines = pointerLinesForColored useColor pointerIndent currentText oldSubText newSubText
          highlightedNext = highlightSubstring useColor newSubText nextTopText
          sep = [""]
          newLines = accLines ++ pointerLines ++ sep ++ [arrow ++ highlightedNext]
      replayUntilFixedPoint useColor arrow pointerIndent renderInput renderOutput (nextTop, nextTopText, newLines) steps
    _ -> case pickStep current currentText renderInput steps of
      Nothing -> continueOrFallback (current, currentText, accLines) steps
      Just (idx, step, next) -> do
        let matchedChild = fromMaybe (tsInput step) (findFirstChild (tsInput step) current)
            rest = removeAt idx steps
            (next', useOutputRender) = collapseSeqAfterStep next
            renderInput' = if useOutputRender then renderOutput else renderInput
        subInput <- renderSubInputForPointer currentText matchedChild
        subOutput <- renderOutput (tsOutput step)
        nextText <- renderInput' next'
        if stripAnsiForCompare nextText == stripAnsiForCompare currentText
          then replayUntilFixedPoint useColor arrow pointerIndent renderInput' renderOutput (next', nextText, accLines) rest
          else do
            let pointerLines = pointerLinesForColored useColor pointerIndent currentText subInput subOutput
                highlightedNext = highlightSubstring useColor subOutput nextText
                sep = [""]
                newLines = accLines ++ pointerLines ++ sep ++ [arrow ++ highlightedNext]
            replayUntilFixedPoint useColor arrow pointerIndent renderInput' renderOutput (next', nextText, newLines) rest
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

    continueOrFallback state@(cur, curText, linesAcc) restSteps =
      case findHeadFallback cur restSteps of
        Just (idx, oldSub, newSub, cur') -> do
          oldSubText <- renderInput oldSub
          newSubText <- renderOutput newSub
          curText' <- renderInput cur'
          let pointerLines = pointerLinesForColored useColor pointerIndent curText oldSubText newSubText
              highlightedCur = highlightSubstring useColor newSubText curText'
              sep = [""]
              linesAcc' = linesAcc ++ pointerLines ++ sep ++ [arrow ++ highlightedCur]
          replayUntilFixedPoint useColor arrow pointerIndent renderInput renderOutput (cur', curText', linesAcc') (removeAt idx restSteps)
        Nothing -> reduceSeqFallback state restSteps
    reduceSeqFallback state@(cur, curText, linesAcc) restSteps =
      case reduceSeqFirst cur of
        Nothing -> reduceBooleanFallback state restSteps
        Just (oldSub, newSub, cur') -> do
          oldSubText <- renderInput oldSub
          newSubText <- renderOutput newSub
          curText' <- renderInput cur'
          let pointerLines = pointerLinesForColored useColor pointerIndent curText oldSubText newSubText
              highlightedCur = highlightSubstring useColor newSubText curText'
              sep = [""]
              linesAcc' = linesAcc ++ pointerLines ++ sep ++ [arrow ++ highlightedCur]
          replayUntilFixedPoint useColor arrow pointerIndent renderInput renderOutput (cur', curText', linesAcc') restSteps
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
    reduceBooleanFallback state@(cur, curText, linesAcc) restSteps =
      case reduceBooleanMatchFirst cur of
        Nothing -> return state
        Just (oldSub, newSub, cur') -> do
          oldSubText <- renderInput oldSub
          newSubText <- renderOutput newSub
          curText' <- renderInput cur'
          let pointerLines = pointerLinesForColored useColor pointerIndent curText oldSubText newSubText
              highlightedCur = highlightSubstring useColor newSubText curText'
              sep = [""]
              linesAcc' = linesAcc ++ pointerLines ++ sep ++ [arrow ++ highlightedCur]
          replayUntilFixedPoint useColor arrow pointerIndent renderInput renderOutput (cur', curText', linesAcc') restSteps

-- | Streaming variant of 'replayUntilFixedPoint'.
--
-- Emits each newly formatted line immediately and keeps only minimal state
-- needed to continue replaying transitions.
replayUntilFixedPointStreaming :: Monad m
                               => Bool
                               -> String
                               -> String
                               -> (Exp Ann -> m String)
                               -> (Exp Ann -> m String)
                               -> (Exp Ann, String, String)
                               -> [TraceStep]
                               -> (String -> m ())
                               -> m (Exp Ann, String, String)
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
    _ -> case pickStep current currentText renderInput steps of
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

    continueOrFallback state@(cur, curText, stateLastLine) restSteps =
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
    reduceSeqFallback state@(cur, curText, stateLastLine) restSteps =
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
    reduceBooleanFallback state@(cur, curText, stateLastLine) restSteps =
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
substituteFirstByHead :: Exp Ann -> Exp Ann -> Exp Ann -> Maybe (Exp Ann, Exp Ann, Exp Ann)
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

    goArgs ann f revPref [] = Nothing
    goArgs ann f revPref (x:xs) =
      case go True x of
        Just (oldSub, newSub, x') ->
          let pref = reverse revPref
          in Just (oldSub, newSub, App ann f (pref ++ (x' : xs)))
        Nothing -> goArgs ann f (x : revPref) xs

    goClauses ann scr revPref [] = Nothing
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
pickStep :: Exp Ann
         -> String
         -> (Exp Ann -> a)
         -> [TraceStep]
         -> Maybe (Int, TraceStep, Exp Ann)
pickStep current _currentText _renderInput steps =
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
removeAt :: Int -> [a] -> [a]
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
reduceBooleanMatchFirst :: Exp Ann -> Maybe (Exp Ann, Exp Ann, Exp Ann)
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
pickBoolClause :: Exp Ann -> [Clause Ann] -> Maybe (Exp Ann)
pickBoolClause scr clauses = do
  b <- boolValue scr
  let matchClause [] = Nothing
      matchClause (Clause pat body:rest) =
        if patMatchesBool b pat then Just body else matchClause rest
  matchClause clauses

-- | Extract boolean value from an expression if it's a boolean constructor.
boolValue :: Exp Ann -> Maybe Bool
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
patMatchesBool :: Bool -> Pat Ann -> Bool
patMatchesBool _ (PWildcard _) = True
patMatchesBool _ (PVar _ _) = True
patMatchesBool b (PCtor (ctor, _) _) =
  if b then isTrueIdent ctor else isFalseIdent ctor
patMatchesBool _ _ = False

-- | Check if an identifier is "doğru" (true).
isTrueIdent :: Identifier -> Bool
isTrueIdent (_, w) = w == T.pack "doğru"

-- | Check if an identifier is "yanlış" (false).
isFalseIdent :: Identifier -> Bool
isFalseIdent (_, w) = w == T.pack "yanlış"

-- | Reduce a boolean match only if the entire expression is a conditional.
-- This is more conservative than reduceBooleanMatchFirst.
reduceTopBooleanMatch :: Exp Ann -> Maybe (Exp Ann, Exp Ann)
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
isTraceValue :: Exp Ann -> Bool
isTraceValue (IntLit _ _)   = True
isTraceValue (FloatLit _ _) = True
isTraceValue (StrLit _ _)   = True
isTraceValue (CharLit _ _)  = True
isTraceValue Var {}          = True
isTraceValue _              = False

-- | Substitute a single bind variable in an expression.
-- Replaces occurrences of @name@ with @value@, preserving the variable's
-- case annotation on the substituted value.
substituteBindVar :: Identifier -> Exp Ann -> Exp Ann -> Exp Ann
substituteBindVar name value = go
  where
    go expr = case expr of
      Var ann vName candidates
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
reduceSeqFirst :: Exp Ann -> Maybe (Exp Ann, Exp Ann, Exp Ann)
reduceSeqFirst expr =
  case expr of
    Seq ann (Bind _ bName _ bExp) second
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
collapseSeqAfterStep :: Exp Ann -> (Exp Ann, Bool)
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

-- | Format steps grouped by top-level evaluation.
formatStepsGrouped :: Bool
                   -> (Exp Ann -> IO String)
                   -> (Exp Ann -> IO String)
                   -> [(Int, TraceStep)]
                   -> IO String
formatStepsGrouped useColor renderInput renderOutput steps = do
  groups <- groupByTopLevel steps
  formattedGroups <- mapM (formatGroup useColor renderInput renderOutput) groups
  let nonEmpty = filter (not . null) formattedGroups
      deduped = dedupeGroupBoundaries nonEmpty
  return (intercalate "\n\n" deduped)

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
stripStepsCopulaTRmorph :: RenderCache -> FSM -> String -> IO String
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

-- | Strip Turkish copula suffixes from rendered trace text.
-- Used only by :steps output.
stripStepsCopula :: String -> String
stripStepsCopula [] = []
stripStepsCopula s@(c:cs)
  | isWordChar c =
      let (w, rest) = span isWordChar s
      in stripWordCopula w ++ stripStepsCopula rest
  | otherwise = c : stripStepsCopula cs
  where
    isWordChar ch = isAlpha ch || isDigit ch || ch == '\'' || ch == '’'

    stripWordCopula w =
      fromMaybe w (firstMatch copulaSuffixes)
      where
        firstMatch [] = Nothing
        firstMatch (suf:sufs)
          | suf `isSuffixOf` w && length w > length suf =
              Just (take (length w - length suf) w)
          | otherwise = firstMatch sufs

    copulaSuffixes =
      [ "dır", "dir", "dur", "dür"
      , "tır", "tir", "tur", "tür"
      ]

-- | Remove duplicated boundary lines where a group's first line repeats
-- the previous group's last line.
dedupeGroupBoundaries :: [String] -> [String]
dedupeGroupBoundaries [] = []
dedupeGroupBoundaries (g:gs) = reverse (foldl' step [g] gs)
  where
    step acc next =
      case acc of
        [] -> [next]
        prev:rest ->
          let prevLines = lines prev
              nextLines = lines next
          in case (reverse prevLines, nextLines) of
               (pLast:_, nFirst:nRest) | pLast == nFirst && not (null nRest) ->
                 intercalate "\n" (prevLines ++ nRest) : rest
               _ -> next : acc

-- | Group steps by top-level (depth 0) steps.
-- Each group consists of sub-steps (depth > 0) followed by their parent (depth 0).
groupByTopLevel :: [(Int, TraceStep)] -> IO [[(Int, TraceStep)]]
groupByTopLevel [] = return []
groupByTopLevel steps = do
  let go [] currentSubsRev groupsRev =
        case groupsRev of
          [] -> []
          lastGroupRev:restRev ->
            let groupsWithTrailingSubs =
                  if null currentSubsRev
                    then groupsRev
                    else (currentSubsRev ++ lastGroupRev) : restRev
            in map reverse (reverse groupsWithTrailingSubs)
      go ((i, s):xs) currentSubsRev groupsRev
        | tsDepth s > 0 = go xs ((i, s) : currentSubsRev) groupsRev
        | otherwise =
            let grpRev = (i, s) : currentSubsRev
            in go xs [] (grpRev : groupsRev)
  return (go steps [] [])

-- | Format a single group (sub-steps followed by their parent top-level step).
formatGroup :: Bool
            -> (Exp Ann -> IO String)
            -> (Exp Ann -> IO String)
            -> [(Int, TraceStep)]
            -> IO String
formatGroup _ _ _ [] = return ""
formatGroup useColor renderInput renderOutput group = do
  let (preSubs, rest) = span (\(_, s) -> tsDepth s > 0) group
      arrow = if useColor then dim "⇝ " else "⇝ "
  case rest of
    [] -> return ""
    ((_, topStep):postSubs) -> do
      let pre = map snd preSubs
          post = map snd postSubs
          allSubs = pre ++ post
      if null allSubs
        then do
          topInput <- renderInput (tsInput topStep)
          topOutput <- renderOutput (tsOutput topStep)
          return (topInput ++ "\n" ++ arrow ++ topOutput)
        else do
          let startAST = chooseTraceStart topStep pre
          startText <- renderInput startAST
          (_, _, outLines) <- foldM
            (processSubStepAST arrow renderInput renderOutput)
            (startAST, startText, [arrow ++ startText])
            (zip [0 ..] allSubs)
          return (intercalate "\n" outLines)

-- | Pick whether sub-step substitution should start from the top input
-- or top output expression for this group.
chooseTraceStart :: TraceStep -> [TraceStep] -> Exp Ann
chooseTraceStart topStep subSteps =
  case subSteps of
    [] -> tsInput topStep
    firstSub : _ ->
      let needle = tsInput firstSub
          inInput = containsExp needle (tsInput topStep)
          inOutput = containsExp needle (tsOutput topStep)
      in if inOutput && not inInput then tsOutput topStep else tsInput topStep

-- | Check whether an expression appears as a sub-expression.
containsExp :: Exp Ann -> Exp Ann -> Bool
containsExp needle haystack
  | eqTraceExp needle haystack = True
  | otherwise =
      case haystack of
        App _ fn args ->
          containsExp needle fn || any (containsExp needle) args
        Match _ scrut cls ->
          containsExp needle scrut
            || any (\(Clause _ body) -> containsExp needle body) cls
        Seq _ first second ->
          containsExp needle first || containsExp needle second
        Bind _ _ _ bindExp ->
          containsExp needle bindExp
        Let _ _ body ->
          containsExp needle body
        Ascribe _ _ ascExp ->
          containsExp needle ascExp
        _ -> False

-- ============================================================================
-- String Utilities
-- ============================================================================

-- | Find the starting position of a substring in a string.
-- Returns Nothing if the substring is not found.
findSubstring :: String -> String -> Maybe Int
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
pointerLinesForColored :: Bool -> String -> String -> String -> String -> [String]
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

    stripOuterParens s
      | length s >= 2 && head s == '(' && last s == ')' = tail (init s)
      | otherwise = s

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

-- | Uncolored version for backwards compatibility
pointerLinesFor :: String -> String -> String -> String -> [String]
pointerLinesFor = pointerLinesForColored False

-- | Highlight a substring in blue within a larger text, if found.
-- Used to keep evaluated sub-expressions highlighted when they appear
-- in the next evaluation step.
highlightSubstring :: Bool -> String -> String -> String
highlightSubstring useColor needle haystack
  | not useColor = haystack
  | otherwise =
      case findSubstring needle haystack of
        Nothing -> haystack
        Just ix ->
          let (before, rest) = splitAt ix haystack
              (match, after) = splitAt (length needle) rest
          in before ++ blue match ++ after

-- | Process a single sub-step using AST-level substitution.
-- Returns the updated parent AST, rendered parent string, and accumulated output lines.
processSubStepAST :: String  -- Arrow prefix
                  -> (Exp Ann -> IO String)  -- Renderer for inputs
                  -> (Exp Ann -> IO String)  -- Renderer for outputs
                  -> (Exp Ann, String, [String])  -- (current parent AST, current parent string, accumulated lines)
                  -> (Int, TraceStep)             -- Sub-step to process
                  -> IO (Exp Ann, String, [String])
processSubStepAST arrow renderInput renderOutput (currentParentAST, currentParent, accLines) (_, subStep) = do
  let replacement = tsOutput subStep
      (changed, newParentAST) = substituteFirstChild (tsInput subStep) replacement currentParentAST
  subInput <- renderInput (tsInput subStep)
  subOutput <- renderOutput (tsOutput subStep)
  newParent <- if changed then renderInput newParentAST else return currentParent

  if newParent == currentParent
    then return (currentParentAST, currentParent, accLines)
    else do
      let pos = findSubstring subInput currentParent
          subLen = length subInput
      let (underlineLine, resultLine) =
            case pos of
              Just idx | subLen >= 3 ->
                let underline = replicate idx ' ' ++ "└" ++ replicate (subLen - 2) '─' ++ "┘"
                    centerOffset = idx + subLen `div` 2
                    result = replicate centerOffset ' ' ++ subOutput
                in (underline, result)
              Just idx ->
                let result = replicate idx ' ' ++ subOutput
                in ("", result)
              Nothing -> ("", "")
          pointerLines = filter (not . null) [underlineLine, resultLine]
          newLines = accLines ++ pointerLines ++ [arrow ++ newParent]
      return (newParentAST, newParent, newLines)


-- ============================================================================
-- Expression Substitution and Equality
-- ============================================================================

-- | Replace only the first matching sub-expression in pre-order traversal.
-- Returns (changed flag, updated expression).
--
-- The 'changed' flag indicates whether a substitution was made.
substituteFirstChild :: Exp Ann -> Exp Ann -> Exp Ann -> (Bool, Exp Ann)
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
findFirstChild :: Exp Ann -> Exp Ann -> Maybe (Exp Ann)
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
eqTraceExp :: Exp Ann -> Exp Ann -> Bool
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
setTopCaseNom :: Exp Ann -> Exp Ann
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
copyCase :: Exp Ann -> Exp Ann -> Exp Ann
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
