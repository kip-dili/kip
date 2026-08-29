{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluator for Kip expressions and statements.
module Kip.Eval where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Kip.AST
import qualified Kip.Primitive as Prim
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Text.Megaparsec.Pos (SourcePos(..))

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Control.Exception (SomeException, try)
import Data.Bifunctor (bimap)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import System.IO (hFlush, stdout)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>), isRelative)
import System.Random (randomRIO)
import Data.Word (Word32)
import Control.Monad (unless, zipWithM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Maybe (isNothing, listToMaybe, maybeToList)
import Data.List (find)
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM

-- | Statically selected overload key: canonical name plus declared argument types.
--
-- The typechecker records this serializable key at the source span of a call.
-- Evaluator indexes use the same declaration types, so a checked direct call can
-- bypass runtime candidate discovery and argument-type inference.
type ResolvedCall = (Identifier, [Ty Ann])

-- | User-defined function signature and its ordered pattern clauses.
type FunctionDef = ([Arg Ann], [Clause Ann])

-- | Primitive signature paired with its host-language implementation.
type PrimitiveDef = ([Arg Ann], [Exp Ann] -> EvalM (Exp Ann))

-- | Memoized dispatch decision for one authored call-head span.
--
-- Positive entries retain the already selected callable.  'DirectFallback'
-- caches statically authored sites that still require dynamic resolution,
-- preventing repeated probes of the cumulative typechecker table.
data DirectCall
  = DirectPrimitive PrimitiveDef -- ^ Invoke this already selected primitive implementation.
  | DirectFunction FunctionDef -- ^ Invoke this already selected user-function definition.
  | DirectFallback -- ^ Retain dynamic lookup for a site that cannot be dispatched safely.

-- | Adaptive memo for exact-dispatch decisions.
--
-- Most evaluations repeatedly visit only a few call sites, for which a short
-- association list and 'sameSpanFast' are cheaper than a tree lookup. Once a
-- statement grows beyond 'directCallSmallLimit' distinct sites, the cache
-- promotes to a 'Map.Map' so programs with broad call graphs do not pay a
-- linear scan on every application.
data DirectCallCache
  = SmallDirectCallCache !Int ![(Span, DirectCall)] -- ^ Entry count and pointer-fast association list.
  | LargeDirectCallCache !(Map.Map Span DirectCall) -- ^ Ordered index used after the small memo fills.

-- | Lazily promoted index over the typechecker's cumulative call log.
--
-- Short-lived programs avoid constructing a second global tree at startup and
-- scan the log only on the first execution of each call site. Once a statement
-- actually exercises a broad call graph, 'IndexedResolvedCalls' removes further
-- linear first-call scans. The separate 'DirectCallCache' handles repeated hot
-- calls before and after promotion.
data ResolvedCallIndex
  = UnindexedResolvedCalls -- ^ Use the cumulative newest-first log directly.
  | IndexedResolvedCalls !(Map.Map Span ResolvedCall) -- ^ Exact span index built for a broad live call graph.

-- | Maximum number of entries retained in the pointer-fast small memo.
directCallSmallLimit :: Int
directCallSmallLimit = 32

-- | Empty exact-dispatch memo used for a newly checked statement.
emptyDirectCallCache :: DirectCallCache
emptyDirectCallCache = SmallDirectCallCache 0 []

-- | Empty lazy resolution index used whenever the cumulative log changes.
emptyResolvedCallIndex :: ResolvedCallIndex
emptyResolvedCallIndex = UnindexedResolvedCalls

-- | Evaluator state: runtime bindings plus render function.
--
-- The overloadable bindings ('evalFuncs', 'evalPrimFuncs', 'evalSelectors')
-- use @'Map.Map' k [v]@ instead of @Data.MultiMap@.  This eliminates the
-- @MultiMap@ wrapper overhead (an extra @Int@ count field and lazy
-- operations) while using the same underlying structure.  Lookup is
-- performed via @'Map.findWithDefault' []@ and insertion via
-- @'Map.insertWith' (++) k [v]@.
data EvalState =
  MkEvalState
    -- Strict fields prevent deferred record updates from building up in the
    -- evaluator state during long REPL sessions and module loads.
    { evalVals :: !(Map.Map Identifier (Exp Ann)) -- ^ Value bindings.
    , evalFuncs :: !(Map.Map Identifier [FunctionDef]) -- ^ Function clauses indexed by source-level name for dynamic calls.
    , evalPrimFuncs :: !(Map.Map Identifier [PrimitiveDef]) -- ^ Primitive implementations indexed by source-level name for dynamic calls.
    , evalResolvedCalls :: ![(Span, ResolvedCall)] -- ^ Cumulative newest-first call log supplied by the typechecker.
    , evalResolvedCallIndex :: !ResolvedCallIndex -- ^ Lazy span index promoted only for broad live call graphs.
    , evalDirectCalls :: !DirectCallCache -- ^ Adaptive per-statement memo of exact targets and deliberate dynamic fallbacks.
    , evalSelectors :: !(Map.Map Identifier [Int]) -- ^ Record selector indices.
    , evalCtors :: !(Map.Map Identifier ([Ty Ann], Ty Ann)) -- ^ Constructor signatures.
    , evalTyCons :: !(Map.Map Identifier Int) -- ^ Type constructor arities.
    , evalCurrentFile :: !(Maybe FilePath) -- ^ Current file path for relative I/O.
    , evalArgs :: ![Text] -- ^ CLI arguments visible to the running program.
    , evalRender :: !(EvalState -> Exp Ann -> IO String) -- ^ Render function for values.
    , evalRandState :: !(Maybe Word32) -- ^ Optional deterministic random state.
    }

-- | Empty evaluator state with a simple pretty-printer.
emptyEvalState :: EvalState -- ^ Default evaluator state.
emptyEvalState =
  MkEvalState
    { evalVals = Map.empty
    , evalFuncs = Map.empty
    , evalPrimFuncs = Map.empty
    , evalResolvedCalls = []
    , evalResolvedCallIndex = emptyResolvedCallIndex
    , evalDirectCalls = emptyDirectCallCache
    , evalSelectors = Map.empty
    , evalCtors = Map.empty
    , evalTyCons = Map.empty
    , evalCurrentFile = Nothing
    , evalArgs = []
    , evalRender = \_ e -> return (prettyExp e)
    , evalRandState = Nothing
    }

-- | Evaluation errors (currently minimal).
data EvalError =
   Unknown
   -- ^ Failure with no more specific classification.
   | UnboundVariable Identifier
   -- ^ A variable was referenced but is not bound in the environment.
   | NoMatchingFunction Identifier
   -- ^ No function definition matches the applied name and arguments.
   | NoMatchingClause
   -- ^ Every clause of the applied function failed to match its arguments.
   | RuntimeTypeErrorNonValue
   -- ^ A primitive received an argument that did not reduce to a value.
   deriving (Show, Eq, Generic, Binary)
-- | Evaluator monad stack.
type EvalM = StateT EvalState (ExceptT EvalError IO)

-- | Install a restored cumulative exact-call table as the evaluator's base.
--
-- The newest-first list is retained without rebuilding a second global map at
-- every process startup or typechecked statement. Changing the log resets the
-- lazy span index and per-statement target memo; 'promoteResolvedCallIndex'
-- rebuilds the former only when the running program demonstrates a broad call
-- graph.
setResolvedCalls :: [(Span, ResolvedCall)] -- ^ Restored cumulative resolution table.
                 -> EvalState -- ^ Evaluator state to initialize.
                 -> EvalState -- ^ State with a shared call log and empty lazy index.
setResolvedCalls calls st =
  st
    { evalResolvedCalls = calls
    , evalResolvedCallIndex = emptyResolvedCallIndex
    , evalDirectCalls = emptyDirectCallCache
    }

-- | Look up a memoized call target using source-object sharing as a fast path.
--
-- The small representation stores spans taken directly from the typed AST, so
-- repeated execution normally reuses the complete span object. 'sameSpanFast'
-- exploits that sharing while retaining structural equality as a correctness
-- fallback. Broad call graphs use the ordered representation instead.
lookupDirectCall :: Span -- ^ Authored call-head span.
                 -> DirectCallCache -- ^ Per-statement dispatch memo.
                 -> Maybe DirectCall -- ^ Previously resolved decision, if present.
lookupDirectCall target cache =
  case cache of
    SmallDirectCallCache _ entries -> lookupSmall entries
    LargeDirectCallCache entries -> Map.lookup target entries
  where
    lookupSmall [] = Nothing
    lookupSmall ((sp, directCall):rest)
      | sameSpanFast target sp = Just directCall
      | otherwise = lookupSmall rest
{-# INLINE lookupDirectCall #-}

-- | Insert a decision, promoting a full small memo to logarithmic lookup.
insertDirectCall :: Span -- ^ Authored call-head span.
                 -> DirectCall -- ^ Exact target or deliberate fallback.
                 -> DirectCallCache -- ^ Existing per-statement memo.
                 -> DirectCallCache -- ^ Memo containing the new decision.
insertDirectCall sp directCall cache =
  case cache of
    SmallDirectCallCache size entries
      | size < directCallSmallLimit ->
          SmallDirectCallCache (size + 1) ((sp, directCall) : entries)
      | otherwise ->
          LargeDirectCallCache (Map.insert sp directCall (Map.fromList entries))
    LargeDirectCallCache entries ->
      LargeDirectCallCache (Map.insert sp directCall entries)

-- | Find the typechecker's selected overload for a call-head span.
--
-- Before promotion, a call scans the newest-first log once and its selected
-- target (or deliberate miss) is retained by 'evalDirectCalls'. After a live
-- statement crosses 'directCallSmallLimit', later first calls use logarithmic
-- lookup in the promoted index.
lookupResolvedCall :: Span -- ^ Authored call-head span.
                   -> ResolvedCallIndex -- ^ Unbuilt marker or promoted exact index.
                   -> [(Span, ResolvedCall)] -- ^ Cumulative newest-first fallback log.
                   -> Maybe ResolvedCall -- ^ Selected signature, if this is an exact call site.
lookupResolvedCall target resolvedCallIndex fallbackCalls =
  case resolvedCallIndex of
    IndexedResolvedCalls indexedCalls -> Map.lookup target indexedCalls
    UnindexedResolvedCalls -> lookupFallback fallbackCalls
  where
    lookupFallback [] = Nothing
    lookupFallback ((sp, resolvedCall):rest)
      | sameSpanFast target sp = Just resolvedCall
      | otherwise = lookupFallback rest
{-# INLINE lookupResolvedCall #-}

-- | Build the exact span index once a statement executes a broad call graph.
--
-- 'foldr' visits older log entries first, so a newer duplicate span overwrites
-- it and preserves the typechecker's newest-first lookup semantics. Promotion
-- is keyed to the already-built direct-call memo, avoiding this global work for
-- startup-heavy programs and statements with only a few live call sites.
promoteResolvedCallIndex :: DirectCallCache -- ^ Direct-call memo after inserting the current site.
                         -> ResolvedCallIndex -- ^ Current lazy or promoted resolution index.
                         -> [(Span, ResolvedCall)] -- ^ Cumulative newest-first call log.
                         -> ResolvedCallIndex -- ^ Promoted index when the live call graph is broad.
promoteResolvedCallIndex directCalls resolvedCallIndex calls =
  case (directCalls, resolvedCallIndex) of
    (LargeDirectCallCache _, UnindexedResolvedCalls) ->
      IndexedResolvedCalls
        (foldr
          (\(sp, resolvedCall) -> Map.insert sp resolvedCall)
          Map.empty
          calls)
    _ -> resolvedCallIndex

-- | Compare spans with safe structural fallbacks after pointer fast paths.
--
-- 'reallyUnsafePtrEquality#' is used only to prove equality early for a complete
-- span or its source strings. A negative answer always falls back to ordinary
-- equality, so copying or deserializing source metadata cannot change semantics.
sameSpanFast :: Span -- ^ First span.
             -> Span -- ^ Second span.
             -> Bool -- ^ Whether both spans identify the same source range.
sameSpanFast a b
  | isTrue# (reallyUnsafePtrEquality# a b) = True
sameSpanFast NoSpan NoSpan = True
sameSpanFast (Span startA endA pathA) (Span startB endB pathB) =
  sameSourcePosFast startA startB &&
  sameSourcePosFast endA endB &&
  samePathFast pathA pathB
  where
    sameSourcePosFast (SourcePos nameA lineA columnA) (SourcePos nameB lineB columnB) =
      sameStringFast nameA nameB && lineA == lineB && columnA == columnB
    samePathFast Nothing Nothing = True
    samePathFast (Just a) (Just b) = sameStringFast a b
    samePathFast _ _ = False
    sameStringFast a b = isTrue# (reallyUnsafePtrEquality# a b) || a == b
sameSpanFast _ _ = False
{-# INLINE sameSpanFast #-}

-- | Construct the exact key shared by typechecker resolutions and evaluator indexes.
--
-- The typechecker canonicalizes primitive aliases before recording signatures;
-- the evaluator must do the same when indexing declarations or structurally
-- equal source types could miss the fast path.
functionSignatureKey :: Identifier -- ^ Canonical function or primitive name.
                     -> [Arg Ann] -- ^ Declared arguments whose types identify the overload.
                     -> ResolvedCall -- ^ Stable structural key for the overload.
functionSignatureKey name args = normalizeResolvedCall (name, map argType args)

-- | Normalize a selected signature for evaluator exact lookup.
--
-- Source spans do not participate in overload identity: the same semantic
-- signature can be declared in a primitive module and then repeated by user
-- code.  Grammatical cases remain in the key because they affect argument
-- alignment, while primitive aliases are canonicalized just as they are by
-- the typechecker.
normalizeResolvedCall :: ResolvedCall -- ^ Signature carrying source annotations.
                      -> ResolvedCall -- ^ Structurally comparable evaluator key.
normalizeResolvedCall (name, tys) = (name, map normalizeResolvedTy tys)

-- | Clear spans recursively while preserving the cases used for call alignment.
normalizeResolvedTy :: Ty Ann -- ^ Declared type with source annotations.
                    -> Ty Ann -- ^ Equivalent type suitable for an exact map key.
normalizeResolvedTy ty =
  case normalizePrimTy ty of
    TyString ann -> TyString (withoutSpan ann)
    TyInt ann -> TyInt (withoutSpan ann)
    TyFloat ann -> TyFloat (withoutSpan ann)
    TyChar ann -> TyChar (withoutSpan ann)
    TyInd ann name -> TyInd (withoutSpan ann) name
    TyVar ann name -> TyVar (withoutSpan ann) name
    TySkolem ann name -> TySkolem (withoutSpan ann) name
    Arr ann domain image ->
      Arr (withoutSpan ann) (normalizeResolvedTy domain) (normalizeResolvedTy image)
    TyApp ann ctor args ->
      TyApp (withoutSpan ann) (normalizeResolvedTy ctor) (map normalizeResolvedTy args)
  where
    withoutSpan ann = mkAnn (annCase ann) NoSpan

-- | Evaluate an expression in the current evaluator state.
evalExp :: Exp Ann -- ^ Expression to evaluate.
        -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExp = evalExpWith []

{- | Check if a variable can be resolved in an application context.

Variables in Kip can refer to different kinds of bindings stored in separate namespaces:
- Values/definitions (evalVals)
- Functions (evalFuncs)
- Primitive functions (evalPrimFuncs)
- Type constructors (evalTyCons)
- Data constructors (evalCtors)
- Record selectors (evalSelectors)

When evaluating a standalone Var, we only look in evalVals. If not found there,
we check if it exists in any other namespace - if so, it's a function/constructor
reference that will be resolved when used in an App. Otherwise, it's truly undefined.

The candidates are scanned once, checking every namespace for each candidate.
-}
isResolvableInAppContext :: [(Identifier, Case)] -- ^ Variable candidates.
                         -> EvalState -- ^ Current evaluation state.
                         -> Bool -- ^ True if resolvable in App context.
isResolvableInAppContext varCandidates st =
  isRandomCandidate varCandidates ||
  any (\(ident, _) ->
    Map.member ident (evalFuncs st) ||
    Map.member ident (evalPrimFuncs st) ||
    Map.member ident (evalSelectors st) ||
    Map.member ident (evalTyCons st) ||
    Map.member ident (evalCtors st)
  ) varCandidates

-- | Find a 0-arg primitive function matching one of the variable candidates.
find0ArgPrim :: [(Identifier, Case)]
             -- ^ Variable candidates to try in source order.
             -> Map.Map Identifier [([Arg Ann], [Exp Ann] -> EvalM (Exp Ann))]
             -- ^ Primitive overloads grouped by canonical identifier.
             -> Maybe ([Exp Ann] -> EvalM (Exp Ann))
             -- ^ First zero-argument implementation that matches a candidate.
find0ArgPrim candidates primFuncs =
  listToMaybe
    [ impl
    | (ident, _) <- candidates
    , defs <- maybeToList (Map.lookup ident primFuncs)
    , (args, impl) <- defs
    , null args
    ]

-- | Check whether an evaluated expression is a runtime value.
--
-- REPL output must only print values. If evaluation yields a non-value
-- (for example, an unresolved bare application), callers should surface a
-- runtime type error instead of rendering the raw expression.
isRuntimeValue :: EvalState -- ^ Current evaluator state.
               -> Exp Ann -- ^ Evaluated expression.
               -> Bool -- ^ True when expression is a value.
isRuntimeValue st expr =
  case expr of
    IntLit {} -> True
    FloatLit {} -> True
    StrLit {} -> True
    CharLit {} -> True
    SetLit {} -> True
    MapLit {} -> True
    Var {varCandidates} ->
      isRandomCandidate varCandidates ||
      any (\(ident, _) ->
        Map.member ident (evalVals st) ||
        Map.member ident (evalFuncs st) ||
        Map.member ident (evalPrimFuncs st) ||
        Map.member ident (evalSelectors st) ||
        Map.member ident (evalTyCons st) ||
        Map.member ident (evalCtors st)
      ) varCandidates
    App {fn, args} ->
      let (fnRoot, preAppliedArgs) = flattenApplied fn
          allArgs = preAppliedArgs ++ args
      in case fnRoot of
           Var {varCandidates} ->
             all isRuntimeArg allArgs && any (isSaturatedCtor allArgs . fst) varCandidates
           _ -> False
    _ -> False
  where
    isRuntimeArg = isRuntimeValue st
    isSaturatedCtor allArgs ident =
      case Map.lookup ident (evalCtors st) of
        Just (argTys, _) -> length argTys == length allArgs
        Nothing -> False

-- | A single evaluation step for trampolining tail calls.
data EvalStep
  = Done (Exp Ann)
  -- ^ Evaluation finished with this final value.
  | Continue (HM.HashMap Identifier (Exp Ann)) (Exp Ann)
  -- ^ Evaluate the expression under the given environment, without growing the
  -- Haskell call stack.

-- | A single recorded evaluation step for tracing.
data TraceStep = TraceStep
  { tsDepth  :: Int     -- ^ Call-stack depth (0 = top-level).
  , tsInput  :: Exp Ann -- ^ Expression before this step.
  , tsOutput :: Exp Ann -- ^ Expression after this step.
  }

-- | Evaluate an expression with a local environment.
-- |
-- | This is a trampoline: tail-position transitions return 'Continue' so the
-- | evaluator can loop without consuming Haskell stack frames. Non-tail work
-- | still evaluates recursively, but tail calls become iterative.
evalExpWith :: [(Identifier, Exp Ann)] -- ^ Local environment bindings.
            -> Exp Ann -- ^ Expression to evaluate.
            -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExpWith bindings = evalExpLoop (HM.fromList bindings) 

-- | Main trampoline loop.
-- |
-- | The loop is strict in the next step: it only recurses in Haskell when the
-- | evaluation cannot be in tail position (e.g., computing subexpressions).
evalExpLoop :: HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
            -> Exp Ann -- ^ Expression to evaluate.
            -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExpLoop localEnv e = do
  evalStep localEnv e >>= \case
    Done v -> return v
    Continue env' e' -> evalExpLoop env' e'

-- | Evaluate an expression and collect evaluation trace steps.
evalExpTraced :: Exp Ann -- ^ Expression to evaluate from an empty local environment.
              -> EvalM (Exp Ann, [TraceStep]) -- ^ Final value and chronological trace steps.
evalExpTraced e = do
  ref <- liftIO (newIORef [])
  ctr <- liftIO (newIORef (0 :: Int))
  result <- evalExpLoopTraced ref ctr 0 HM.empty e
  steps <- liftIO (readIORef ref)
  return (result, reverse steps)

-- | Step limit for traced evaluation.
traceStepLimit :: Int
traceStepLimit = 1000

-- | Traced trampoline loop. Records a @TraceStep@ for each step that
-- produces a different output than its input (skipping trivial literal
-- identity steps). Falls back to the normal 'evalExpLoop' when the step
-- counter reaches 'traceStepLimit'.
evalExpLoopTraced :: IORef [TraceStep] -- ^ Reverse-ordered trace accumulator.
                  -> IORef Int -- ^ Number of trace steps recorded so far.
                  -> Int -- ^ Current call-stack depth for display.
                  -> HM.HashMap Identifier (Exp Ann) -- ^ Local value environment.
                  -> Exp Ann -- ^ Expression to evaluate.
                  -> EvalM (Exp Ann) -- ^ Final evaluated value.
evalExpLoopTraced ref ctr depth localEnv e = do
  count <- liftIO (readIORef ctr)
  if count >= traceStepLimit
    then evalExpLoop localEnv e
    else do
      subsRef <- liftIO (newIORef [])
      let trackingSubEval env sub = do
            result <- evalSubTraced ref ctr depth env sub
            unless (sameExp sub result) $
              liftIO (modifyIORef' subsRef ((sub, result) :))
            return result
      evalStepWith trackingSubEval localEnv e >>= \case
        Done v -> do
          let shownInput = substituteTraceEnv localEnv e
          let shownOutput = substituteTraceEnv localEnv v
          -- Always use original expression as input for display clarity
          unless (sameExp shownInput shownOutput) $ do
            liftIO (modifyIORef' ctr (+ 1))
            liftIO (modifyIORef' ref (TraceStep depth shownInput shownOutput :))
          return v
        Continue env' e' -> do
          let shownInput = substituteTraceEnv localEnv e
          let shownOutput = substituteTraceEnv env' e'
          -- Use original expression as input, Continue target as output
          liftIO (modifyIORef' ctr (+ 1))
          liftIO (modifyIORef' ref (TraceStep depth shownInput shownOutput :))
          evalExpLoopTraced ref ctr depth env' e'

-- | Sub-evaluator for traced mode: evaluates at depth + 1.
evalSubTraced :: IORef [TraceStep] -- ^ Shared reverse-ordered trace accumulator.
              -> IORef Int -- ^ Shared trace-step counter.
              -> Int -- ^ Parent expression depth.
              -> HM.HashMap Identifier (Exp Ann) -- ^ Local value environment.
              -> Exp Ann -- ^ Subexpression to evaluate.
              -> EvalM (Exp Ann) -- ^ Evaluated subexpression value.
evalSubTraced ref ctr depth = evalExpLoopTraced ref ctr (depth + 1)

-- | Substitute local environment bindings into an expression for trace display.
-- This is only for human-readable tracing and does not affect evaluation.
substituteTraceEnv :: HM.HashMap Identifier (Exp Ann) -- ^ Local bindings visible at the trace point.
                   -> Exp Ann -- ^ Expression shown in the trace.
                   -> Exp Ann -- ^ Expression with local variables expanded up to the safety limit.
substituteTraceEnv env = go 0
  where
    maxDepth :: Int
    maxDepth = 64

    go :: Int -> Exp Ann -> Exp Ann
    go depth expr
      | depth >= maxDepth = expr
      | otherwise =
          case expr of
            Var ann name candidates ->
              case lookupByCandidatesHM env candidates of
                Just bound ->
                  let bound' = go (depth + 1) bound
                  in bound' { annExp = setAnnCase (annExp bound') (annCase ann) }
                Nothing ->
                  case lookupByCandidateSuffixHM env candidates of
                    Just bound ->
                      let bound' = go (depth + 1) bound
                      in bound' { annExp = setAnnCase (annExp bound') (annCase ann) }
                    Nothing ->
                      case lookupBySuffixHM env name of
                        Just bound ->
                          let bound' = go (depth + 1) bound
                          in bound' { annExp = setAnnCase (annExp bound') (annCase ann) }
                        Nothing -> expr
            App ann fn args ->
              App ann (go depth fn) (map (go depth) args)
            SetLit ann entries ->
              SetLit ann (Map.map (go depth) entries)
            MapLit ann entries ->
              MapLit ann (Map.map (bimap (go depth) (go depth)) entries)
            Bind ann nm na bexp ->
              Bind ann nm na (go depth bexp)
            Seq ann first second ->
              Seq ann (go depth first) (go depth second)
            Match ann scrut cls ->
              Match ann (go depth scrut) (map substClause cls)
            Let ann nm body ->
              Let ann nm (go depth body)
            Ascribe ann ty ascExp ->
              Ascribe ann ty (go depth ascExp)
            _ -> expr

    substClause :: Clause Ann -> Clause Ann
    substClause (Clause pat body) = Clause pat (go 0 body)

-- | Check whether two expressions are trivially the same (literal identity).
sameExp :: Exp Ann -- ^ Expression before an evaluation step.
        -> Exp Ann -- ^ Expression after an evaluation step.
        -> Bool -- ^ 'True' when the pair is trivial enough to omit from a trace.
sameExp (IntLit _ a) (IntLit _ b) = a == b
sameExp (FloatLit _ a) (FloatLit _ b) = a == b
sameExp (StrLit _ a) (StrLit _ b) = a == b
sameExp (CharLit _ a) (CharLit _ b) = a == b
sameExp (SetLit _ a) (SetLit _ b) = a == b
sameExp (MapLit _ a) (MapLit _ b) = a == b
sameExp (Var _ n1 _) (Var _ n2 _) = n1 == n2
sameExp _ _ = False

-- | Check if two expressions are structurally equal, ignoring annotations.
eqIgnoringAnn :: Exp Ann -- ^ First expression to compare.
              -> Exp Ann -- ^ Second expression to compare.
              -> Bool -- ^ 'True' when structures agree after ignoring annotations.
eqIgnoringAnn (Var _ n1 c1) (Var _ n2 c2) = n1 == n2 && c1 == c2
eqIgnoringAnn (App _ f1 a1) (App _ f2 a2) =
  eqIgnoringAnn f1 f2 && length a1 == length a2 && and (zipWith eqIgnoringAnn a1 a2)
eqIgnoringAnn (SetLit _ e1) (SetLit _ e2) =
  sameLength kvs1 kvs2 && and (zipWith eqKV kvs1 kvs2)
  where
    kvs1 = Map.toAscList e1
    kvs2 = Map.toAscList e2
    eqKV (k1, v1) (k2, v2) = k1 == k2 && eqIgnoringAnn v1 v2
eqIgnoringAnn (MapLit _ e1) (MapLit _ e2) =
  sameLength kvs1 kvs2 && and (zipWith eqKV kvs1 kvs2)
  where
    kvs1 = Map.toAscList e1
    kvs2 = Map.toAscList e2
    eqKV (k1, (ka1, va1)) (k2, (ka2, va2)) = k1 == k2 && eqIgnoringAnn ka1 ka2 && eqIgnoringAnn va1 va2
eqIgnoringAnn (IntLit _ n1) (IntLit _ n2) = n1 == n2
eqIgnoringAnn (FloatLit _ n1) (FloatLit _ n2) = n1 == n2
eqIgnoringAnn (StrLit _ s1) (StrLit _ s2) = s1 == s2
eqIgnoringAnn (CharLit _ c1) (CharLit _ c2) = c1 == c2
eqIgnoringAnn (Bind _ n1 na1 e1) (Bind _ n2 na2 e2) =
  n1 == n2 && na1 == na2 && eqIgnoringAnn e1 e2
eqIgnoringAnn (Seq _ f1 s1) (Seq _ f2 s2) =
  eqIgnoringAnn f1 f2 && eqIgnoringAnn s1 s2
eqIgnoringAnn (Match _ sc1 cl1) (Match _ sc2 cl2) =
  eqIgnoringAnn sc1 sc2 && length cl1 == length cl2 && and (zipWith eqClause cl1 cl2)
  where eqClause (Clause p1 e1) (Clause p2 e2) = p1 == p2 && eqIgnoringAnn e1 e2
eqIgnoringAnn (Let _ n1 b1) (Let _ n2 b2) =
  n1 == n2 && eqIgnoringAnn b1 b2
eqIgnoringAnn (Ascribe _ t1 e1) (Ascribe _ t2 e2) =
  t1 == t2 && eqIgnoringAnn e1 e2
eqIgnoringAnn _ _ = False

-- | Substitute evaluated sub-expressions in a parent expression.
-- Takes a list of (original, result) pairs from the tracking sub-evaluator
-- and recursively walks the tree, replacing any nodes that match.
-- The case annotation from the original child position is preserved on the result.
substituteChildren :: [(Exp Ann, Exp Ann)] -- ^ Original subexpressions paired with evaluated results.
                   -> Exp Ann -- ^ Parent expression in which replacements are made.
                   -> Exp Ann -- ^ Parent with matching descendants replaced.
substituteChildren subs parent =
  -- First check if the parent itself matches
  case find (\(orig, _) -> eqIgnoringAnn orig parent) subs of
    Just (orig, result) ->
      result { annExp = setAnnCase (annExp result) (annCase (annExp orig)) }
    Nothing ->
      -- If not, recursively substitute in children
      case parent of
        App ann fn args ->
          App ann (replaceChild fn) (map replaceChild args)
        SetLit ann entries ->
          SetLit ann (Map.map replaceChild entries)
        MapLit ann entries ->
          MapLit ann (Map.map (bimap replaceChild replaceChild) entries)
        Match ann scr cls ->
          Match ann (replaceChild scr) (map (\(Clause p e) -> Clause p (replaceChild e)) cls)
        Seq ann first second ->
          case first of
            Bind bAnn nm na bexp ->
              Seq ann (Bind bAnn nm na (replaceChild bexp)) (replaceChild second)
            _ -> Seq ann (replaceChild first) (replaceChild second)
        Bind ann nm na bexp ->
          Bind ann nm na (replaceChild bexp)
        Let ann nm body ->
          Let ann nm (replaceChild body)
        Ascribe ann ty e ->
          Ascribe ann ty (replaceChild e)
        _ -> parent
  where
    replaceChild = substituteChildren subs

-- | One evaluation step.
-- |
-- | IMPORTANT: Only tail-position transitions should return 'Continue':
-- | - Sequence second position
-- | - Match clause body
-- | - Let/Ascribe bodies
-- | - Function bodies
-- | - Variable evaluation that expands to another expression
-- |
-- | Everything else returns a 'Done' value (possibly after non-tail recursion).
-- |
-- | Optimization notes:
-- | 1) Overload resolution is the hottest path for most programs. It performs
-- |    'inferType' on every argument to select a definition. That is correct,
-- |    but wasteful when no candidate functions/primops/selectors exist for the
-- |    name. In that case, the semantics are: leave the application untouched
-- |    (constructor application or unevaluated call). We therefore short-circuit
-- |    before calling 'pickPrimByTypes'/'pickFunctionByTypes'. This avoids all
-- |    type inference work for obviously non-callable identifiers.
-- |
-- |    Safety: We only skip resolution when there are no candidates in any of
-- |    the relevant namespaces (functions, primops, selectors, random). If a
-- |    candidate exists, we fall back to the full resolution logic.
-- |
-- | 2) Selector and random lookups are *name-based* and do not depend on
-- |    argument types. If there are no function/primitive candidates, we can
-- |    resolve selectors (single-arg) and the random primitive without calling
-- |    'inferType'. This removes a full round of type inference in a common
-- |    case for record access and random number generation.
-- |
-- | 3) We keep 'fn' and 'args' evaluation strict (non-tail) so evaluation order
-- |    and effects are unchanged. The only change is *what happens after* the
-- |    arguments are evaluated: we build a 'Done' or 'Continue' step rather than
-- |    performing another recursive call in Haskell.
-- |
-- | 4) The trampoline structure is intentionally minimal: we only introduce
-- |    iteration for tail positions, leaving non-tail recursion untouched. This
-- |    preserves current behavior while eliminating stack growth for tail calls.
evalStep :: HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
         -> Exp Ann -- ^ Expression to evaluate.
         -> EvalM EvalStep -- ^ Trampoline step.
evalStep = evalStepWith evalExpLoop

-- | Parameterized evaluation step that uses a given sub-evaluator for
-- non-tail recursive calls. This allows the same step logic to be reused
-- for both normal and traced evaluation.
evalStepWith :: (HM.HashMap Identifier (Exp Ann) -> Exp Ann -> EvalM (Exp Ann))
             -> HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
             -> Exp Ann -- ^ Expression to evaluate.
             -> EvalM EvalStep -- ^ Trampoline step.
evalStepWith subEval localEnv e =
  case e of
    Var {annExp, varName, varCandidates} ->
      case lookupByCandidatesHM localEnv varCandidates of
        Just v -> return (Done v)
        Nothing ->
          case lookupBySuffixHM localEnv varName of
            Just v -> return (Done v)
            Nothing -> do
              st@MkEvalState{evalVals} <- get
              case lookupByCandidates evalVals varCandidates of
                Nothing ->
                  -- Check for 0-arg primitive functions (e.g. boş-sözlük, boş-küme).
                  -- Unlike constructors, primitives must be invoked to produce a value.
                  case find0ArgPrim varCandidates (evalPrimFuncs st) of
                    Just impl -> do
                      result <- impl []
                      return (Done result)
                    Nothing ->
                      -- Not a value binding. Check if it's a function/constructor/etc.
                      -- that will be resolved when applied in an App context.
                      if isResolvableInAppContext varCandidates st
                        then return (Done (Var annExp varName varCandidates))
                        else throwError (UnboundVariable varName)
                Just v ->
                  -- Tail-position indirection: keep evaluating the bound value.
                  return (Continue localEnv v)
    App {annExp = annApp, fn, args} -> do
      -- Non-tail: we must compute function and arguments before applying.
      -- Keep the authored call head separate from the evaluated one: a
      -- higher-order parameter may evaluate to a global function whose span
      -- describes the value's origin, not this call site's static resolution.
      (fn', exactCallSpan) <-
        case fn of
          Var {annExp = annSourceFn, varName, varCandidates} -> do
            MkEvalState{evalVals} <- get
            case lookupByCandidatesHM localEnv varCandidates of
              Just _ -> do
                evaluated <- subEval localEnv fn
                pure (evaluated, Nothing)
              Nothing ->
                case lookupBySuffixHM localEnv varName of
                  Just _ -> do
                    evaluated <- subEval localEnv fn
                    pure (evaluated, Nothing)
                  Nothing ->
                    case lookupByCandidates evalVals varCandidates of
                      Just _ -> do
                        evaluated <- subEval localEnv fn
                        pure (evaluated, Nothing)
                      Nothing ->
                        let sp = annSpan annSourceFn
                        in pure (fn, if sp == NoSpan then Nothing else Just sp)
          _ -> do
            fnEvaluated <- subEval localEnv fn
            let sourceRoot = fst (flattenApplied fn)
                evaluatedRoot = fst (flattenApplied fnEvaluated)
                nestedCallSpan =
                  case (sourceRoot, evaluatedRoot) of
                    ( Var {annExp = annSourceFn, varName = sourceName}
                      , Var {annExp = annEvaluatedFn, varName = evaluatedName}
                      )
                        | sourceName == evaluatedName
                        , annSpan annSourceFn /= NoSpan
                        , annSpan annSourceFn == annSpan annEvaluatedFn ->
                            Just (annSpan annSourceFn)
                    _ -> Nothing
            pure (fnEvaluated, nestedCallSpan)
      args' <- mapM (subEval localEnv) args
      let (fnResolved, preAppliedArgs) = flattenApplied fn'
          allArgs = preAppliedArgs ++ args'
      case fnResolved of
        Var {varCandidates} -> do
          -- Pull state once for both exact and dynamic resolution paths.
          MkEvalState
            { evalFuncs
            , evalPrimFuncs
            , evalResolvedCalls = resolvedCalls
            , evalResolvedCallIndex = resolvedCallIndex
            , evalDirectCalls = directCalls
            , evalSelectors
            , evalTyCons
            } <- get
          let partialCall = not (null preAppliedArgs)
              callArgs = reorderSectionArgs preAppliedArgs allArgs
          directStep <-
            case exactCallSpan of
              Nothing -> return Nothing
              Just sp -> do
                directCall <-
                  case lookupDirectCall sp directCalls of
                    Just cachedCall -> return cachedCall
                    Nothing -> do
                      let resolvedLookup =
                            lookupResolvedCall sp resolvedCallIndex resolvedCalls
                          selectedCall =
                            case resolvedLookup of
                              Nothing -> DirectFallback
                              Just resolvedCall ->
                                resolveDirectCall resolvedCall evalPrimFuncs evalFuncs
                      modify (\s ->
                        let nextDirectCalls =
                              insertDirectCall sp selectedCall (evalDirectCalls s)
                        in s
                            { evalResolvedCallIndex =
                                promoteResolvedCallIndex
                                  nextDirectCalls
                                  (evalResolvedCallIndex s)
                                  (evalResolvedCalls s)
                            , evalDirectCalls = nextDirectCalls
                            })
                      return selectedCall
                tryDirectCall
                  fnResolved
                  localEnv
                  partialCall
                  callArgs
                  directCall
          case directStep of
            Just step -> return step
            Nothing -> do
              let fnCandidates = map fst varCandidates
                  matches = [(n, def) | n <- fnCandidates, def <- Map.findWithDefault [] n evalFuncs]
                  primMatches = [(n, def) | n <- fnCandidates, def <- Map.findWithDefault [] n evalPrimFuncs]
                  selectorMatches = [idx | n <- fnCandidates, idx <- Map.findWithDefault [] n evalSelectors]
                  hasTyConCandidate = any (\(ident, _) -> Map.member ident evalTyCons) varCandidates
              -- If there are no function/primitive candidates, we can
              -- decide selector/random/constructor outcomes without type inference.
              if null matches && null primMatches
                then
                  case allArgs of
                    -- Type-case application is a fallback when no callable definition exists.
                    [arg] | hasTyConCandidate ->
                      return (Done (applyTypeCase (annCase (annExp fnResolved)) arg))
                    -- Fast-path selectors when the only possible resolution is a selector.
                    [arg] ->
                      case selectorMatches of
                        idx:_ -> Done <$> applySelector idx arg (App annApp fnResolved allArgs)
                        [] -> return (Done (App annApp fnResolved allArgs))
                    _ ->
                      return (Done (App annApp fnResolved allArgs)) -- Constructor application or unevaluated call.
                else do
                  let pickPrim = if partialCall then pickPrimByTypesPartial else pickPrimByTypes
                      pickFn = if partialCall then pickFunctionByTypesPartial else pickFunctionByTypes
                  -- Infer argument types once and share across all pick functions.
                  -- See pickFunctionByTypes Haddock for details.
                  argTys <- mapM inferType callArgs
                  pickPrim primMatches callArgs argTys >>= \case
                    Just (primImpl, primArgs) -> Done <$> primImpl primArgs
                    Nothing ->
                      pickFn matches callArgs argTys >>= \case
                        Just (def, fnArgs) -> applyFunctionStep fnResolved localEnv def fnArgs
                        Nothing ->
                          case (selectorMatches, allArgs) of
                            (idx:_, [arg]) ->
                              Done <$> applySelector idx arg (App annApp fnResolved allArgs)
                            _ ->
                              return (Done (App annApp fnResolved allArgs)) -- Constructor application or unevaluated call
        _ | null allArgs -> return (Done fnResolved)
          | otherwise -> return (Done (App annApp fnResolved allArgs))
    StrLit {annExp, lit} ->
      return (Done (StrLit annExp lit))
    IntLit {annExp, intVal} ->
      return (Done (IntLit annExp intVal))
    FloatLit {annExp, floatVal} ->
      return (Done (FloatLit annExp floatVal))
    CharLit {annExp, charVal} ->
      return (Done (CharLit annExp charVal))
    SetLit {annExp, setEntries} ->
      return (Done (SetLit annExp setEntries))
    MapLit {annExp, mapEntries} ->
      return (Done (MapLit annExp mapEntries))
    Bind {annExp, bindName, bindNameAnn, bindExp} -> do
      -- Non-tail: evaluate the binding expression, but the bind itself is a value.
      v <- subEval localEnv bindExp
      return (Done (Bind annExp bindName bindNameAnn v))
    Seq {first, second} -> do
      case first of
        Bind {bindName, bindExp} -> do
          -- Tail position: continue with the extended environment and second.
          v <- subEval localEnv bindExp
          return (Continue (HM.insert bindName v localEnv) second)
        _ -> do
          -- Evaluate the first expression, then tail-continue into second.
          _ <- subEval localEnv first
          return (Continue localEnv second)
    Match {scrutinee, clauses} -> do
      -- Non-tail: we need the scrutinee value to select the clause.
      scrutinee' <- subEval localEnv scrutinee
      case findClause scrutinee' clauses of
        Nothing -> throwError NoMatchingClause
        Just (Clause _ body, patBindings) -> do
          let env = HM.fromList patBindings `HM.union` localEnv
          -- Tail position: continue with the clause body.
          return (Continue env body)
    Let {body} ->
      -- Tail position: the body is the result of the let.
      return (Continue localEnv body)
    Ascribe {ascExp} ->
      -- Tail position: ascriptions do not affect evaluation.
      return (Continue localEnv ascExp)
  where
    -- | Find the first matching clause for a scrutinee.
    findClause :: Exp Ann -- ^ Scrutinee expression.
               -> [Clause Ann] -- ^ Clauses to search.
               -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
    findClause scrut = go
      where
        -- | Walk clauses left-to-right until one matches.
        go :: [Clause Ann] -- ^ Remaining clauses.
           -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
        go [] = Nothing
        go (c@(Clause pat _):rest) =
          case matchPat pat (Just scrut) of
            Just binds -> Just (c, binds)
            Nothing -> go rest

-- | Apply a function definition to evaluated arguments.
-- |
-- | This is a tail-position producer: when a clause matches, we return a
-- | 'Continue' step so the trampoline can evaluate the body without growing
-- | the Haskell stack.
applyFunctionStep :: Exp Ann -- ^ Function expression.
                  -> HM.HashMap Identifier (Exp Ann) -- ^ Local environment bindings.
                  -> ([Arg Ann], [Clause Ann]) -- ^ Function signature and clauses.
                  -> [Exp Ann] -- ^ Evaluated arguments.
                  -> EvalM EvalStep -- ^ Trampoline step.
applyFunctionStep fn localEnv (args, clauses) values = do
  let argNames = map argIdent args
      argBindings = zip argNames values
  case findClause values clauses of
    Nothing -> return (Done (App (annExp fn) fn values))
    Just (Clause _ body, patBindings) -> do
      let env = HM.fromList patBindings `HM.union` HM.fromList argBindings `HM.union` localEnv
      return (Continue env body)
  where
    -- | Find the first matching clause for argument values.
    findClause :: [Exp Ann] -- ^ Argument values.
               -> [Clause Ann] -- ^ Clauses to search.
               -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
    findClause vs = go
      where
        -- | Walk clauses left-to-right until one matches.
        go :: [Clause Ann] -- ^ Remaining clauses.
           -> Maybe (Clause Ann, [(Identifier, Exp Ann)]) -- ^ Matching clause and bindings.
        go [] = Nothing
        go (c@(Clause pat _):rest) =
          case matchPat pat (scrutinee vs) of
            Just binds -> Just (c, binds)
            Nothing -> go rest
    -- | Use the first argument as the match scrutinee.
    scrutinee :: [Exp Ann] -- ^ Argument values.
              -> Maybe (Exp Ann) -- ^ Scrutinee expression.
    scrutinee vs =
      case vs of
        [] -> Nothing
        (v:_) -> Just v

-- | Match a pattern against a possible expression.
matchPat :: Pat Ann -- ^ Pattern to match.
         -> Maybe (Exp Ann) -- ^ Scrutinee expression.
         -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchPat pat mval =
  case pat of
    PWildcard _ -> Just []
    PVar n _ ->
      case mval of
        Nothing -> Nothing
        Just v -> Just [(n, v)]
    PCtor (ctor, _) pats ->
      case mval of
        Nothing -> Nothing
        Just v -> matchCtor ctor pats v
    PIntLit n _ ->
      case mval of
        Just (IntLit _ n') | n == n' -> Just []
        _ -> Nothing
    PFloatLit n _ ->
      case mval of
        Just (FloatLit _ n') | n == n' -> Just []
        _ -> Nothing
    PStrLit s _ ->
      case mval of
        Just (StrLit _ s') | s == s' -> Just []
        _ -> Nothing
    PCharLit c _ ->
      case mval of
        Just (CharLit _ c') | c == c' -> Just []
        _ -> Nothing
    PListLit pats ->
      case mval of
        Nothing -> Nothing
        Just v -> matchList pats v

-- | Match a constructor pattern against an expression.
matchCtor :: Identifier -- ^ Constructor identifier.
          -> [Pat Ann] -- ^ Sub-patterns.
          -> Exp Ann -- ^ Scrutinee expression.
          -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchCtor ctor pats v =
  case v of
    Var {varCandidates, varName} ->
      if ctorMatches ctor (Just varName) (map fst varCandidates)
        -- A bare constructor can be used as a unary "tag check" or as a
        -- one-argument pattern application; anything beyond that cannot
        -- match a Var without args.
        then case pats of
          [] -> Just []
          [p] -> matchPat p (Just v)
          _ -> Nothing
        else Nothing
    App {fn, args} ->
      case fn of
        Var {varCandidates, varName} | ctorMatches ctor (Just varName) (map fst varCandidates) ->
          -- Constructor patterns are right-aligned with actual args so that
          -- nested patterns (especially for list literals) match the tail.
          if length pats <= length args
            then do
              -- Drop leading args when the pattern list is shorter.
              let args' = drop (length args - length pats) args
              -- Recursively match each sub-pattern
              bindings <- zipWithM matchPat pats (map Just args')
              return (concat bindings)
            else
              Nothing
        _ -> Nothing
    _ -> Nothing
  where
    -- | Check constructor identity with possessive fallback.
    -- Prefers exact candidate matches before heuristic normalization.
    ctorMatches :: Identifier -- ^ Constructor name.
                -> Maybe Identifier -- ^ Optional variable name.
                -> [Identifier] -- ^ Candidate identifiers.
                -> Bool -- ^ True when constructors match.
    ctorMatches name mVarName candidates =
      let candidates' = candidates ++ maybe [] normalizeIdent mVarName
      in name `elem` candidates'
         || any (identMatchesPoss name) candidates'

    -- | Normalize identifiers by removing copula suffixes.
    normalizeIdent :: Identifier -- ^ Identifier to normalize.
                   -> [Identifier] -- ^ Normalized identifiers.
    normalizeIdent ident@(mods, word) =
      case stripCopulaSuffix word of
        Just stripped -> [(mods, stripped)]
        Nothing -> [ident]

    -- | Compare identifiers, allowing possessive/root normalization.
    --
    -- Compare the two possible roots directly without intermediate lists.
    identMatchesPoss :: Identifier -- ^ Left identifier.
                     -> Identifier -- ^ Right identifier.
                     -> Bool -- ^ True when identifiers match loosely.
    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2) &&
      rootsOverlap x1 x2

    -- | Check if two words share any candidate root.
    -- Each word has its own text plus an optional normalized variant
    -- (trailing vowel + soft-g removal).  We compare directly instead
    -- of building intermediate lists.
    rootsOverlap :: Text -> Text -> Bool
    rootsOverlap a b =
      a == b
      || altRoot a == Just b
      || altRoot b == Just a
      || case (altRoot a, altRoot b) of
           (Just a', Just b') -> a' == b'
           _ -> False
      where
        altRoot txt = dropTrailingVowel txt >>= dropTrailingSoftG

    -- | Drop a trailing Turkish vowel for heuristic matching.
    dropTrailingVowel :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Word without trailing vowel.
    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing

    -- | Replace trailing soft g with k for heuristic matching.
    dropTrailingSoftG :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Word with trailing soft g normalized.
    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> "k")
        _ -> Nothing

    -- | Strip copula suffixes from a surface word.
    --
    -- Match against the shared suffix list, then strip the original text to
    -- preserve its casing.
    stripCopulaSuffix :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Stripped word.
    stripCopulaSuffix txt =
      let lowerTxt = T.toLower txt
      in go copulaSuffixes lowerTxt
      where
        go [] _ = Nothing
        go (suf:rest) lower =
          if T.isSuffixOf suf lower && T.length txt > 3
            then Just (T.take (T.length txt - 3) txt)
            else go rest lower

-- | Match a list pattern against an expression.
matchList :: [Pat Ann] -- ^ Element patterns.
          -> Exp Ann -- ^ Scrutinee expression.
          -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchList [] (Var _ ([], name) _)
  | name == ("boş" :: T.Text) = Just []
matchList (p:ps) (App _ (Var _ ([], name) _) [elem, rest])
  | name == ("eki" :: T.Text) = do
      elemBinds <- matchPat p (Just elem)
      restBinds <- matchList ps rest
      return (elemBinds ++ restBinds)
matchList _ _ = Nothing

-- | Check whether any candidate names Kip's random-integer primitive.
isRandomCandidate :: [(Identifier, Case)] -- ^ Names and cases attached to a variable expression.
                  -> Bool -- ^ 'True' when a random primitive is among the candidates.
isRandomCandidate =
  any (\(ident, _) -> ident == (["sayı"], "çek") || ident == ([], "sayı-çek"))

-- | Apply a record selector or fall back when out of range.
applySelector :: Int -- ^ Selector index.
              -> Exp Ann -- ^ Argument expression.
              -> Exp Ann -- ^ Fallback expression.
              -> EvalM (Exp Ann) -- ^ Selected expression.
applySelector idx arg fallback =
  case arg of
    App {args} ->
      if idx < length args
        then return (args !! idx)
        else return fallback
    _ -> return fallback

-- | Check two list lengths in lock-step, stopping at the first mismatch.
sameLength :: [a] -- ^ First list to measure lazily.
           -> [b] -- ^ Second list to measure lazily.
           -> Bool -- ^ 'True' when both lists terminate together.
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

-- | Reorder arguments for one-argument section applications.
-- Instrumental fixed arguments are left sections; other fixed arguments are
-- treated as right sections.
reorderSectionArgs :: [Exp Ann] -- ^ Arguments fixed by the section expression.
                   -> [Exp Ann] -- ^ Arguments supplied when invoking the section.
                   -> [Exp Ann] -- ^ Invocation arguments in evaluator call order.
reorderSectionArgs preApplied args =
  case (preApplied, args) of
    ([fixed], [x, y])
      | annCase (annExp fixed) /= Ins -> [y, x]
    _ -> args

-- | Resolve one typechecker-selected signature to a callable runtime target.
--
-- Resolution probes only the already canonical name, then compares its small
-- overload list against the selected signature.  The result is memoized by
-- source span by the caller, so recursive and iterative hot paths pay this
-- work once rather than rebuilding global exact-signature indexes at startup.
-- Primitive precedence matches both the legacy evaluator and JavaScript
-- backend.
resolveDirectCall :: ResolvedCall -- ^ Exact name and declaration signature selected by the typechecker.
                  -> Map.Map Identifier [PrimitiveDef] -- ^ Primitive overloads grouped by canonical name.
                  -> Map.Map Identifier [FunctionDef] -- ^ User functions grouped by canonical name.
                  -> DirectCall -- ^ Selected callable, or a cached dynamic fallback.
resolveDirectCall selectedCall primFuncs funcs =
  case find matchesSelected (Map.findWithDefault [] name primFuncs) of
    Just primDef -> DirectPrimitive primDef
    Nothing ->
      case find matchesSelected (Map.findWithDefault [] name funcs) of
        Just functionDef -> DirectFunction functionDef
        Nothing -> DirectFallback
  where
    normalizedCall@(name, _) = normalizeResolvedCall selectedCall
    matchesSelected (args, _) = functionSignatureKey name args == normalizedCall

-- | Attempt a call through a memoized exact-dispatch decision.
--
-- A positive entry bypasses candidate-name scans, runtime 'inferType', and the
-- dynamic overload pickers below.  'DirectFallback' deliberately retains the
-- old path for higher-order, unresolved, and otherwise unsafe call sites.
tryDirectCall :: Exp Ann -- ^ Evaluated symbolic function head, retained for errors and fallback values.
              -> HM.HashMap Identifier (Exp Ann) -- ^ Lexical environment used for a selected user function.
              -> Bool -- ^ Whether the runtime expression originated as a partial application.
              -> [Exp Ann] -- ^ Evaluated arguments in current call order.
              -> DirectCall -- ^ Memoized target or dynamic-fallback decision.
              -> EvalM (Maybe EvalStep) -- ^ Direct step, or 'Nothing' when dynamic resolution must handle it.
tryDirectCall fn localEnv partialCall callArgs directCall =
  case directCall of
    DirectPrimitive (primArgs, impl) ->
      case alignArgs primArgs of
        Nothing -> return Nothing
        Just args -> Just . Done <$> impl args
    DirectFunction def@(fnArgs, _) ->
      case alignArgs fnArgs of
        Nothing -> return Nothing
        Just args -> Just <$> applyFunctionStep fn localEnv def args
    DirectFallback -> return Nothing
  where
    alignArgs expectedArgs
      -- A nested partial application has the same function-head span as the
      -- eventual saturated call.  Reject its early lookup by arity so normal
      -- partial-application evaluation can retain the inner 'App' value.
      | length expectedArgs /= length callArgs = Nothing
      | not partialCall = Just callArgs
      | otherwise =
          let expectedCases = map (annCase . annTy . argType) expectedArgs
              actualCases = map (annCase . annExp) callArgs
          in reorderByCasesForEval expectedCases actualCases callArgs

-- | Choose a function definition based on inferred argument types.
--
-- Argument types are computed once by the caller and shared by primitive and
-- function overload resolution.
pickFunctionByTypes :: [(Identifier, ([Arg Ann], [Clause Ann]))] -- ^ Candidate function definitions.
                    -> [Exp Ann] -- ^ Evaluated arguments.
                    -> [Maybe (Ty Ann)] -- ^ Pre-computed argument types (one per arg).
                    -> EvalM (Maybe (([Arg Ann], [Clause Ann]), [Exp Ann])) -- ^ Selected function and args.
pickFunctionByTypes defs args argTys = do
  MkEvalState{evalTyCons} <- get
  let hasUnknownArgTy = any isNothing argTys
      matches =
        [ (def, args)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
        , sameLength tys args
        , and (zipWith (typeMatchesAllowUnknown evalTyCons) argTys tys)
        ]
      fallback =
        [ (def, args)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
        , sameLength tys args
        ]
  return $ case matches of
    d:_ -> Just d
    [] ->
      if hasUnknownArgTy
        then case fallback of
          d:_ -> Just d
          [] -> Nothing
        else Nothing

-- | Choose a primitive implementation based on inferred argument types.
--
-- Accepts pre-computed @argTys@ to avoid duplicate 'inferType' calls.
-- See 'pickFunctionByTypes' for full rationale.
pickPrimByTypes :: [(Identifier, ([Arg Ann], [Exp Ann] -> EvalM (Exp Ann)))] -- ^ Primitive candidates.
                -> [Exp Ann] -- ^ Evaluated arguments.
                -> [Maybe (Ty Ann)] -- ^ Pre-computed argument types (one per arg).
                -> EvalM (Maybe ([Exp Ann] -> EvalM (Exp Ann), [Exp Ann])) -- ^ Selected primitive and args.
pickPrimByTypes defs args argTys = do
  MkEvalState{evalTyCons} <- get
  let matches =
        [ (impl, args)
        | (_, (args', impl)) <- defs
        , let tys = map snd args'
        , sameLength tys args
        , and (zipWith (typeMatchesAllowUnknown evalTyCons) argTys tys)
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> Nothing

-- | Choose a function definition for calls that originated from partial application.
-- Reorders arguments by expected case, allowing nominative values to fill gaps.
--
-- Accepts pre-computed @argTys@ to avoid duplicate 'inferType' calls.
-- See 'pickFunctionByTypes' for full rationale.
pickFunctionByTypesPartial :: [(Identifier, ([Arg Ann], [Clause Ann]))]
                           -> [Exp Ann]
                           -> [Maybe (Ty Ann)] -- ^ Pre-computed argument types (one per arg).
                           -> EvalM (Maybe (([Arg Ann], [Clause Ann]), [Exp Ann]))
pickFunctionByTypesPartial defs args argTys = do
  MkEvalState{evalTyCons} <- get
  let hasUnknownArgTy = any isNothing argTys
      argCases = map (annCase . annExp) args
      matches =
        [ (def, argsForSig)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
              expCases = map (annCase . annTy . snd) args'
        , sameLength tys args
        , Just argsForSig <- [reorderByCasesForEval expCases argCases args]
        , Just argTysForSig <- [reorderByCasesForEval expCases argCases argTys]
        , and (zipWith (typeMatchesAllowUnknown evalTyCons) argTysForSig tys)
        ]
      fallback =
        [ (def, argsForSig)
        | (_, def@(args', _)) <- defs
        , let tys = map snd args'
              expCases = map (annCase . annTy . snd) args'
        , sameLength tys args
        , Just argsForSig <- [reorderByCasesForEval expCases argCases args]
        ]
  return $ case matches of
    d:_ -> Just d
    [] ->
      if hasUnknownArgTy
        then case fallback of
          d:_ -> Just d
          [] -> Nothing
        else Nothing

-- | Choose a primitive implementation for calls that originated from partial application.
--
-- Accepts pre-computed @argTys@ to avoid duplicate 'inferType' calls.
-- See 'pickFunctionByTypes' for full rationale.
pickPrimByTypesPartial :: [(Identifier, ([Arg Ann], [Exp Ann] -> EvalM (Exp Ann)))]
                       -> [Exp Ann]
                       -> [Maybe (Ty Ann)] -- ^ Pre-computed argument types (one per arg).
                       -> EvalM (Maybe ([Exp Ann] -> EvalM (Exp Ann), [Exp Ann]))
pickPrimByTypesPartial defs args argTys = do
  MkEvalState{evalTyCons} <- get
  let argCases = map (annCase . annExp) args
      matches =
        [ (impl, argsForSig)
        | (_, (args', impl)) <- defs
        , let tys = map snd args'
              expCases = map (annCase . annTy . snd) args'
        , sameLength tys args
        , Just argsForSig <- [reorderByCasesForEval expCases argCases args]
        , Just argTysForSig <- [reorderByCasesForEval expCases argCases argTys]
        , and (zipWith (typeMatchesAllowUnknownPartial evalTyCons) argTysForSig tys)
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> Nothing

-- | Type comparison used in partial-application primitive dispatch.
-- Unknown argument types are accepted to avoid dropping valid sections.
--
-- Takes 'Map.Map' directly to avoid repeated 'Map.toList' conversions
-- at each call site (see 'typeMatchesAllowUnknown' for details).
typeMatchesAllowUnknownPartial :: Map.Map Identifier Int -- ^ Type constructor arities (as 'Map.Map').
                               -> Maybe (Ty Ann)
                               -> Ty Ann
                               -> Bool
typeMatchesAllowUnknownPartial tyCons mTy ty =
  case mTy of
    Nothing -> True
    Just _ -> typeMatchesAllowUnknown tyCons mTy ty

-- | Reorder values for evaluator call matching.
-- Uses shared nominative fallback and additionally allows instrumental slots
-- to consume genitive values when no exact or nominative match exists.
reorderByCasesForEval :: [Case] -- ^ Cases required by the selected signature.
                      -> [Case] -- ^ Cases attached to supplied values.
                      -> [a] -- ^ Values aligned with the supplied cases.
                      -> Maybe [a] -- ^ Values reordered to signature order when alignment succeeds.
reorderByCasesForEval expected actual xs =
  case reorderByCasesNomFallback expected actual xs of
    Just reordered -> Just reordered
    Nothing -> reorderInsFromGen expected actual xs
  where
    reorderInsFromGen expCases actCases vals
      | length expCases /= length actCases || length actCases /= length vals = Nothing
      | otherwise = map snd <$> go (zip actCases vals) expCases
    go _ [] = Just []
    go rems (c:cs) =
      case pick c rems of
        Nothing -> Nothing
        Just (v, rems') -> (v :) <$> go rems' cs
    pick c rems =
      case break (\(ac, _) -> ac == c) rems of
        (before, m:after) -> Just (m, before ++ after)
        (_, []) ->
          if c == Nom
            then Nothing
            else case break (\(ac, _) -> ac == Nom) rems of
              (before, m:after) -> Just (m, before ++ after)
              (_, []) ->
                if c == Ins
                  then case break (\(ac, _) -> ac == Gen) rems of
                    (before, m:after) -> Just (m, before ++ after)
                    (_, []) ->
                      case rems of
                        m:after -> Just (m, after)
                        [] -> Nothing
                  else Nothing

-- | Type comparison allowing unknowns for primitive resolution.
--
-- The constructor map is passed through directly for recursive lookups.
typeMatchesAllowUnknown :: Map.Map Identifier Int -- ^ Type constructor arities (as 'Map.Map').
                        -> Maybe (Ty Ann) -- ^ Possibly unknown type.
                        -> Ty Ann -- ^ Expected type.
                        -> Bool -- ^ True when types match.
typeMatchesAllowUnknown tyCons mTy ty =
  case mTy of
    Nothing -> allowsUnknown ty
    Just t ->
      case ty of
        TyVar {} -> True
        TySkolem {} -> tyEq tyCons t ty
        _ -> typeMatches tyCons (Just t) ty
  where
    allowsUnknown expectedTy =
      case expectedTy of
        TyVar {} -> True
        TySkolem {} -> False
        Arr _ d i -> allowsUnknown d || allowsUnknown i
        TyApp _ c args -> allowsUnknown c || any allowsUnknown args
        _ -> False

-- | Lookup by candidates in a 'Map.Map'-based environment.
--
-- Candidate pairs are traversed directly, without an intermediate name list.
lookupByCandidates :: forall a.
                      Map.Map Identifier a -- ^ Candidate bindings.
                   -> [(Identifier, Case)] -- ^ Candidate identifiers.
                   -> Maybe a -- ^ Matching binding when found.
lookupByCandidates env = go
  where
    go [] = Nothing
    go ((n, _):rest) =
      case Map.lookup n env of
        Just v -> Just v
        Nothing -> go rest

-- | Lookup by candidates in a 'HM.HashMap'-based environment (O(1) average).
--
-- Candidate pairs are traversed directly, without an intermediate name list.
lookupByCandidatesHM :: forall a.
                        HM.HashMap Identifier a -- ^ Candidate bindings.
                     -> [(Identifier, Case)] -- ^ Candidate identifiers.
                     -> Maybe a -- ^ Matching binding when found.
lookupByCandidatesHM env = go
  where
    go [] = Nothing
    go ((n, _):rest) =
      case HM.lookup n env of
        Just v -> Just v
        Nothing -> go rest

-- | Heuristic fallback for matching inflected variables in HashMap-based local bindings (O(1) average).
lookupBySuffixHM :: HM.HashMap Identifier a -- ^ Local environment bindings.
                 -> Identifier -- ^ Surface identifier.
                 -> Maybe a -- ^ Matching binding when found.
lookupBySuffixHM env (mods, word) =
  let stripped = stripCaseRoots mods word
  in findMatch stripped
  where
    findMatch [] = Nothing
    findMatch (ident:rest) =
      case HM.lookup ident env of
        Just v -> Just v
        Nothing -> findMatch rest

-- | Produce likely local variable roots by removing visible case suffixes.
--
-- Turkish words carry at most one grammatical case suffix at a time, so we
-- stop at the first matching entry in 'bareCaseSuffixes'.
--
-- In addition to the plain stripped root, a trailing-@n@ fallback is
-- included for pronoun stems (e.g. @bun@ → @bu@).
stripCaseRoots :: [Text] -- ^ Namespace modules.
              -> Text   -- ^ Surface word to strip.
              -> [Identifier] -- ^ Candidate root identifiers (at most 2).
stripCaseRoots mods word = go bareCaseSuffixes
  where
    go [] = []
    go (suf:rest) =
      case T.stripSuffix suf word of
        Nothing -> go rest
        Just root
          | T.null root -> go rest
          | otherwise ->
              let base = (mods, root)
              in case T.stripSuffix "n" root of
                   Just r | not (T.null r) && r /= root -> [base, (mods, r)]
                   _ -> [base]

-- | Lookup by suffix fallback over all candidates in order.
lookupByCandidateSuffixHM :: HM.HashMap Identifier a
                          -- ^ Local environment indexed by canonical identifier.
                          -> [(Identifier, Case)]
                          -- ^ Candidate identifiers to normalize and try.
                          -> Maybe a
                          -- ^ First binding found by suffix fallback.
lookupByCandidateSuffixHM env = go
  where
    go [] = Nothing
    go (((mods, word), _):rest) =
      case lookupBySuffixHM env (mods, word) of
        Just v -> Just v
        Nothing -> go rest

-- | Turkish case suffixes used for heuristic variable resolution.
--
-- Ordered __longest first__ so that more specific suffixes (e.g. @"dan"@)
-- are tried before shorter ones that are their substrings (e.g. @"a"@).
-- Since 'stripCaseRoots' stops at the first match, this ordering ensures
-- we strip the correct suffix.
bareCaseSuffixes :: [Text]
bareCaseSuffixes =
  [ "nın", "nin", "nun", "nün"   -- 3-letter: genitive
  , "dan", "den", "tan", "ten"   -- 3-letter: ablative
  , "yı", "yi", "yu", "yü"      -- 2-letter: buffered accusative
  , "ya", "ye"                   -- 2-letter: buffered dative
  , "ın", "in", "un", "ün"      -- 2-letter: genitive (no buffer)
  , "da", "de", "ta", "te"      -- 2-letter: locative
  , "la", "le"                   -- 2-letter: instrumental
  , "ı", "i", "u", "ü"          -- 1-letter: accusative
  , "a", "e"                     -- 1-letter: dative
  ]

-- | Turkish copula suffixes (all exactly 3 characters).
copulaSuffixes :: [Text]
copulaSuffixes = ["dir","dır","dur","dür","tir","tır","tur","tür"]

-- | Lookup a constructor binding by candidates.
lookupCtorByCandidates :: Map.Map Identifier a -- ^ Candidate constructors.
                       -> [(Identifier, Case)] -- ^ Candidate identifiers.
                       -> Maybe a -- ^ Matching constructor.
lookupCtorByCandidates = lookupByCandidates

-- | Infer a type for an expression when possible.
inferType :: Exp Ann -- ^ Expression to infer.
          -> EvalM (Maybe (Ty Ann)) -- ^ Inferred type.
inferType e =
  case e of
    IntLit {} -> return (Just (TyInt (mkAnn Nom NoSpan)))
    FloatLit {} -> return (Just (TyFloat (mkAnn Nom NoSpan)))
    StrLit {} -> return (Just (TyString (mkAnn Nom NoSpan)))
    CharLit {} -> return (Just (TyChar (mkAnn Nom NoSpan)))
    SetLit {} ->
      return
        (Just
          (TyApp
            (mkAnn Nom NoSpan)
            (TyInd (mkAnn Nom NoSpan) ([], "küme"))
            [TyVar (mkAnn Nom NoSpan) ([], "öğe")]))
    MapLit {} ->
      return
        (Just
          (TyApp
            (mkAnn Nom NoSpan)
            (TyInd (mkAnn Nom NoSpan) ([], "sözlük"))
            [ TyVar (mkAnn Nom NoSpan) ([], "anahtar")
            , TyVar (mkAnn Nom NoSpan) ([], "değer")
            ]))
    Bind {bindExp} -> inferType bindExp
    Seq {second} -> inferType second
    Var {varCandidates} -> do
      MkEvalState{evalVals, evalCtors, evalFuncs, evalPrimFuncs} <- get
      case lookupByCandidates evalVals varCandidates of
        Just v -> inferType v
        Nothing ->
          case lookupCtorByCandidates evalCtors varCandidates of
            Just (argTys, ty) ->
              return (Just (foldr (Arr (mkAnn Nom NoSpan)) ty argTys))
            _ ->
              case functionValueType evalFuncs evalPrimFuncs varCandidates of
                Just fnTy -> return (Just fnTy)
                Nothing -> return Nothing
    App {fn, args} -> do
      fn' <- evalExpWith [] fn
      case fn' of
        Var {varCandidates} -> do
          MkEvalState{evalCtors, evalTyCons} <- get
          case lookupCtorByCandidates evalCtors varCandidates of
            Just (tys, resTy)
              | sameLength tys args ->
                  unifyCtorArgsLazy evalTyCons (zip tys args) [] >>= \case
                    Just subst -> return (Just (applySubst subst resTy))
                    Nothing -> return Nothing
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing
  where
    -- | Unify constructor argument types against actual arguments,
    -- inferring each argument's type only when the expected type still has
    -- free variables not yet pinned down by the substitution so far.
    --
    -- Evaluation follows successful typechecking, so arguments whose expected
    -- types have no unresolved variables cannot change the result type and do
    -- not need inference. In particular, list tails are not traversed once the
    -- head has fixed the element type.
    unifyCtorArgsLazy :: Map.Map Identifier Int
                      -> [(Ty Ann, Exp Ann)]
                      -> [(Identifier, Ty Ann)]
                      -> EvalM (Maybe [(Identifier, Ty Ann)])
    unifyCtorArgsLazy _ [] subst = return (Just subst)
    unifyCtorArgsLazy tyCons ((ty, argExp) : rest) subst
      | all (\v -> v `elem` map fst subst) (tyFreeVars ty) =
          unifyCtorArgsLazy tyCons rest subst
      | otherwise = do
          mArgTy <- inferType argExp
          case mArgTy of
            Nothing -> return Nothing
            Just argTy ->
              case unifyOneTy tyCons subst (normalizeTy tyCons ty) (normalizeTy tyCons argTy) of
                Just subst' -> unifyCtorArgsLazy tyCons rest subst'
                Nothing -> return Nothing
    functionValueType ::
         Map.Map Identifier [([Arg Ann], [Clause Ann])]
      -> Map.Map Identifier [([Arg Ann], [Exp Ann] -> EvalM (Exp Ann))]
      -> [(Identifier, Case)]
      -> Maybe (Ty Ann)
    functionValueType fnMap primMap candidates =
      let names = map fst candidates
          fnArgTys =
            [ map snd sigArgs
            | name <- names
            , (sigArgs, _) <- Map.findWithDefault [] name fnMap
            ]
          primArgTys =
            [ map snd sigArgs
            | name <- names
            , (sigArgs, _) <- Map.findWithDefault [] name primMap
            ]
          retTy = TyVar (mkAnn Nom NoSpan) ([], "r")
      in case fnArgTys ++ primArgTys of
           (argTys:_) -> Just (foldr (Arr (mkAnn Nom NoSpan)) retTy argTys)
           [] -> Nothing

-- | Apply a case annotation to an expression if it is a value.
applyTypeCase :: Case -- ^ Case to apply.
              -> Exp Ann -- ^ Expression to update.
              -> Exp Ann -- ^ Updated expression.
applyTypeCase cas exp =
  case exp of
    Var ann name candidates ->
      let filtered = filter (\(_, c) -> c == cas) candidates
          candidates' = if null filtered then candidates else filtered
      in Var (setAnnCase ann cas) name candidates'
    IntLit ann n ->
      IntLit (setAnnCase ann cas) n
    FloatLit ann n ->
      FloatLit (setAnnCase ann cas) n
    CharLit ann c ->
      CharLit (setAnnCase ann cas) c
    SetLit ann entries ->
      SetLit (setAnnCase ann cas) entries
    MapLit ann entries ->
      MapLit (setAnnCase ann cas) entries
    _ -> exp

-- | Check whether an inferred type matches an expected type.
--
-- Uses the same constructor map as 'typeMatchesAllowUnknown'.
typeMatches :: Map.Map Identifier Int -- ^ Type constructor arities (as 'Map.Map').
            -> Maybe (Ty Ann) -- ^ Possibly unknown type.
            -> Ty Ann -- ^ Expected type.
            -> Bool -- ^ True when types match.
typeMatches tyCons mTy ty =
  case mTy of
    Nothing -> False
    Just t -> tyEq tyCons t ty

-- | Compare two types for compatibility.
--
-- Constructor arities stay in a map for recursive normalization lookups.
tyEq :: Map.Map Identifier Int -- ^ Type constructor arities (as 'Map.Map').
     -> Ty Ann -- ^ Left type.
     -> Ty Ann -- ^ Right type.
     -> Bool -- ^ True when types are compatible.
tyEq tyCons t1 t2 =
  let n1 = normalizeTy tyCons t1
      n2 = normalizeTy tyCons t2
  in case (n1, n2) of
    (TyString _, TyString _) -> True
    (TyInt _, TyInt _) -> True
    (TyFloat _, TyFloat _) -> True
    (TyChar _, TyChar _) -> True
    (Arr _ d1 i1, Arr _ d2 i2) -> tyEq tyCons d1 d2 && tyEq tyCons i1 i2
    (TyInd _ n1', TyInd _ n2') -> identMatches n1' n2'
    (TySkolem _ n1', TySkolem _ n2') -> n1' == n2'
    (TySkolem {}, TyVar {}) -> True
    (TyVar {}, TySkolem {}) -> True
    (TySkolem {}, _) -> False
    (_, TySkolem {}) -> False
    (TyVar _ _, _) -> True
    (_, TyVar _ _) -> True
    (TyApp _ c1 as1, TyApp _ c2 as2) ->
      tyEq tyCons c1 c2 && sameLength as1 as2 && and (zipWith (tyEq tyCons) as1 as2)
    _ -> False

-- | Normalize type applications using constructor arities.
normalizeTy :: Map.Map Identifier Int -- ^ Type constructor arities (as 'Map.Map').
            -> Ty Ann -- ^ Type to normalize.
            -> Ty Ann -- ^ Normalized type.
normalizeTy tyCons ty =
  case ty of
    TyInt {} -> ty
    TyFloat {} -> ty
    TySkolem {} -> ty
    TyApp ann (TyInd _ name) args ->
      case Map.lookup name tyCons of
        Just arity | arity > 0 -> TyApp ann (TyInd (mkAnn Nom NoSpan) name) (map (normalizeTy tyCons) args)
        _ -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizeTy tyCons ctor) (map (normalizeTy tyCons) args)
    Arr ann d i ->
      Arr ann (normalizeTy tyCons d) (normalizeTy tyCons i)
    _ -> ty

-- | Unify expected types with actual types, returning substitutions.
unifyTypes :: Map.Map Identifier Int -- ^ Type constructor arities (as 'Map.Map').
           -> [Ty Ann] -- ^ Expected types.
           -> [Ty Ann] -- ^ Actual types.
           -> Maybe [(Identifier, Ty Ann)] -- ^ Substitution when unification succeeds.
unifyTypes tyCons expected actual =
  foldl' (unifyTypesStep tyCons) (Just []) (zip expected actual)

-- | Fold step for 'unifyTypes': unify one expected/actual type pair,
-- threading the substitution built up so far.
--
-- Hoisted to the top level (out of 'unifyTypes') so 'inferType' can reuse it
-- for lazy, argument-by-argument constructor unification; see
-- @unifyCtorArgsLazy@.
unifyTypesStep :: Map.Map Identifier Int -- ^ Type constructor arities.
               -> Maybe [(Identifier, Ty Ann)] -- ^ Current substitution.
               -> (Ty Ann, Ty Ann) -- ^ Expected and actual types.
               -> Maybe [(Identifier, Ty Ann)] -- ^ Updated substitution.
unifyTypesStep _ Nothing _ = Nothing
unifyTypesStep tyCons (Just subst) (e, a) =
  unifyOneTy tyCons subst (normalizeTy tyCons e) (normalizeTy tyCons a)

-- | Unify a single expected/actual type pair against a substitution.
unifyOneTy :: Map.Map Identifier Int -- ^ Type constructor arities.
           -> [(Identifier, Ty Ann)] -- ^ Current substitution.
           -> Ty Ann -- ^ Expected type.
           -> Ty Ann -- ^ Actual type.
           -> Maybe [(Identifier, Ty Ann)] -- ^ Updated substitution.
unifyOneTy tyCons subst e a =
  case e of
    TyInt _ ->
      case a of
        TyInt _ -> Just subst
        _ -> Nothing
    TyVar _ name ->
      case lookup name subst of
        Just bound ->
          if tyEq tyCons bound a
            then Just subst
            else Nothing
        Nothing -> Just ((name, a) : subst)
    TySkolem _ name ->
      case a of
        TySkolem _ name' | name == name' -> Just subst
        TyVar {} -> Just subst
        _ -> Nothing
    TyInd _ n1 ->
      case a of
        TyInd _ n2 | n1 == n2 -> Just subst
        _ -> Nothing
    TyString _ ->
      case a of
        TyString _ -> Just subst
        _ -> Nothing
    TyChar _ ->
      case a of
        TyChar _ -> Just subst
        _ -> Nothing
    Arr _ d1 i1 ->
      case a of
        Arr _ d2 i2 -> do
          subst' <- unifyOneTy tyCons subst d1 d2
          unifyOneTy tyCons subst' i1 i2
        _ -> Nothing
    TyApp _ c1 as1 ->
      case a of
        TyApp _ c2 as2
          | sameLength as1 as2 -> do
              subst' <- unifyOneTy tyCons subst c1 c2
              foldl' (unifyTypesStep tyCons) (Just subst') (zip as1 as2)
        _ -> Nothing

-- | Free type variable names occurring in a type.
--
-- Types are small, fixed-size expressions (a handful of nodes at most), so
-- a plain list is cheaper here than allocating a 'Data.Set.Set' for this
-- module's only consumer, @unifyCtorArgsLazy@.
tyFreeVars :: Ty Ann -- ^ Type tree to inspect.
           -> [Identifier] -- ^ Free type-variable names, including duplicates in traversal order.
tyFreeVars ty =
  case ty of
    TyVar _ name -> [name]
    TySkolem {} -> []
    TyInt {} -> []
    TyFloat {} -> []
    TyString {} -> []
    TyChar {} -> []
    TyInd {} -> []
    Arr _ d i -> tyFreeVars d ++ tyFreeVars i
    TyApp _ c as -> tyFreeVars c ++ concatMap tyFreeVars as

-- | Apply a type substitution to a type.
applySubst :: [(Identifier, Ty Ann)] -- ^ Substitution bindings.
           -> Ty Ann -- ^ Type to rewrite.
           -> Ty Ann -- ^ Rewritten type.
applySubst subst ty =
  case ty of
    TyVar ann name ->
      case lookup name subst of
        Just t -> t
        Nothing -> TyVar ann name
    TySkolem {} -> ty
    TyInt {} -> ty
    TyFloat {} -> ty
    TyInd {} -> ty
    TyString {} -> ty
    TyChar {} -> ty
    Arr ann d i -> Arr ann (applySubst subst d) (applySubst subst i)
    TyApp ann ctor args ->
      TyApp ann (applySubst subst ctor) (map (applySubst subst) args)

-- | Evaluate a statement with optional module context.
evalStmtInFile :: Maybe FilePath -- ^ Current file path.
               -> Stmt Ann -- ^ Statement to evaluate.
               -> EvalM () -- ^ No result.
evalStmtInFile mPath stmt =
  do
    modify (\s -> s { evalCurrentFile = mPath })
    case stmt of
      Defn name _ e -> do
        v <- evalExp e
        modify (\s -> s { evalVals = Map.insert name v (evalVals s) })
      Function name args _ body _ ->
        modify (\s ->
          let def = (args, body)
          in s
              { evalFuncs = Map.insertWith (++) name [def] (evalFuncs s)
              , evalDirectCalls = emptyDirectCallCache
              })
      PrimFunc name args _ _ ->
        case primImpl mPath name args of
          Nothing -> return ()
          Just impl ->
            modify (\s ->
              let def = (args, impl)
              in s
                  { evalPrimFuncs = Map.insertWith (++) name [def] (evalPrimFuncs s)
                  , evalDirectCalls = emptyDirectCallCache
                  })
      Load _ _ ->
        return ()
      NewType name params ctors -> do
        let selectors = []
            resultTy =
              case params of
                [] -> TyInd (mkAnn Nom NoSpan) name
                _ -> TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) params
            ctorSigs =
              [ (ctorName, (ctorArgs, resultTy))
              | ((ctorName, _), ctorArgs) <- ctors
              ]
        modify (\s -> s { evalSelectors = foldr (\(k, v) m -> Map.insertWith (++) k [v] m) (evalSelectors s) selectors
                        , evalCtors = Map.union (Map.fromList ctorSigs) (evalCtors s)
                        , evalTyCons = Map.insert name (length params) (evalTyCons s)
                        })
      PrimType name params ->
        modify (\s -> s { evalTyCons = Map.insert name (length params) (evalTyCons s) })
      ExpStmt e -> do
        _ <- evalExp e
        return ()

-- | Lookup the primitive implementation for a name and argument list.
primImpl :: Maybe FilePath -- ^ Current file path.
         -> Identifier -- ^ Primitive name.
         -> [Arg Ann] -- ^ Argument types.
         -> Maybe ([Exp Ann] -> EvalM (Exp Ann)) -- ^ Primitive implementation when known.
primImpl = Prim.primitiveEvalImpl mkPrimitiveEvalOps

-- | Host callbacks used by 'Prim.primitiveEvalImpl' in the evaluator runtime.
mkPrimitiveEvalOps :: Prim.PrimitiveEvalOps EvalM
mkPrimitiveEvalOps =
  Prim.PrimitiveEvalOps
    { Prim.peWriteText = liftIO . TIO.putStrLn
    , Prim.peWriteInteger = liftIO . print
    , Prim.peWriteDouble = liftIO . print
    , Prim.peFlushStdout = liftIO (hFlush stdout)
    , Prim.peReadLine = liftIO TIO.getLine
    , Prim.peReadFirstPath = liftIO . readFirstPath
    , Prim.peReadEnv = \name -> do
        mVal <- liftIO (lookupEnv (T.unpack name))
        pure (T.pack <$> mVal)
    , Prim.peGetCurrentFile = gets evalCurrentFile
    , Prim.peGetArgs = gets evalArgs
    , Prim.peWriteFileText = \path content -> do
        st <- get
        let resolved = resolvePath st (T.pack path)
        result <- liftIO (try (TIO.writeFile resolved content) :: IO (Either SomeException ()))
        pure (either (const False) (const True) result)
    , Prim.peGetRandState = gets evalRandState
    , Prim.peSetRandState = \seed -> modify (\s -> s { evalRandState = Just seed })
    , Prim.peLookupRandomSeed = liftIO $ do
        mSeed <- lookupEnv "KIP_RANDOM_SEED"
        pure (mSeed >>= readMaybe)
    , Prim.peRandomRange = \lo hi -> liftIO (randomRIO (lo, hi))
    }

-- | Resolve a file path relative to the current file when needed.
resolvePath :: EvalState -- ^ Current evaluator state.
            -> Text -- ^ Input path.
            -> FilePath -- ^ Resolved path.
resolvePath st path =
  let raw = T.unpack path
  in case evalCurrentFile st of
       Just base | isRelative raw -> takeDirectory base </> raw
       _ -> raw

-- | Build read candidates by walking up parent directories.
resolveReadCandidates :: EvalState -- ^ Current evaluator state.
                      -> Text -- ^ Input path.
                      -> [FilePath] -- ^ Candidate paths.
resolveReadCandidates st path =
  let raw = T.unpack path
  in case evalCurrentFile st of
       Just base | isRelative raw ->
         let start = takeDirectory base
         in map (</> raw) (parentDirs start)
       _ -> [raw]

-- | Try reading from the first existing candidate path.
readFirstPath :: [FilePath] -- ^ Candidate paths.
              -> IO (Maybe Text) -- ^ First readable contents.
readFirstPath paths =
  case paths of
    [] -> return Nothing
    p:ps -> do
      res <- try (TIO.readFile p) :: IO (Either SomeException Text)
      case res of
        Right content -> return (Just content)
        Left _ -> readFirstPath ps

-- | Collect parent directories up to the filesystem root.
parentDirs :: FilePath -- ^ Directory path.
           -> [FilePath] -- ^ Parent directories.
parentDirs dir =
  let parent = takeDirectory dir
  in if parent == dir
       then [dir]
       else dir : parentDirs parent

-- | Run an evaluator action with a starting state.
runEvalM :: EvalM a -- ^ Evaluator computation.
         -> EvalState -- ^ Initial evaluator state.
         -> IO (Either EvalError (a, EvalState)) -- ^ Result or error.
runEvalM m s = runExceptT (runStateT m s)
