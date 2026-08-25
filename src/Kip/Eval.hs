{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Evaluator for Kip expressions and statements.
module Kip.Eval where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Kip.AST
import qualified Kip.Primitive as Prim

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
    , evalFuncs :: !(Map.Map Identifier [([Arg Ann], [Clause Ann])]) -- ^ Function clauses (can be overloaded).
    , evalPrimFuncs :: !(Map.Map Identifier [([Arg Ann], [Exp Ann] -> EvalM (Exp Ann))]) -- ^ Primitive implementations (can be overloaded).
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
emptyEvalState = MkEvalState Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Nothing [] (\_ e -> return (prettyExp e)) Nothing

-- | Evaluation errors (currently minimal).
data EvalError =
   Unknown
   | UnboundVariable Identifier
   | NoMatchingFunction Identifier
   | NoMatchingClause
   | RuntimeTypeErrorNonValue
   deriving (Show, Eq, Generic, Binary)
-- | Evaluator monad stack.
type EvalM = StateT EvalState (ExceptT EvalError IO)

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
-}
-- | Check if a variable can be resolved in an application context.
--
-- Iterates the candidate list __once__, checking all namespaces per
-- candidate and short-circuiting on the first hit.  The previous
-- implementation allocated @fnCandidates = map fst varCandidates@ (1.9 %
-- alloc) and then iterated it 5 separate times (once per namespace).
-- This version eliminates the intermediate list and does at most one
-- pass.  Each per-candidate check uses 'Map.member' (O(log n) key-only)
-- for all five maps.
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
             -> Map.Map Identifier [([Arg Ann], [Exp Ann] -> EvalM (Exp Ann))]
             -> Maybe ([Exp Ann] -> EvalM (Exp Ann))
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
-- | Done: final value.
-- | Continue: evaluate a new (env, exp) without growing the Haskell call stack.
data EvalStep
  = Done (Exp Ann)
  | Continue (HM.HashMap Identifier (Exp Ann)) (Exp Ann)

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
evalExpTraced :: Exp Ann -> EvalM (Exp Ann, [TraceStep])
evalExpTraced e = do
  ref <- liftIO (newIORef [])
  ctr <- liftIO (newIORef (0 :: Int))
  result <- evalExpLoopTraced ref ctr 0 HM.empty e
  steps <- liftIO (readIORef ref)
  return (result, reverse steps)

-- | Step limit for traced evaluation.
traceStepLimit :: Int
traceStepLimit = 1000

-- | Traced trampoline loop. Records a 'TraceStep' for each step that
-- produces a different output than its input (skipping trivial literal
-- identity steps). Falls back to the normal 'evalExpLoop' when the step
-- counter reaches 'traceStepLimit'.
evalExpLoopTraced :: IORef [TraceStep] -> IORef Int -> Int
                  -> HM.HashMap Identifier (Exp Ann) -> Exp Ann
                  -> EvalM (Exp Ann)
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
evalSubTraced :: IORef [TraceStep] -> IORef Int -> Int
              -> HM.HashMap Identifier (Exp Ann) -> Exp Ann
              -> EvalM (Exp Ann)
evalSubTraced ref ctr depth = evalExpLoopTraced ref ctr (depth + 1)

-- | Substitute local environment bindings into an expression for trace display.
-- This is only for human-readable tracing and does not affect evaluation.
substituteTraceEnv :: HM.HashMap Identifier (Exp Ann) -> Exp Ann -> Exp Ann
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
sameExp :: Exp Ann -> Exp Ann -> Bool
sameExp (IntLit _ a) (IntLit _ b) = a == b
sameExp (FloatLit _ a) (FloatLit _ b) = a == b
sameExp (StrLit _ a) (StrLit _ b) = a == b
sameExp (CharLit _ a) (CharLit _ b) = a == b
sameExp (SetLit _ a) (SetLit _ b) = a == b
sameExp (MapLit _ a) (MapLit _ b) = a == b
sameExp (Var _ n1 _) (Var _ n2 _) = n1 == n2
sameExp _ _ = False

-- | Check if two expressions are structurally equal, ignoring annotations.
eqIgnoringAnn :: Exp Ann -> Exp Ann -> Bool
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
substituteChildren :: [(Exp Ann, Exp Ann)] -> Exp Ann -> Exp Ann
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
      fn' <-
        case fn of
          Var {varName, varCandidates} -> do
            st@MkEvalState{evalVals} <- get
            case lookupByCandidatesHM localEnv varCandidates of
              Just _ -> subEval localEnv fn
              Nothing ->
                case lookupBySuffixHM localEnv varName of
                  Just _ -> subEval localEnv fn
                  Nothing ->
                    case lookupByCandidates evalVals varCandidates of
                      Just _ -> subEval localEnv fn
                      Nothing -> pure fn
          _ -> subEval localEnv fn
      args' <- mapM (subEval localEnv) args
      let (fnResolved, preAppliedArgs) = flattenApplied fn'
          allArgs = preAppliedArgs ++ args'
      case fnResolved of
        Var {varName, varCandidates} -> do
          -- Pull state once for all resolution steps.
          MkEvalState{evalFuncs, evalPrimFuncs, evalSelectors, evalTyCons} <- get
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
              let partialCall = not (null preAppliedArgs)
                  callArgs = reorderSectionArgs preAppliedArgs allArgs
                  pickPrim = if partialCall then pickPrimByTypesPartial else pickPrimByTypes
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
    Seq {annExp, first, second} -> do
      case first of
        Bind {bindName, bindNameAnn, bindExp} -> do
          -- Tail position: continue with the extended environment and second.
          v <- subEval localEnv bindExp
          return (Continue (HM.insert bindName v localEnv) second)
        _ -> do
          -- Evaluate the first expression, then tail-continue into second.
          _ <- subEval localEnv first
          return (Continue localEnv second)
    Match {annExp, scrutinee, clauses} -> do
      -- Non-tail: we need the scrutinee value to select the clause.
      scrutinee' <- subEval localEnv scrutinee
      case findClause scrutinee' clauses of
        Nothing -> throwError NoMatchingClause
        Just (Clause _ body, patBindings) -> do
          let env = HM.fromList patBindings `HM.union` localEnv
          -- Tail position: continue with the clause body.
          return (Continue env body)
    Let {annExp, varName, body} ->
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
    Just (Clause pat body, patBindings) -> do
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
    -- 'roots' produces at most 2 candidates (@txt@ itself plus an
    -- optional vowel-drop variant).  The previous @nub . catMaybes@
    -- and @intersect@ on such tiny lists allocated intermediate lists
    -- unnecessarily.  Now uses direct element comparison via
    -- 'rootsOverlap' to avoid all list allocation.
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
    -- Uses the module-level 'copulaSuffixes' list (all 3 chars long)
    -- instead of allocating a local list on every call.  Strips from
    -- the original text using character count to preserve the original
    -- casing.
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

-- | Try resolving an ip-converb function to its base name when prim lookup fails.
-- | Recognize the random primitive in either split or dashed identifier form.

-- | Match a list pattern against an expression.
matchList :: [Pat Ann] -- ^ Element patterns.
          -> Exp Ann -- ^ Scrutinee expression.
          -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
-- With @OverloadedStrings@ enabled, string literals are compiled as
-- top-level 'Text' constants by GHC, avoiding a per-call 'T.pack'
-- allocation.  The previous @T.pack \"boş\"@ / @T.pack \"eki\"@ calls
-- allocated fresh 'Text' values on every pattern-match attempt.
matchList [] (Var _ ([], name) _)
  | name == ("boş" :: T.Text) = Just []
matchList (p:ps) (App _ (Var _ ([], name) _) [elem, rest])
  | name == ("eki" :: T.Text) = do
      elemBinds <- matchPat p (Just elem)
      restBinds <- matchList ps rest
      return (elemBinds ++ restBinds)
matchList _ _ = Nothing
isRandomCandidate :: [(Identifier, Case)] -> Bool
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

-- | Check whether two lists have the same length without computing both
-- lengths fully.
--
-- @'sameLength' xs ys@ walks both lists in lock-step and returns 'False'
-- as soon as one list is exhausted before the other.  This is O(min(m,n))
-- instead of O(m + n) for @'length' xs == 'length' ys@.
--
-- The @length tys == length args@ guard appears in every list
-- comprehension inside 'pickFunctionByTypes', 'pickPrimByTypes' and
-- their @Partial@ variants.  For programs with many overloaded
-- definitions, the guard is evaluated for every candidate, so
-- short-circuiting on the first mismatch avoids unnecessary spine
-- traversal.
sameLength :: [a] -> [b] -> Bool
sameLength [] [] = True
sameLength (_:xs) (_:ys) = sameLength xs ys
sameLength _ _ = False

-- | Reorder arguments for one-argument section applications.
-- Instrumental fixed arguments are left sections; other fixed arguments are
-- treated as right sections.
reorderSectionArgs :: [Exp Ann] -> [Exp Ann] -> [Exp Ann]
reorderSectionArgs preApplied args =
  case (preApplied, args) of
    ([fixed], [x, y])
      | annCase (annExp fixed) /= Ins -> [y, x]
    _ -> args

-- | Choose a function definition based on inferred argument types.
--
-- Previously called @mapM inferType args@ internally.  Since overload
-- resolution tries primitives first ('pickPrimByTypes') and then falls
-- back to functions, the same argument list had its types inferred
-- /twice/ on every call that goes through both paths.  Profiling showed
-- 'inferType' at __~9.5 % time / ~17.8 % allocation__ for evaluation-heavy
-- workloads.  By accepting pre-computed @argTys@ from the call site we
-- eliminate the duplicate inference entirely.
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
reorderByCasesForEval :: [Case] -> [Case] -> [a] -> Maybe [a]
reorderByCasesForEval expected actual xs =
  case reorderByCasesNomFallback expected actual xs of
    Just reordered -> Just reordered
    Nothing -> reorderInsFromGen expected actual xs
  where
    reorderInsFromGen expCases actCases vals
      | length expCases /= length actCases || length actCases /= length vals = Nothing
      | otherwise = map snd <$> go (zip actCases vals) expCases
    go rems [] = Just []
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
-- Previously accepted @[(Identifier, Int)]@ which forced every call site
-- (4 in total across 'pickFunctionByTypes', 'pickPrimByTypes', and their
-- @Partial@ variants) to convert via @'Map.toList' evalTyCons@.  Each of
-- those conversions allocated a fresh spine of pairs on every function
-- call.  By accepting the 'Map.Map' directly we eliminate those
-- allocations entirely and let 'normalizeTy' use O(log n) 'Map.lookup'
-- instead of O(n) list 'lookup'.
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

-- | Lookup a binding by candidate identifiers in a list-based environment.
--
-- Iterates the candidate pairs directly, avoiding the intermediate list
-- allocation that @map fst candidates@ would create.
--
-- Profiling showed the previous @let names = map fst candidates@ pattern
-- accounted for __3.6 % of allocation__ in evaluation-heavy workloads.
lookupByCandidatesList :: forall a.
                          [(Identifier, a)] -- ^ Candidate bindings.
                       -> [(Identifier, Case)] -- ^ Candidate identifiers.
                       -> Maybe a -- ^ Matching binding when found.
lookupByCandidatesList env = go
  where
    go [] = Nothing
    go ((n, _):rest) =
      case lookup n env of
        Just v -> Just v
        Nothing -> go rest

-- | Lookup by candidates in a 'Map.Map'-based environment.
--
-- Iterates candidate pairs directly to avoid allocating an intermediate
-- @[Identifier]@ list.  This is the primary lookup function used by
-- 'evalStepWith' for global value bindings.
--
-- Combined with the HashMap variant, eliminating the @map fst@ allocation
-- saves __~6 % of total allocation__ in evaluation-heavy workloads.
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

-- | Heuristic fallback for matching inflected variables in list-based local bindings.
lookupBySuffixList :: [(Identifier, a)] -- ^ Local environment bindings.
                   -> Identifier -- ^ Surface identifier.
                   -> Maybe a -- ^ Matching binding when found.
lookupBySuffixList env (mods, word) =
  let stripped = stripCaseRoots mods word
  in findMatch stripped
  where
    findMatch [] = Nothing
    findMatch (ident:rest) =
      case lookup ident env of
        Just v -> Just v
        Nothing -> findMatch rest

-- | Heuristic fallback for matching inflected variables in Map-based local bindings.
lookupBySuffixMap :: Map.Map Identifier a -- ^ Local environment bindings.
                  -> Identifier -- ^ Surface identifier.
                  -> Maybe a -- ^ Matching binding when found.
lookupBySuffixMap env (mods, word) =
  let stripped = stripCaseRoots mods word
  in findMatch stripped
  where
    findMatch [] = Nothing
    findMatch (ident:rest) =
      case Map.lookup ident env of
        Just v -> Just v
        Nothing -> findMatch rest

-- | Lookup by candidates in a 'HM.HashMap'-based environment (O(1) average).
--
-- Iterates candidate pairs directly without allocating an intermediate
-- @[Identifier]@ list via @map fst@.  Uses 'HM.lookup' for O(1) average
-- lookup time.
--
-- Profiling showed the previous @map fst candidates@ allocation in this
-- function alone accounted for __2.3 % of allocation__.
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
-- stop at the first matching suffix rather than trying all 24 entries in
-- 'bareCaseSuffixes'.  This turns an O(s * n) list comprehension followed
-- by 'nub' into an O(s) scan (where s = number of suffixes).
--
-- In addition to the plain stripped root, a trailing-@n@ fallback is
-- included for pronoun stems (e.g. @bun@ → @bu@).
--
-- Profiling showed the previous implementation at __18.6 % of runtime__
-- and __23 % of allocation__ for evaluation-heavy workloads because it
-- generated up to 48 candidates (24 suffixes × 2 variants) and then
-- called 'Data.List.nub' on the result.
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
                          -> [(Identifier, Case)]
                          -> Maybe a
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
--
-- Lifted to the module level so the list is allocated once instead of
-- on every call to @stripCopulaSuffix@ inside 'matchCtor'.
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
    -- The previous implementation called 'mapM inferType' on __every__
    -- constructor argument up front, before unification even started. For a
    -- binary constructor like list cons (@eki :: a -> Liste a -> Liste a@),
    -- inferring the second (recursive tail) argument's type walks the
    -- entire rest of the list, even though the type variable @a@ is always
    -- already resolved by the *first* argument alone. That turned every
    -- 'inferType' call on a list into an O(n) traversal of the whole
    -- structure, and O(n^2) when called at each step of a recursive
    -- traversal.
    --
    -- Since 'inferType' only ever runs on already-typechecked programs
    -- (evaluation happens strictly after a successful typecheck pass), an
    -- argument whose expected type has no free variables left to resolve
    -- cannot change the constructor's inferred result type -- so its actual
    -- type never needs to be computed at all. This turns list-cons
    -- inference into O(1): only the head element's type is ever inferred,
    -- never the tail's.
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
-- Accepts 'Map.Map' to stay consistent with 'typeMatchesAllowUnknown'
-- and avoid list conversion overhead (see that function's documentation).
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
-- Accepts 'Map.Map' so that the recursive calls to 'normalizeTy' benefit
-- from O(log n) lookups instead of O(n) list scans.
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
--
-- Previously accepted @[(Identifier, Int)]@ and used 'lookup' (O(n) linear
-- scan).  Now accepts 'Map.Map' and uses 'Map.lookup' (O(log n)).  Since
-- 'normalizeTy' is called recursively on every sub-term during type
-- comparison, this change turns a quadratic lookup pattern into an
-- efficient logarithmic one.
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
--
-- Accepts 'Map.Map' so that 'normalizeTy' (called on every type pair)
-- uses O(log n) lookups instead of O(n) list scans.
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
-- 'unifyCtorArgLazy'.
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
-- module's only consumer, 'unifyCtorArgsLazy'.
tyFreeVars :: Ty Ann -> [Identifier]
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
        modify (\s -> s { evalFuncs = Map.insertWith (++) name [(args, body)] (evalFuncs s) })
      PrimFunc name args _ _ ->
        case primImpl mPath name args of
          Nothing -> return ()
          Just impl ->
            modify (\s -> s { evalPrimFuncs = Map.insertWith (++) name [(args, impl)] (evalPrimFuncs s) })
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

-- | Evaluate a statement in the global context.
evalStmt :: Stmt Ann -- ^ Statement to evaluate.
         -> EvalM () -- ^ No result.
evalStmt = evalStmtInFile Nothing

-- | Evaluate a statement inside the REPL.
replStmt :: Stmt Ann -- ^ Statement to evaluate.
         -> EvalM () -- ^ No result.
replStmt stmt =
  case stmt of
    ExpStmt e -> do
      _ <- evalExp e
      liftIO (putStrLn "")
      -- liftIO (putStrLn (prettyExp e'))
    PrimFunc {} -> evalStmt stmt
    PrimType {} -> evalStmt stmt
    _ -> evalStmt stmt

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
