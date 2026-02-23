{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
-- | Type checker and type inference for Kip.
-- |
-- | This module performs a single-pass, syntax-directed check over the AST while
-- | threading a mutable 'TCState'. The main flow is:
-- |
-- |   1. 'registerForwardDecls' pre-seeds 'tcCtx', 'tcFuncSigs', 'tcCtors', and
-- |      'tcTyCons' so later references can resolve in one pass.
-- |   2. 'tcStmt' walks statements, checking and recording definitions,
-- |      constructor signatures, and return types in 'tcFuncSigRets'.
-- |   3. 'tcExp1With' checks expressions bottom-up, resolves names, matches
-- |      overloads, and rewrites applications with case-ordered arguments.
-- |
-- | Name resolution and overloads
-- |
-- |   * Each identifier occurrence carries candidate bindings and a grammatical
-- |     case. 'resolveVar' filters candidates by scope ('tcCtx'), optional
-- |     arity (for applications), and then case; failure yields 'UnknownName',
-- |     'NoType', or 'Ambiguity'.
-- |   * Functions may be overloaded ('tcFuncSigs'). For a call, we filter by
-- |     arity, then by case compatibility, then by type compatibility.
-- |   * Constructors ('tcCtors') are /not/ overloaded: any mismatch in arity,
-- |     case, or type yields 'NoMatchingCtor'.
-- |
-- | Grammatical cases and when they are resolved
-- |
-- |   * Cases are stored in annotations ('Ann') on expressions and in argument
-- |     signatures. During application, 'tcExp1With' gathers the actual cases
-- |     and attempts to align them with the signature’s expected case order.
-- |   * 'reorderByCases' permutes the argument list only when the expected and
-- |     actual cases are the same set with no duplicates; otherwise the
-- |     signature is rejected.
-- |   * After reordering, each argument is checked for a case mismatch. Strict
-- |     mismatches reject the overload, except for /flexible/ cases:
-- |
-- |       - Pattern-bound variables (found in 'tcVarTys') are allowed to float
-- |         across cases.
-- |       - Constructor applications are always strict (no flexible case).
-- |       - Other function calls are treated as flexible.
-- |
-- |   * Case resolution happens at call sites (not later): successful matches
-- |     return an 'App' with arguments reordered to the signature. When a type
-- |     constructor is applied as a unary "case marker", 'applyTypeCase' updates
-- |     the argument’s case immediately.
-- |
-- | Type inference and unknowns
-- |
-- |   * 'inferType' returns 'Maybe (Ty Ann)'. Unknowns propagate as 'Nothing' so
-- |     we can defer errors during partial inference.
-- |   * For variables, we first consult 'tcVarTys' (pattern/let bindings), then
-- |     inlineable values ('tcVals'), then nullary constructors. Otherwise we
-- |     fall back to a 'TyVar' tagged with the occurrence case, or 'Nothing' if
-- |     the name is not in scope.
-- |   * For applications, constructors use 'unifyTypes' to produce a
-- |     substitution for type variables, which is applied to the constructor’s
-- |     result type. Functions use 'typeMatchesAllowUnknown' and
-- |     'tcFuncSigRets' to pick a return type; unknown arguments allow us to
-- |     keep going without choosing arbitrarily.
-- |
-- | Parametric polymorphism and skolems
-- |
-- |   * Unknown type identifiers in annotations are parsed as 'TyVar' and are
-- |     treated as implicitly quantified type variables. A 'TyInd' whose name
-- |     is /not/ present in 'tcTyCons' is also treated as a type variable.
-- |   * When type-checking a function body, argument types are skolemized
-- |     ('TyVar' -> 'TySkolem') before being added to 'tcVarTys'/'tcFuncSigs'.
-- |     This makes them rigid inside the body and prevents unification with
-- |     concrete types (e.g., @tam-sayı@) unless instantiated at the call site.
-- |   * 'tyEq' and 'unifyTypes' treat 'TyVar' as flexible (with the exception
-- |     that it cannot unify with function types), while 'TySkolem' matches only
-- |     itself (or a flexible 'TyVar').
-- |   * For explicit polymorphic annotations, 'tyMatchesRigid' compares the
-- |     inferred type against the declared type, requiring rigid variables on
-- |     the right-hand side to match /exactly/. This avoids collapsing
-- |     parametric types into primitives.
-- |
-- | Pattern matching
-- |
-- |   * 'tcClause' infers pattern-bound variables via 'inferPatTypes'. The
-- |     scrutinee type is unified with the constructor’s result type; the
-- |     resulting substitution is applied to the constructor’s argument types
-- |     and bound into 'tcVarTys'.
-- |   * A mismatch raises 'PatternTypeMismatch' with both expected and actual
-- |     types.
-- |
-- | Type name syntax
-- |
-- |   * Hyphens are part of a type name (e.g., @tam-sayı@). A space between
-- |     identifiers denotes type application, not a composite name, so
-- |     @tam sayı@ is parsed as applying @sayı@ to the argument @tam@.
-- |   * The checker does not normalize between hyphenated and space-separated
-- |     forms; it relies on the parser's structure to distinguish names from
-- |     applications.
module Kip.TypeCheck where

import GHC.Generics (Generic)
import Data.Binary (Binary, Get)
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Binary as B
import Data.Word (Word8)
import Kip.AST
import qualified Kip.Primitive as Prim

import Control.Monad (unless, when, forM_, guard)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.List (find, foldl', intersect, nub, zipWith4)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, isJust, maybeToList, listToMaybe)
import qualified Data.Map.Strict as Map
import System.FilePath (FilePath)
import qualified Data.Set as Set
import qualified Data.Text as T
import Kip.Parser (stripBareCaseSuffix)

-- | Record a resolved name at a source span (most-recent wins).
recordResolvedName :: Span -> Identifier -> TCM ()
recordResolvedName sp ident =
  modify (\s -> s { tcResolvedNames = (sp, ident) : tcResolvedNames s })

recordResolvedSig :: Span -> Identifier -> [Ty Ann] -> TCM ()
recordResolvedSig sp ident tys =
  modify (\s -> s { tcResolvedSigs = (sp, (ident, tys)) : tcResolvedSigs s })

recordResolvedType :: Span -> Ty Ann -> TCM ()
recordResolvedType sp ty =
  modify (\s -> s { tcResolvedTypes = (sp, ty) : tcResolvedTypes s })

-- | Merge definition locations from a file (latest wins).
recordDefLocations :: FilePath -> Map.Map Identifier Span -> TCM ()
recordDefLocations path defs =
  modify (\s -> s { tcDefLocations = Map.union (Map.map (path,) defs) (tcDefLocations s) })

recordFuncSigLocations :: FilePath -> Map.Map (Identifier, [Ty Ann]) Span -> TCM ()
recordFuncSigLocations path defs =
  modify (\s -> s { tcFuncSigLocs = Map.union (Map.map (path,) defs) (tcFuncSigLocs s) })

-- | Type checker state for names, signatures, and constructors.
data TCState =
  MkTCState
    { tcCtx :: Set.Set Identifier -- ^ Names in scope.
    , tcFuncs :: Map.Map Identifier [Int] -- ^ Known function arities (list for overloading).
    , tcFuncSigs :: Map.Map Identifier [[Arg Ann]] -- ^ Function argument signatures (list for overloading).
    -- | Exact-arity signature index for overload resolution.
    --
    -- ==== Performance note (Optimization: overload candidate pruning)
    -- Call checking repeatedly requests "signatures of name N with arity K".
    -- Keeping this index avoids filtering every signature list on each call.
    , tcFuncSigsByArity :: Map.Map (Identifier, Int) [[Arg Ann]]
    , tcFuncSigRets :: Map.Map (Identifier, [Ty Ann]) (Ty Ann) -- ^ Function return types by arg types.
    , tcVarTys :: [(Identifier, Ty Ann)] -- ^ Variable type bindings (list for shadowing).
    , tcVals :: Map.Map Identifier (Exp Ann) -- ^ Value bindings for inlining.
    , tcCtors :: Map.Map Identifier ([Ty Ann], Ty Ann) -- ^ Constructor signatures.
    , tcTyCons :: Map.Map Identifier Int -- ^ Type constructor arities.
    , tcInfinitives :: Set.Set Identifier -- ^ Infinitive (effectful) functions.
    , tcResolvedNames :: [(Span, Identifier)] -- ^ Resolved variable names by span.
    , tcResolvedSigs :: [(Span, (Identifier, [Ty Ann]))] -- ^ Resolved function signatures by span.
    , tcResolvedTypes :: [(Span, Ty Ann)] -- ^ Resolved variable types by span.
    , tcDefLocations :: Map.Map Identifier (FilePath, Span) -- ^ Definition locations by identifier.
    , tcFuncSigLocs :: Map.Map (Identifier, [Ty Ann]) (FilePath, Span) -- ^ Definition locations by signature.
    -- | Environment-change token for inferType memo safety.
    --
    -- The token is bumped whenever surrounding typing context changes.
    -- This lets future memoized inferType lookups remain context-aware.
    , tcInferMemoToken :: !Int
    -- | Per-pass inferType memo table.
    --
    -- ==== Note
    -- The memo infrastructure is intentionally conservative right now; we
    -- keep the state plumbing and invalidation hooks in place, but inference
    -- currently uses the uncached path to preserve exact behavior.
    , tcInferTypeMemo :: Map.Map (Int, Span, T.Text) (Maybe (Ty Ann))
    }
  deriving (Generic)

-- | Binary instance for type checker state.
instance Binary TCState where
  put MkTCState{..} = do
    B.put tcCtx
    B.put [(k, v) | (k, vs) <- Map.toList tcFuncs, v <- vs]
    B.put [(k, v) | (k, vs) <- Map.toList tcFuncSigs, v <- vs]
    B.put (Map.toList tcFuncSigRets)
    B.put tcVarTys
    B.put (Map.toList tcVals)
    B.put (Map.toList tcCtors)
    B.put (Map.toList tcTyCons)
    B.put tcInfinitives
    B.put tcResolvedNames
    B.put tcResolvedSigs
    B.put tcResolvedTypes
    B.put tcDefLocations
    B.put tcFuncSigLocs
  get = do
    ctx <- B.get
    funcs <- Map.fromListWith (++) . map (\(k,v) -> (k,[v])) <$> B.get
    funcSigs <- Map.fromListWith (++) . map (\(k,v) -> (k,[v])) <$> B.get
    funcSigRets <- Map.fromList <$> B.get
    varTys <- B.get
    vals <- Map.fromList <$> B.get
    ctors <- Map.fromList <$> B.get
    tyCons <- Map.fromList <$> B.get
    infinitives <- B.get
    resolvedNames <- B.get
    resolvedSigs <- B.get
    resolvedTypes <- B.get
    defLocs <- B.get
    funcSigLocs <- B.get
    let byArity = buildFuncSigsByArity funcSigs
    return (MkTCState ctx funcs funcSigs byArity funcSigRets varTys vals ctors tyCons infinitives resolvedNames resolvedSigs resolvedTypes defLocs funcSigLocs 0 Map.empty)

-- | Empty type checker state.
emptyTCState :: TCState -- ^ Empty type checker state.
emptyTCState = MkTCState Set.empty Map.empty Map.empty Map.empty Map.empty [] Map.empty Map.empty Map.empty Set.empty [] [] [] Map.empty Map.empty 0 Map.empty

-- | Prepend a single value to the list stored under a key in a 'Map.Map'.
--
-- Replaces @Data.MultiMap.insert@ with the same prepend semantics.
-- Used for overloadable bindings ('tcFuncs', 'tcFuncSigs').
mmInsert :: Ord k => k -> v -> Map.Map k [v] -> Map.Map k [v]
mmInsert k v = Map.insertWith (++) k [v]

-- | Build an exact-arity index for function signatures.
--
-- This is rebuilt when loading cached state and then maintained
-- incrementally by 'insertFuncSig'.
buildFuncSigsByArity :: Map.Map Identifier [[Arg Ann]]
                    -> Map.Map (Identifier, Int) [[Arg Ann]]
buildFuncSigsByArity sigs =
  Map.fromListWith (++)
    [ ((name, length args), [args])
    | (name, sigList) <- Map.toList sigs
    , args <- sigList
    ]

-- | Insert one function signature into both signature indices.
--
-- Keeping both maps synchronized is cheaper than rebuilding the arity index
-- after every declaration.
insertFuncSig :: Identifier -> [Arg Ann] -> TCState -> TCState
insertFuncSig name args st =
  let argsNorm = map (Bifunctor.second normalizePrimTy) args
  in st
      { tcFuncSigs = mmInsert name argsNorm (tcFuncSigs st)
      , tcFuncSigsByArity = mmInsert (name, length argsNorm) argsNorm (tcFuncSigsByArity st)
      }

-- | Invalidate inferType memo after any environment-affecting state change.
--
-- This centralizes invalidation so future memo-enabled inference can remain
-- sound as bindings/signatures enter and leave scope.
invalidateInferMemo :: TCState -> TCState
invalidateInferMemo st =
  st
    { tcInferMemoToken = tcInferMemoToken st + 1
    , tcInferTypeMemo = Map.empty
    }

-- | Type checker errors.
data TCError =
   Unknown
 | NoType Span
 | Ambiguity Span
 | UnknownName Identifier Span
 | NoMatchingOverload Identifier [Maybe (Ty Ann)] [(Identifier, [Arg Ann])] Span
 | NoMatchingCtor Identifier [Maybe (Ty Ann)] [Ty Ann] Span
 | PatternTypeMismatch Identifier (Ty Ann) (Ty Ann) Span  -- ctor, expected (ctor result), actual (scrutinee)
 | NonExhaustivePattern [Pat Ann] Span
 | UnimplementedPrimitive Identifier [Arg Ann] Span
 | InvalidReturnCase Case Span
  deriving (Show, Ord, Eq, Generic)

-- | Binary instance for type checker errors.
instance Binary TCError where
  put Unknown = B.put (0 :: Word8)
  put (NoType sp) = B.put (1 :: Word8) >> B.put sp
  put (Ambiguity sp) = B.put (2 :: Word8) >> B.put sp
  put (UnknownName ident sp) = B.put (3 :: Word8) >> B.put ident >> B.put sp
  put (NoMatchingOverload ident mty sigs sp) = B.put (4 :: Word8) >> B.put ident >> B.put mty >> B.put sigs >> B.put sp
  put (NoMatchingCtor ident mty tys sp) = B.put (5 :: Word8) >> B.put ident >> B.put mty >> B.put tys >> B.put sp
  put (PatternTypeMismatch ctor expTy actTy sp) = B.put (6 :: Word8) >> B.put ctor >> B.put expTy >> B.put actTy >> B.put sp
  put (NonExhaustivePattern pats sp) = B.put (7 :: Word8) >> B.put pats >> B.put sp
  put (UnimplementedPrimitive ident args sp) = B.put (8 :: Word8) >> B.put ident >> B.put args >> B.put sp
  put (InvalidReturnCase cas sp) = B.put (9 :: Word8) >> B.put cas >> B.put sp

  get = do
    tag <- B.get :: Get Word8
    case tag of
      0 -> return Unknown
      1 -> NoType <$> B.get
      2 -> Ambiguity <$> B.get
      3 -> UnknownName <$> B.get <*> B.get
      4 -> NoMatchingOverload <$> B.get <*> B.get <*> B.get <*> B.get
      5 -> NoMatchingCtor <$> B.get <*> B.get <*> B.get <*> B.get
      6 -> PatternTypeMismatch <$> B.get <*> B.get <*> B.get <*> B.get
      7 -> NonExhaustivePattern <$> B.get <*> B.get
      8 -> UnimplementedPrimitive <$> B.get <*> B.get <*> B.get
      9 -> InvalidReturnCase <$> B.get <*> B.get
      _ -> fail "Invalid TCError tag"

-- | Type checker monad stack.
type TCM = StateT TCState (ExceptT TCError IO)

-- | Type-check an expression and return all possible variants.
tcExp :: Exp Ann -- ^ Expression to type-check.
      -> TCM [Exp Ann] -- ^ Type-checked expression variants.
tcExp e = do
  e' <- tcExp1With False e
  return [e']

-- | Type-check a single expression.
tcExp1 :: Exp Ann -- ^ Expression to type-check.
       -> TCM (Exp Ann) -- ^ Type-checked expression.
tcExp1 = tcExp1With False

-- | Type-check a single expression, optionally allowing effects.
tcExp1With :: Bool -- ^ Whether to allow effects.
           -> Exp Ann -- ^ Expression to type-check.
           -> TCM (Exp Ann) -- ^ Type-checked expression.
tcExp1With allowEffect e =
  case e of
    Var {annExp, varName, varCandidates} -> do
      resolved <- resolveVar annExp varName Nothing varCandidates
      case resolved of
        Var {annExp = annRes, varCandidates = [(ident, _)]} -> do
          recordResolvedName (annSpan annRes) ident
          -- Record variable type for LSP hover
          mTy <- inferType resolved
          forM_ mTy (recordResolvedType (annSpan annRes))
          unless allowEffect (rejectReadEffect annExp ident)
          MkTCState{tcFuncs} <- get
          if 0 `elem` Map.findWithDefault [] ident tcFuncs
            then do
              recordResolvedSig (annSpan annRes) ident []
              return (App annExp resolved [])
            else return resolved
        _ -> return resolved
    App {annExp = annApp, fn, args} -> do
      fn' <- case fn of
        Var {annExp, varName, varCandidates} ->
          resolveVar annExp varName (Just (length args)) varCandidates
        _ -> tcExp1With allowEffect fn
      args' <- mapM (tcExp1With False) args
      let (fnResolved, preAppliedArgs) = flattenApplied fn'
          allArgs = preAppliedArgs ++ args'
      case fnResolved of
        Var {annExp = annFn, varName, varCandidates} -> do
          case varCandidates of
            (ident, _) : _ -> unless allowEffect (rejectReadEffect annFn ident)
            _ -> return ()
          MkTCState{tcFuncSigs, tcFuncSigsByArity, tcTyCons, tcCtors, tcFuncSigRets, tcVarTys} <- get
          let higherOrderResultTy =
                case lookupByCandidates tcVarTys varCandidates of
                  Just (Arr _ _ imgTy) -> Just imgTy
                  _ -> Nothing
              isBoundHigherOrderVar =
                case lookupByCandidates tcVarTys varCandidates of
                  Just Arr {} -> True
                  _ -> False
              isEffectfulHigherOrderVar =
                case lookupByCandidates tcVarTys varCandidates of
                  Just (Arr _ domTy _) -> annCase (annTy domTy) /= Gen
                  _ -> False
              allowsVerbLikeHigherOrderCall =
                annCase annFn /= Gen && isBoundHigherOrderVar && isEffectfulHigherOrderVar
              isConditionalResultTy ty =
                let tyNorm = normalizePrimTy ty
                    tyConsList = Map.toList tcTyCons
                    nullaryCtorCount =
                      length
                        [ ()
                        | (ctorArgs, resTy) <- Map.elems tcCtors
                        , null ctorArgs
                        , tyEq tyConsList resTy tyNorm || tyEq tyConsList tyNorm resTy
                        ]
                in nullaryCtorCount >= 2
          case higherOrderResultTy of
            Just imgTy
              | annCase annFn /= Gen
              , not allowsVerbLikeHigherOrderCall
              , not (isConditionalResultTy imgTy) ->
                  lift (throwE (NoType (annSpan annApp)))
            _ -> return ()
          let tyNames = Map.keys tcTyCons
              funcNames = Map.keys tcFuncSigs
          case allArgs of
            [arg] | any (\(ident, _) -> ident `elem` tyNames) varCandidates
                  , not (any (\(ident, _) -> ident `elem` funcNames) varCandidates) ->
              return (applyTypeCase (annCase annFn) arg)
            _ -> do
              let nameForErr =
                    case varCandidates of
                      (ident, _):_ -> ident
                      [] -> varName
              let fnNames = map fst varCandidates
                  allSigs = [(n, sig) | n <- fnNames, sig <- Map.findWithDefault [] n tcFuncSigs]
                  -- Fast-path: exact-arity overloads come from the dedicated
                  -- index instead of filtering all signatures by length.
                  exactSigs =
                    [ (n, sig)
                    | n <- fnNames
                    , sig <- Map.findWithDefault [] (n, length allArgs) tcFuncSigsByArity
                    ]
                  partialSigs =
                    [ (n, sig)
                    | n <- fnNames
                    , sig <- Map.findWithDefault [] n tcFuncSigs
                    , length sig > length allArgs
                    ]
              if null exactSigs && null partialSigs
                then do
                  case lookupByCandidatesMap tcCtors varCandidates of
                    Just (tys, _) -> do
                      argTys <- mapM inferType allArgs
                      if length tys /= length allArgs
                        then lift (throwE (NoMatchingCtor nameForErr argTys tys (annSpan annApp)))
                        else
                          if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTys tys)
                            then do
                              case lookupByCandidatesMap tcCtors varCandidates of
                                Just (ctorArgTys, resTy)
                                  | length ctorArgTys == length allArgs && Nothing `notElem` argTys ->
                                      case unifyTypes (Map.toList tcTyCons) ctorArgTys (catMaybes argTys) of
                                        Just subst ->
                                          recordResolvedType (annSpan annApp) (applySubst subst resTy)
                                        Nothing -> return ()
                                _ -> return ()
                              return (App annApp fnResolved allArgs)
                            else lift (throwE (NoMatchingCtor nameForErr argTys tys (annSpan annApp)))
                    _ -> do
                      case lookupByCandidates tcVarTys varCandidates of
                        Just TyVar {} -> lift (throwE (NoType (annSpan annApp)))
                        Just TySkolem {} -> lift (throwE (NoType (annSpan annApp)))
                        _ -> return (App annApp fnResolved allArgs)
                else do
                  argTys <- mapM inferType allArgs
                  MkTCState{tcVarTys, tcCtors, tcCtx} <- get
                  let argCases = map (annCase . annExp) allArgs
                      boundVarNames = map fst tcVarTys
                      hasBoundCandidate =
                        any (\(ident, _) -> ident `elem` boundVarNames)
                      shouldAllowFlexibleCase arg = case arg of
                        Var {varCandidates, varName} ->
                          isJust (lookupByCandidates tcVarTys varCandidates)
                            || hasBoundCandidate varCandidates
                            || varName `elem` boundVarNames
                        App {fn} -> case fn of
                          Var {varCandidates} ->
                            case lookupByCandidatesMap tcCtors varCandidates of
                              Just _ -> False
                              Nothing -> True
                          _ -> True
                        _ -> False
                      hasExpectedCaseCandidate expCase arg =
                        let hasCase = any ((== expCase) . snd)
                        in case arg of
                          Var {varCandidates} -> hasCase varCandidates
                          App {fn} -> case fn of
                            Var {varCandidates} -> hasCase varCandidates
                            _ -> False
                          _ -> False
                      isBareAccInCtx name =
                        case stripBareCaseSuffix name of
                          Just (base, Acc) -> Set.member base tcCtx && not (Set.member name tcCtx)
                          Nothing -> False
                          _ -> False
                      matchExactSig (name, argsSig) =
                        let expCases = map (annCase . annTy . snd) argsSig
                            argsForSig = fromMaybe allArgs (reorderByCasesNomFallback expCases argCases allArgs)
                            argTysForSig = fromMaybe argTys (reorderByCasesNomFallback expCases argCases argTys)
                            argCasesReordered = map (annCase . annExp) argsForSig
                            tys = map snd argsSig
                            hasCaseMismatch = or (zipWith4 checkCaseMismatch expCases tys argCasesReordered argsForSig)
                            checkCaseMismatch expCase expTy argCase arg =
                              let flexible = shouldAllowFlexibleCase arg
                                  higherOrder = case expTy of
                                    Arr {} -> True
                                    _ -> False
                                  strictGenToIns = expCase == Ins && argCase == Gen
                                  ambiguousP3sAcc =
                                    expCase == Acc &&
                                    argCase == P3s &&
                                    hasExpectedCaseCandidate expCase arg
                                  ambiguousP3sBareAccHead =
                                    expCase == Acc &&
                                    argCase == P3s &&
                                    case arg of
                                      App {fn = Var {varName}} ->
                                        not (T.isSuffixOf (T.pack "ki") (snd varName)) &&
                                        isBareAccInCtx varName
                                      _ -> False
                                  ambiguousBareAcc =
                                    expCase == Acc &&
                                    argCase == Acc &&
                                    case arg of
                                      App {fn = Var {varName}} ->
                                        not (T.isSuffixOf (T.pack "ki") (snd varName)) &&
                                        isBareAccInCtx varName
                                      _ -> False
                              in ambiguousP3sAcc || ambiguousP3sBareAccHead || ambiguousBareAcc || (expCase /= argCase && (not flexible || strictGenToIns) && not higherOrder)
                        in if hasCaseMismatch
                             then Nothing
                             else if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTysForSig tys)
                               then Just (argsForSig, tys)
                               else Nothing
                      matchPartialSig (name, argsSig) = do
                        let tys = map snd argsSig
                            expCases = map (annCase . annTy . snd) argsSig
                            idxs = matchPartialCaseIndices expCases argCases
                        idxs <- idxs
                        let pickedExpectedTys = map (tys !!) idxs
                        guard (and (zipWith (typeMatchesAllowUnknown tcTyCons) argTys pickedExpectedTys))
                        retTy <- Map.lookup (name, tys) tcFuncSigRets
                        let remainingIdxs = [i | i <- [0 .. length tys - 1], i `notElem` idxs]
                            remainingTys = map (tys !!) remainingIdxs
                        return (foldr (Arr (mkAnn Nom NoSpan)) retTy remainingTys)
                      exactMatches =
                        [ (name, tysForSig, argsForSig)
                        | (name, argsSig) <- exactSigs
                        , Just (argsForSig, tysForSig) <- [matchExactSig (name, argsSig)]
                        ]
                      partialMatches = mapMaybe matchPartialSig partialSigs
                  case exactMatches of
                    (name, tysForSig, firstMatch):_ -> do
                      case fnResolved of
                        Var {annExp = annVar, varCandidates = (ident, _):_} -> do
                          recordResolvedName (annSpan annVar) ident
                          recordResolvedSig (annSpan annVar) name tysForSig
                        _ -> return ()
                      forM_ (Map.lookup (name, tysForSig) tcFuncSigRets) (recordResolvedType (annSpan annApp))
                      return (App annApp fnResolved firstMatch)
                    [] ->
                      case partialMatches of
                        partialTy:_ -> do
                          recordResolvedType (annSpan annApp) partialTy
                          return (App annApp fnResolved allArgs)
                        [] ->
                          if Nothing `elem` argTys
                            then return (App annApp fnResolved allArgs)
                            else lift (throwE (NoMatchingOverload nameForErr argTys allSigs (annSpan annApp)))
        _ -> return (App annApp fnResolved allArgs)
    StrLit {annExp, lit} -> do
      recordResolvedType (annSpan annExp) (TyString (mkAnn Nom (annSpan annExp)))
      return (StrLit annExp lit)
    IntLit {annExp, intVal} -> do
      recordResolvedType (annSpan annExp) (TyInt (mkAnn Nom (annSpan annExp)))
      return (IntLit annExp intVal)
    FloatLit {annExp, floatVal} -> do
      recordResolvedType (annSpan annExp) (TyFloat (mkAnn Nom (annSpan annExp)))
      return (FloatLit annExp floatVal)
    CharLit {annExp, charVal} -> do
      recordResolvedType (annSpan annExp) (TyChar (mkAnn Nom (annSpan annExp)))
      return (CharLit annExp charVal)
    Bind {annExp = annBind, bindName, bindNameAnn, bindExp} -> do
      -- Enforce the dative case requirement for "dersek" bindings.
      -- The parser marks such binds by setting the binder annotation case to Dat.
      -- This keeps normal "için" bindings unrestricted while ensuring
      -- "(...-e) n dersek" uses dative on the bound expression.
      when (annCase bindNameAnn == Dat && annCase (annExp bindExp) /= Dat) $
        lift (throwE (NoType (annSpan (annExp bindExp))))
      exp' <- tcExp1With allowEffect bindExp
      mTy <- inferType exp'
      forM_ mTy (recordResolvedType (annSpan bindNameAnn))
      return (Bind annBind bindName bindNameAnn exp')
    Seq {annExp = annSeq, first, second} -> do
      case first of
        Bind {bindName, bindNameAnn, bindExp} -> do
          -- Same "dersek" dative check for binds in sequences.
          when (annCase bindNameAnn == Dat && annCase (annExp bindExp) /= Dat) $
            lift (throwE (NoType (annSpan (annExp bindExp))))
          bindExp' <- tcExp1With True bindExp
          mTy <- inferType bindExp'
          let tys = maybe [] (\t -> [(bindName, t)]) mTy
          forM_ mTy (recordResolvedType (annSpan bindNameAnn))
          second' <- withCtx [bindName] (withVarTypes tys (tcExp1With allowEffect second))
          return (Seq annSeq (Bind (annExp first) bindName bindNameAnn bindExp') second')
        _ -> do
          first' <- tcExp1With True first
          second' <- tcExp1With allowEffect second
          return (Seq annSeq first' second')
    Match {annExp, scrutinee, clauses} -> do
      scrutinee' <- expectOne (tcExp scrutinee)
      mScrutTy <- inferType scrutinee'
      let scrutArg =
            case mScrutTy of
              Just ty -> [((([], T.pack "_scrutinee"), mkAnn Nom NoSpan), ty)]
              Nothing -> []
      clauses' <- mapM (tcClause scrutArg allowEffect) clauses
      case mScrutTy of
        Just scrutTy -> checkExhaustivePatterns scrutTy clauses annExp
        Nothing -> return ()
      return (Match annExp scrutinee' clauses')
    Let {annExp, varName, body} ->
      withCtx [varName] (tcExp1With allowEffect body)
    Ascribe {annExp, ascType, ascExp} -> do
      exp' <- tcExp1With allowEffect ascExp
      mExpTy <- inferType exp'
      MkTCState{tcTyCons, tcFuncSigs} <- get
      when (containsTyVars tcTyCons ascType) $
        lift (throwE (NoType (annSpan annExp)))
      let tcList = Map.toList tcTyCons
          nAsc = normalizeTy tcList ascType
          expIsAppToOverload =
            case exp' of
              App {fn = Var {varCandidates}, args} ->
                let fnNames = map fst varCandidates
                    exactSigs =
                      [ sig
                      | name <- fnNames
                      , sig <- Map.findWithDefault [] name tcFuncSigs
                      , length sig == length args
                      ]
                in length exactSigs > 1
              _ -> False
          matchesKnownType expTy = tyEq tcList nAsc (normalizeTy tcList expTy)
      case mExpTy of
        Just expTy
          | matchesKnownType expTy -> return (Ascribe annExp ascType exp')
          | expIsAppToOverload && typeMatchesAllowUnknown tcTyCons (Just expTy) nAsc ->
              return (Ascribe annExp ascType exp')
          | otherwise ->
              lift (throwE (PatternTypeMismatch ([], T.pack "ascribe") ascType expTy (annSpan annExp)))
        Nothing ->
          lift (throwE (NoType (annSpan annExp)))

-- | Reject pure uses of effectful read primitives and infinitive functions.
rejectReadEffect :: Ann -- ^ Expression annotation.
                 -> Identifier -- ^ Identifier being checked.
                 -> TCM () -- ^ No result.
rejectReadEffect ann ident = do
  MkTCState{tcInfinitives} <- get
  when (ident == ([], T.pack "oku") || Set.member ident tcInfinitives) $
    lift (throwE (NoType (annSpan ann)))

-- | Apply a grammatical case to a value expression.
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
    _ -> exp

-- | Match provided argument cases to expected signature cases for partial application.
-- Returns expected argument indices matched by each provided argument, in order.
matchPartialCaseIndices :: [Case] -> [Case] -> Maybe [Int]
matchPartialCaseIndices expectedCases = go []
  where
    go _ [] = Just []
    go used (pc:pcs) =
      let matches = [i | (i, ec) <- zip [0..] expectedCases, ec == pc, i `notElem` used]
      in case matches of
           [i] -> (i :) <$> go (i : used) pcs
           _ -> Nothing

-- | Resolve a variable by candidates, arity, and scope.
resolveVar :: Ann -- ^ Annotation of the variable occurrence.
           -> Identifier -- ^ Original identifier.
           -> Maybe Int -- ^ Optional arity constraint.
           -> [(Identifier, Case)] -- ^ Candidate identifiers and cases.
           -> TCM (Exp Ann) -- ^ Resolved variable expression.
resolveVar annExp originalName mArity candidates = do
  MkTCState{tcCtx, tcFuncs} <- get
  let filtered = filter (\(ident, _) -> Set.member ident tcCtx) candidates
  if null filtered
    then
      case fallbackCopulaIdent tcCtx originalName of
        Just ident ->
          return (Var (setAnnCase annExp (annCase annExp)) originalName [(ident, annCase annExp)])
        Nothing -> lift (throwE (UnknownName originalName (annSpan annExp)))
    else do
      let arityFiltered =
            case mArity of
              Nothing -> filtered
              Just arity ->
                let names = nub [name | (name, ars) <- Map.toList tcFuncs, ar <- ars, arity == ar]
                    narrowed = filter (\(ident, _) -> ident `elem` names) filtered
                in if null names || null narrowed
                     then filtered
                     else narrowed
          caseFiltered = filter (\(_, cas) -> cas == annCase annExp) arityFiltered
          scoped =
            if null caseFiltered
              then arityFiltered
              else caseFiltered
      case scoped of
        [] -> do
          -- NOTE: We may have candidates in scope that fail the case/arity
          -- filtering, but the surface form could still be a copula/infinitive
          -- variant of a known name (e.g. "yazmaktır" -> "yazmak").
          -- Try the copula/infinitive fallback *again* here so we don't
          -- incorrectly reject a valid reference and lose overload info.
          case fallbackCopulaIdent tcCtx originalName of
            Just ident ->
              -- Preserve the original case on the surface form while
              -- resolving to the base identifier.
              return (Var (setAnnCase annExp (annCase annExp)) originalName [(ident, annCase annExp)])
            Nothing -> lift (throwE (NoType (annSpan annExp)))
        [(ident, cas)] -> return (Var (setAnnCase annExp cas) originalName [(ident, cas)])
        _ -> lift (throwE (Ambiguity (annSpan annExp)))

-- | Try to match copula-suffixed identifiers to context names.
-- This is a heuristic fallback because the type checker does not have TRmorph access.
fallbackCopulaIdent :: Set.Set Identifier -- ^ Context identifiers.
                    -> Identifier -- ^ Identifier to normalize.
                    -> Maybe Identifier -- ^ Matching context identifier.
fallbackCopulaIdent ctx (mods, word) = do
  stripped <- stripCopulaSuffix word
  let baseRoots = catMaybes [Just stripped, dropTrailingVowel stripped >>= dropTrailingSoftG]
      infinitiveRoots = catMaybes
        [ stripInfinitiveSuffix stripped
        , stripInfinitiveSuffix stripped >>= dropTrailingVowel >>= dropTrailingSoftG
        ]
      roots = nub (baseRoots ++ infinitiveRoots)
  find (`Set.member` ctx) [(mods, root) | root <- roots]
  where
    -- | Strip common copula suffixes from a surface word.
    stripCopulaSuffix :: T.Text -- ^ Surface word.
                      -> Maybe T.Text -- ^ Stripped word.
    stripCopulaSuffix txt =
      let lowerTxt = T.toLower txt
          suffixes = map T.pack ["dir","dır","dur","dür","tir","tır","tur","tür"]
          match = find (`T.isSuffixOf` lowerTxt) suffixes
      in case match of
           Nothing -> Nothing
           Just suff ->
             let len = T.length suff
             in if T.length txt > len
                 then Just (T.take (T.length txt - len) txt)
                 else Nothing
    -- | Strip infinitive suffixes from a surface word.
    stripInfinitiveSuffix :: T.Text -- ^ Surface word.
                      -> Maybe T.Text -- ^ Stripped infinitive root.
    stripInfinitiveSuffix txt
      | T.pack "mak" `T.isSuffixOf` txt = Just (T.dropEnd 3 txt)
      | T.pack "mek" `T.isSuffixOf` txt = Just (T.dropEnd 3 txt)
      | otherwise = Nothing
    -- | Drop a trailing Turkish vowel for heuristic matching.
    dropTrailingVowel :: T.Text -- ^ Surface word.
                      -> Maybe T.Text -- ^ Word without trailing vowel.
    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing
    -- | Replace trailing soft g with k for heuristic matching.
    dropTrailingSoftG :: T.Text -- ^ Surface word.
                      -> Maybe T.Text -- ^ Word with trailing soft g normalized.
    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing

-- | Expect exactly one result from a multi-variant computation.
expectOne :: TCM [Exp Ann] -- ^ Computation returning expressions.
          -> TCM (Exp Ann) -- ^ Single expression.
expectOne m = do
  xs <- m
  case xs of
    [] -> lift (throwE (NoType NoSpan))
    _:_:_ -> lift (throwE (Ambiguity NoSpan))
    [x] -> return x

-- | Run a computation with an extended name context.
withCtx :: [Identifier] -- ^ Identifiers to add to context.
        -> TCM a -- ^ Computation to run.
        -> TCM a -- ^ Result of the computation.
withCtx idents m = do
  st <- get
  put (invalidateInferMemo (st { tcCtx = Set.union (Set.fromList idents) (tcCtx st) }))
  res <- m
  modify (\s -> invalidateInferMemo (s { tcCtx = tcCtx st }))
  return res

-- | Normalize primitive types to their canonical forms.
normalizePrimTy :: Ty Ann -- ^ Type to normalize.
                -> Ty Ann -- ^ Normalized type.
normalizePrimTy ty =
  case ty of
    TyInd ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | isCharIdent name -> TyChar ann
      | otherwise -> TyInd ann name
    TyVar ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | isCharIdent name -> TyChar ann
      | otherwise -> TyVar ann name  -- Keep TyVar for polymorphic types
    TyApp ann ctor args ->
      TyApp ann (normalizePrimTy ctor) (map normalizePrimTy args)
    Arr ann d i ->
      Arr ann (normalizePrimTy d) (normalizePrimTy i)
    TySkolem ann name ->
      TySkolem ann name
    _ -> ty

-- | Whether a function return type case is currently accepted.
--
-- The language rule intends return types to be nominative ('Nom') or
-- possessive ('P3s').  We also temporarily accept accusative ('Acc')
-- because morphology can currently resolve some possessive-looking forms
-- ambiguously as accusative in return-type positions.  Keeping 'Acc' here
-- avoids widespread false positives while that ambiguity is being resolved.
isAllowedReturnCase :: Case -> Bool
isAllowedReturnCase cas = cas == Nom || cas == P3s || cas == Acc

-- | Type-check a statement and update the checker state.
tcStmt :: Stmt Ann -- ^ Statement to type-check.
       -> TCM (Stmt Ann) -- ^ Type-checked statement.
tcStmt stmt =
  case stmt of
    Defn name ty e -> do
      e' <- expectOne (tcExp e)
      -- Check that the inferred type matches the declared type
      -- Type variables in the declared type are treated as rigid (universally quantified)
      -- Only apply this check if the declared type contains type variables (polymorphism)
      -- AND the type annotation is explicit (not the default TyString)
      mInferredTy <- inferType e'
      MkTCState{tcTyCons} <- get
      let explicit = annSpan (annTy ty) /= NoSpan
          hasTyVars = containsTyVars tcTyCons ty
      when (explicit && hasTyVars) $ do
        case mInferredTy of
          Just inferredTy -> do
            let matches = tyMatchesRigid tcTyCons inferredTy ty
            unless matches $ do
              -- Type error: inferred type doesn't match declared type with rigid type variables
              lift (throwE (NoType NoSpan))
          Nothing -> return ()
      modify (\s -> invalidateInferMemo (s { tcCtx = Set.insert name (tcCtx s)
                                           , tcVals = Map.insert name e' (tcVals s)
                                           }))
      return (Defn name ty e')
    Function name args ty body isInfinitive -> do
      let argNames = map argIdent args
          skolemArgs = map (\((ident, ann), ty) -> ((ident, ann), skolemizeTy ty)) args
          skolemBindings = map (\((ident, _), ty) -> (ident, skolemizeTy ty)) args
      mRet <- withCtx (name : argNames) (withVarTypes skolemBindings (inferReturnType body))
      body' <- withCtx (name : argNames) (withFuncRet name (map (skolemizeTy . argType) args) mRet (withFuncSig name skolemArgs (mapM (tcClause skolemArgs isInfinitive) body)))
      case skolemBindings of
        (_, argTy):_ -> checkExhaustivePatterns argTy body (annTy ty)
        _ -> return ()
      -- Check that the return type case is one of the allowed forms.
      -- We permit Acc as well because existing stdlib signatures use it.
      let retSpan = annSpan (annTy ty)
          retCase = annCase (annTy ty)
      when (retSpan /= NoSpan && not (isAllowedReturnCase retCase)) $
        lift (throwE (InvalidReturnCase retCase retSpan))
      -- Check that the inferred return type matches the declared type with rigid type variables
      -- Only apply this check if the declared type contains type variables (polymorphism)
      -- AND the type annotation is explicit (not the default TyString)
      MkTCState{tcTyCons} <- get
      let explicit = retSpan /= NoSpan
          hasTyVars = containsTyVars tcTyCons ty
      when (explicit && hasTyVars) $ do
        case mRet of
          Just inferredRet -> do
            let matches = tyMatchesRigid tcTyCons inferredRet ty
            unless matches $
              lift (throwE (NoType NoSpan))
          Nothing -> return ()
      modify (\s ->
        let explicit = annSpan (annTy ty) /= NoSpan
            defaultInfRet = TyInd (mkAnn Nom NoSpan) ([], T.pack "bitim")
            inferredRet =
              case mRet of
                Just (TyVar _ n) | isInfinitive && n == name -> defaultInfRet
                _ -> fromMaybe ty mRet
            retTy = if explicit then ty else inferredRet
            s' = insertFuncSig name args s
        in invalidateInferMemo
             (s'
               { tcCtx = Set.insert name (tcCtx s')
               , tcFuncs = mmInsert name (length args) (tcFuncs s')
               , tcFuncSigRets =
                   Map.insert (name, map (normalizePrimTy . snd) args) (normalizePrimTy retTy) (tcFuncSigRets s')
               , tcInfinitives = if isInfinitive then Set.insert name (tcInfinitives s') else tcInfinitives s'
               }))
      return (Function name args ty body' isInfinitive)
    PrimFunc name args ty isInfinitive -> do
      -- Check that the return type case is one of the allowed forms.
      -- We permit Acc as well because existing stdlib signatures use it.
      let retSpan = annSpan (annTy ty)
          retCase = annCase (annTy ty)
      when (retSpan /= NoSpan && not (isAllowedReturnCase retCase)) $
        lift (throwE (InvalidReturnCase retCase retSpan))
      -- Validate that the primitive function is actually implemented
      unless (Prim.isImplementedPrimitive name args) $
        lift (throwE (UnimplementedPrimitive name args NoSpan))
      modify (\s ->
        let s' = insertFuncSig name args s
        in invalidateInferMemo
             (s'
               { tcCtx = Set.insert name (tcCtx s')
               , tcFuncs = mmInsert name (length args) (tcFuncs s')
               , tcFuncSigRets = Map.insert (name, map (normalizePrimTy . snd) args) (normalizePrimTy ty) (tcFuncSigRets s')
               , tcInfinitives = if isInfinitive then Set.insert name (tcInfinitives s') else tcInfinitives s'
               }))
      return (PrimFunc name args ty isInfinitive)
    Load dirPath name ->
      return (Load dirPath name)
    NewType name params ctors -> do
      MkTCState{tcTyCons = existingTyCons} <- get
      let ctorNames = map (fst . fst) ctors
          paramNames = Set.fromList [n | TyVar _ n <- params]
          -- Include the type being defined for recursive type support
          tyConsWithSelf = Map.insert name (length params) existingTyCons
          resultTy =
            case params of
              [] -> TyInd (mkAnn Nom NoSpan) name
              _ -> TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) params
          ctorSigs =
            [ (ctorName, (ctorArgs, resultTy))
            | ((ctorName, _), ctorArgs) <- ctors
            ]
      -- Validate that all type variables in constructor arguments are either
      -- defined types or declared type parameters (including self-reference)
      let checkTyVar n sp =
            when (not (Map.member n tyConsWithSelf) && not (Set.member n paramNames)) $
              lift (throwE (UnknownName n sp))
          validateTy ty = case ty of
            TyVar ann n -> checkTyVar n (annSpan ann)
            TyInd ann n -> checkTyVar n (annSpan ann)
            TyApp _ ctor args -> do
              validateTy ctor
              mapM_ validateTy args
            Arr _ d i -> do
              validateTy d
              validateTy i
            _ -> return ()
      mapM_ (\((_, _), ctorArgs) -> mapM_ validateTy ctorArgs) ctors
      modify (\s -> invalidateInferMemo (s { tcCtx = Set.insert name (Set.union (Set.fromList ctorNames) (tcCtx s))
                                           , tcCtors = Map.union (Map.fromList ctorSigs) (tcCtors s)
                                           , tcTyCons = Map.insert name (length params) (tcTyCons s)
                                           }))
      return (NewType name params ctors)
    PrimType name -> do
      modify (\s -> invalidateInferMemo (s { tcCtx = Set.insert name (tcCtx s)
                                           , tcTyCons = Map.insert name 0 (tcTyCons s)
                                           }))
      return (PrimType name)
    ExpStmt e -> do
      e' <- tcExp1With True e
      return (ExpStmt e')

-- | Replace universally quantified type variables with skolems for rigid checking.
skolemizeTy :: Ty Ann -- ^ Type to skolemize.
            -> Ty Ann -- ^ Skolemized type.
skolemizeTy ty =
  case ty of
    TyVar ann name -> TySkolem ann name
    Arr ann d i -> Arr ann (skolemizeTy d) (skolemizeTy i)
    TyApp ann ctor args ->
      TyApp ann (skolemizeTy ctor) (map skolemizeTy args)
    TyInd {} -> ty
    TyInt {} -> ty
    TyFloat {} -> ty
    TyString {} -> ty
    TyChar {} -> ty
    TySkolem {} -> ty

-- | Reorder values to match expected grammatical cases.
reorderByCases :: forall a.
                 [Case] -- ^ Expected cases.
               -> [Case] -- ^ Actual cases.
               -> [a] -- ^ Values to reorder.
               -> Maybe [a] -- ^ Reordered values when possible.
reorderByCases expected actual xs
  | length expected /= length actual = Nothing
  | Set.size expectedSet /= length expected = Nothing
  | Set.size actualSet /= length actual = Nothing
  | expectedSet /= actualSet = Nothing
  | otherwise = mapM pick expected
  where
    expectedSet = Set.fromList expected
    actualSet = Set.fromList actual
    mapping = Map.fromList (zip actual xs)
    -- | Pick the value corresponding to a case.
    pick :: Case -- ^ Desired case.
         -> Maybe a -- ^ Selected value.
    pick cas = Map.lookup cas mapping

-- | Type-check a clause in the context of argument types.
tcClause :: [Arg Ann] -- ^ Argument signature.
         -> Bool -- ^ Whether this is an infinitive function (allows effects).
         -> Clause Ann -- ^ Clause to check.
         -> TCM (Clause Ann) -- ^ Type-checked clause.
tcClause args isInfinitive (Clause pat body) = do
  let argNames = map argIdent args
      patNames = patIdentifiers pat
  patTys <- inferPatTypes pat args
  patSpans <- inferPatTypesWithSpans pat args
  mapM_ (uncurry recordResolvedType) patSpans
  forM_ args (\((_, ann), ty) -> recordResolvedType (annSpan ann) ty)
  let argTys = map (\((ident, _), ty) -> (ident, ty)) args
  body' <- withCtx (patNames ++ argNames) (withVarTypes (patTys ++ argTys) (tcExp1With isInfinitive body))
  return (Clause pat body')

-- | Collect identifiers bound by a pattern.
patIdentifiers :: Pat Ann -- ^ Pattern to inspect.
               -> [Identifier] -- ^ Identifiers in the pattern.
patIdentifiers pat =
  case pat of
    PWildcard _ -> []
    PVar n _ -> [n]
    PCtor _ pats -> concatMap patIdentifiers pats
    PIntLit _ _ -> []
    PFloatLit _ _ -> []
    PStrLit _ _ -> []
    PCharLit _ _ -> []
    PListLit pats -> concatMap patIdentifiers pats

-- | Lookup a binding by candidate identifiers.
lookupByCandidates :: forall a.
                     [(Identifier, a)] -- ^ Candidate bindings.
                   -> [(Identifier, Case)] -- ^ Candidate identifiers.
                   -> Maybe a -- ^ Matching binding when found.
lookupByCandidates env candidates =
  let names = map fst candidates
  in go names
  where
    -- | Try candidates in order.
    go :: [Identifier] -- ^ Remaining candidate names.
       -> Maybe a -- ^ Matching binding.
    go [] = Nothing
    go (n:ns) =
      case lookup n env of
        Just v -> Just v
        Nothing -> go ns

-- | Lookup a binding by candidate identifiers (Map version).
lookupByCandidatesMap :: forall a.
                        Map.Map Identifier a -- ^ Candidate bindings.
                      -> [(Identifier, Case)] -- ^ Candidate identifiers.
                      -> Maybe a -- ^ Matching binding when found.
lookupByCandidatesMap env candidates =
  let names = map fst candidates
  in go names
  where
    -- | Try candidates in order.
    go :: [Identifier] -- ^ Remaining candidate names.
       -> Maybe a -- ^ Matching binding.
    go [] = Nothing
    go (n:ns) =
      case Map.lookup n env of
        Just v -> Just v
        Nothing -> go ns

-- | Lookup a function return type by candidates and argument types.
lookupFuncRet :: [(Identifier, Int)] -- ^ Type constructor arities for type comparison.
              -> [((Identifier, [Ty Ann]), Ty Ann)] -- ^ Return types by identifier and arg types.
              -> [(Identifier, Case)] -- ^ Candidate identifiers.
              -> [Ty Ann] -- ^ Argument types to match.
              -> Maybe (Ty Ann) -- ^ Matching return type.
lookupFuncRet tyCons env candidates argTys =
  let names = map fst candidates
  in go names
  where
    go :: [Identifier] -- ^ Remaining candidate names.
       -> Maybe (Ty Ann) -- ^ Matching return type.
    go [] = Nothing
    go (n:ns) =
      case find (\((name, sigArgTys), _) -> name == n && matchArgTypes sigArgTys) env of
        Just (_, retTy) -> Just retTy
        Nothing -> go ns
    matchArgTypes sigArgTys =
      length sigArgTys == length argTys &&
      and (zipWith (tyEq tyCons) argTys sigArgTys)

-- | Lookup a function return type by candidates and argument types (Map version).
lookupFuncRetMap :: Map.Map Identifier Int -- ^ Type constructor arities for type comparison.
                 -> Map.Map (Identifier, [Ty Ann]) (Ty Ann) -- ^ Return types by identifier and arg types.
                 -> [(Identifier, Case)] -- ^ Candidate identifiers.
                 -> [Ty Ann] -- ^ Argument types to match.
                 -> Maybe (Ty Ann) -- ^ Matching return type.
lookupFuncRetMap tyCons env candidates argTys =
  let names = map fst candidates
      tyConsList = Map.toList tyCons
  in go names tyConsList
  where
    go :: [Identifier] -- ^ Remaining candidate names.
       -> [(Identifier, Int)] -- ^ Type constructor arities as list.
       -> Maybe (Ty Ann) -- ^ Matching return type.
    go [] _ = Nothing
    go (n:ns) tcList =
      case find (\((name, sigArgTys), _) -> name == n && matchArgTypes tcList sigArgTys) (Map.toList env) of
        Just (_, retTy) -> Just retTy
        Nothing -> go ns tcList
    matchArgTypes tcList sigArgTys =
      length sigArgTys == length argTys &&
      and (zipWith (tyEq tcList) argTys sigArgTys)

-- | Infer a type for an expression when possible.
inferType :: Exp Ann -- ^ Expression to infer.
          -> TCM (Maybe (Ty Ann)) -- ^ Inferred type.
inferType e = inferTypeUncached
  where
    inferTypeUncached :: TCM (Maybe (Ty Ann))
    inferTypeUncached =
      case e of
        IntLit {} -> return (Just (TyInt (mkAnn Nom NoSpan)))
        FloatLit {} -> return (Just (TyFloat (mkAnn Nom NoSpan)))
        StrLit {} -> return (Just (TyString (mkAnn Nom NoSpan)))
        CharLit {} -> return (Just (TyChar (mkAnn Nom NoSpan)))
        Bind {bindExp} -> inferType bindExp
        Seq {second} -> inferType second
        Var {varCandidates} -> do
          MkTCState{tcVals, tcCtors, tcCtx, tcVarTys, tcFuncSigs, tcFuncSigRets} <- get
          case lookupByCandidates tcVarTys varCandidates of
            Just ty -> return (Just ty)
            Nothing ->
              case lookupByCandidatesMap tcVals varCandidates of
                Just v -> inferType v
                Nothing ->
                  case lookupByCandidatesMap tcCtors varCandidates of
                    Just (argTys, ty) ->
                      return (Just (foldr (Arr (mkAnn Nom NoSpan)) ty argTys))
                    _ ->
                      case inferFunctionValueType varCandidates tcFuncSigs tcFuncSigRets of
                        Just ty -> return (Just ty)
                        Nothing ->
                          case find (\(ident, _) -> Set.member ident tcCtx) varCandidates of
                            Just (ident, cas) -> return (Just (TyVar (mkAnn cas NoSpan) ident))
                            Nothing -> return Nothing
        App {fn, args} ->
          case fn of
            Var {annExp = annFn, varCandidates} -> do
              MkTCState{tcCtors, tcTyCons, tcFuncSigRets, tcCtx, tcFuncSigs, tcFuncSigsByArity, tcVarTys} <- get
              case lookupByCandidates tcVarTys varCandidates of
                Just fnTy@(Arr {}) ->
                  case applyFunTy (length args) fnTy of
                    Just retTy -> return (Just retTy)
                    Nothing -> return Nothing
                _ -> do
                  case lookupByCandidatesMap tcCtors varCandidates of
                    Just (tys, resTy)
                      | length tys == length args -> do
                          argTys <- mapM inferType args
                          if Nothing `elem` argTys
                            then return Nothing
                            else do
                              let actuals = catMaybes argTys
                              case unifyTypes (Map.toList tcTyCons) tys actuals of
                                Just subst -> return (Just (applySubst subst resTy))
                                Nothing -> return Nothing
                    _ -> do
                      -- Find matching overload by argument types and return its return type
                      argTys <- mapM inferType args
                      let fnNames = map fst varCandidates
                          argCount = length args
                          -- Exact overload retrieval is indexed by (name, arity).
                          exactSigs = [(n, sig) | n <- fnNames, sig <- Map.findWithDefault [] (n, argCount) tcFuncSigsByArity]
                          partialSigs = [(n, sig) | n <- fnNames, sig <- Map.findWithDefault [] n tcFuncSigs, length sig > argCount]
                          matchExactSig (name, argsSig) =
                            let tys = map snd argsSig
                            in if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTys tys)
                                 then Map.lookup (name, tys) tcFuncSigRets
                                 else Nothing
                          matchPartialSig (name, argsSig) =
                            let tys = map snd argsSig
                                expCases = map (annCase . annTy . snd) argsSig
                                actCases = map (annCase . annExp) args
                            in case matchPartialCaseIndices expCases actCases of
                                 Just idxs ->
                                   let appliedTys = map (tys !!) idxs
                                       remainingIdxs = [i | i <- [0 .. length tys - 1], i `notElem` idxs]
                                       remainingTys = map (tys !!) remainingIdxs
                                       mkPartial retTy = foldr (Arr (mkAnn Nom NoSpan)) retTy remainingTys
                                   in if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTys appliedTys)
                                        then fmap mkPartial (Map.lookup (name, tys) tcFuncSigRets)
                                        else Nothing
                                 Nothing -> Nothing
                          matches = mapMaybe matchExactSig exactSigs ++ mapMaybe matchPartialSig partialSigs
                      case matches of
                        retTy:_ -> return (Just retTy)
                        [] ->
                          -- Fallback: try to find any matching return type
                          let actuals = catMaybes argTys
                          in case lookupFuncRetMap tcTyCons tcFuncSigRets varCandidates actuals of
                            Just retTy -> return (Just retTy)
                            Nothing ->
                              let inCtx = any (\(ident, _) -> Set.member ident tcCtx) varCandidates
                                  inSigs = any (\(ident, _) -> not (null (Map.findWithDefault [] ident tcFuncSigs))) varCandidates
                              in case find (\(ident, _) -> Set.member ident tcCtx) varCandidates of
                                   Just (ident, _) -> return (Just (TyVar (mkAnn (annCase annFn) NoSpan) ident))
                                   Nothing ->
                                     if inCtx || inSigs
                                       then case varCandidates of
                                         (ident, _):_ -> return (Just (TyVar (mkAnn (annCase annFn) NoSpan) ident))
                                         [] -> return Nothing
                                       else return Nothing
            _ -> return Nothing
        Match {clauses} ->
          case clauses of
            [] -> return Nothing
            Clause _ body:_ -> inferType body
        Ascribe {ascType} -> return (Just ascType)
        _ -> return Nothing
    -- | Apply an n-argument application to a function type.
    -- Returns the remaining result type when the application is valid.
    applyFunTy :: Int -> Ty Ann -> Maybe (Ty Ann)
    applyFunTy n ty
      | n <= 0 = Just ty
      | otherwise =
          case ty of
            Arr _ _ imgTy -> applyFunTy (n - 1) imgTy
            _ -> Nothing

    inferFunctionValueType :: [(Identifier, Case)]
                           -> Map.Map Identifier [[Arg Ann]]
                           -> Map.Map (Identifier, [Ty Ann]) (Ty Ann)
                           -> Maybe (Ty Ann)
    inferFunctionValueType candidates sigs retMap =
      let candidateNames = map fst candidates
          sigEntries =
            [ (name, map snd argsSig)
            | name <- candidateNames
            , argsSig <- Map.findWithDefault [] name sigs
            , not (null argsSig)
            ]
          buildFunctionType argTys retTy =
            foldr (Arr (mkAnn Nom NoSpan)) retTy argTys
          fromEntry (name, argTys) =
            fmap (buildFunctionType argTys) (Map.lookup (name, argTys) retMap)
      in listToMaybe (mapMaybe fromEntry sigEntries)

-- | Infer a return type from a list of clauses.
inferReturnType :: [Clause Ann] -- ^ Clauses to inspect.
                -> TCM (Maybe (Ty Ann)) -- ^ Inferred return type.
inferReturnType clauses = do
  tys <- mapM (\(Clause _ body) -> inferType body) clauses
  return (firstJust tys)
  where
    -- | Pick the first successful inference result.
    firstJust :: [Maybe (Ty Ann)] -- ^ Candidate types.
              -> Maybe (Ty Ann) -- ^ First inferred type.
    firstJust [] = Nothing
    firstJust (Just t:_) = Just t
    firstJust (Nothing:rest) = firstJust rest

-- | Run a computation with a function return type in scope.
withFuncRet :: Identifier -- ^ Function name.
            -> [Ty Ann] -- ^ Function argument types.
            -> Maybe (Ty Ann) -- ^ Return type when known.
            -> TCM a -- ^ Computation to run.
            -> TCM a -- ^ Result of the computation.
withFuncRet _ _ Nothing m = m
withFuncRet name argTys (Just ty) m = do
  st <- get
  put (invalidateInferMemo (st { tcFuncSigRets = Map.insert (name, argTys) ty (tcFuncSigRets st) }))
  res <- m
  modify (\s -> invalidateInferMemo (s { tcFuncSigRets = tcFuncSigRets st }))
  return res

-- | Run a computation with a function signature in scope.
withFuncSig :: Identifier -- ^ Function name.
            -> [Arg Ann] -- ^ Argument types.
            -> TCM a -- ^ Computation to run.
            -> TCM a -- ^ Result of the computation.
withFuncSig name args m = do
  st <- get
  put (invalidateInferMemo ((insertFuncSig name args st) { tcFuncs = mmInsert name (length args) (tcFuncs st) }))
  res <- m
  modify (\s -> invalidateInferMemo (s { tcFuncs = tcFuncs st, tcFuncSigs = tcFuncSigs st, tcFuncSigsByArity = tcFuncSigsByArity st }))
  return res

-- | Run a computation with variable types in scope.
withVarTypes :: [(Identifier, Ty Ann)] -- ^ Variable bindings.
             -> TCM a -- ^ Computation to run.
             -> TCM a -- ^ Result of the computation.
withVarTypes [] m = m
withVarTypes tys m = do
  st <- get
  put (invalidateInferMemo (st { tcVarTys = tys ++ tcVarTys st }))
  res <- m
  modify (\s -> invalidateInferMemo (s { tcVarTys = tcVarTys st }))
  return res

-- | Infer types for identifiers bound in a pattern.
inferPatTypes :: Pat Ann -- ^ Pattern to inspect.
              -> [Arg Ann] -- ^ Constructor argument types.
              -> TCM [(Identifier, Ty Ann)] -- ^ Inferred bindings.
inferPatTypes pat args =
  case (pat, args) of
    (PWildcard _, _) -> return []
    (PVar n ann, (_, ty):_) -> return [(n, ty)]
    (PIntLit _ _, _) -> return []
    (PFloatLit _ _, _) -> return []
    (PStrLit _ _, _) -> return []
    (PCharLit _ _, _) -> return []
    (PListLit pats, (_, scrutTy):_) -> do
      MkTCState{tcTyCons} <- get
      -- For list patterns, infer the element type from the scrutinee type
      -- List type is represented as nested applications of eki constructor
      -- We need to extract the element type and apply it to each pattern
      let elemTy = extractListElemTypeMap tcTyCons scrutTy
      concat <$> mapM (\p -> inferPatTypes p [(undefined, elemTy)]) pats
    (PCtor (ctor, _) pats, (_, scrutTy):_) -> do
      MkTCState{tcCtors, tcTyCons} <- get
      case Map.lookup ctor tcCtors of
        Just (argTys, resTy) ->
          let resTyNorm = stripTyCaseForMatch resTy
              scrutTyNorm = stripTyCaseForMatch scrutTy
          in case unifyTypes (Map.toList tcTyCons) [resTyNorm] [scrutTyNorm] of
            Just subst -> do
              let argTys' = map (applySubst subst) argTys
                  -- Nested patterns match from the right, so we align argument
                  -- types with the pattern list by dropping leading args when
                  -- the constructor has more parameters than the pattern specifies.
                  argTysAligned =
                    if length pats < length argTys'
                      then drop (length argTys' - length pats) argTys'
                      else argTys'
              -- Recursively infer types for nested patterns
              bindings <- sequence
                [ inferPatTypes p [((([], T.pack "_"), mkAnn Nom NoSpan), ty)]
                | (p, ty) <- zip pats argTysAligned
                ]
              return (concat bindings)
            Nothing -> do
              -- Pattern type doesn't match scrutinee type
              let sp = case pats of
                         (PVar _ ann):_ -> annSpan ann
                         (PWildcard ann):_ -> annSpan ann
                         _ -> annSpan (annTy scrutTy)
              lift (throwE (PatternTypeMismatch ctor resTy scrutTy sp))
        Nothing -> return []  -- Constructor not found - might be undefined, let other checks handle it
    _ -> return []

-- | Infer types for identifiers bound in a pattern, returning their spans.
inferPatTypesWithSpans :: Pat Ann -- ^ Pattern to inspect.
                       -> [Arg Ann] -- ^ Constructor argument types.
                       -> TCM [(Span, Ty Ann)] -- ^ Inferred bindings with spans.
inferPatTypesWithSpans pat args =
  case (pat, args) of
    (PWildcard _, _) -> return []
    (PVar _ ann, (_, ty):_) -> return [(annSpan ann, ty)]
    (PIntLit _ _, _) -> return []
    (PFloatLit _ _, _) -> return []
    (PStrLit _ _, _) -> return []
    (PCharLit _ _, _) -> return []
    (PListLit pats, (_, scrutTy):_) -> do
      MkTCState{tcTyCons} <- get
      let elemTy = extractListElemTypeMap tcTyCons scrutTy
      concat <$> mapM (\p -> inferPatTypesWithSpans p [(undefined, elemTy)]) pats
    (PCtor (ctor, _) pats, (_, scrutTy):_) -> do
      MkTCState{tcCtors, tcTyCons} <- get
      case Map.lookup ctor tcCtors of
        Just (argTys, resTy) ->
          let resTyNorm = stripTyCaseForMatch resTy
              scrutTyNorm = stripTyCaseForMatch scrutTy
          in case unifyTypes (Map.toList tcTyCons) [resTyNorm] [scrutTyNorm] of
            Just subst -> do
              let argTys' = map (applySubst subst) argTys
                  argTysAligned =
                    if length pats < length argTys'
                      then drop (length argTys' - length pats) argTys'
                      else argTys'
              bindings <- sequence
                [ inferPatTypesWithSpans p [((([], T.pack "_"), mkAnn Nom NoSpan), ty)]
                | (p, ty) <- zip pats argTysAligned
                ]
              return (concat bindings)
            Nothing -> return []
        Nothing -> return []
    _ -> return []

-- | Normalize type-case annotations for constructor-pattern unification.
--
-- Pattern matching should compare ADT identity/shape; surface case
-- inflections on type names are not semantically relevant here.
stripTyCaseForMatch :: Ty Ann -> Ty Ann
stripTyCaseForMatch ty =
  case ty of
    TyString ann -> TyString (setAnnCase ann Nom)
    TyInt ann -> TyInt (setAnnCase ann Nom)
    TyFloat ann -> TyFloat (setAnnCase ann Nom)
    TyChar ann -> TyChar (setAnnCase ann Nom)
    TyInd ann name -> TyInd (setAnnCase ann Nom) name
    TyVar ann name -> TyVar (setAnnCase ann Nom) name
    TySkolem ann name -> TySkolem (setAnnCase ann Nom) name
    TyApp ann ctor args ->
      TyApp (setAnnCase ann Nom) (stripTyCaseForMatch ctor) (map stripTyCaseForMatch args)
    Arr ann d i ->
      Arr (setAnnCase ann Nom) (stripTyCaseForMatch d) (stripTyCaseForMatch i)

-- | Check whether a set of patterns exhausts a scrutinee type.

-- | Extract the element type from a list type.
extractListElemType :: [(Identifier, Int)] -- ^ Type constructor arities.
                    -> Ty Ann -- ^ List type.
                    -> Ty Ann -- ^ Element type.
extractListElemType tcTyCons ty =
  case ty of
    TyApp _ _ (elemTy:_) -> elemTy
    _ -> TyVar (mkAnn Nom NoSpan) ([], T.pack "a")

-- | Extract the element type from a list type (Map version).
extractListElemTypeMap :: Map.Map Identifier Int -- ^ Type constructor arities.
                       -> Ty Ann -- ^ List type.
                       -> Ty Ann -- ^ Element type.
extractListElemTypeMap tcTyCons ty =
  case ty of
    TyApp _ _ (elemTy:_) -> elemTy
    _ -> TyVar (mkAnn Nom NoSpan) ([], T.pack "a")
checkExhaustivePatterns :: Ty Ann -- ^ Scrutinee type.
                        -> [Clause Ann] -- ^ Clauses to inspect.
                        -> Ann -- ^ Annotation for span reporting.
                        -> TCM ()
checkExhaustivePatterns scrutTy clauses ann = do
  MkTCState{tcTyCons} <- get
  let pats = map (\(Clause pat _) -> pat) clauses
      hasTopWildcard = any isWildcardPat pats
  if hasTopWildcard
    then return ()
    else do
      mCtors <- ctorsForType scrutTy
      case mCtors of
        Nothing -> return ()
        Just _ -> do
          missing <- missingPatternsForType scrutTy pats
          case missing of
            [] -> return ()
            _ -> lift (throwE (NonExhaustivePattern missing (annSpan ann)))
  where
    isWildcardPat pat =
      case pat of
        PWildcard _ -> True
        PVar {} -> True
        _ -> False

-- | Constructor info for exhaustiveness checking.
data CtorInfo = CtorInfo
  { ctorName :: Identifier
  , ctorArgs :: [Ty Ann]
  }

-- | Resolve constructors for a concrete scrutinee type.
ctorsForType :: Ty Ann -- ^ Scrutinee type.
             -> TCM (Maybe [CtorInfo]) -- ^ Constructors when the type is known.
ctorsForType ty =
  case ty of
    TyVar {} -> return Nothing
    TySkolem {} -> return Nothing
    _ -> do
      MkTCState{tcCtors, tcTyCons} <- get
      let pickCtor (ctor, (argTys, resTy)) =
            case unifyTypes (Map.toList tcTyCons) [resTy] [ty] of
              Just subst -> Just (CtorInfo ctor (map (applySubst subst) argTys))
              Nothing -> Nothing
          ctors = mapMaybe pickCtor (Map.toList tcCtors)
      return $
        if null ctors
          then Nothing
          else Just ctors

-- | Check if a pattern matrix exhausts all cases for the given types.
isExhaustive :: [Ty Ann] -- ^ Column types.
             -> [[Pat Ann]] -- ^ Pattern matrix.
             -> TCM Bool -- ^ True when exhaustive.
isExhaustive tys matrix = do
  useful <- isUseful tys matrix (replicate (length tys) (PWildcard (mkAnn Nom NoSpan)))
  return (not useful)

-- | Compute missing patterns for a single scrutinee type.
missingPatternsForType :: Ty Ann -- ^ Scrutinee type.
                       -> [Pat Ann] -- ^ Existing patterns.
                       -> TCM [Pat Ann]
missingPatternsForType scrutTy pats = do
  vectors <- missingVectors [scrutTy] (map (: []) pats)
  annotated <- mapM (\case
                            [] -> error "missingPatternsForType: unexpected empty vector"
                            (p:_) -> annotateMissingPattern scrutTy p) vectors
  return (nub annotated)

-- | Compute missing pattern vectors for a pattern matrix.
missingVectors :: [Ty Ann] -- ^ Column types.
               -> [[Pat Ann]] -- ^ Pattern matrix.
               -> TCM [[Pat Ann]]
missingVectors [] matrix =
  if null matrix then return [[]] else return []
missingVectors (t:ts) matrix = do
  if null matrix
    then do
      mCtors <- ctorsForType t
      case mCtors of
        Nothing ->
          return [PWildcard (mkAnn Nom NoSpan) : replicate (length ts) (PWildcard (mkAnn Nom NoSpan))]
        Just ctors ->
          return
            [ PCtor (ctorName ctorInfo, mkAnn Nom NoSpan) (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)))
              : replicate (length ts) (PWildcard (mkAnn Nom NoSpan))
            | ctorInfo <- ctors
            ]
    else do
      mCtors <- ctorsForType t
      case mCtors of
        Nothing -> do
          rest <- missingVectors ts (mapMaybe safeTail matrix)
          return (map (PWildcard (mkAnn Nom NoSpan) :) rest)
        Just ctors -> do
          -- Some rows may be empty due to earlier drops; filter them out
          -- before inspecting heads to avoid partial pattern matches.
          let nonEmptyRows = filter (not . null) matrix
              wildRows = filter (\case
                                          [] -> False
                                          (p:_) -> isWildcardHead p) nonEmptyRows
          if not (null wildRows)
            then do
              rest <- missingVectors ts (defaultMatrix matrix)
              return (map (PWildcard (mkAnn Nom NoSpan) :) rest)
            else do
              let ctorMiss ctorInfo = do
                    let matrix' = specializeMatrix ctorInfo matrix
                    missingArgs <- missingVectors (ctorArgs ctorInfo ++ ts) matrix'
                    return
                      [ PCtor (ctorName ctorInfo, mkAnn Nom NoSpan) argPats : restPats
                      | vec <- missingArgs
                      , let (argPats, restPats) = splitAt (length (ctorArgs ctorInfo)) vec
                      ]
              misses <- mapM ctorMiss ctors
              return (concat misses)
  where
    safeTail row = case row of
      [] -> Nothing
      (_:xs) -> Just xs

    isWildcardHead pat =
      case pat of
        PWildcard {} -> True
        PVar {} -> True
        _ -> False

    defaultMatrix = mapMaybe (\case
                                        [] -> Nothing
                                        (p:ps) -> if isWildcardHead p then Just ps else Nothing)

    specializeMatrix ctorInfo =
      mapMaybe (specializeRow ctorInfo)
    specializeRow ctorInfo row =
      case row of
        [] -> Nothing
        (p:ps) ->
          case p of
            PCtor (name, _) subPats | identMatchesCtor (ctorName ctorInfo) name ->
              Just (subPats ++ ps)
            PWildcard {} ->
              Just (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)) ++ ps)
            PVar {} ->
              Just (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)) ++ ps)
            _ -> Nothing

    identMatchesCtor left right =
      identMatches left right || identMatchesPoss left right

    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2)
      && not (null (roots x1 `intersect` roots x2))

    roots txt =
      nub (catMaybes [Just txt, dropTrailingVowel txt >>= dropTrailingSoftG])

    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing

    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing

-- | Replace wildcards in a missing pattern with fresh variables and cases.
annotateMissingPattern :: Ty Ann -- ^ Expected type for the pattern.
                       -> Pat Ann -- ^ Pattern with wildcards.
                       -> TCM (Pat Ann)
annotateMissingPattern scrutTy pat = do
  (pat', _) <- go 0 scrutTy pat
  return pat'
  where
    go :: Int -> Ty Ann -> Pat Ann -> TCM (Pat Ann, Int)
    go idx ty p =
      case p of
        PWildcard _ -> do
          let (name, idx') = freshIdent idx
              ann = mkAnn (annCase (annTy ty)) NoSpan
          return (PVar name ann, idx')
        PVar n ann -> return (PVar n ann, idx)
        PCtor (ctor, ann) subPats -> do
          mCtor <- ctorInfoFor ty ctor
          case mCtor of
            Nothing ->
              if null subPats
                then do
                  let (name, idx') = freshIdent idx
                      ann = mkAnn (annCase (annTy ty)) NoSpan
                  return (PVar name ann, idx')
                else return (PCtor (ctor, ann) subPats, idx)
            Just ctorInfo ->
              if null (ctorArgs ctorInfo)
                then do
                  let ann = mkAnn (annCase (annTy ty)) NoSpan
                  return (PVar ctor ann, idx)
                else do
                  let argTys = ctorArgs ctorInfo
                      subPats' = take (length argTys) (subPats ++ repeat (PWildcard (mkAnn Nom NoSpan)))
                  (subPatsAnn, idx') <- goList idx (zip subPats' argTys)
                  return (PCtor (ctor, ann) subPatsAnn, idx')

    goList :: Int -> [(Pat Ann, Ty Ann)] -> TCM ([Pat Ann], Int)
    goList idx [] = return ([], idx)
    goList idx ((p, ty):rest) = do
      (p', idx') <- go idx ty p
      (rest', idx'') <- goList idx' rest
      return (p' : rest', idx'')

    ctorInfoFor :: Ty Ann -> Identifier -> TCM (Maybe CtorInfo)
    ctorInfoFor ty ctorIdent = do
      mCtors <- ctorsForType ty
      case mCtors of
        Nothing -> return Nothing
        Just ctors -> return (find (\ctorInfo -> identMatchesCtor (ctorName ctorInfo) ctorIdent) ctors)

    freshIdent :: Int -> (Identifier, Int)
    freshIdent idx =
      let letters = ['a'..'z']
          base = letters !! (idx `mod` length letters)
          suffix = idx `div` length letters
          name =
            if suffix == 0
              then T.singleton base
              else T.singleton base <> T.pack (show suffix)
      in (([], name), idx + 1)

    identMatchesCtor left right =
      identMatches left right || identMatchesPoss left right

    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2)
      && not (null (roots x1 `intersect` roots x2))

    roots txt =
      nub (catMaybes [Just txt, dropTrailingVowel txt >>= dropTrailingSoftG])

    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing

    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing

-- | Determine whether a pattern vector is useful (matches an uncovered case).
isUseful :: [Ty Ann] -- ^ Column types.
         -> [[Pat Ann]] -- ^ Pattern matrix.
         -> [Pat Ann] -- ^ Pattern vector.
         -> TCM Bool
isUseful _ [] _ = return True
isUseful [] _ _ = return False
isUseful tys matrix vec =
  case (tys, vec) of
    (t:ts, p:ps) -> do
      mCtors <- ctorsForType t
      case mCtors of
        Nothing ->
          isUseful ts (mapMaybe safeTail matrix) ps
        Just ctors ->
          case p of
            PWildcard {} -> usefulWildcard ctors ts matrix ps
            PVar {} -> usefulWildcard ctors ts matrix ps
            PCtor (ctorName, _) subPats ->
              case findCtor ctors ctorName of
                Nothing -> return True
                Just ctorInfo ->
                  let matrix' = specializeMatrix ctorInfo matrix
                  in isUseful (ctorArgs ctorInfo ++ ts) matrix' (subPats ++ ps)
    _ -> return False
  where
    safeTail row = case row of
      [] -> Nothing
      (_:xs) -> Just xs

    usefulWildcard ctors ts matrix ps = do
      let present = constructorsInColumn matrix
          complete = constructorsComplete ctors present
      if complete
        then anyM (\ctorInfo ->
          isUseful (ctorArgs ctorInfo ++ ts)
                   (specializeMatrix ctorInfo matrix)
                   (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)) ++ ps)
          ) ctors
        else isUseful ts (defaultMatrix matrix) ps

    constructorsInColumn = mapMaybe firstCtor
    firstCtor row =
      case row of
        (PCtor (name, _) _ : _) -> Just name
        _ -> Nothing

    constructorsComplete ctors present =
      all (\ctorInfo -> any (identMatchesCtor (ctorName ctorInfo)) present) ctors

    findCtor ctors name =
      find (\ctorInfo -> identMatchesCtor (ctorName ctorInfo) name) ctors

    defaultMatrix = mapMaybe (\case
                                        [] -> Nothing
                                        (p:ps) -> if isWildcardHead p then Just ps else Nothing)
    isWildcardHead pat =
      case pat of
        PWildcard {} -> True
        PVar {} -> True
        _ -> False

    specializeMatrix ctorInfo =
      mapMaybe (specializeRow ctorInfo)
    specializeRow ctorInfo row =
      case row of
        [] -> Nothing
        (p:ps) ->
          case p of
            PCtor (name, _) subPats | identMatchesCtor (ctorName ctorInfo) name ->
              Just (subPats ++ ps)
            PWildcard {} ->
              Just (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)) ++ ps)
            PVar {} ->
              Just (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)) ++ ps)
            _ -> Nothing

    anyM _ [] = return False
    anyM f (x:xs) = do
      ok <- f x
      if ok then return True else anyM f xs

    identMatchesCtor left right =
      identMatches left right || identMatchesPoss left right

    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2)
      && not (null (roots x1 `intersect` roots x2))

    roots txt =
      nub (catMaybes [Just txt, dropTrailingVowel txt >>= dropTrailingSoftG])

    dropTrailingVowel txt =
      case T.unsnoc txt of
        Just (pref, c)
          | c `elem` ['i', 'ı', 'u', 'ü'] -> Just pref
        _ -> Nothing

    dropTrailingSoftG txt =
      case T.unsnoc txt of
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing

-- | Compare a maybe-inferred type with an expected type.
typeMatches :: [(Identifier, Int)] -- ^ Type constructor arities.
            -> Maybe (Ty Ann) -- ^ Possibly unknown type.
            -> Ty Ann -- ^ Expected type.
            -> Bool -- ^ True when the types match.
typeMatches tyCons mTy ty =
  case mTy of
    Nothing -> False
    Just t -> tyEq tyCons t ty

-- | Compare types while allowing unknown inferred types.
typeMatchesAllowUnknown :: Map.Map Identifier Int -- ^ Type constructor arities.
                        -> Maybe (Ty Ann) -- ^ Possibly unknown type.
                        -> Ty Ann -- ^ Expected type.
                        -> Bool -- ^ True when the types match.
typeMatchesAllowUnknown tyCons mTy ty =
  let tyCons' = Map.toList tyCons
  in case mTy of
    Nothing -> True
    Just t ->
      tyEq tyCons' t ty
      || isJust (unifyTypes tyCons' [ty] [t])
      || isJust (unifyTypes tyCons' [t] [ty])

-- | Check if a type contains any type variables or undefined type identifiers.
-- In Kip, undefined type identifiers are treated as implicitly quantified type variables.
containsTyVars :: Map.Map Identifier Int -- ^ Type constructor arities (defined types).
               -> Ty Ann -- ^ Type to check.
               -> Bool -- ^ True if the type contains type variables or undefined types.
containsTyVars tyCons ty =
  case ty of
    TyVar {} -> True
    TySkolem {} -> True
    TyInd _ name -> not (Map.member name tyCons)  -- Undefined type = type variable
    Arr _ d i -> containsTyVars tyCons d || containsTyVars tyCons i
    TyApp _ c args -> containsTyVars tyCons c || any (containsTyVars tyCons) args
    _ -> False

-- | Check if an inferred type matches a declared type with rigid type variables.
-- Type variables in the declared (right) type are treated as universally quantified
-- and can only match themselves, not concrete types.
-- Undefined type identifiers (TyInd not in tyCons) are treated as rigid type variables.
tyMatchesRigid :: Map.Map Identifier Int -- ^ Type constructor arities.
               -> Ty Ann -- ^ Inferred type.
               -> Ty Ann -- ^ Declared type (with rigid type variables).
               -> Bool -- ^ True when the inferred type matches the declared type.
tyMatchesRigid tyCons inferred declared =
  let n1 = canonicalizeTypeVars tyCons (normalizeTyMap tyCons inferred)
      n2 = canonicalizeTypeVars tyCons (normalizeTyMap tyCons declared)
      isDefinedType name = Map.member name tyCons
  in case (n1, n2) of
    (TyString _, TyString _) -> True
    (TyInt _, TyInt _) -> True
    (TyFloat _, TyFloat _) -> True
    (TyChar _, TyChar _) -> True
    (Arr _ d1 i1, Arr _ d2 i2) -> tyMatchesRigid tyCons d1 d2 && tyMatchesRigid tyCons i1 i2
    (TyInd _ n1', TyInd _ n2')
      | isDefinedType n2' -> identMatches n1' n2'  -- Both are defined types, check if they match
      | otherwise -> n1' == n2'  -- n2' is undefined (type variable), must match exactly
    (_, TyInd _ n2') | not (isDefinedType n2') -> False  -- Concrete type cannot match rigid type variable
    (TySkolem _ n1', TySkolem _ n2') -> n1' == n2'
    (TySkolem _ n1', TyVar _ n2') -> n1' == n2'
    (TyVar _ n1', TySkolem _ n2') -> n1' == n2'
    (TyVar _ n1', TyVar _ n2') -> n1' == n2'  -- Type variables must match exactly
    (_, TyVar _ _) -> False  -- Concrete types cannot match rigid type variables
    (TyVar _ _, _) -> True  -- Flexible type variables in inferred type can match anything
    (_, TySkolem {}) -> False
    (TySkolem {}, _) -> False
    (TyApp _ c1 as1, TyApp _ c2 as2) ->
      tyMatchesRigid tyCons c1 c2 && length as1 == length as2 && and (zipWith (tyMatchesRigid tyCons) as1 as2)
    _ -> False

-- | Rename all type variables in a type to canonical placeholders (@_t0@,
-- @_t1@, …) assigned in left-to-right, depth-first order.
--
-- Two types that are structurally identical but use different variable names
-- (i.e. are alpha-equivalent) will produce the same canonical form, so a
-- simple structural equality check on the results is enough to decide
-- alpha-equivalence.
--
-- Concrete type constructors (those present in @tyCons@) and literal types
-- ('TyInt', 'TyFloat', 'TyString') are left untouched.  Everything else —
-- 'TyVar', 'TySkolem', and 'TyInd' names that are /not/ in @tyCons@ — is
-- treated as a type variable and gets a fresh canonical name.
--
-- This is used by 'tyMatchesRigid' so that a declared polymorphic return type
-- like @a olasılığı@ matches an inferred @x olasılığı@ regardless of the
-- variable names chosen by the programmer vs. the type-checker.
canonicalizeTypeVars :: Map.Map Identifier Int -> Ty Ann -> Ty Ann
canonicalizeTypeVars tyCons ty =
  let (ty', _, _) = go Map.empty 0 ty
  in ty'
  where
    go env n t =
      case t of
        TyVar ann name -> bindVar ann name env n
        TySkolem ann name -> bindVar ann name env n
        TyInd ann name
          | Map.member name tyCons -> (TyInd ann name, env, n)
          | otherwise -> bindVar ann name env n
        Arr ann d i ->
          let (d', env1, n1) = go env n d
              (i', env2, n2) = go env1 n1 i
          in (Arr ann d' i', env2, n2)
        TyApp ann ctor args ->
          let (ctor', env1, n1) = go env n ctor
              (args', env2, n2) = goList env1 n1 args
          in (TyApp ann ctor' args', env2, n2)
        TyInt{} -> (t, env, n)
        TyFloat{} -> (t, env, n)
        TyString{} -> (t, env, n)
        TyChar{} -> (t, env, n)

    goList env n [] = ([], env, n)
    goList env n (x:xs) =
      let (x', env1, n1) = go env n x
          (xs', env2, n2) = goList env1 n1 xs
      in (x' : xs', env2, n2)

    bindVar ann name env n =
      case Map.lookup name env of
        Just canon -> (TyVar ann canon, env, n)
        Nothing ->
          let canon = ([], T.pack ("_t" ++ show n))
          in (TyVar ann canon, Map.insert name canon env, n + 1)

-- | Check two types for compatibility.
tyEq :: [(Identifier, Int)] -- ^ Type constructor arities.
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
    (TyVar {}, Arr {}) -> False  -- Type variables cannot match function types
    (Arr {}, TyVar {}) -> False  -- Type variables cannot match function types
    (TyVar _ _, _) -> True
    (_, TyVar _ _) -> True
    (TyApp _ c1 as1, TyApp _ c2 as2) ->
      tyEq tyCons c1 c2 && length as1 == length as2 && and (zipWith (tyEq tyCons) as1 as2)
    _ -> False

-- | Normalize type applications by constructor arity and primitive types.
normalizeTy :: [(Identifier, Int)] -- ^ Type constructor arities.
            -> Ty Ann -- ^ Type to normalize.
            -> Ty Ann -- ^ Normalized type.
normalizeTy tyCons ty =
  case ty of
    TyInd ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | isCharIdent name -> TyChar ann
      | otherwise -> TyInd ann name
    TySkolem ann name ->
      TySkolem ann name
    TyApp ann (TyInd _ name) args ->
      case lookup name tyCons of
        Just arity | arity > 0 ->
          TyApp ann (TyInd (mkAnn Nom NoSpan) name) (map (normalizeTy tyCons) args)
        _ -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizeTy tyCons ctor) (map (normalizeTy tyCons) args)
    Arr ann d i ->
      Arr ann (normalizeTy tyCons d) (normalizeTy tyCons i)
    _ -> ty

-- | Normalize type applications by constructor arity and primitive types (Map version).
normalizeTyMap :: Map.Map Identifier Int -- ^ Type constructor arities.
               -> Ty Ann -- ^ Type to normalize.
               -> Ty Ann -- ^ Normalized type.
normalizeTyMap tyCons ty =
  case ty of
    TyInd ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | isCharIdent name -> TyChar ann
      | otherwise -> TyInd ann name
    TySkolem ann name ->
      TySkolem ann name
    TyApp ann (TyInd _ name) args ->
      case Map.lookup name tyCons of
        Just arity | arity > 0 ->
          TyApp ann (TyInd (mkAnn Nom NoSpan) name) (map (normalizeTyMap tyCons) args)
        _ -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizeTyMap tyCons ctor) (map (normalizeTyMap tyCons) args)
    Arr ann d i ->
      Arr ann (normalizeTyMap tyCons d) (normalizeTyMap tyCons i)
    _ -> ty

-- | Compare identifiers, allowing missing namespaces.
identMatches :: Identifier -- ^ Left identifier.
             -> Identifier -- ^ Right identifier.
             -> Bool -- ^ True when identifiers match loosely.
identMatches (xs1, x1) (xs2, x2) =
  x1 == x2 && (xs1 == xs2 || null xs1 || null xs2)

-- | Check for the integer type identifier.
isIntIdent :: Identifier -- ^ Identifier to inspect.
           -> Bool -- ^ True when identifier matches integer type.
isIntIdent (mods, name) = mods == [T.pack "tam"] && name == T.pack "sayı"

-- | Check for the floating-point type identifier.
isFloatIdent :: Identifier -- ^ Identifier to inspect.
             -> Bool -- ^ True when identifier matches floating-point type.
isFloatIdent (mods, name) = mods == [T.pack "ondalık"] && name == T.pack "sayı"

-- | Check for the string type identifier.
isStringIdent :: Identifier -- ^ Identifier to inspect.
              -> Bool -- ^ True when identifier matches string type.
isStringIdent (mods, name) = null mods && name == T.pack "dizge"

-- | Check for the character type identifier.
isCharIdent :: Identifier -- ^ Identifier to inspect.
            -> Bool -- ^ True when identifier matches character type.
isCharIdent (mods, name) = null mods && name == T.pack "karakter"

-- | Unify expected and actual types to produce substitutions.
unifyTypes :: [(Identifier, Int)] -- ^ Type constructor arities.
           -> [Ty Ann] -- ^ Expected types.
           -> [Ty Ann] -- ^ Actual types.
           -> Maybe [(Identifier, Ty Ann)] -- ^ Substitution when unification succeeds.
unifyTypes tyCons expected actual =
  foldl' go (Just []) (zip expected actual)
  where
    -- | Fold step for unification.
    go :: Maybe [(Identifier, Ty Ann)] -- ^ Current substitution.
       -> (Ty Ann, Ty Ann) -- ^ Expected and actual types.
       -> Maybe [(Identifier, Ty Ann)] -- ^ Updated substitution.
    go Nothing _ = Nothing
    go (Just subst) (e, a) =
      unifyOne subst (normalizeTy tyCons e) (normalizeTy tyCons a)

    -- | Unify a single expected/actual pair.
    unifyOne :: [(Identifier, Ty Ann)] -- ^ Current substitution.
             -> Ty Ann -- ^ Expected type.
             -> Ty Ann -- ^ Actual type.
             -> Maybe [(Identifier, Ty Ann)] -- ^ Updated substitution.
    unifyOne subst e a =
      case e of
        TyInt _ ->
          case a of
            TyInt _ -> Just subst
            _ -> Nothing
        TyFloat _ ->
          case a of
            TyFloat _ -> Just subst
            _ -> Nothing
        TyVar _ name ->
          case lookup name subst of
            Just bound ->
              if tyEq tyCons bound a
                then Just subst
                else Nothing
            Nothing ->
              case a of
                Arr {} -> Nothing  -- Type variables cannot unify with function types
                _ -> Just ((name, a) : subst)
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
              subst' <- unifyOne subst d1 d2
              unifyOne subst' i1 i2
            _ -> Nothing
        TyApp _ c1 as1 ->
          case a of
            TyApp _ c2 as2
              | length as1 == length as2 -> do
                  subst' <- unifyOne subst c1 c2
                  foldl' go (Just subst') (zip as1 as2)
            _ -> Nothing

-- | Apply a substitution to a type.
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

-- | Run a type checker action with a starting state.
runTCM :: TCM a -- ^ Type checker computation.
       -> TCState -- ^ Initial type checker state.
       -> IO (Either TCError (a, TCState)) -- ^ Result or error.
runTCM m s = runExceptT (runStateT m (invalidateInferMemo s))

-- | Pre-register forward declarations for all functions and types.
-- This allows forward references within a file.
registerForwardDecls :: [Stmt Ann] -- ^ Statements to scan.
                     -> TCM () -- ^ No result.
registerForwardDecls = mapM_ registerStmt
  where
    -- | Register a single statement for forward references.
    registerStmt :: Stmt Ann -- ^ Statement to register.
                 -> TCM () -- ^ No result.
    registerStmt stmt =
      case stmt of
        Function name args _ _ isInfinitive ->
          modify (\s ->
            let s' = insertFuncSig name args s
            in invalidateInferMemo
                 (s'
                   { tcCtx = Set.insert name (tcCtx s')
                   , tcFuncs = mmInsert name (length args) (tcFuncs s')
                   , tcInfinitives = if isInfinitive then Set.insert name (tcInfinitives s') else tcInfinitives s'
                   }))
        PrimFunc name args _ isInfinitive -> do
          unless (Prim.isImplementedPrimitive name args) $
            lift (throwE (UnimplementedPrimitive name args NoSpan))
          modify (\s ->
            let s' = insertFuncSig name args s
            in invalidateInferMemo
                 (s'
                   { tcCtx = Set.insert name (tcCtx s')
                   , tcFuncs = mmInsert name (length args) (tcFuncs s')
                   , tcInfinitives = if isInfinitive then Set.insert name (tcInfinitives s') else tcInfinitives s'
                   }))
        Defn name _ _ ->
          modify (\s -> invalidateInferMemo (s { tcCtx = Set.insert name (tcCtx s) }))
        NewType name params ctors -> do
          MkTCState{tcTyCons = existingTyCons} <- get
          let ctorNames = map (fst . fst) ctors
              paramNames = Set.fromList [n | TyVar _ n <- params]
              -- Include the type being defined for recursive type support
              tyConsWithSelf = Map.insert name (length params) existingTyCons
              resultTy =
                case params of
                  [] -> TyInd (mkAnn Nom NoSpan) name
                  _ -> TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) params
              ctorSigs =
                [ (ctorName, (ctorArgs, resultTy))
                | ((ctorName, _), ctorArgs) <- ctors
                ]
          -- Validate that all type variables in constructor arguments are either
          -- defined types or declared type parameters (including self-reference)
          let checkTyVar n sp =
                when (not (Map.member n tyConsWithSelf) && not (Set.member n paramNames)) $
                  lift (throwE (UnknownName n sp))
              validateTy ty = case ty of
                TyVar ann n -> checkTyVar n (annSpan ann)
                TyInd ann n -> checkTyVar n (annSpan ann)
                TyApp _ ctor args -> do
                  validateTy ctor
                  mapM_ validateTy args
                Arr _ d i -> do
                  validateTy d
                  validateTy i
                _ -> return ()
          mapM_ (\((_, _), ctorArgs) -> mapM_ validateTy ctorArgs) ctors
          modify (\s -> invalidateInferMemo (s { tcCtx = Set.insert name (Set.union (Set.fromList ctorNames) (tcCtx s))
                                               , tcCtors = Map.union (Map.fromList ctorSigs) (tcCtors s)
                                               , tcTyCons = Map.insert name (length params) (tcTyCons s)
                                               }))
        PrimType name ->
          modify (\s -> invalidateInferMemo (s { tcCtx = Set.insert name (tcCtx s)
                                               , tcTyCons = Map.insert name 0 (tcTyCons s)
                                               }))
        _ -> return ()
