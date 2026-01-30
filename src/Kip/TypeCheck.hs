{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad (unless, when)
import Control.Applicative ((<|>))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.List (find, foldl', intersect, nub)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe, isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T

-- | Type checker state for names, signatures, and constructors.
data TCState =
  MkTCState
    { tcCtx :: [Identifier] -- ^ Names in scope.
    , tcFuncs :: [(Identifier, Int)] -- ^ Known function arities.
    , tcFuncSigs :: [(Identifier, [Arg Ann])] -- ^ Function argument signatures.
    , tcFuncSigRets :: [((Identifier, [Ty Ann]), Ty Ann)] -- ^ Function return types by arg types.
    , tcVarTys :: [(Identifier, Ty Ann)] -- ^ Variable type bindings.
    , tcVals :: [(Identifier, Exp Ann)] -- ^ Value bindings for inlining.
    , tcCtors :: [(Identifier, ([Ty Ann], Ty Ann))] -- ^ Constructor signatures.
    , tcTyCons :: [(Identifier, Int)] -- ^ Type constructor arities.
    , tcGerunds :: [Identifier] -- ^ Gerund (effectful) functions.
    }
  deriving (Generic)

-- | Binary instance for type checker state.
instance Binary TCState where
  put MkTCState{..} = do
    B.put tcCtx
    B.put tcFuncs
    B.put tcFuncSigs
    B.put tcFuncSigRets
    B.put tcVarTys
    B.put tcVals
    B.put tcCtors
    B.put tcTyCons
    B.put tcGerunds
  get = MkTCState <$> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get <*> B.get

-- | Empty type checker state.
emptyTCState :: TCState -- ^ Empty type checker state.
emptyTCState = MkTCState [] [] [] [] [] [] [] [] []

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
        Var {varCandidates = [(ident, _)]} -> do
          unless allowEffect (rejectReadEffect annExp ident)
          MkTCState{tcFuncs} <- get
          if (ident, 0) `elem` tcFuncs
            then return (App annExp resolved [])
            else return resolved
        _ -> return resolved
    App {annExp = annApp, fn, args} -> do
      fn' <- case fn of
        Var {annExp, varName, varCandidates} ->
          resolveVar annExp varName (Just (length args)) varCandidates
        _ -> tcExp1With allowEffect fn
      args' <- mapM (tcExp1With False) args
      case fn' of
        Var {annExp = annFn, varName, varCandidates} -> do
          case varCandidates of
            (ident, _) : _ -> unless allowEffect (rejectReadEffect annFn ident)
            _ -> return ()
          MkTCState{tcFuncSigs, tcTyCons, tcCtors} <- get
          let tyNames = map fst tcTyCons
              funcNames = map fst tcFuncSigs
          case args' of
            [arg] | any (\(ident, _) -> ident `elem` tyNames) varCandidates
                  , not (any (\(ident, _) -> ident `elem` funcNames) varCandidates) ->
              return (applyTypeCase (annCase annFn) arg)
            _ -> do
              let nameForErr =
                    case varCandidates of
                      (ident, _):_ -> ident
                      [] -> varName
              let fnNames = map fst varCandidates
                  allSigs = filter (\(n, _) -> n `elem` fnNames) tcFuncSigs
                  sigs = filter (\(n, argsSig) -> n `elem` fnNames && length argsSig == length args') tcFuncSigs
              if null sigs
                then do
                  case lookupByCandidates tcCtors varCandidates of
                    Just (tys, _) -> do
                      argTys <- mapM inferType args'
                      if length tys /= length args'
                        then lift (throwE (NoMatchingCtor nameForErr argTys tys (annSpan annApp)))
                        else
                          if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTys tys)
                            then return (App annApp fn' args')
                            else lift (throwE (NoMatchingCtor nameForErr argTys tys (annSpan annApp)))
                    _ -> do
                      -- Check if we're trying to apply a type variable as a function (parametric polymorphism violation)
                      MkTCState{tcVarTys} <- get
                      case lookupByCandidates tcVarTys varCandidates of
                        Just TyVar {} -> lift (throwE (NoType (annSpan annApp)))
                        Just TySkolem {} -> lift (throwE (NoType (annSpan annApp)))
                        _ -> return (App annApp fn' args')
                else do
                  argTys <- mapM inferType args'
                  MkTCState{tcVarTys, tcCtors, tcCtx} <- get
                  let argCases = map (annCase . annExp) args'
                      -- Check if an argument should allow flexible case
                      shouldAllowFlexibleCase arg = case arg of
                        Var {varCandidates} ->
                          -- Allow flexible case only for pattern-bound vars
                          isJust (lookupByCandidates tcVarTys varCandidates)
                        App {fn} -> case fn of
                          -- If calling a constructor, enforce strict case
                          Var {varCandidates} ->
                            case lookupByCandidates tcCtors varCandidates of
                              Just _ -> False  -- Constructor application - strict case
                              Nothing -> True  -- Function call - flexible case
                          _ -> True  -- Other function calls - flexible case
                        _ -> False  -- Other expressions require strict case matching
                      -- If the argument still has multiple candidates,
                      -- we can use that set to detect ambiguity between
                      -- expected/actual cases before accepting a match.
                      hasExpectedCaseCandidate expCase arg =
                        let hasCase = any ((== expCase) . snd)
                        in case arg of
                          Var {varCandidates} -> hasCase varCandidates
                          App {fn} -> case fn of
                            Var {varCandidates} -> hasCase varCandidates
                            _ -> False
                          _ -> False
                      -- Detect a bare accusative surface form whose base
                      -- is in scope but the surface is not; this is used
                      -- to reject ambiguous parses that should have been
                      -- P3s + Acc (e.g. "varlığı" vs "varlık").
                      isBareAccInCtx name =
                        case stripBareAccSuffix name of
                          Just base -> base `elem` tcCtx && name `notElem` tcCtx
                          Nothing -> False
                      matchSig argsSig =
                        let expCases = map (annCase . annTy . snd) argsSig
                            argsForSig = fromMaybe args' (reorderByCases expCases argCases args')
                            argTysForSig = fromMaybe argTys (reorderByCases expCases argCases argTys)
                            argCasesReordered = map (annCase . annExp) argsForSig
                            tys = map snd argsSig
                            -- Check for case mismatches after reordering (unless flexible case is allowed)
                            hasCaseMismatch = or (zipWith3 checkCaseMismatch expCases argCasesReordered argsForSig)
                            checkCaseMismatch expCase argCase arg =
                              let flexible = shouldAllowFlexibleCase arg
                                  -- Ambiguous: argument was parsed as P3s but could also
                                  -- satisfy an expected Acc case (e.g. P3s+Acc collapse).
                                  ambiguousP3sAcc =
                                    expCase == Acc &&
                                    argCase == P3s &&
                                    hasExpectedCaseCandidate expCase arg
                                  -- Ambiguous: bare accusative form whose base is in scope.
                                  -- We reject these for Acc expectations to avoid silently
                                  -- accepting the wrong case; "ki" is exempt (e.g. "ilki").
                                  ambiguousBareAcc =
                                    expCase == Acc &&
                                    argCase == Acc &&
                                    case arg of
                                      App {fn = Var {varName}} ->
                                        not (T.isSuffixOf (T.pack "ki") (snd varName)) &&
                                        isBareAccInCtx varName
                                      _ -> False
                              in ambiguousP3sAcc || ambiguousBareAcc || (expCase /= argCase && not flexible)
                        in if hasCaseMismatch
                             then Nothing  -- Reject case mismatch
                             else if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTysForSig tys)
                               then Just argsForSig
                               else Nothing
                      matches =
                        [ argsForSig
                        | (_, argsSig) <- sigs
                        , Just argsForSig <- [matchSig argsSig]
                        ]
                  if null matches
                    then
                      if Nothing `elem` argTys
                        then return (App annApp fn' args')
                        else lift (throwE (NoMatchingOverload nameForErr argTys allSigs (annSpan annApp)))
                    else
                      case matches of
                        firstMatch:_ -> return (App annApp fn' firstMatch)
                        [] -> return (App annApp fn' args')
        _ -> return (App annApp fn' args')
    StrLit {annExp, lit} ->
      return (StrLit annExp lit)
    IntLit {annExp, intVal} ->
      return (IntLit annExp intVal)
    FloatLit {annExp, floatVal} ->
      return (FloatLit annExp floatVal)
    Bind {annExp, bindName, bindExp} -> do
      exp' <- tcExp1With allowEffect bindExp
      return (Bind annExp bindName exp')
    Seq {annExp = annSeq, first, second} -> do
      case first of
        Bind {bindName, bindExp} -> do
          bindExp' <- tcExp1With True bindExp
          mTy <- inferType bindExp'
          let tys = maybe [] (\t -> [(bindName, t)]) mTy
          second' <- withCtx [bindName] (withVarTypes tys (tcExp1With allowEffect second))
          return (Seq annSeq (Bind (annExp first) bindName bindExp') second')
        _ -> do
          first' <- tcExp1With True first
          second' <- tcExp1With allowEffect second
          return (Seq annSeq first' second')
    Match {annExp, scrutinee, clauses} -> do
      scrutinee' <- expectOne (tcExp scrutinee)
      mScrutTy <- inferType scrutinee'
      let scrutArg =
            case mScrutTy of
              Just ty -> [(([], T.pack "_scrutinee"), ty)]
              Nothing -> []
      clauses' <- mapM (tcClause scrutArg allowEffect) clauses
      case mScrutTy of
        Just scrutTy -> checkExhaustivePatterns scrutTy clauses annExp
        Nothing -> return ()
      return (Match annExp scrutinee' clauses')
    Let {annExp, varName, body} ->
      withCtx [varName] (tcExp1With allowEffect body)

-- | Reject pure uses of effectful read primitives and gerund functions.
rejectReadEffect :: Ann -- ^ Expression annotation.
                 -> Identifier -- ^ Identifier being checked.
                 -> TCM () -- ^ No result.
rejectReadEffect ann ident = do
  MkTCState{tcGerunds} <- get
  when (ident == ([], T.pack "oku") || ident `elem` tcGerunds) $
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
    _ -> exp

-- | Resolve a variable by candidates, arity, and scope.
resolveVar :: Ann -- ^ Annotation of the variable occurrence.
           -> Identifier -- ^ Original identifier.
           -> Maybe Int -- ^ Optional arity constraint.
           -> [(Identifier, Case)] -- ^ Candidate identifiers and cases.
           -> TCM (Exp Ann) -- ^ Resolved variable expression.
resolveVar annExp originalName mArity candidates = do
  MkTCState{tcCtx, tcFuncs} <- get
  let filtered = filter (\(ident, _) -> ident `elem` tcCtx) candidates
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
                let names = nub [name | (name, n) <- tcFuncs, n == arity]
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
        [] -> lift (throwE (NoType (annSpan annExp)))
        [(ident, cas)] -> return (Var (setAnnCase annExp cas) originalName [(ident, cas)])
        _ -> lift (throwE (Ambiguity (annSpan annExp)))

-- | Try to match copula-suffixed identifiers to context names.
-- This is a heuristic fallback because the type checker does not have TRmorph access.
fallbackCopulaIdent :: [Identifier] -- ^ Context identifiers.
                    -> Identifier -- ^ Identifier to normalize.
                    -> Maybe Identifier -- ^ Matching context identifier.
fallbackCopulaIdent ctx (mods, word) = do
  stripped <- stripCopulaSuffix word
  let baseRoots = catMaybes [Just stripped, dropTrailingVowel stripped >>= dropTrailingSoftG]
      gerundRoots = catMaybes
        [ stripGerundSuffix stripped
        , stripGerundSuffix stripped >>= dropTrailingVowel >>= dropTrailingSoftG
        ]
      roots = nub (baseRoots ++ gerundRoots)
  find (`elem` ctx) [(mods, root) | root <- roots]
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
    -- | Strip gerund suffixes from a surface word.
    stripGerundSuffix :: T.Text -- ^ Surface word.
                      -> Maybe T.Text -- ^ Stripped gerund root.
    stripGerundSuffix txt
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

-- | Strip a bare accusative suffix (no apostrophe) from an identifier.
-- This is intentionally narrow and only used for ambiguity detection
-- where surface forms can collapse (p3s+acc) and mislead overload matching.
stripBareAccSuffix :: Identifier -- ^ Surface identifier.
                   -> Maybe Identifier -- ^ Base identifier.
stripBareAccSuffix (mods, word) =
  let suffixes = map T.pack ["yi", "yı", "yu", "yü", "i", "ı", "u", "ü"]
      tryStrip suf =
        case T.stripSuffix suf word of
          Just base | T.length base > 1 -> Just (mods, base)
          _ -> Nothing
  in foldr (\s acc -> acc <|> tryStrip s) Nothing suffixes

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
  put st { tcCtx = idents ++ tcCtx st }
  res <- m
  modify (\s -> s { tcCtx = tcCtx st })
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
      | otherwise -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizePrimTy ctor) (map normalizePrimTy args)
    Arr ann d i ->
      Arr ann (normalizePrimTy d) (normalizePrimTy i)
    TySkolem ann name ->
      TySkolem ann name
    _ -> ty

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
      modify (\s -> s { tcCtx = name : tcCtx s
                      , tcVals = (name, e') : tcVals s
                      })
      return (Defn name ty e')
    Function name args ty body isGerund -> do
      let argNames = map fst args
          skolemArgs = map (Bifunctor.second skolemizeTy) args
      mRet <- withCtx (name : argNames) (withVarTypes skolemArgs (inferReturnType body))
      body' <- withCtx (name : argNames) (withFuncRet name (map snd skolemArgs) mRet (withFuncSig name skolemArgs (mapM (tcClause skolemArgs isGerund) body)))
      case skolemArgs of
        (_, argTy):_ -> checkExhaustivePatterns argTy body (annTy ty)
        _ -> return ()
      -- Check that the inferred return type matches the declared type with rigid type variables
      -- Only apply this check if the declared type contains type variables (polymorphism)
      -- AND the type annotation is explicit (not the default TyString)
      MkTCState{tcTyCons} <- get
      let explicit = annSpan (annTy ty) /= NoSpan
          hasTyVars = containsTyVars tcTyCons ty
      when (explicit && hasTyVars) $ do
        case mRet of
          Just inferredRet -> do
            let matches = tyMatchesRigid tcTyCons inferredRet ty
            unless matches $
              lift (throwE (NoType NoSpan))
          Nothing -> return ()
      modify (\s -> s { tcCtx = name : tcCtx s
                      , tcFuncs = (name, length args) : tcFuncs s
                      , tcFuncSigs = (name, args) : tcFuncSigs s
                      , tcFuncSigRets =
                          let explicit = annSpan (annTy ty) /= NoSpan
                              retTy = if explicit then ty else fromMaybe ty mRet
                          in ((name, map snd args), normalizePrimTy retTy) : tcFuncSigRets s
                      , tcGerunds = if isGerund then name : tcGerunds s else tcGerunds s
                      })
      return (Function name args ty body' isGerund)
    PrimFunc name args ty isGerund -> do
      modify (\s ->
        s { tcCtx = name : tcCtx s
          , tcFuncs = (name, length args) : tcFuncs s
          , tcFuncSigs = (name, args) : tcFuncSigs s
          , tcFuncSigRets = ((name, map snd args), normalizePrimTy ty) : tcFuncSigRets s
          , tcGerunds = if isGerund then name : tcGerunds s else tcGerunds s
          })
      return (PrimFunc name args ty isGerund)
    Load name ->
      return (Load name)
    NewType name params ctors -> do
      let ctorNames = map (fst . fst) ctors
          resultTy =
            case params of
              [] -> TyInd (mkAnn Nom NoSpan) name
              _ -> TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) params
          ctorSigs =
            [ (ctorName, (ctorArgs, resultTy))
            | ((ctorName, _), ctorArgs) <- ctors
            ]
      modify (\s -> s { tcCtx = name : ctorNames ++ tcCtx s
                      , tcCtors = ctorSigs ++ tcCtors s
                      , tcTyCons = (name, length params) : tcTyCons s
                      })
      return (NewType name params ctors)
    PrimType name -> do
      modify (\s -> s { tcCtx = name : tcCtx s
                      , tcTyCons = (name, 0) : tcTyCons s
                      })
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
         -> Bool -- ^ Whether this is a gerund function (allows effects).
         -> Clause Ann -- ^ Clause to check.
         -> TCM (Clause Ann) -- ^ Type-checked clause.
tcClause args isGerund (Clause pat body) = do
  let argNames = map fst args
      patNames = patIdentifiers pat
  patTys <- inferPatTypes pat args
  let argTys = args
  body' <- withCtx (patNames ++ argNames) (withVarTypes (patTys ++ argTys) (tcExp1With isGerund body))
  return (Clause pat body')

-- | Collect identifiers bound by a pattern.
patIdentifiers :: Pat Ann -- ^ Pattern to inspect.
               -> [Identifier] -- ^ Identifiers in the pattern.
patIdentifiers pat =
  case pat of
    PWildcard _ -> []
    PVar n _ -> [n]
    PCtor _ pats -> concatMap patIdentifiers pats

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

-- | Infer a type for an expression when possible.
inferType :: Exp Ann -- ^ Expression to infer.
          -> TCM (Maybe (Ty Ann)) -- ^ Inferred type.
inferType e =
  case e of
    IntLit {} -> return (Just (TyInt (mkAnn Nom NoSpan)))
    FloatLit {} -> return (Just (TyFloat (mkAnn Nom NoSpan)))
    StrLit {} -> return (Just (TyString (mkAnn Nom NoSpan)))
    Bind {bindExp} -> inferType bindExp
    Seq {second} -> inferType second
    Var {varCandidates} -> do
      MkTCState{tcVals, tcCtors, tcCtx, tcVarTys} <- get
      case lookupByCandidates tcVarTys varCandidates of
        Just ty -> return (Just ty)
        Nothing ->
          case lookupByCandidates tcVals varCandidates of
            Just v -> inferType v
            Nothing ->
              case lookupByCandidates tcCtors varCandidates of
                Just ([], ty) -> return (Just ty)
                _ ->
                  case find (\(ident, _) -> ident `elem` tcCtx) varCandidates of
                    Just (ident, cas) -> return (Just (TyVar (mkAnn cas NoSpan) ident))
                    Nothing -> return Nothing
    App {fn, args} ->
      case fn of
        Var {annExp, varCandidates} -> do
          MkTCState{tcCtors, tcTyCons, tcFuncSigRets, tcCtx, tcFuncSigs} <- get
          case lookupByCandidates tcCtors varCandidates of
            Just (tys, resTy)
              | length tys == length args -> do
                  argTys <- mapM inferType args
                  if Nothing `elem` argTys
                    then return Nothing
                    else do
                      let actuals = catMaybes argTys
                      case unifyTypes tcTyCons tys actuals of
                        Just subst -> return (Just (applySubst subst resTy))
                        Nothing -> return Nothing
            _ -> do
              -- Find matching overload by argument types and return its return type
              argTys <- mapM inferType args
              let fnNames = map fst varCandidates
                  sigs = filter (\(n, argsSig) -> n `elem` fnNames && length argsSig == length args) tcFuncSigs
                  matchSig (name, argsSig) =
                    let tys = map snd argsSig
                    in if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTys tys)
                         then lookup (name, tys) tcFuncSigRets
                         else Nothing
                  matches = mapMaybe matchSig sigs
              case matches of
                retTy:_ -> return (Just retTy)
                [] ->
                  -- Fallback: try to find any matching return type
                  let actuals = catMaybes argTys
                  in case lookupFuncRet tcTyCons tcFuncSigRets varCandidates actuals of
                    Just retTy -> return (Just retTy)
                    Nothing ->
                      let inCtx = any (\(ident, _) -> ident `elem` tcCtx) varCandidates
                          inSigs = any (\(ident, _) -> ident `elem` map fst tcFuncSigs) varCandidates
                      in case find (\(ident, _) -> ident `elem` tcCtx) varCandidates of
                           Just (ident, _) -> return (Just (TyVar (mkAnn (annCase annExp) NoSpan) ident))
                           Nothing ->
                             if inCtx || inSigs
                               then case varCandidates of
                                 (ident, _):_ -> return (Just (TyVar (mkAnn (annCase annExp) NoSpan) ident))
                                 [] -> return Nothing
                               else return Nothing
        _ -> return Nothing
    Match {clauses} ->
      case clauses of
        [] -> return Nothing
        Clause _ body:_ -> inferType body
    _ -> return Nothing

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
  put st { tcFuncSigRets = ((name, argTys), ty) : tcFuncSigRets st }
  res <- m
  modify (\s -> s { tcFuncSigRets = tcFuncSigRets st })
  return res

-- | Run a computation with a function signature in scope.
withFuncSig :: Identifier -- ^ Function name.
            -> [Arg Ann] -- ^ Argument types.
            -> TCM a -- ^ Computation to run.
            -> TCM a -- ^ Result of the computation.
withFuncSig name args m = do
  st <- get
  put st { tcFuncs = (name, length args) : tcFuncs st
         , tcFuncSigs = (name, args) : tcFuncSigs st
         }
  res <- m
  modify (\s -> s { tcFuncs = tcFuncs st, tcFuncSigs = tcFuncSigs st })
  return res

-- | Run a computation with variable types in scope.
withVarTypes :: [(Identifier, Ty Ann)] -- ^ Variable bindings.
             -> TCM a -- ^ Computation to run.
             -> TCM a -- ^ Result of the computation.
withVarTypes [] m = m
withVarTypes tys m = do
  st <- get
  put st { tcVarTys = tys ++ tcVarTys st }
  res <- m
  modify (\s -> s { tcVarTys = tcVarTys st })
  return res

-- | Infer types for identifiers bound in a pattern.
inferPatTypes :: Pat Ann -- ^ Pattern to inspect.
              -> [Arg Ann] -- ^ Constructor argument types.
              -> TCM [(Identifier, Ty Ann)] -- ^ Inferred bindings.
inferPatTypes pat args =
  case (pat, args) of
    (PWildcard _, _) -> return []
    (PVar n ann, (_, ty):_) -> return [(n, ty)]
    (PCtor ctor pats, (_, scrutTy):_) -> do
      MkTCState{tcCtors, tcTyCons} <- get
      case lookup ctor tcCtors of
        Just (argTys, resTy) ->
          case unifyTypes tcTyCons [resTy] [scrutTy] of
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
                [ inferPatTypes p [(([], T.pack "_"), ty)]
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

-- | Check whether a set of patterns exhausts a scrutinee type.
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
            case unifyTypes tcTyCons [resTy] [ty] of
              Just subst -> Just (CtorInfo ctor (map (applySubst subst) argTys))
              Nothing -> Nothing
          ctors = mapMaybe pickCtor tcCtors
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
  annotated <- mapM (annotateMissingPattern scrutTy . head) vectors
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
            [ PCtor (ctorName ctorInfo) (replicate (length (ctorArgs ctorInfo)) (PWildcard (mkAnn Nom NoSpan)))
              : replicate (length ts) (PWildcard (mkAnn Nom NoSpan))
            | ctorInfo <- ctors
            ]
    else do
      mCtors <- ctorsForType t
      case mCtors of
        Nothing -> do
          rest <- missingVectors ts (map tail matrix)
          return (map (PWildcard (mkAnn Nom NoSpan) :) rest)
        Just ctors -> do
          -- Some rows may be empty due to earlier drops; filter them out
          -- before inspecting heads to avoid partial pattern matches.
          let nonEmptyRows = filter (not . null) matrix
              wildRows = filter (isWildcardHead . head) nonEmptyRows
          if not (null wildRows)
            then do
              rest <- missingVectors ts (defaultMatrix matrix)
              return (map (PWildcard (mkAnn Nom NoSpan) :) rest)
            else do
              let ctorMiss ctorInfo = do
                    let matrix' = specializeMatrix ctorInfo matrix
                    missingArgs <- missingVectors (ctorArgs ctorInfo ++ ts) matrix'
                    return
                      [ PCtor (ctorName ctorInfo) argPats : restPats
                      | vec <- missingArgs
                      , let (argPats, restPats) = splitAt (length (ctorArgs ctorInfo)) vec
                      ]
              misses <- mapM ctorMiss ctors
              return (concat misses)
  where
    isWildcardHead pat =
      case pat of
        PWildcard {} -> True
        PVar {} -> True
        _ -> False

    defaultMatrix = map tail . filter (isWildcardHead . head)

    specializeMatrix ctorInfo =
      mapMaybe (specializeRow ctorInfo)
    specializeRow ctorInfo row =
      case row of
        [] -> Nothing
        (p:ps) ->
          case p of
            PCtor name subPats | identMatchesCtor (ctorName ctorInfo) name ->
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
        PCtor ctor subPats -> do
          mCtor <- ctorInfoFor ty ctor
          case mCtor of
            Nothing ->
              if null subPats
                then do
                  let (name, idx') = freshIdent idx
                      ann = mkAnn (annCase (annTy ty)) NoSpan
                  return (PVar name ann, idx')
                else return (PCtor ctor subPats, idx)
            Just ctorInfo ->
              if null (ctorArgs ctorInfo)
                then do
                  let ann = mkAnn (annCase (annTy ty)) NoSpan
                  return (PVar ctor ann, idx)
                else do
                  let argTys = ctorArgs ctorInfo
                      subPats' = take (length argTys) (subPats ++ repeat (PWildcard (mkAnn Nom NoSpan)))
                  (subPatsAnn, idx') <- goList idx (zip subPats' argTys)
                  return (PCtor ctor subPatsAnn, idx')

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
          isUseful ts (map tail matrix) ps
        Just ctors ->
          case p of
            PWildcard {} -> usefulWildcard ctors ts matrix ps
            PVar {} -> usefulWildcard ctors ts matrix ps
            PCtor ctorName subPats ->
              case findCtor ctors ctorName of
                Nothing -> return True
                Just ctorInfo ->
                  let matrix' = specializeMatrix ctorInfo matrix
                  in isUseful (ctorArgs ctorInfo ++ ts) matrix' (subPats ++ ps)
    _ -> return False
  where
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
        (PCtor name _ : _) -> Just name
        _ -> Nothing

    constructorsComplete ctors present =
      all (\ctorInfo -> any (identMatchesCtor (ctorName ctorInfo)) present) ctors

    findCtor ctors name =
      find (\ctorInfo -> identMatchesCtor (ctorName ctorInfo) name) ctors

    defaultMatrix = map tail . filter (isWildcardHead . head)
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
            PCtor name subPats | identMatchesCtor (ctorName ctorInfo) name ->
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
typeMatchesAllowUnknown :: [(Identifier, Int)] -- ^ Type constructor arities.
                        -> Maybe (Ty Ann) -- ^ Possibly unknown type.
                        -> Ty Ann -- ^ Expected type.
                        -> Bool -- ^ True when the types match.
typeMatchesAllowUnknown tyCons mTy ty =
  case mTy of
    Nothing -> True
    Just t ->
      tyEq tyCons t ty
      || isJust (unifyTypes tyCons [ty] [t])
      || isJust (unifyTypes tyCons [t] [ty])

-- | Check if a type contains any type variables or undefined type identifiers.
-- In Kip, undefined type identifiers are treated as implicitly quantified type variables.
containsTyVars :: [(Identifier, Int)] -- ^ Type constructor arities (defined types).
               -> Ty Ann -- ^ Type to check.
               -> Bool -- ^ True if the type contains type variables or undefined types.
containsTyVars tyCons ty =
  case ty of
    TyVar {} -> True
    TySkolem {} -> True
    TyInd _ name -> name `notElem` map fst tyCons  -- Undefined type = type variable
    Arr _ d i -> containsTyVars tyCons d || containsTyVars tyCons i
    TyApp _ c args -> containsTyVars tyCons c || any (containsTyVars tyCons) args
    _ -> False

-- | Check if an inferred type matches a declared type with rigid type variables.
-- Type variables in the declared (right) type are treated as universally quantified
-- and can only match themselves, not concrete types.
-- Undefined type identifiers (TyInd not in tyCons) are treated as rigid type variables.
tyMatchesRigid :: [(Identifier, Int)] -- ^ Type constructor arities.
               -> Ty Ann -- ^ Inferred type.
               -> Ty Ann -- ^ Declared type (with rigid type variables).
               -> Bool -- ^ True when the inferred type matches the declared type.
tyMatchesRigid tyCons inferred declared =
  let n1 = normalizeTy tyCons inferred
      n2 = normalizeTy tyCons declared
      isDefinedType name = name `elem` map fst tyCons
  in case (n1, n2) of
    (TyString _, TyString _) -> True
    (TyInt _, TyInt _) -> True
    (TyFloat _, TyFloat _) -> True
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
    Arr ann d i -> Arr ann (applySubst subst d) (applySubst subst i)
    TyApp ann ctor args ->
      TyApp ann (applySubst subst ctor) (map (applySubst subst) args)

-- | Run a type checker action with a starting state.
runTCM :: TCM a -- ^ Type checker computation.
       -> TCState -- ^ Initial type checker state.
       -> IO (Either TCError (a, TCState)) -- ^ Result or error.
runTCM m s = runExceptT (runStateT m s)

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
        Function name args _ _ isGerund ->
          modify (\s -> s { tcCtx = name : tcCtx s
                          , tcFuncs = (name, length args) : tcFuncs s
                          , tcFuncSigs = (name, args) : tcFuncSigs s
                          , tcGerunds = if isGerund then name : tcGerunds s else tcGerunds s
                          })
        PrimFunc name args _ isGerund ->
          modify (\s -> s { tcCtx = name : tcCtx s
                          , tcFuncs = (name, length args) : tcFuncs s
                          , tcFuncSigs = (name, args) : tcFuncSigs s
                          , tcGerunds = if isGerund then name : tcGerunds s else tcGerunds s
                          })
        Defn name _ _ ->
          modify (\s -> s { tcCtx = name : tcCtx s })
        NewType name params ctors -> do
          let ctorNames = map (fst . fst) ctors
              resultTy =
                case params of
                  [] -> TyInd (mkAnn Nom NoSpan) name
                  _ -> TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) params
              ctorSigs =
                [ (ctorName, (ctorArgs, resultTy))
                | ((ctorName, _), ctorArgs) <- ctors
                ]
          modify (\s -> s { tcCtx = name : ctorNames ++ tcCtx s
                          , tcCtors = ctorSigs ++ tcCtors s
                          , tcTyCons = (name, length params) : tcTyCons s
                          })
        PrimType name ->
          modify (\s -> s { tcCtx = name : tcCtx s
                          , tcTyCons = (name, 0) : tcTyCons s
                          })
        _ -> return ()
