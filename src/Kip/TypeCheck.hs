{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Type checker and type inference for Kip.
module Kip.TypeCheck where

import GHC.Generics (Generic)
import Data.Binary (Binary, Get)
import qualified Data.Binary as B
import Data.Word (Word8)
import Kip.AST

import Control.Monad (unless, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Data.List (nub, elemIndex, sort, find, foldl')
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
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
                    _ -> return (App annApp fn' args')
                else do
                  argTys <- mapM inferType args'
                  let argCases = map (annCase . annExp) args'
                      matchSig argsSig =
                        let expCases = map (annCase . annTy . snd) argsSig
                            argsForSig = fromMaybe args' (reorderByCases expCases argCases args')
                            argTysForSig = fromMaybe argTys (reorderByCases expCases argCases argTys)
                            tys = map snd argsSig
                        in if and (zipWith (typeMatchesAllowUnknown tcTyCons) argTysForSig tys)
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
                    else return (App annApp fn' (head matches))
        _ -> return (App annApp fn' args')
    StrLit {annExp, lit} ->
      return (StrLit annExp lit)
    IntLit {annExp, intVal} ->
      return (IntLit annExp intVal)
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
      -- Only check pattern type for simple variable scrutinees whose type is
      -- known from function arguments or local bindings. This avoids false
      -- positives for complex scrutinees like function applications where
      -- we can't reliably infer the return type.
      MkTCState{tcVarTys} <- get
      let scrutArg = case scrutinee' of
            Var {varCandidates} ->
              case lookupByCandidates tcVarTys varCandidates of
                Just ty -> [(([], T.pack "_scrutinee"), ty)]
                Nothing -> []
            IntLit {} -> [(([], T.pack "_scrutinee"), TyInt (mkAnn Nom NoSpan))]
            StrLit {} -> [(([], T.pack "_scrutinee"), TyString (mkAnn Nom NoSpan))]
            _ -> []  -- Skip check for complex expressions
      clauses' <- mapM (tcClause scrutArg allowEffect) clauses
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
      | isStringIdent name -> TyString ann
      | otherwise -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizePrimTy ctor) (map normalizePrimTy args)
    Arr ann d i ->
      Arr ann (normalizePrimTy d) (normalizePrimTy i)
    _ -> ty
  where
    -- | Check for the integer type identifier.
    isIntIdent :: Identifier -- ^ Identifier to inspect.
               -> Bool -- ^ True when identifier matches integer type.
    isIntIdent (mods, name) = name == T.pack "sayı" && (mods == [T.pack "tam"] || null mods)
    -- | Check for the string type identifier.
    isStringIdent :: Identifier -- ^ Identifier to inspect.
                  -> Bool -- ^ True when identifier matches string type.
    isStringIdent (mods, name) = null mods && name == T.pack "dizge"

-- | Type-check a statement and update the checker state.
tcStmt :: Stmt Ann -- ^ Statement to type-check.
       -> TCM (Stmt Ann) -- ^ Type-checked statement.
tcStmt stmt =
  case stmt of
    Defn name ty e -> do
      e' <- expectOne (tcExp e)
      modify (\s -> s { tcCtx = name : tcCtx s
                      , tcVals = (name, e') : tcVals s
                      })
      return (Defn name ty e')
    Function name args ty body isGerund -> do
      let argNames = map fst args
      mRet <- withCtx (name : argNames) (withVarTypes args (inferReturnType body))
      body' <- withCtx (name : argNames) (withFuncRet name (map snd args) mRet (withFuncSig name args (mapM (tcClause args isGerund) body)))
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
      let ctorNames = map fst ctors
          resultTy =
            case params of
              [] -> TyInd (mkAnn Nom NoSpan) name
              _ ->
                let paramTys = map (TyVar (mkAnn Nom NoSpan)) params
                in TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) paramTys
          ctorSigs =
            [ (ctorName, (ctorArgs, resultTy))
            | (ctorName, ctorArgs) <- ctors
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

-- | Reorder values to match expected grammatical cases.
reorderByCases :: forall a.
                 [Case] -- ^ Expected cases.
               -> [Case] -- ^ Actual cases.
               -> [a] -- ^ Values to reorder.
               -> Maybe [a] -- ^ Reordered values when possible.
reorderByCases expected actual xs
  | length expected /= length actual = Nothing
  | nub expected /= expected = Nothing
  | nub actual /= actual = Nothing
  | sort expected /= sort actual = Nothing
  | otherwise = mapM pick expected
  where
    -- | Pick the value corresponding to a case.
    pick :: Case -- ^ Desired case.
         -> Maybe a -- ^ Selected value.
    pick cas = do
      idx <- elemIndex cas actual
      return (xs !! idx)

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
    PWildcard -> []
    PCtor _ vars -> map fst vars

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
    (PCtor ctor vars, (_, scrutTy):_) -> do
      MkTCState{tcCtors, tcTyCons} <- get
      case lookup ctor tcCtors of
        Just (argTys, resTy) ->
          case unifyTypes tcTyCons [resTy] [scrutTy] of
            Just subst ->
              let argTys' = map (applySubst subst) argTys
                  names = map fst vars
              in return (zip names argTys')
            Nothing -> do
              -- Pattern type doesn't match scrutinee type
              let sp = case vars of
                         (_, ann):_ -> annSpan ann
                         [] -> annSpan (annTy scrutTy)
              lift (throwE (PatternTypeMismatch ctor resTy scrutTy sp))
        Nothing -> return []  -- Constructor not found - might be undefined, let other checks handle it
    _ -> return []

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
    Just t -> tyEq tyCons t ty

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
    (Arr _ d1 i1, Arr _ d2 i2) -> tyEq tyCons d1 d2 && tyEq tyCons i1 i2
    (TyInd _ n1', TyInd _ n2') -> identMatches n1' n2'
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
      | isStringIdent name -> TyString ann
      | otherwise -> TyInd ann name
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
  where
    isIntIdent (mods, name) = null mods && name == T.pack "tam-sayı"
    isStringIdent (mods, name) = null mods && name == T.pack "dizge"

-- | Compare identifiers, allowing missing namespaces.
identMatches :: Identifier -- ^ Left identifier.
             -> Identifier -- ^ Right identifier.
             -> Bool -- ^ True when identifiers match loosely.
identMatches (xs1, x1) (xs2, x2) =
  x1 == x2 && (xs1 == xs2 || null xs1 || null xs2)

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
        TyVar _ name ->
          case lookup name subst of
            Just bound ->
              if tyEq tyCons bound a
                then Just subst
                else Nothing
            Nothing -> Just ((name, a) : subst)
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
          let ctorNames = map fst ctors
              resultTy =
                case params of
                  [] -> TyInd (mkAnn Nom NoSpan) name
                  _ ->
                    let paramTys = map (TyVar (mkAnn Nom NoSpan)) params
                    in TyApp (mkAnn Nom NoSpan) (TyInd (mkAnn Nom NoSpan) name) paramTys
              ctorSigs =
                [ (ctorName, (ctorArgs, resultTy))
                | (ctorName, ctorArgs) <- ctors
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
