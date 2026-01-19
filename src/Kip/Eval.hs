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

import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Exception (SomeException, try)
import System.IO (hFlush, stdout)
import System.FilePath (takeFileName, takeDirectory, (</>), isRelative)
import Control.Monad (guard)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.Maybe (catMaybes)
import Data.List (find, foldl', intersect, nub)

-- | Evaluator state: runtime bindings plus render function.
data EvalState =
  MkEvalState
    { evalVals :: [(Identifier, Exp Ann)] -- ^ Value bindings.
    , evalFuncs :: [(Identifier, ([Arg Ann], [Clause Ann]))] -- ^ Function clauses.
    , evalPrimFuncs :: [(Identifier, [Arg Ann], [Exp Ann] -> EvalM (Exp Ann))] -- ^ Primitive implementations.
    , evalSelectors :: [(Identifier, Int)] -- ^ Record selector indices.
    , evalCtors :: [(Identifier, ([Ty Ann], Ty Ann))] -- ^ Constructor signatures.
    , evalTyCons :: [(Identifier, Int)] -- ^ Type constructor arities.
    , evalCurrentFile :: Maybe FilePath -- ^ Current file path for relative I/O.
    , evalRender :: EvalState -> Exp Ann -> IO String -- ^ Render function for values.
    }

-- | Empty evaluator state with a simple pretty-printer.
emptyEvalState :: EvalState -- ^ Default evaluator state.
emptyEvalState = MkEvalState [] [] [] [] [] [] Nothing (\_ e -> return (prettyExp e))

-- | Evaluation errors (currently minimal).
data EvalError =
   Unknown
   deriving (Show, Eq, Generic, Binary)
-- | Evaluator monad stack.
type EvalM = StateT EvalState (ExceptT EvalError IO)

-- | Evaluate an expression in the current evaluator state.
evalExp :: Exp Ann -- ^ Expression to evaluate.
        -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExp = evalExpWith []

-- | Evaluate an expression with a local environment.
evalExpWith :: [(Identifier, Exp Ann)] -- ^ Local environment bindings.
            -> Exp Ann -- ^ Expression to evaluate.
            -> EvalM (Exp Ann) -- ^ Evaluated expression.
evalExpWith localEnv e =
  case e of
    Var {annExp, varName, varCandidates} ->
      case lookupByCandidates localEnv varCandidates of
        Just v -> return v
        Nothing -> do
          MkEvalState{evalVals} <- get
          case lookupByCandidates evalVals varCandidates of
            Nothing -> return (Var annExp varName varCandidates)
            Just v -> evalExpWith localEnv v
    App {annExp = annApp, fn, args} -> do
      fn' <- evalExpWith localEnv fn
      args' <- mapM (evalExpWith localEnv) args
      case fn' of
        Var {varName, varCandidates} -> do
          MkEvalState{evalFuncs, evalPrimFuncs, evalSelectors, evalTyCons} <- get
          case args' of
            [arg] | any (\(ident, _) -> ident `elem` map fst evalTyCons) varCandidates ->
              return (applyTypeCase (annCase (annExp fn')) arg)
            _ -> do
              let fnCandidates = map fst varCandidates
                  matches = filter (\(n, _) -> n `elem` fnCandidates) evalFuncs
                  primMatches = filter (\(n, _, _) -> n `elem` fnCandidates) evalPrimFuncs
              pickPrimByTypes primMatches args' >>= \case
                Just primImpl -> primImpl args'
                Nothing ->
                  pickFunctionByTypes matches args' >>= \case
                    Just def -> applyFunction fn' localEnv def args'
                    Nothing ->
                      case (lookupByCandidates evalSelectors varCandidates, args') of
                        (Just idx, [arg]) -> applySelector idx arg (App annApp fn' args')
                        _ -> return (App annApp fn' args')
        _ -> return (App annApp fn' args')
    StrLit {annExp, lit} ->
      return (StrLit annExp lit)
    IntLit {annExp, intVal} ->
      return (IntLit annExp intVal)
    Bind {annExp, bindName, bindExp} -> do
      v <- evalExpWith localEnv bindExp
      return (Bind annExp bindName v)
    Seq {annExp, first, second} -> do
      case first of
        Bind {bindName, bindExp} -> do
          v <- evalExpWith localEnv bindExp
          evalExpWith ((bindName, v) : localEnv) second
        _ -> do
          _ <- evalExpWith localEnv first
          evalExpWith localEnv second
    Match {annExp, scrutinee, clauses} -> do
      scrutinee' <- evalExpWith localEnv scrutinee
      case findClause scrutinee' clauses of
        Nothing -> return (Match annExp scrutinee' clauses)
        Just (Clause _ body, patBindings) -> do
          let env = patBindings ++ localEnv
          evalExpWith env body
    Let {annExp, varName, body} ->
      evalExpWith localEnv body
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
applyFunction :: Exp Ann -- ^ Function expression.
              -> [(Identifier, Exp Ann)] -- ^ Local environment bindings.
              -> ([Arg Ann], [Clause Ann]) -- ^ Function signature and clauses.
              -> [Exp Ann] -- ^ Evaluated arguments.
              -> EvalM (Exp Ann) -- ^ Result expression.
applyFunction fn localEnv (args, clauses) values = do
  let argNames = map fst args
      argBindings = zip argNames values
  case findClause values clauses of
    Nothing -> return (App (annExp fn) fn values)
    Just (Clause pat body, patBindings) -> do
      let env = patBindings ++ argBindings ++ localEnv
      evalExpWith env body
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
    PWildcard -> Just []
    PCtor ctor vars ->
      case mval of
        Nothing -> Nothing
        Just v -> matchCtor ctor vars v

-- | Match a constructor pattern against an expression.
matchCtor :: Identifier -- ^ Constructor identifier.
          -> [(Identifier, Ann)] -- ^ Pattern variables.
          -> Exp Ann -- ^ Scrutinee expression.
          -> Maybe [(Identifier, Exp Ann)] -- ^ Bindings when matched.
matchCtor ctor vars v =
  case v of
    Var {varCandidates, varName} ->
      if ctorMatches ctor (Just varName) (map fst varCandidates) && null vars
        then Just []
        else Nothing
    App {fn, args} ->
      case fn of
        Var {varCandidates, varName} | ctorMatches ctor (Just varName) (map fst varCandidates) ->
          if length vars == length args
            then Just (zip (map fst vars) args)
            else Nothing
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
    identMatchesPoss :: Identifier -- ^ Left identifier.
                     -> Identifier -- ^ Right identifier.
                     -> Bool -- ^ True when identifiers match loosely.
    identMatchesPoss (xs1, x1) (xs2, x2) =
      (xs1 == xs2 || null xs1 || null xs2) &&
      not (null (roots x1 `intersect` roots x2))

    -- | Produce candidate roots for heuristic matching.
    roots :: Text -- ^ Surface word.
          -> [Text] -- ^ Candidate roots.
    roots txt =
      nub (catMaybes [Just txt, dropTrailingVowel txt >>= dropTrailingSoftG])

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
        Just (pref, 'ğ') -> Just (pref <> T.pack "k")
        _ -> Nothing

    -- | Strip copula suffixes from a surface word.
    stripCopulaSuffix :: Text -- ^ Surface word.
                      -> Maybe Text -- ^ Stripped word.
    stripCopulaSuffix txt =
      let lowerTxt = T.toLower txt
          suffixes = ["dir","dır","dur","dür","tir","tır","tur","tür"]
          match = find (`T.isSuffixOf` lowerTxt) suffixes
      in case match of
           Nothing -> Nothing
           Just suff ->
             let len = T.length suff
             in if T.length txt > len
                  then Just (T.take (T.length txt - len) txt)
                  else Nothing

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

-- | Choose a function definition based on inferred argument types.
pickFunctionByTypes :: [(Identifier, ([Arg Ann], [Clause Ann]))] -- ^ Candidate function definitions.
                    -> [Exp Ann] -- ^ Evaluated arguments.
                    -> EvalM (Maybe ([Arg Ann], [Clause Ann])) -- ^ Selected function.
pickFunctionByTypes defs args = do
  MkEvalState{evalTyCons} <- get
  argTys <- mapM inferType args
  let matches =
        [ def
        | (n, def@(args', _)) <- defs
        , let tys = map snd args'
        , length tys == length args
        , and (zipWith (typeMatches evalTyCons) argTys tys)
        ]
      fallback =
        [ def
        | (n, def@(args', _)) <- defs
        , let tys = map snd args'
        , length tys == length args
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> case fallback of
      d:_ -> Just d
      [] -> Nothing

-- | Choose a primitive implementation based on inferred argument types.
pickPrimByTypes :: [(Identifier, [Arg Ann], [Exp Ann] -> EvalM (Exp Ann))] -- ^ Primitive candidates.
                -> [Exp Ann] -- ^ Evaluated arguments.
                -> EvalM (Maybe ([Exp Ann] -> EvalM (Exp Ann))) -- ^ Selected primitive implementation.
pickPrimByTypes defs args = do
  MkEvalState{evalTyCons} <- get
  argTys <- mapM inferType args
  let matches =
        [ impl
        | (_, args', impl) <- defs
        , let tys = map snd args'
        , length tys == length args
        , and (zipWith (typeMatchesAllowUnknown evalTyCons) argTys tys)
        ]
  return $ case matches of
    d:_ -> Just d
    [] -> Nothing

-- | Type comparison allowing unknowns for primitive resolution.
typeMatchesAllowUnknown :: [(Identifier, Int)] -- ^ Type constructor arities.
                        -> Maybe (Ty Ann) -- ^ Possibly unknown type.
                        -> Ty Ann -- ^ Expected type.
                        -> Bool -- ^ True when types match.
typeMatchesAllowUnknown tyCons mTy ty =
  case mTy of
    Nothing ->
      case ty of
        TyVar {} -> True
        _ -> False
    Just t ->
      case ty of
        TyVar {} -> True
        _ -> typeMatches tyCons (Just t) ty

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

-- | Lookup a constructor binding by candidates.
lookupCtorByCandidates :: [(Identifier, a)] -- ^ Candidate constructors.
                       -> [(Identifier, Case)] -- ^ Candidate identifiers.
                       -> Maybe a -- ^ Matching constructor.
lookupCtorByCandidates = lookupByCandidates

-- | Infer a type for an expression when possible.
inferType :: Exp Ann -- ^ Expression to infer.
          -> EvalM (Maybe (Ty Ann)) -- ^ Inferred type.
inferType e =
  case e of
    IntLit {} -> return (Just (TyInt (mkAnn Nom NoSpan)))
    StrLit {} -> return (Just (TyString (mkAnn Nom NoSpan)))
    Bind {bindExp} -> inferType bindExp
    Seq {second} -> inferType second
    Var {varCandidates} -> do
      MkEvalState{evalVals, evalCtors} <- get
      case lookupByCandidates evalVals varCandidates of
        Just v -> inferType v
        Nothing ->
          case lookupCtorByCandidates evalCtors varCandidates of
            Just ([], ty) -> return (Just ty)
            _ -> return Nothing
    App {fn, args} -> do
      fn' <- evalExpWith [] fn
      case fn' of
        Var {varCandidates} -> do
          MkEvalState{evalCtors, evalTyCons} <- get
          case lookupCtorByCandidates evalCtors varCandidates of
            Just (tys, resTy)
              | length tys == length args -> do
                  argTys <- mapM inferType args
                  if Nothing `elem` argTys
                    then return Nothing
                    else do
                      let actuals = catMaybes argTys
                      case unifyTypes evalTyCons tys actuals of
                        Just subst -> return (Just (applySubst subst resTy))
                        Nothing -> return Nothing
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

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
    _ -> exp

-- | Check whether an inferred type matches an expected type.
typeMatches :: [(Identifier, Int)] -- ^ Type constructor arities.
            -> Maybe (Ty Ann) -- ^ Possibly unknown type.
            -> Ty Ann -- ^ Expected type.
            -> Bool -- ^ True when types match.
typeMatches tyCons mTy ty =
  case mTy of
    Nothing -> False
    Just t -> tyEq tyCons t ty

-- | Compare two types for compatibility.
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

-- | Normalize type applications using constructor arities.
normalizeTy :: [(Identifier, Int)] -- ^ Type constructor arities.
            -> Ty Ann -- ^ Type to normalize.
            -> Ty Ann -- ^ Normalized type.
normalizeTy tyCons ty =
  case ty of
    TyInt {} -> ty
    TyApp ann (TyInd _ name) args ->
      case lookup name tyCons of
        Just arity | arity > 0 -> TyApp ann (TyInd (mkAnn Nom NoSpan) name) (map (normalizeTy tyCons) args)
        _ -> TyInd ann name
    TyApp ann ctor args ->
      TyApp ann (normalizeTy tyCons ctor) (map (normalizeTy tyCons) args)
    Arr ann d i ->
      Arr ann (normalizeTy tyCons d) (normalizeTy tyCons i)
    _ -> ty

-- | Compare identifiers, allowing missing namespace prefixes.
identMatches :: Identifier -- ^ Left identifier.
             -> Identifier -- ^ Right identifier.
             -> Bool -- ^ True when identifiers match loosely.
identMatches (xs1, x1) (xs2, x2) =
  x1 == x2 && (xs1 == xs2 || null xs1 || null xs2)

-- | Unify expected types with actual types, returning substitutions.
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

    -- | Unify a single expected/actual type pair.
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
    TyInd {} -> ty
    TyString {} -> ty
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
      Defn name _ e ->
        modify (\s -> s { evalVals = (name, e) : evalVals s })
      Function name args _ body _ ->
        modify (\s -> s { evalFuncs = (name, (args, body)) : evalFuncs s })
      PrimFunc name args _ _ ->
        case primImpl mPath name args of
          Nothing -> return ()
          Just impl ->
            modify (\s -> s { evalPrimFuncs = (name, args, impl) : evalPrimFuncs s })
      Load _ ->
        return ()
      NewType name params ctors -> do
        let selectors = []
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
        modify (\s -> s { evalSelectors = selectors ++ evalSelectors s
                        , evalCtors = ctorSigs ++ evalCtors s
                        , evalTyCons = (name, length params) : evalTyCons s
                        })
      PrimType name ->
        modify (\s -> s { evalTyCons = (name, 0) : evalTyCons s })
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
primImpl mPath ident args = do
  guard (primFileMatches mPath ident)
  case ident of
    ([], "yaz") ->
      case args of
        [(_, TyInt _)] -> Just primWrite
        [(_, TyString _)] -> Just primWrite
        [_, _] -> Just primWriteFile
        _ -> Nothing
    ([], "oku") ->
      case args of
        [] -> Just primRead
        [_] -> Just primReadFile
        _ -> Just primRead
    ([], "uzunluk") -> Just primStringLength
    ([], "birleşim") -> Just primStringConcat
    (["tam", "sayı"], "hal") -> Just primStringToInt
    ([], "ters") ->
      case args of
        [(_, TyString _)] -> Just primStringReverse
        _ -> Nothing
    ([], "toplam") -> Just (primIntBin "toplam" (+))
    ([], "çarpım") -> Just (primIntBin "çarpım" (*))
    ([], "fark") -> Just (primIntBin "fark" (-))
    (["dizge"], "hal") -> Just primIntToString
    ([], "eşitlik") -> Just (primIntCmp "eşitlik" (==))
    ([], "küçüklük") -> Just (primIntCmp "küçüklük" (<))
    (["küçük"], "eşitlik") -> Just (primIntCmp "küçük-eşitlik" (<=))
    ([], "büyüklük") -> Just (primIntCmp "büyüklük" (>))
    (["büyük"], "eşitlik") -> Just (primIntCmp "büyük-eşitlik" (>=))
    _ -> Nothing

-- | Check whether a primitive belongs to a given file context.
primFileMatches :: Maybe FilePath -- ^ Current file path.
                -> Identifier -- ^ Primitive name.
                -> Bool -- ^ True when primitive belongs to file.
primFileMatches mPath ident =
  case (mPath, primFile ident) of
    (Just path, Just primPath) -> takeFileName path == primPath
    _ -> False

-- | Map a primitive identifier to the file that defines it.
primFile :: Identifier -- ^ Primitive identifier.
         -> Maybe FilePath -- ^ Source file path when present.
primFile ident =
  case ident of
    ([], "yaz") -> Just "temel-etki.kip"
    ([], "oku") -> Just "temel-etki.kip"
    ([], "uzunluk") -> Just "temel-dizge.kip"
    ([], "birleşim") -> Just "temel-dizge.kip"
    (["tam", "sayı"], "hal") -> Just "temel-dizge.kip"
    ([], "ters") -> Just "temel-dizge.kip"
    ([], "toplam") -> Just "temel-tam-sayı.kip"
    ([], "çarpım") -> Just "temel-tam-sayı.kip"
    ([], "fark") -> Just "temel-tam-sayı.kip"
    (["dizge"], "hal") -> Just "temel-tam-sayı.kip"
    ([], "eşitlik") -> Just "temel-tam-sayı.kip"
    ([], "küçüklük") -> Just "temel-tam-sayı.kip"
    (["küçük"], "eşitlik") -> Just "temel-tam-sayı.kip"
    ([], "büyüklük") -> Just "temel-tam-sayı.kip"
    (["büyük"], "eşitlik") -> Just "temel-tam-sayı.kip"
    _ -> Nothing

-- | Primitive print for integers and strings.
primWrite :: [Exp Ann] -- ^ Arguments.
          -> EvalM (Exp Ann) -- ^ Result expression.
primWrite args =
  case args of
    [StrLit _ s] -> do
      liftIO (putStrLn (T.unpack s))
      liftIO (hFlush stdout)
      return (Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)])
    [IntLit _ n] -> do
      liftIO (print n)
      liftIO (hFlush stdout)
      return (Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "yaz") []) args)

-- | Primitive read from standard input.
primRead :: [Exp Ann] -- ^ Arguments.
         -> EvalM (Exp Ann) -- ^ Result expression.
primRead args =
  case args of
    [] -> do
      line <- liftIO getLine
      return (StrLit (mkAnn Nom NoSpan) (T.pack line))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "oku") []) args)

-- | Primitive read from a file path.
primReadFile :: [Exp Ann] -- ^ Arguments.
             -> EvalM (Exp Ann) -- ^ Result expression.
primReadFile args =
  case args of
    [StrLit _ path] -> do
      st <- get
      content <- liftIO (readFirstPath (resolveReadCandidates st path))
      case content of
        Nothing -> return (Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)])
        Just text ->
          return
            (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)])
              [StrLit (mkAnn Nom NoSpan) text])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "oku") []) args)

-- | Primitive write to a file path.
primWriteFile :: [Exp Ann] -- ^ Arguments.
              -> EvalM (Exp Ann) -- ^ Result expression.
primWriteFile args =
  case args of
    [StrLit _ path, StrLit _ content] -> do
      st <- get
      let resolved = resolvePath st path
      res <- liftIO (try (TIO.writeFile resolved content) :: IO (Either SomeException ()))
      case res of
        Left _ -> return (boolToExp False)
        Right _ -> return (boolToExp True)
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "yaz") []) args)

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

-- | Primitive to compute string length.
primStringLength :: [Exp Ann] -- ^ Arguments.
                 -> EvalM (Exp Ann) -- ^ Result expression.
primStringLength args =
  case args of
    [StrLit ann s] ->
      return (IntLit ann (fromIntegral (T.length s)))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "uzunluk") []) args)

-- | Primitive string concatenation.
primStringConcat :: [Exp Ann] -- ^ Arguments.
                 -> EvalM (Exp Ann) -- ^ Result expression.
primStringConcat args =
  case args of
    [StrLit ann a, StrLit _ b] ->
      return (StrLit ann (a <> b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "birleşim") []) args)

-- | Primitive string reverse.
primStringReverse :: [Exp Ann] -- ^ Arguments.
                  -> EvalM (Exp Ann) -- ^ Result expression.
primStringReverse args =
  case args of
    [StrLit ann s] ->
      return (StrLit ann (T.reverse s))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "ters") []) args)

-- | Primitive to parse a string into an integer option.
primStringToInt :: [Exp Ann] -- ^ Arguments.
                -> EvalM (Exp Ann) -- ^ Result expression.
primStringToInt args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n ->
          return
            (App ann (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)])
              [IntLit ann n])
        Nothing ->
          return (Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)])
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "tam-sayı-hali") []) args)

-- | Primitive integer binary operator.
primIntBin :: Text -- ^ Operator name.
           -> (Integer -> Integer -> Integer) -- ^ Integer operator.
           -> [Exp Ann] -- ^ Arguments.
           -> EvalM (Exp Ann) -- ^ Result expression.
primIntBin fname op args =
  case args of
    [IntLit ann a, IntLit _ b] ->
      return (IntLit ann (op a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], fname) []) args)

-- | Primitive integer comparison operator.
primIntCmp :: Text -- ^ Operator name.
           -> (Integer -> Integer -> Bool) -- ^ Predicate.
           -> [Exp Ann] -- ^ Arguments.
           -> EvalM (Exp Ann) -- ^ Result expression.
primIntCmp fname op args =
  case args of
    [IntLit _ a, IntLit _ b] ->
      return (boolToExp (op a b))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], fname) []) args)

-- | Primitive integer to string conversion.
primIntToString :: [Exp Ann] -- ^ Arguments.
                -> EvalM (Exp Ann) -- ^ Result expression.
primIntToString args =
  case args of
    [IntLit ann n] ->
      return (StrLit ann (T.pack (show n)))
    _ -> return (App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) (["dizge"], "hal") []) args)

-- | Convert a boolean into a Kip boolean value expression.
boolToExp :: Bool -- ^ Boolean value.
          -> Exp Ann -- ^ Kip boolean expression.
boolToExp b =
  let name = if b then ([], "doğru") else ([], "yanlış")
  in Var (mkAnn Nom NoSpan) name [(name, Nom)]


-- | Run an evaluator action with a starting state.
runEvalM :: EvalM a -- ^ Evaluator computation.
         -> EvalState -- ^ Initial evaluator state.
         -> IO (Either EvalError (a, EvalState)) -- ^ Result or error.
runEvalM m s = runExceptT (runStateT m s)
