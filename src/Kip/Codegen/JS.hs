{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{- |
JavaScript code generator for Kip.

= Design

This backend emits JavaScript text directly from the typed Kip AST. The output is
intended for execution in environments that support top-level @await@ (Node ESM
and the browser playground loader), so generated code can keep Kip's effectful
operations (read/write/random) in a uniform async style.

= Output layout

'codegenProgram' always emits:

1. A primitive prelude ('Prim.primitiveJsPrelude').
2. An async runner function containing user code.
3. User statements in source order (with function declarations emitted first).

= Semantics choices

* Expressions are lowered to /awaitable/ JavaScript expressions.
* Pattern matching is lowered to ordered @if/else if@ chains.
* Kip ADTs become tagged JS objects: @{ tag, args }@.
* Partial-application edge cases are preserved by dedicated lowering helpers.

= Notes

This module prefers explicit textual codegen over an intermediate JS AST to keep
debuggability high and to keep generated code close to the source language model.
-}
module Kip.Codegen.JS
  ( codegenProgram
  , pruneProgramTaggedStmts
  , codegenRuntime
  , runtimeExportNames
  , codegenStmtsInProgram
  , definedJsNamesInProgram
  ) where

import Data.Char (isAlphaNum, isLetter)
import qualified Data.Foldable as F
import Data.List (partition)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Kip.AST
import qualified Kip.Primitive as Prim

-- | Everything code generation needs to turn a name into a JavaScript symbol.
data CodegenCtx = MkCodegenCtx
  { sectionableFns :: Set.Set Identifier
    -- ^ Functions that may be called with one argument fixed, forming a section.
  , resolvedCallNames :: Map.Map Span Text
    -- ^ JS name chosen by the typechecker for the call at each span.
  , overloadRegistry :: Map.Map (Identifier, [Ty Ann]) Text
    -- ^ Mangled JS name for each overload of an overloaded function.
  , callTargetsByIdent :: Map.Map Identifier [CallTarget]
    -- ^ Call targets indexed by full identifier.
  , callTargetsByRoot :: Map.Map Text [CallTarget]
    -- ^ Call targets indexed by bare name, ignoring the module qualifier.
  , callTargetsByIdentArity :: Map.Map (Identifier, Int) [CallTarget]
    -- ^ Call targets indexed by full identifier and arity.
  , callTargetsByRootArity :: Map.Map (Text, Int) [CallTarget]
    -- ^ Call targets indexed by bare name and arity.
  , localScope :: [Identifier]
    -- ^ Names bound by enclosing patterns and lets, which shadow globals.
  , currentFunction :: Maybe (Identifier, [Ty Ann], Text)
    -- ^ Name, signature, and JS name of the function being emitted, so that
    -- recursive calls resolve to it.
  }

-- | One callable definition: its argument types and the JS name it is emitted as.
type CallTarget = ([Ty Ann], Text)

-- | A reference to a definition, used for reachability pruning.
data DefRef
  = RefExact Identifier [Ty Ann]
  -- ^ A specific overload, identified by name and argument types.
  | RefName Identifier
  -- ^ A name whose overload could not be determined; every overload is kept.
  deriving (Eq, Ord, Show)

-- | Build exact/root and exact-arity/root-arity call-target indexes.
--
-- 'Map.insertWith (flip (++))' preserves the user-before-primitive ordering of
-- the source target list, matching the former ordered 'Map.toList' scans.
buildCallTargetIndexes
  :: [(Identifier, CallTarget)]
  -> ( Map.Map Identifier [CallTarget]
     , Map.Map Text [CallTarget]
     , Map.Map (Identifier, Int) [CallTarget]
     , Map.Map (Text, Int) [CallTarget]
     )
buildCallTargetIndexes = foldl' add (Map.empty, Map.empty, Map.empty, Map.empty)
  where
    add (byIdent, byRoot, byIdentArity, byRootArity) (ident@(_, root), target@(sig, _)) =
      let arity = length sig
      in ( Map.insertWith (flip (++)) ident [target] byIdent
         , Map.insertWith (flip (++)) root [target] byRoot
         , Map.insertWith (flip (++)) (ident, arity) [target] byIdentArity
         , Map.insertWith (flip (++)) (root, arity) [target] byRootArity
         )

-- | Build codegen context from resolved call signatures and program statements.
buildCodegenCtx :: Map.Map Span (Identifier, [Ty Ann]) -- ^ Function and argument types the typechecker resolved at each call span.
                -> [Stmt Ann] -- ^ Statements of the whole program.
                -> CodegenCtx -- ^ Context with every name index populated.
buildCodegenCtx resolvMap stmts =
  let arityMap = foldl collectArity Map.empty stmts
      sectionable =
        Set.fromList
          [ ident
          | (ident, arities) <- Map.toList arityMap
          , any (> 1) arities
          , 1 `notElem` arities
          ]
      -- Build overload registry: group functions by Identifier, then generate
      -- qualified names for identifiers that have more than one signature.
      sigMap = foldl collectSigs Map.empty stmts
      overloaded = Map.filter (\sigs -> length sigs > 1) sigMap
      registry = Map.fromList
        [ ((ident, sig), qualifiedJsName ident sig)
        | (ident, sigs) <- Map.toList overloaded
        , sig <- sigs
        ]
      -- All user function signatures (overloaded and non-overloaded) mapped to
      -- their emitted JS names.
      userFuncs = Map.fromList
        [ ((ident, sig), Map.findWithDefault (toJsIdent ident) (ident, sig) registry)
        | Function ident args _ _ _ <- stmts
        , let sig = normalizeSig (map argType args)
        ]
      -- Build map of primitive signatures to emitted JS function names.
      prims = Map.fromList
        [ ((ident, argTys), lookupPrimJsName ident argTys)
        | PrimFunc ident args _ _ <- stmts
        , let argTys = normalizeSig (map argType args)
        ]
      -- Typechecker resolutions are authoritative. Compile them to final JS
      -- symbols once instead of normalizing and probing signature maps at
      -- every call/value occurrence.
      resolvedNames = Map.map resolveTarget resolvMap
      resolveTarget (ident, argTys) =
        let sig = normalizeSig argTys
        in case Map.lookup (ident, sig) prims of
             Just primName -> primName
             Nothing -> Map.findWithDefault (toJsIdent ident) (ident, sig) registry
      targetEntries =
        [ (ident, (sig, jsName))
        | ((ident, sig), jsName) <- Map.toList userFuncs ++ Map.toList prims
        ]
      (targetsByIdent, targetsByRoot, targetsByIdentArity, targetsByRootArity) =
        buildCallTargetIndexes targetEntries
  in MkCodegenCtx
       { sectionableFns = sectionable
       , resolvedCallNames = resolvedNames
       , overloadRegistry = registry
       , callTargetsByIdent = targetsByIdent
       , callTargetsByRoot = targetsByRoot
       , callTargetsByIdentArity = targetsByIdentArity
       , callTargetsByRootArity = targetsByRootArity
       , localScope = []
       , currentFunction = Nothing
       }
  where
    collectArity acc stmt =
      case stmt of
        Function ident args _ _ _ -> Map.insertWith mergeArities ident [length args] acc
        PrimFunc ident args _ _ -> Map.insertWith mergeArities ident [length args] acc
        _ -> acc
    mergeArities new old = Set.toList (Set.fromList (new ++ old))

    collectSigs acc stmt =
      case stmt of
        Function ident args _ _ _ ->
          Map.insertWith mergeSigs ident [normalizeSig (map argType args)] acc
        PrimFunc ident args _ _ ->
          Map.insertWith mergeSigs ident [normalizeSig (map argType args)] acc
        _ -> acc
    mergeSigs new old =
      let all' = old ++ new
      in [x | (x:_) <- groupByEq all']

    groupByEq [] = []
    groupByEq (x:xs) = let (same, rest) = partition (== x) xs
                        in (x : same) : groupByEq rest

    qualifiedJsName ident sig =
      toJsIdent ident <> "$" <> T.intercalate "$" (map tyToSuffix sig)

-- | Look up the JS name for a function, using qualified name if overloaded.
lookupOverloadName :: CodegenCtx -- ^ Code generation context.
                   -> Identifier -- ^ Function name.
                   -> [Ty Ann] -- ^ Argument types selecting the overload.
                   -> Text -- ^ Mangled JS name when overloaded, otherwise the plain name.
lookupOverloadName ctx name argTys =
  case Map.lookup (name, normalizeSig argTys) (overloadRegistry ctx) of
    Just qualName -> qualName
    Nothing -> toJsIdent name

-- | Look up the JS name for a call site using the resolved signature.
lookupCallName :: CodegenCtx -- ^ Code generation context.
               -> Span -- ^ Span of the call site.
               -> Exp Ann -- ^ Head expression, used when the span has no resolution.
               -> [Exp Ann] -- ^ Argument expressions, used as resolution hints.
               -> Text -- ^ JS name to call.
lookupCallName ctx span' fallback args =
  case Map.lookup span' (resolvedCallNames ctx) of
    Just resolvedName -> resolvedName
    Nothing -> fallbackCallName ctx fallback args

-- | Best-effort call target recovery when no span-based resolution exists.
fallbackCallName :: CodegenCtx -- ^ Code generation context.
                 -> Exp Ann -- ^ Head expression of the call.
                 -> [Exp Ann] -- ^ Argument expressions, used as resolution hints.
                 -> Text -- ^ Best-guess JS name to call.
fallbackCallName ctx fallback args =
  case fallback of
    Var {varName, varCandidates} ->
      let candidates = uniqIdents (map fst varCandidates ++ [varName])
      in case resolveCurrentFunction ctx candidates args of
           Just jsName -> jsName
           Nothing ->
             case resolveByCandidates ctx candidates args of
               Just jsName -> jsName
               Nothing ->
                 case varCandidates of
                   ((ident, _) : _) -> toJsIdent ident
                   [] -> toJsIdent varName
    _ -> codegenExpWith ctx fallback

-- | Try resolving a call to the currently-emitted function for recursion.
resolveCurrentFunction :: CodegenCtx -- ^ Code generation context.
                       -> [Identifier] -- ^ Candidate names for the call.
                       -> [Exp Ann] -- ^ Argument expressions, matched against arity.
                       -> Maybe Text -- ^ JS name of the enclosing function when the call recurses into it.
resolveCurrentFunction ctx candidates args =
  case currentFunction ctx of
    Just (ident, sig, jsName)
      | any (identMatches ident) candidates
      , length sig == length args ->
          Just jsName
    _ -> Nothing

-- | Fallback call resolution using candidate identifiers and lightweight hints.
resolveByCandidates :: CodegenCtx -- ^ Code generation context.
                    -> [Identifier] -- ^ Candidate names for the call.
                    -> [Exp Ann] -- ^ Argument expressions, used as arity and literal hints.
                    -> Maybe Text -- ^ JS name when exactly one target matches.
resolveByCandidates ctx candidates args =
  let byArity = concatMap (\ident -> callTargetsForIdentAtArity ctx ident (length args)) candidates
      exact = filterByArgHints byArity args
  in case exact of
       [(_, jsName)] -> Just jsName
       _ ->
         if length args == 1
           then case args of
                  [fixedArg] ->
                    let sectionCands = concatMap (sectionTargetsForIdent ctx fixedArg) candidates
                    in case sectionCands of
                         [(_, jsName)] -> Just jsName
                         _ -> Nothing
                  _ -> Nothing
           else Nothing

-- | Collect all possible JS targets for an identifier in the current program.
callTargetsForIdent :: CodegenCtx -- ^ Code generation context.
                    -> Identifier -- ^ Name to look up.
                    -> [([Ty Ann], Text)] -- ^ Every definition the name could refer to.
callTargetsForIdent ctx ident@(mods, root)
  | null mods = Map.findWithDefault [] root (callTargetsByRoot ctx)
  | otherwise =
      uniqTargets
        ( Map.findWithDefault [] ident (callTargetsByIdent ctx)
          ++ Map.findWithDefault [] ([], root) (callTargetsByIdent ctx)
        )

-- | Look up call targets by identifier and exact arity.
callTargetsForIdentAtArity :: CodegenCtx -- ^ Code generation context.
                           -> Identifier -- ^ Name to look up.
                           -> Int -- ^ Number of arguments at the call site.
                           -> [CallTarget] -- ^ Definitions of that name taking exactly that many arguments.
callTargetsForIdentAtArity ctx ident@(mods, root) arity
  | null mods = Map.findWithDefault [] (root, arity) (callTargetsByRootArity ctx)
  | otherwise =
      uniqTargets
        ( Map.findWithDefault [] (ident, arity) (callTargetsByIdentArity ctx)
          ++ Map.findWithDefault [] (([], root), arity) (callTargetsByIdentArity ctx)
        )

-- | Resolve section-call targets using fixed-argument case/type hints.
sectionTargetsForIdent :: CodegenCtx -- ^ Code generation context.
                       -> Exp Ann -- ^ The single argument supplied in the section.
                       -> Identifier -- ^ Name to look up.
                       -> [([Ty Ann], Text)] -- ^ Two-argument definitions consistent with that argument.
sectionTargetsForIdent ctx fixedArg ident =
  let fixedIdx = if annCase (annExp fixedArg) == Ins then 0 else 1
      hint = inferSimpleTy fixedArg
      targets = callTargetsForIdentAtArity ctx ident 2
      hinted =
        case hint of
          Just simpleTy ->
            filter (\(sig, _) -> sigMatchesHint sig fixedIdx simpleTy) targets
          Nothing -> targets
  in uniqTargets hinted

-- | Filter candidate signatures by obvious literal argument hints.
filterByArgHints :: [([Ty Ann], Text)] -- ^ Candidate definitions.
                 -> [Exp Ann] -- ^ Argument expressions supplying literal type hints.
                 -> [([Ty Ann], Text)] -- ^ Candidates whose signatures agree with every hint.
filterByArgHints targets args =
  let hints = [(ix, ty) | (ix, arg) <- zip [0 ..] args, Just ty <- [inferSimpleTy arg]]
  in case hints of
       [] -> targets
       _ -> filter (\(sig, _) -> all (uncurry (sigMatchesHint sig)) hints) targets

-- | A coarse type read directly off a literal, used to disambiguate overloads.
data SimpleTy
  = SimpleInt -- ^ Integer literal.
  | SimpleFloat -- ^ Floating-point literal.
  | SimpleString -- ^ String literal.
  | SimpleChar -- ^ Character literal.

-- | Infer a coarse type from syntax-only expression forms.
inferSimpleTy :: Exp Ann -- ^ Expression to inspect.
              -> Maybe SimpleTy -- ^ Coarse type for a literal, 'Nothing' otherwise.
inferSimpleTy exp' =
  case exp' of
    IntLit {} -> Just SimpleInt
    FloatLit {} -> Just SimpleFloat
    StrLit {} -> Just SimpleString
    CharLit {} -> Just SimpleChar
    _ -> Nothing

-- | Check whether a signature position matches an inferred coarse type.
sigMatchesHint :: [Ty Ann] -- ^ Signature to test.
               -> Int -- ^ Argument position the hint applies to.
               -> SimpleTy -- ^ Coarse type inferred at that position.
               -> Bool -- ^ 'True' when the signature has that type at that position.
sigMatchesHint sig idx hint
  | idx < 0 = False
  | otherwise =
      case (listIndex sig idx, hint) of
        (Just TyInt {}, SimpleInt) -> True
        (Just TyFloat {}, SimpleFloat) -> True
        (Just TyString {}, SimpleString) -> True
        (Just TyChar {}, SimpleChar) -> True
        _ -> False
  where
    listIndex [] _ = Nothing
    listIndex (x:_) 0 = Just x
    listIndex (_:xs) n = listIndex xs (n - 1)

-- | Deduplicate target pairs while preserving order.
uniqTargets :: [([Ty Ann], Text)] -- ^ Targets that may contain duplicates.
            -> [([Ty Ann], Text)] -- ^ First occurrence of each target in original order.
uniqTargets = reverse . fst . foldl' add ([], Set.empty)
  where
    add (acc, seen) item
      | Set.member item seen = (acc, seen)
      | otherwise = (item : acc, Set.insert item seen)

-- | Deduplicate identifier candidates while preserving order.
uniqIdents :: [Identifier] -- ^ Candidates that may contain duplicates.
           -> [Identifier] -- ^ First occurrence of each candidate in original order.
uniqIdents = reverse . fst . foldl' add ([], Set.empty)
  where
    add (acc, seen) ident
      | Set.member ident seen = (acc, seen)
      | otherwise = (ident : acc, Set.insert ident seen)

-- | Get the JS primitive name for a given function name and arg types.
-- Falls back to the bare JS identifier if no special mapping exists.
lookupPrimJsName :: Identifier -- ^ Primitive function name.
                 -> [Ty Ann] -- ^ Argument types, which select between container overloads.
                 -> Text -- ^ JS runtime function name.
lookupPrimJsName name argTys =
  case (name, argTys) of
    (([], "birleşim"), [leftTy, rightTy])
      | isSetTyForLookup leftTy && isSetTyForLookup rightTy -> "küme_birleşim"
    (([], "boyut"), [ty]) | isSetTyForLookup ty -> "küme_boyut"
    (([], "ek"), [setTy, _]) | isSetTyForLookup setTy -> "küme_ilave"
    (([], "çıkarılmış"), [setTy, _]) | isSetTyForLookup setTy -> "küme_çıkarma"
    (([], "üyelik"), [setTy, _]) | isSetTyForLookup setTy -> "küme_içerik"
    (([], "liste-hal"), [ty]) | isSetTyForLookup ty -> "küme_liste"
    ((["liste"], "hal"), [ty]) | isSetTyForLookup ty -> "küme_liste"
    (([], "küme-hal"), [ty]) | isListTyForLookup ty -> "liste_küme"
    ((["küme"], "hal"), [ty]) | isListTyForLookup ty -> "liste_küme"
    (([], "birleşim"), [leftTy, rightTy])
      | isMapTyForLookup leftTy && isMapTyForLookup rightTy -> "sözlük_birleşim"
    (([], "boyut"), [ty]) | isMapTyForLookup ty -> "sözlük_boyut"
    (([], "ek"), [mapTy, _, _]) | isMapTyForLookup mapTy -> "sözlük_ek"
    (([], "çıkarılmış"), [mapTy, _]) | isMapTyForLookup mapTy -> "sözlük_çıkarılmış"
    (([], "karşılık"), [mapTy, _]) | isMapTyForLookup mapTy -> "sözlük_karşılık"
    (([], "liste-hal"), [ty]) | isMapTyForLookup ty -> "sözlük_liste"
    ((["liste"], "hal"), [ty]) | isMapTyForLookup ty -> "sözlük_liste"
    (([], "ters"), [_]) -> "__kip_prim_ters"
    (([], "birleşim"), [_, _]) -> "__kip_prim_birleşim"
    (([], "uzunluk"), [_]) -> "__kip_prim_uzunluk"
    (([], "toplam"), [_, _]) -> "__kip_prim_toplam"
    (([], "fark"), [_, _]) -> "__kip_prim_fark"
    (([], "öğe"), [TyString {}, TyInt {}]) -> "__kip_prim_öğe"
    (([], "alış"), [TyString {}, TyInt {}]) -> "__kip_prim_alış"
    (([], "bırakış"), [TyString {}, TyInt {}]) -> "__kip_prim_bırakış"
    (([], "son"), [TyString {}]) -> "__kip_prim_son"
    (([], "boşluk"), [TyString {}]) -> "__kip_prim_bosluk"
    (([], "satırlar"), [TyString {}]) -> "__kip_prim_satirlar"
    (([], "kelimeler"), [TyString {}]) -> "__kip_prim_kelimeler"
    ((["büyük"], "hal"), [TyString {}]) -> "__kip_prim_dizge_büyük_hal"
    ((["küçük"], "hal"), [TyString {}]) -> "__kip_prim_dizge_küçük_hal"
    (([], "eşitlik"), [TyString {}, TyString {}]) -> "__kip_prim_dizge_eşitlik"
    (([], "eşitlik"), [TyChar {}, TyChar {}]) -> "__kip_prim_karakter_eşitlik"
    ((["dizge"], "hal"), [TyChar {}]) -> "__kip_prim_karakter_dizge_hal"
    (([], "harflik"), [TyChar {}]) -> "__kip_prim_karakter_harflik"
    (([], "rakamlık"), [TyChar {}]) -> "__kip_prim_karakter_rakamlık"
    ((["harf"], "rakamlık"), [TyChar {}]) -> "__kip_prim_karakter_harf_rakamlık"
    ((["büyük"], "harflik"), [TyChar {}]) -> "__kip_prim_karakter_buyuk_harflik"
    ((["küçük"], "harflik"), [TyChar {}]) -> "__kip_prim_karakter_kucuk_harflik"
    (([], "boşlukluk"), [TyChar {}]) -> "__kip_prim_karakter_boslukluk"
    (([], "oku"), []) -> "__kip_prim_oku_stdin"
    (([], "oku"), [_]) -> "__kip_prim_oku_dosya"
    ((["argüman"], "oku"), []) -> "__kip_prim_arguman_oku"
    ((["çevreden"], "oku"), [TyString {}]) -> "__kip_prim_cevreden_oku"
    (([], "yaz"), [_, _]) -> "__kip_prim_yaz_dosya"
    _ -> toJsIdent name

-- | Detect @öğe küme'si@ types in primitive JS-name lookup.
isSetTyForLookup :: Ty Ann -- ^ Type to classify.
                 -> Bool -- ^ 'True' for a one-parameter set type.
isSetTyForLookup ty =
  case ty of
    TyApp {tyCtor = TyInd {indName = ([], "küme")}, tyArgs = [_]} -> True
    _ -> False

-- | Detect @öğe listesi@ types in primitive JS-name lookup.
isListTyForLookup :: Ty Ann -- ^ Type to classify.
                  -> Bool -- ^ 'True' for a one-parameter list type.
isListTyForLookup ty =
  case ty of
    TyApp {tyCtor = TyInd {indName = ([], "liste")}, tyArgs = [_]} -> True
    _ -> False

-- | Detect @anahtar'dan değer'e sözlük@ types in primitive JS-name lookup.
isMapTyForLookup :: Ty Ann -- ^ Type to classify.
                 -> Bool -- ^ 'True' for a two-parameter dictionary type.
isMapTyForLookup ty =
  case ty of
    TyApp {tyCtor = TyInd {indName = ([], "sözlük")}, tyArgs = [_, _]} -> True
    _ -> False

-- | Convert a type to a suffix string for qualified overload names.
tyToSuffix :: Ty Ann -- ^ Argument type to encode.
           -> Text -- ^ JS-safe suffix identifying the type in a mangled name.
tyToSuffix ty =
  case ty of
    TyInt {} -> "tam_sayı"
    TyFloat {} -> "ondalık_sayı"
    TyString {} -> "dizge"
    TyChar {} -> "karakter"
    TyInd {indName} -> toJsIdent indName
    TyApp {tyCtor, tyArgs} ->
      tyToSuffix tyCtor <> "$" <> T.intercalate "$" (map tyToSuffix tyArgs)
    TyVar {} -> "any"
    TySkolem {} -> "any"
    Arr {} -> "fn"

-- | Normalize a signature for stable map-key lookup.
normalizeSig :: [Ty Ann] -- ^ Argument types as written.
             -> [Ty Ann] -- ^ Same types with annotations normalized for use as a map key.
normalizeSig = map normalizeTyForLookup

-- | Normalize type annotations so equivalent types share lookup keys.
normalizeTyForLookup :: Ty Ann -- ^ Type as written.
                     -> Ty Ann -- ^ Type with spans and cases cleared and all variables unified.
normalizeTyForLookup ty =
  case ty of
    TyInt {} -> TyInt (mkAnn Nom NoSpan)
    TyFloat {} -> TyFloat (mkAnn Nom NoSpan)
    TyString {} -> TyString (mkAnn Nom NoSpan)
    TyChar {} -> TyChar (mkAnn Nom NoSpan)
    TyInd {indName} -> TyInd (mkAnn Nom NoSpan) indName
    TyVar {} -> TyVar (mkAnn Nom NoSpan) ([], "__any")
    TySkolem {} -> TyVar (mkAnn Nom NoSpan) ([], "__any")
    Arr {dom, img} ->
      Arr (mkAnn Nom NoSpan) (normalizeTyForLookup dom) (normalizeTyForLookup img)
    TyApp {tyCtor, tyArgs} ->
      TyApp (mkAnn Nom NoSpan) (normalizeTyForLookup tyCtor) (map normalizeTyForLookup tyArgs)

-- | Codegen a list of statements into a JS-like program.
-- Order: primitives, then async IIFE containing:
--   - function definitions (hoisted)
--   - expression statements (executed)
codegenProgram :: Map.Map Span (Identifier, [Ty Ann]) -- ^ Function and argument types the typechecker resolved at each call span.
               -> [Stmt Ann] -- ^ Statements of the whole program.
               -> Text -- ^ Complete JavaScript source, runtime prelude included.
codegenProgram resolvMap stmts =
  let ctx = buildCodegenCtx resolvMap stmts
      -- Separate function definitions from other statements
      (funcDefs, otherStmts) = partition isFunctionDef stmts
      mergedFuncDefs = mergeCompatibleFunctions ctx funcDefs
      -- Function definitions first (they hoist anyway)
      funcCode = renderStmtBlock ctx mergedFuncDefs
      -- Expression statements last
      exprCode = renderStmtBlock ctx otherStmts
      -- All user code inside async IIFE.
      wrapped = T.unlines
        [ "const __kip_run = async () => {"
        , funcCode
        , ""
        , exprCode
        , "};"
        , "await __kip_run();"
        , "__kip_close_stdin();"
        ]
      runtimePrelude = pruneJsPrimitives wrapped
  in formatJsOutput (runtimePrelude <> "\n\n" <> wrapped)

-- | Prune runtime primitive bindings that are not referenced by generated code.
pruneJsPrimitives :: Text -- ^ Generated program body, scanned for runtime references.
                  -> Text -- ^ Runtime prelude with unreferenced primitives removed.
pruneJsPrimitives programText =
  let initialUsed =
        Set.fromList
          [ name
          | (name, _, _) <- Prim.primitiveJsPrunableSpecs
          , textMentionsIdent programText name
          ]
      used = closeRuntimeDeps initialUsed
  in foldl'
       (\acc (name, _, snippet) ->
          if name `Set.member` used
            then acc
            else T.replace snippet "" acc)
       Prim.primitiveJsPrelude
       Prim.primitiveJsPrunableSpecs

-- | Close runtime symbol usage through inter-primitive dependencies.
closeRuntimeDeps :: Set.Set Text -- ^ Runtime symbols referenced directly by generated code.
                 -> Set.Set Text -- ^ Those symbols plus everything they transitively depend on.
closeRuntimeDeps roots = go roots (Set.toList roots)
  where
    depMap = Map.fromList [(name, deps) | (name, deps, _) <- Prim.primitiveJsPrunableSpecs]

    go seen [] = seen
    go seen (name:rest) =
      let deps = Map.findWithDefault [] name depMap
          newDeps = filter (`Set.notMember` seen) deps
          seen' = foldl' (flip Set.insert) seen newDeps
      in go seen' (rest ++ newDeps)

-- | Check whether an identifier occurs as a standalone token in text.
textMentionsIdent :: Text -- ^ Text to search.
                  -> Text -- ^ Identifier to look for.
                  -> Bool -- ^ 'True' when it occurs bounded by non-identifier characters.
textMentionsIdent hay ident
  | T.null ident = False
  | otherwise = go hay
  where
    n = T.length ident

    go t =
      case T.breakOn ident t of
        (_, rest) | T.null rest -> False
        (before, rest) ->
          let prevChar = if T.null before then Nothing else Just (T.last before)
              after = T.drop n rest
              nextChar = if T.null after then Nothing else Just (T.head after)
              matched = isBoundary prevChar && isBoundary nextChar
          in matched || go (T.drop 1 rest)

    isBoundary Nothing = True
    isBoundary (Just c) = not (isJsIdentChar c)

-- | JS identifier-character predicate used by token-boundary checks.
isJsIdentChar :: Char -- ^ Character to classify.
              -> Bool -- ^ 'True' when it may appear inside a JS identifier.
isJsIdentChar c = isAlphaNum c || c == '_' || c == '$'

-- | Render a statement block, skipping statements that intentionally emit no JS.
renderStmtBlock :: CodegenCtx -- ^ Code generation context.
                -> [Stmt Ann] -- ^ Statements to emit.
                -> Text -- ^ Emitted statements, blank-line separated.
renderStmtBlock ctx stmts =
  T.intercalate "\n\n" (mapMaybe (renderStmtMaybe ctx) stmts)

-- | Render one statement when it has concrete JS output.
renderStmtMaybe :: CodegenCtx -- ^ Code generation context.
                -> Stmt Ann -- ^ Statement to emit.
                -> Maybe Text -- ^ Emitted JS, or 'Nothing' for statements with no runtime effect.
renderStmtMaybe ctx stmt =
  let out = codegenStmtWith ctx stmt
  in if T.null out then Nothing else Just out

-- | Prune tagged statements by reachability from root-tagged statements.
--
-- Any statement whose tag satisfies the supplied predicate is treated as a
-- root and always kept. Additional statements are kept only if reached
-- transitively through references.
pruneProgramTaggedStmts ::
     Map.Map Span (Identifier, [Ty Ann])
     -- ^ Function and argument types the typechecker resolved at each call span.
  -> (tag -> Bool)
     -- ^ Predicate selecting the tags whose statements are roots.
  -> [(tag, Stmt Ann)]
     -- ^ All statements, each tagged with its source file.
  -> [(tag, Stmt Ann)]
     -- ^ Roots and everything they reach, in original order.
pruneProgramTaggedStmts resolvMap isRoot taggedStmts =
  let indexed = zip [0 ..] taggedStmts
      idxMap = Map.fromList indexed
      rootIdx = Set.fromList [i | (i, (tag, _)) <- indexed, isRoot tag]
      exactDefs =
        foldl'
          (\m (i, (_, s)) -> foldl' (\m' (n, tys) -> Map.insertWith (++) (n, normalizeSig tys) [i] m') m (stmtExactDefs s))
          Map.empty
          indexed
      nameDefs =
        foldl'
          (\m (i, (_, s)) -> foldl' (\m' n -> Map.insertWith (++) n [i] m') m (stmtNameDefs s))
          Map.empty
          indexed
      initialRefs =
        concatMap
          (\i -> maybe [] (stmtRefs resolvMap . snd) (Map.lookup i idxMap))
          (Set.toList rootIdx)
      keptIdx = closeTaggedRefs resolvMap idxMap exactDefs nameDefs rootIdx Set.empty initialRefs
  in [item | (i, item) <- indexed, i `Set.member` keptIdx]

-- | Close the dependency set for tagged statements.
closeTaggedRefs ::
     Map.Map Span (Identifier, [Ty Ann])
     -- ^ Function and argument types the typechecker resolved at each call span.
  -> Map.Map Int (tag, Stmt Ann)
     -- ^ Every statement by index.
  -> Map.Map (Identifier, [Ty Ann]) [Int]
     -- ^ Indices of statements defining each exact overload.
  -> Map.Map Identifier [Int]
     -- ^ Indices of statements defining each name, ignoring overloads.
  -> Set.Set Int
     -- ^ Indices kept so far.
  -> Set.Set DefRef
     -- ^ References already followed.
  -> [DefRef]
     -- ^ Worklist of references still to follow.
  -> Set.Set Int
     -- ^ Indices of every reachable statement.
closeTaggedRefs _ _ _ _ kept _ [] = kept
closeTaggedRefs resolvMap idxMap exactDefs nameDefs kept seen (r:rs)
  | r `Set.member` seen = closeTaggedRefs resolvMap idxMap exactDefs nameDefs kept seen rs
  | otherwise =
      let nextSeen = Set.insert r seen
          candidateIdx =
            case r of
              RefExact name tys -> Map.findWithDefault [] (name, normalizeSig tys) exactDefs
              RefName name -> take 1 (Map.findWithDefault [] name nameDefs)
          (kept', newRefs) = foldl' collect (kept, []) candidateIdx
      in closeTaggedRefs resolvMap idxMap exactDefs nameDefs kept' nextSeen (rs ++ newRefs)
  where
    collect (k, accRefs) idx
      | idx `Set.member` k = (k, accRefs)
      | otherwise =
          case Map.lookup idx idxMap of
            Nothing -> (k, accRefs)
            Just (_, stmt) ->
              let refs = stmtRefs resolvMap stmt
              in (Set.insert idx k, accRefs ++ refs)

-- | Overloads a statement defines, as name and argument types.
stmtExactDefs :: Stmt Ann -- ^ Statement to inspect.
              -> [(Identifier, [Ty Ann])] -- ^ Function overloads it defines.
stmtExactDefs stmt =
  case stmt of
    Function name args _ _ _ -> [(name, map argType args)]
    PrimFunc name args _ _ -> [(name, map argType args)]
    _ -> []

-- | Names a statement introduces, including type and constructor names.
stmtNameDefs :: Stmt Ann -- ^ Statement to inspect.
             -> [Identifier] -- ^ Names it defines.
stmtNameDefs stmt =
  case stmt of
    Defn name _ _ -> [name]
    Function name _ _ _ _ -> [name]
    PrimFunc name _ _ _ -> [name]
    NewType name _ ctors -> name : [ctorName | ((ctorName, _), _) <- ctors]
    PrimType name _ -> [name]
    _ -> []

-- | Definitions a statement's body refers to.
stmtRefs :: Map.Map Span (Identifier, [Ty Ann]) -- ^ Typechecker resolutions by call span.
         -> Stmt Ann -- ^ Statement to inspect.
         -> [DefRef] -- ^ References found in its body.
stmtRefs resolvMap stmt =
  case stmt of
    Defn _ _ exp' -> expRefs resolvMap exp'
    Function _ _ _ clauses _ -> concatMap (\(Clause _ body) -> expRefs resolvMap body) clauses
    ExpStmt exp' -> expRefs resolvMap exp'
    _ -> []

-- | Definitions an expression refers to, preferring exact typechecker resolutions.
expRefs :: Map.Map Span (Identifier, [Ty Ann]) -- ^ Typechecker resolutions by call span.
        -> Exp Ann -- ^ Expression to walk.
        -> [DefRef] -- ^ References found within it.
expRefs resolvMap exp' =
  case exp' of
    Var {annExp, varName, varCandidates} ->
      case Map.lookup (annSpan annExp) resolvMap of
        Just (name, tys) -> [RefExact name tys]
        Nothing ->
          case varCandidates of
            ((name, _) : _) -> [RefName name]
            [] -> [RefName varName]
    StrLit {} -> []
    IntLit {} -> []
    FloatLit {} -> []
    CharLit {} -> []
    App {fn, args} -> expRefs resolvMap fn ++ concatMap (expRefs resolvMap) args
    Bind {bindExp} -> expRefs resolvMap bindExp
    Seq {first, second} -> expRefs resolvMap first ++ expRefs resolvMap second
    Match {scrutinee, clauses} ->
      expRefs resolvMap scrutinee ++ concatMap (\(Clause _ body) -> expRefs resolvMap body) clauses
    Let {body} -> expRefs resolvMap body
    Ascribe {ascExp} -> expRefs resolvMap ascExp

-- | Runtime symbol names to export from the runtime ESM module.
runtimeExportNames :: [Text]
runtimeExportNames =
  [ "__kip_close_stdin", "__kip_call", "__kip_float", "__kip_is_float", "__kip_num"
  , "__kip_prim_ters", "__kip_prim_birleşim", "__kip_prim_uzunluk", "__kip_prim_öğe"
  , "__kip_prim_alış", "__kip_prim_bırakış", "__kip_prim_son", "__kip_prim_bosluk", "__kip_prim_satirlar", "__kip_prim_kelimeler", "__kip_prim_dizge_büyük_hal", "__kip_prim_dizge_küçük_hal", "__kip_prim_toplam"
  , "__kip_prim_fark", "__kip_prim_dizge_eşitlik", "__kip_prim_karakter_eşitlik", "__kip_prim_karakter_dizge_hal"
  , "__kip_prim_karakter_harflik", "__kip_prim_karakter_rakamlık", "__kip_prim_karakter_harf_rakamlık"
  , "__kip_prim_karakter_buyuk_harflik", "__kip_prim_karakter_kucuk_harflik", "__kip_prim_karakter_boslukluk"
  , "__kip_prim_oku_stdin", "__kip_prim_oku_dosya", "__kip_prim_arguman_oku", "__kip_prim_cevreden_oku", "__kip_prim_yaz_dosya"
  , "boş_küme", "küme_ilave", "küme_çıkarma", "küme_içerik", "küme_boyut", "küme_birleşim", "küme_liste", "liste_küme"
  , "boş_sözlük", "sözlük_ek", "sözlük_çıkarılmış", "sözlük_karşılık", "sözlük_boyut", "sözlük_birleşim", "sözlük_liste"
  , "doğru", "yanlış", "varlık", "yokluk", "bitimlik", "yaz", "çarpım", "fark"
  , "bölüm", "kalan", "karekök", "radyan", "derece", "pi_sayısı", "taban", "tavan"
  , "tam_sayı_ondalık_sayı_hali", "sayı_çek", "eşitlik", "küçüklük", "küçük_eşitlik"
  , "büyüklük", "büyük_eşitlik", "dizge_hal", "tam_sayı_hal", "ondalık_sayı_hal"
  ]

-- | Emit the standalone runtime ESM module.
codegenRuntime :: Text
codegenRuntime =
  Prim.primitiveJsPrelude
    <> "\n"
    <> "export { " <> T.intercalate ", " runtimeExportNames <> " };\n"

-- | Codegen statements using a global program context and a local subset.
codegenStmtsInProgram :: Map.Map Span (Identifier, [Ty Ann]) -- ^ Typechecker resolutions by call span.
                      -> [Stmt Ann] -- ^ Statements of the whole program, used to build the context.
                      -> [Stmt Ann] -- ^ Subset of statements to emit.
                      -> Text -- ^ Emitted JavaScript for the subset.
codegenStmtsInProgram resolvMap programStmts stmts =
  let ctx = buildCodegenCtx resolvMap programStmts
      merged = mergeCompatibleFunctions ctx stmts
  in formatJsOutput (renderStmtBlock ctx merged)

-- | List JS definition names for a subset under full-program context.
--
-- Names are deduplicated via a set while folding, avoiding a second-pass
-- @nub@ and reducing allocation for large statement groups.
definedJsNamesInProgram :: Map.Map Span (Identifier, [Ty Ann]) -- ^ Typechecker resolutions by call span.
                        -> [Stmt Ann] -- ^ Statements of the whole program, used to build the context.
                        -> [Stmt Ann] -- ^ Subset of statements to inspect.
                        -> [Text] -- ^ JS names the subset defines, deduplicated and in order.
definedJsNamesInProgram resolvMap programStmts stmts =
  let ctx = buildCodegenCtx resolvMap programStmts
      merged = mergeCompatibleFunctions ctx stmts
      (revNames, _) = foldl' add ([], Set.empty) (concatMap (stmtDefinedNames ctx) merged)
  in reverse revNames
  where
    add (acc, seen) name
      | Set.member name seen = (acc, seen)
      | otherwise = (name : acc, Set.insert name seen)

-- | Check if a statement is a function definition (including types).
isFunctionDef :: Stmt Ann -- ^ Statement to classify.
              -> Bool -- ^ 'True' for declarations, which hoist; 'False' for expression statements.
isFunctionDef stmt =
  case stmt of
    Function {} -> True
    Defn {} -> True
    NewType {} -> True
    PrimFunc {} -> True
    PrimType {} -> True
    Load {} -> True
    ExpStmt {} -> False

-- | Identity under which two function statements may be merged into one
-- JavaScript declaration.
data OverloadKey = OverloadKey
  { odKeyName :: Text
    -- ^ Emitted JS name of the function.
  , odKeyArgs :: [Identifier]
    -- ^ Argument names, which must agree for the clauses to share a declaration.
  }
  deriving (Eq, Ord, Show)

-- | Merge function statements that have the same emitted JS name and exactly
-- the same argument identifiers.
--
-- This keeps codegen predictable while allowing multi-definition functions like
-- @filtre@ in dpll to become one JS declaration with all clauses.
--
-- Uses an index map + sequence updates instead of repeated list splitting,
-- reducing merge cost when many overload clauses are present.
mergeCompatibleFunctions :: CodegenCtx -- ^ Code generation context.
                         -> [Stmt Ann] -- ^ Statements that may contain separable clauses of one function.
                         -> [Stmt Ann] -- ^ Statements with such clauses combined, in original order.
mergeCompatibleFunctions ctx stmts = F.toList (snd (foldl' step (Map.empty, Seq.empty) stmts))
  where
    step (seen, acc) stmt =
      case stmt of
        Function name args _ clauses isInf ->
          let key =
                OverloadKey
                  { odKeyName = lookupOverloadName ctx name (map argType args)
                  , odKeyArgs = map argIdent args
                  }
          in case Map.lookup key seen of
               Nothing ->
                 let idx = Seq.length acc
                 in (Map.insert key idx seen, acc Seq.|> stmt)
               Just idx ->
                 (seen, mergeAt idx clauses isInf acc)
        _ ->
          (seen, acc Seq.|> stmt)

    mergeAt idx newClauses isInf acc =
      case Seq.lookup idx acc of
        Just (Function name oldArgs oldTy oldClauses oldInf) ->
          let mergedInf = oldInf || isInf
              mergedStmt = Function name oldArgs oldTy (oldClauses ++ newClauses) mergedInf
          in Seq.update idx mergedStmt acc
        _ -> acc

-- | JavaScript implementations of Kip primitives.
-- Uses @var@ so user code can override with @const@.
-- Note: Boolean constructors are defined by the library's doğruluk type,
-- so we use a helper to get the correct constructor format at runtime.
-- Primitives that may be overloaded in user/library code (ters, birleşim,
-- uzunluk, toplam, fark, oku, yaz) are available as explicit __kip_prim_*
-- helpers and selected at call sites via typechecker-resolved signatures.
-- The code is async-capable to support interactive browser I/O.

-- | Extract JS names defined by one top-level statement.
stmtDefinedNames :: CodegenCtx -- ^ Code generation context.
                 -> Stmt Ann -- ^ Statement to inspect.
                 -> [Text] -- ^ JS names it declares.
stmtDefinedNames ctx stmt =
  case stmt of
    Defn name _ _ ->
      [toJsIdent name]
    Function name args _ _ _ ->
      [lookupOverloadName ctx name (map argType args)]
    NewType name _ ctors ->
      let ctorNames = [toJsIdent ctorName | ((ctorName, _), _) <- ctors]
      in case ctors of
           [((ctorName, _), [])] | identText name /= identText ctorName ->
             ctorNames ++ [toJsIdent name]
           _ -> ctorNames
    _ ->
      []

-- | Emit JavaScript for one top-level statement.
codegenStmtWith :: CodegenCtx -- ^ Code generation context.
                -> Stmt Ann -- ^ Statement to emit.
                -> Text -- ^ Emitted JS, empty for statements with no runtime effect.
codegenStmtWith ctx stmt =
  case stmt of
    Defn name _ exp' ->
      "const " <> toJsIdent name <> " = " <> codegenExpWith ctx exp' <> ";"
    Function name args _ clauses _ ->
      let argTys = map argType args
          jsName = lookupOverloadName ctx name argTys
          fnCtx = withLocalScope (ctx { currentFunction = Just (name, normalizeSig argTys, jsName) }) (map argIdent args)
      in renderFunctionNamed fnCtx jsName args clauses
    PrimFunc {} ->
      ""
    Load dirPath name ->
      let prefix = if null dirPath then "" else T.intercalate "/" dirPath <> "/"
      in "// load " <> prefix <> identText name
    NewType name _ ctors ->
      renderNewType name ctors
    PrimType _ _ ->
      ""
    ExpStmt exp' ->
      codegenExpWith ctx exp' <> ";"

-- | Extend local scope with newly bound identifiers.
withLocalScope :: CodegenCtx -- ^ Context to extend.
               -> [Identifier] -- ^ Newly bound names, which shadow outer bindings.
               -> CodegenCtx -- ^ Context with those names in local scope.
withLocalScope ctx names =
  ctx { localScope = uniqIdents (names ++ localScope ctx) }

-- | Resolve a variable occurrence to a local binding when available.
lookupLocalVar :: CodegenCtx -- ^ Code generation context.
               -> Identifier -- ^ Name as written at the occurrence.
               -> [(Identifier, Case)] -- ^ Alternative readings of the name from the parser.
               -> Maybe Identifier -- ^ Matching local binding, when the name is locally bound.
lookupLocalVar ctx varName varCandidates =
  case
    [ scopeName
    | ident <- varName : map fst varCandidates
    , scopeName <- localScope ctx
    , identMatches scopeName ident
    ] of
    (scopeName : _) -> Just scopeName
    [] -> Nothing

-- | Emit JavaScript for one expression.
codegenExpWith :: CodegenCtx -- ^ Code generation context.
               -> Exp Ann -- ^ Expression to emit.
               -> Text -- ^ Emitted JS expression.
codegenExpWith ctx exp' =
  case exp' of
    Var {annExp, varName, varCandidates} ->
      case lookupLocalVar ctx varName varCandidates of
        Just localName -> toJsIdent localName
        Nothing ->
          case lookupValueName ctx annExp varName varCandidates of
            Just jsName -> jsName
            Nothing ->
              case varCandidates of
                ((ident, _):_) -> toJsIdent ident
                [] -> toJsIdent varName
    StrLit {lit} ->
      renderString lit
    IntLit {intVal} ->
      T.pack (show intVal)
    FloatLit {floatVal} ->
      "__kip_float(" <> T.pack (show floatVal) <> ")"
    CharLit {charVal} ->
      renderString (T.singleton charVal)
    App {fn, args} ->
      renderCall ctx fn args
    Bind {bindName, bindExp}
      | expMentions bindName bindExp ->
          let tmp = "__kip_shadow_" <> toJsIdent bindName
          in renderIife
               [ "const " <> tmp <> " = " <> codegenExpWith ctx bindExp <> ";"
               , "const " <> toJsIdent bindName <> " = " <> tmp <> ";"
               , "return " <> toJsIdent bindName <> ";"
               ]
      | otherwise ->
          renderIife
            [ "const " <> toJsIdent bindName <> " = " <> codegenExpWith ctx bindExp <> ";"
            , "return " <> toJsIdent bindName <> ";"
            ]
    Seq {first = Bind {bindName, bindExp}, second}
      | expMentions bindName bindExp ->
          -- When a Bind shadows a name used in its own initializer, pass the
          -- new value as an IIFE parameter so the initializer sees the outer
          -- binding and the body sees the shadowed one.
          let paramName = toJsIdent bindName
              bodyCtx = withLocalScope ctx [bindName]
          in "(await (async (" <> paramName <> ") => { return " <> codegenExpWith bodyCtx second <> "; })(" <> codegenExpWith ctx bindExp <> "))"
    Seq {first = first@Bind {bindName}, second} ->
      let bodyCtx = withLocalScope ctx [bindName]
      in renderIife
           (renderExpAsStmt ctx first ++ ["return " <> codegenExpWith bodyCtx second <> ";"])
    Seq {first, second} ->
      renderIife
        (renderExpAsStmt ctx first ++ ["return " <> codegenExpWith ctx second <> ";"])
    Match {scrutinee, clauses} ->
      renderMatch ctx scrutinee clauses
    Let {body} ->
      codegenExpWith ctx body
    Ascribe {ascExp} ->
      codegenExpWith ctx ascExp

-- | Resolve a variable used as a value (not call head).
--
-- Uses resolved signature info when available. Otherwise falls back to candidate
-- target discovery and picks a unique JS target if all candidates agree.
lookupValueName :: CodegenCtx -- ^ Code generation context.
                -> Ann -- ^ Annotation of the occurrence, supplying its span and case.
                -> Identifier -- ^ Name as written.
                -> [(Identifier, Case)] -- ^ Alternative readings of the name from the parser.
                -> Maybe Text -- ^ JS name when it resolves unambiguously.
lookupValueName ctx annExp varName varCandidates =
  case Map.lookup (annSpan annExp) (resolvedCallNames ctx) of
    Just resolvedName -> Just resolvedName
    Nothing | annCase annExp /= Ins ->
      Nothing
    Nothing | null varCandidates ->
      Nothing
    Nothing ->
      let candidates = uniqIdents (map fst varCandidates ++ [varName])
          targets = concatMap (callTargetsForIdent ctx) candidates
          jsNames = uniqTexts [jsName | (_, jsName) <- targets]
      in case jsNames of
           [jsName] -> Just jsName
           _ -> Nothing

-- | Deduplicate text values while preserving order.
uniqTexts :: [Text] -- ^ Values that may contain duplicates.
          -> [Text] -- ^ First occurrence of each value in original order.
uniqTexts = reverse . fst . foldl' add ([], Set.empty)
  where
    add (acc, seen) txt
      | Set.member txt seen = (acc, seen)
      | otherwise = (txt : acc, Set.insert txt seen)

-- | Render a Kip function using an explicit JS function name.
renderFunctionNamed :: CodegenCtx -- ^ Code generation context, with the function in local scope.
                    -> Text -- ^ JS name to declare.
                    -> [Arg Ann] -- ^ Declared arguments.
                    -> [Clause Ann] -- ^ Pattern-matching clauses, tried in order.
                    -> Text -- ^ An @async function@ declaration.
renderFunctionNamed ctx jsName args clauses =
  let argsText = renderArgNames args
      bodyLines =
        case clauses of
          [Clause (PWildcard _) body] ->
            ["return " <> codegenExpWith ctx body <> ";"]
          _ ->
            let arg0 = case args of
                         [] -> "__arg0"
                         (((argName, _), _) : _) -> toJsIdent argName
            in ("const __scrut = " <> arg0 <> ";")
               : renderClauseChain ctx "__scrut" clauses
  in
    T.unlines
      [ "async function " <> jsName <> "(" <> argsText <> ") {"
      , indent 2 (T.unlines bodyLines)
      , "}"
      ]

-- | Clause-chain indirection.
--
-- This alias keeps call sites stable if we later switch the lowering strategy.
renderClauseChain :: CodegenCtx -- ^ Code generation context.
                  -> Text -- ^ JS expression holding the value being matched.
                  -> [Clause Ann] -- ^ Clauses, tried in order.
                  -> [Text] -- ^ Statement lines implementing the dispatch.
renderClauseChain = renderClauseIfChain

-- | Render an if/else chain for ordered clause matching.
renderClauseIfChain :: CodegenCtx -- ^ Code generation context.
                    -> Text -- ^ JS expression holding the value being matched.
                    -> [Clause Ann] -- ^ Clauses, tried in order.
                    -> [Text] -- ^ An if/else chain ending in a no-match throw.
renderClauseIfChain ctx scrutinee =
  go True
  where
    go _ [] =
      ["throw new Error(\"No match\");"]
    go isFirst (Clause pat body : rest) =
      let (cond, binds) = renderPatMatchCond ctx scrutinee pat
          bodyCtx = withLocalScope ctx (patBoundNames pat)
          bodyLines = binds ++ ["return " <> codegenExpWith bodyCtx body <> ";"]
          header =
            if cond == ""
              then if isFirst then "{" else "else {"
              else (if isFirst then "if (" else "else if (") <> cond <> ") {"
          block =
            [ header
            , indent 2 (T.unlines bodyLines)
            , "}"
            ]
      in if cond == ""
           then block
           else block ++ go False rest

-- | Collect variable names bound by a pattern.
patBoundNames :: Pat ann -- ^ Pattern to inspect.
              -> [Identifier] -- ^ Names the pattern binds, including nested ones.
patBoundNames pat =
  case pat of
    PWildcard _ -> []
    PVar ident _ -> [ident]
    PCtor _ subPats -> concatMap patBoundNames subPats
    PIntLit _ _ -> []
    PFloatLit _ _ -> []
    PStrLit _ _ -> []
    PCharLit _ _ -> []
    PListLit pats -> concatMap patBoundNames pats

-- | Render variable bindings implied by a pattern.
--
-- Returns:
--
-- * emitted binding statements
-- * next argument index
--
-- Bindings are right-aligned with constructor arguments to match Kip's pattern
-- semantics for nested constructor/list patterns.
renderPatternBindings :: Text -- ^ JS expression holding the constructor being destructured.
                      -> [Pat Ann] -- ^ Argument patterns.
                      -> Int -- ^ Argument index to start from.
                      -> ([Text], Int) -- ^ Binding statements and the next unused index.
renderPatternBindings scrutinee pats startIdx =
  -- Note: constructor arguments in Kip patterns are matched from the right.
  -- We keep patLen around so we can index from the end of scrutinee.args,
  -- which mirrors how nested patterns are aligned in the AST/typechecker.
  let patLen = length pats
      (bindsRev, idx, _) = foldl' (collect patLen) ([], startIdx, Set.empty) pats
  in (reverse bindsRev, idx)
  where
    collect patLen (acc, idx, seen) pat =
      let (binds, nextIdx, seen') = renderPatBinding scrutinee patLen idx seen pat
      in (reverse binds ++ acc, nextIdx, seen')

    -- Bind variables by walking the pattern while keeping alignment consistent
    -- with right-anchored constructor arguments.
    renderPatBinding scrut patLen idx seen pat =
      case pat of
        PWildcard _ -> ([], idx + 1, seen)
        PVar n _ ->
          let name = toJsIdent n
              argAccess = patArgAccess scrut patLen idx
          in if Set.member name seen
               then ([], idx + 1, seen)
               else ([ "const " <> name <> " = " <> argAccess <> ";" ], idx + 1, Set.insert name seen)
        PCtor _ subPats ->
          let argAccess = patArgAccess scrut patLen idx
              (subBinds, _, seen') = renderPatternBindingsWithSeen argAccess subPats 0 seen
          in (subBinds, idx + 1, seen')
        PIntLit _ _ -> ([], idx + 1, seen)
        PFloatLit _ _ -> ([], idx + 1, seen)
        PStrLit _ _ -> ([], idx + 1, seen)
        PCharLit _ _ -> ([], idx + 1, seen)
        PListLit _ -> ([], idx + 1, seen)

    renderPatternBindingsWithSeen scrut pats idx seen =
      -- Each nested constructor has its own argument list length, so we
      -- recompute patLen for the subpattern list.
      let patLen = length pats
          (bindsRev, nextIdx, seen') = foldl' (collectWithSeen patLen) ([], idx, seen) pats
      in (reverse bindsRev, nextIdx, seen')
      where
        collectWithSeen patLen (acc, ix, seenAcc) p =
          let (binds, nextIx, seen') = renderPatBinding scrut patLen ix seenAcc p
          in (reverse binds ++ acc, nextIx, seen')

-- | Lower a Kip @Match@ expression into an async IIFE expression.
--
-- The scrutinee is evaluated once and captured in @__scrut@ to avoid repeated
-- evaluation and preserve side-effect ordering.
renderMatch :: CodegenCtx -- ^ Code generation context.
            -> Exp Ann -- ^ Scrutinee, evaluated once.
            -> [Clause Ann] -- ^ Clauses, tried in order.
            -> Text -- ^ An async IIFE yielding the matching clause's value.
renderMatch ctx scrutinee clauses =
  renderIife $
    ("const __scrut = " <> codegenExpWith ctx scrutinee <> ";")
      : renderMatchClauses ctx "__scrut" clauses

-- | Render match clauses with the same ordered semantics as function clauses.
renderMatchClauses :: CodegenCtx -- ^ Code generation context.
                   -> Text -- ^ JS expression holding the value being matched.
                   -> [Clause Ann] -- ^ Clauses, tried in order.
                   -> [Text] -- ^ Statement lines implementing the dispatch.
renderMatchClauses = renderClauseIfChain

-- | Render both the boolean guard and binding statements for a pattern.
--
-- The guard determines whether the branch matches; bindings are emitted only
-- for variables appearing in that branch.
renderPatMatchCond :: CodegenCtx -- ^ Code generation context.
                   -> Text -- ^ JS expression holding the value being matched.
                   -> Pat Ann -- ^ Pattern to test.
                   -> (Text, [Text]) -- ^ Guard expression (empty when the pattern always
                   -- matches) and the binding statements for its variables.
renderPatMatchCond ctx scrutinee pat =
  case pat of
    PWildcard _ -> ("", [])
    PVar n _ -> ("", ["const " <> toJsIdent n <> " = " <> scrutinee <> ";"])
    PCtor _ pats ->
      let cond = renderPatCond ctx scrutinee pat
          (binds, _) = renderPatternBindings scrutinee pats 0
      in (cond, binds)
    PIntLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PFloatLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PStrLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PCharLit _ _ -> (renderPatCond ctx scrutinee pat, [])
    PListLit _ -> (renderPatCond ctx scrutinee pat, [])

-- | Render a JavaScript boolean condition for a pattern.
--
-- Constructors are matched by tag and minimum arity; literal patterns are
-- matched by JS equality against the lowered scrutinee.
renderPatCond :: CodegenCtx -- ^ Code generation context.
              -> Text -- ^ JS expression holding the value being matched.
              -> Pat Ann -- ^ Pattern to test.
              -> Text -- ^ JS boolean expression, @"true"@ for irrefutable patterns.
renderPatCond ctx scrutinee pat =
  case pat of
    PWildcard _ -> "true"
    PVar _ _ -> "true"
    PCtor (ctor, _) pats ->
      -- When matching constructor patterns we need to:
      -- 1) check the tag, 2) ensure args are long enough, and
      -- 3) evaluate subpattern guards using right-aligned indexing.
      let patLen = length pats
          headCond = renderCtorTagCond scrutinee ctor
          lenCond =
            if patLen > 0
              then scrutinee <> ".args.length >= " <> T.pack (show patLen)
              else "true"
          argConds =
            [ cond
            | (p, idx) <- zip pats [0 ..]
            , let cond = renderPatCond ctx (patArgAccess scrutinee patLen idx) p
            , cond /= "true"
            ]
      in T.intercalate " && " (headCond : lenCond : argConds)
    PIntLit n _ ->
      scrutinee <> " === " <> T.pack (show n)
    PFloatLit n _ ->
      "__kip_num(" <> scrutinee <> ") === " <> T.pack (show n)
    PStrLit s _ ->
      scrutinee <> " === " <> renderString s
    PCharLit c _ ->
      scrutinee <> " === " <> renderString (T.singleton c)
    PListLit pats ->
      renderListPatCond ctx scrutinee pats

-- | Render a boolean condition for a list pattern.
--
-- Kip lists are represented as nested @eki(head, tail)@ constructors ending in
-- @boş@, so this function recursively emits constructor checks for that shape.
renderListPatCond :: CodegenCtx -- ^ Code generation context.
                  -> Text -- ^ JS expression holding the list being matched.
                  -> [Pat Ann] -- ^ Element patterns, matched positionally.
                  -> Text -- ^ JS boolean expression testing shape and elements.
renderListPatCond _ scrutinee [] =
  -- Empty list pattern matches 'boş'
  scrutinee <> ".tag === \"boş\""
renderListPatCond ctx scrutinee (p:ps) =
  -- Non-empty list pattern matches 'eki' with head and tail
  let headCond = scrutinee <> ".tag === \"eki\""
      lenCond = scrutinee <> ".args.length >= 2"
      headMatch = renderPatCond ctx (scrutinee <> ".args[0]") p
      tailMatch = renderListPatCond ctx (scrutinee <> ".args[1]") ps
      conds = filter (/= "true") [headCond, lenCond, headMatch, tailMatch]
  in if null conds
       then "true"
       else T.intercalate " && " conds

-- | Access a constructor argument using right-aligned pattern indexing.
--
-- If @patLen == 2@ and @idx == 0@, this points to the first element of the
-- last two constructor arguments.
patArgAccess :: Text -- ^ JS expression holding the constructor value.
             -> Int -- ^ Number of argument patterns, used for right alignment.
             -> Int -- ^ Zero-based position of the pattern within them.
             -> Text -- ^ JS expression selecting the matching constructor argument.
patArgAccess scrutinee patLen idx =
  let idxText = T.pack (show idx)
      lenText = T.pack (show patLen)
      -- With right alignment, idx=0 means the *first* pattern argument
      -- matches the *leftmost* of the last patLen elements.
  in if patLen <= 0
       then scrutinee <> ".args[" <> idxText <> "]"
       else scrutinee <> ".args[(" <> scrutinee <> ".args.length - " <> lenText <> " + " <> idxText <> ")]"

-- | Normalize constructor names for runtime tag comparison.
--
-- Some constructor surfaces differ only by Turkish soft-g possessive alternation
-- (for example @varlığı@ vs @varlık@). We accept both spellings to keep
-- pattern matching compatible with primitive option helpers.
renderCtorTagCond :: Text -- ^ JS expression holding the value being matched.
                  -> Identifier -- ^ Constructor name from the pattern.
                  -> Text -- ^ JS boolean expression accepting either spelling of the tag.
renderCtorTagCond scrutinee (mods, name) =
  let exact = toJsIdent (mods, name)
      softened = toJsIdent (mods, stripSoftGPossessive name)
      exactCond = scrutinee <> ".tag === " <> renderString exact
      softCond = scrutinee <> ".tag === " <> renderString softened
  in if exact == softened
       then exactCond
       else "(" <> exactCond <> " || " <> softCond <> ")"

-- | Strip Turkish possessive suffix only when preceded by soft-g.
--
-- Converts final @ğı/ği/ğu/ğü@ back to @k@.
stripSoftGPossessive :: Text -- ^ Constructor root as written.
                     -> Text -- ^ Root with a soft-g possessive undone, otherwise unchanged.
stripSoftGPossessive txt =
  case T.unsnoc txt of
    Just (pref, c)
      | c `elem` ("ıiuü" :: String) ->
          case T.unsnoc pref of
            Just (pref', 'ğ') -> pref' <> "k"
            _ -> txt
    _ -> txt

-- | Emit JavaScript for a Kip ADT declaration.
--
-- Constructors are emitted as tagged value/factory bindings. For single nullary
-- constructors we additionally alias the type name to the constructor.
renderNewType :: Identifier -- ^ Type name.
              -> [Ctor Ann] -- ^ Constructors with their argument types.
              -> Text -- ^ JS bindings for each constructor, plus a type alias for unit types.
renderNewType name ctors =
  let ctorLines =
        [ renderCtor ctorName args
        | ((ctorName, _), args) <- ctors
        ]
      ctorSig =
        T.intercalate " | "
          [ identText ctorName <> "(" <> T.replicate (length args) "_" <> ")"
          | ((ctorName, _), args) <- ctors
          ]
      -- For single-constructor types with no args, also alias the type name
      -- to the constructor (e.g., bitim = bitimlik for unit types)
      typeAlias = case ctors of
        [((ctorName, _), [])] | identText name /= identText ctorName ->
          ["var " <> toJsIdent name <> " = " <> toJsIdent ctorName <> ";"]
        _ -> []
  in
    T.unlines $
      ("/* type " <> identText name <> " = " <> ctorSig <> " */")
        : ctorLines ++ typeAlias

-- | Render a single constructor.
-- Zero-argument constructors are defined as objects (values).
-- Constructors with arguments are defined as functions.
-- Uses toJsIdent for both JS variable name and tag to ensure consistency.
renderCtor :: Identifier -- ^ Constructor name.
           -> [a] -- ^ Argument list, used only for its length.
           -> Text -- ^ A tagged object for nullary constructors, a factory function otherwise.
renderCtor ctorName args =
  let jsName = toJsIdent ctorName
  in case args of
    [] -> "var " <> jsName <> " = { tag: "
            <> renderString jsName <> ", args: [] };"
    _ -> "var " <> jsName <> " = (...args) => ({ tag: "
            <> renderString jsName <> ", args });"

-- | Lower function application.
--
-- Most calls become @(await f(...))@. Section-like partial forms are handled
-- by 'partialSectionCall' to preserve Kip semantics.
renderCall :: CodegenCtx -- ^ Code generation context.
           -> Exp Ann -- ^ Head expression of the application.
           -> [Exp Ann] -- ^ Argument expressions.
           -> Text -- ^ An awaited call, or a section when the application is partial.
renderCall ctx fn args =
  let fnText =
        case fn of
          Var {annExp} ->
            let span' = annSpan annExp
            in lookupCallName ctx span' fn args
          _ -> "(" <> codegenExpWith ctx fn <> ")"
      argsCsv = T.intercalate ", " (map (codegenExpWith ctx) args)
  in case partialSectionCall ctx fn fnText args of
       Just sectionFn -> sectionFn
       Nothing -> "(await " <> fnText <> "(" <> argsCsv <> "))"

-- | Render a single-argument partial application that should become a section.
--
-- Section lowering is enabled only for function names proven section-capable in
-- 'buildCodegenCtx' (has an arity > 1 overload and no unary overload).
partialSectionCall :: CodegenCtx -- ^ Code generation context.
                   -> Exp Ann -- ^ Head expression of the application.
                   -> Text -- ^ JS name the head resolves to.
                   -> [Exp Ann] -- ^ Argument expressions.
                   -> Maybe Text -- ^ A section closure when the call is a section, 'Nothing' otherwise.
partialSectionCall ctx fn fnText args =
  case (fn, args) of
    (Var {annExp = annFn, varCandidates}, [arg])
      | annCase annFn == Ins && isSectionableCall ctx varCandidates ->
          Just (renderCaseDrivenSection ctx fnText arg)
    _ -> Nothing

-- | Render a generic case-driven section for binary calls.
--
-- Instrumental fixed args are treated as left sections; all other fixed-case
-- args are treated as right sections.
renderCaseDrivenSection :: CodegenCtx -- ^ Code generation context.
                        -> Text -- ^ JS name of the binary function.
                        -> Exp Ann -- ^ The supplied argument, whose case decides the side it fills.
                        -> Text -- ^ A closure taking the remaining argument.
renderCaseDrivenSection ctx fnText arg =
  if annCase (annExp arg) == Ins
    then "(async (__kip_arg0) => (await __kip_call(" <> fnText <> ", [" <> codegenExpWith ctx arg <> ", __kip_arg0])))"
    else "(async (__kip_arg0) => (await __kip_call(" <> fnText <> ", [__kip_arg0, " <> codegenExpWith ctx arg <> "])))"

-- | Check whether any candidate reading of a name may form a section.
isSectionableCall :: CodegenCtx -- ^ Code generation context.
                  -> [(Identifier, Case)] -- ^ Candidate readings of the called name.
                  -> Bool -- ^ 'True' when at least one candidate is section-capable.
isSectionableCall ctx =
  any (\(ident, _) -> ident `Set.member` sectionableFns ctx)

-- | Check whether an identifier is mentioned anywhere in an expression.
--
-- This is used to detect self-referencing bindings that would hit JavaScript's
-- temporal dead zone (TDZ) when emitted as @const x = ...x...@.
expMentions :: Identifier -- ^ Name to look for.
            -> Exp Ann -- ^ Expression to walk.
            -> Bool -- ^ 'True' when the name is referenced within it.
expMentions name expr =
  let jsName = toJsIdent name
  in expMentionsJs jsName expr

-- | Check whether a JS identifier name appears in a compiled expression.
expMentionsJs :: Text -- ^ Emitted JS name to look for.
              -> Exp Ann -- ^ Expression to walk.
              -> Bool -- ^ 'True' when some variable in it would be emitted as that name.
expMentionsJs jsName expr =
  case expr of
    Var {varName, varCandidates} ->
      let emitted = case varCandidates of
                      ((ident, _):_) -> toJsIdent ident
                      [] -> toJsIdent varName
      in emitted == jsName
    App {fn, args} -> expMentionsJs jsName fn || any (expMentionsJs jsName) args
    Bind {bindExp} -> expMentionsJs jsName bindExp
    Seq {first, second} -> expMentionsJs jsName first || expMentionsJs jsName second
    Match {scrutinee, clauses} ->
      expMentionsJs jsName scrutinee || any (\(Clause _ body) -> expMentionsJs jsName body) clauses
    Let {body} -> expMentionsJs jsName body
    Ascribe {ascExp} -> expMentionsJs jsName ascExp
    StrLit {} -> False
    IntLit {} -> False
    FloatLit {} -> False
    CharLit {} -> False

-- | Render an expression in statement position.
--
-- Bindings become declarations; all other expressions become single statements.
-- When a binding's name appears in its own initializer, a temporary variable is
-- used to avoid JavaScript's temporal dead zone.
renderExpAsStmt :: CodegenCtx -- ^ Code generation context.
                -> Exp Ann -- ^ Expression in statement position.
                -> [Text] -- ^ Statement lines, a declaration for a binding.
renderExpAsStmt ctx exp' =
  case exp' of
    Bind {bindName, bindExp} ->
      ["const " <> toJsIdent bindName <> " = " <> codegenExpWith ctx bindExp <> ";"]
    _ ->
      [codegenExpWith ctx exp' <> ";"]

-- | Wrap a list of statements in an async IIFE expression.
renderIife :: [Text] -- ^ Statement lines forming the body.
           -> Text -- ^ An awaited async immediately-invoked function expression.
renderIife lines' =
  T.intercalate "\n"
    [ "(await (async () => {"
    , indent 2 (T.unlines lines')
    , "})())"
    ]

-- | Normalize JS layout: avoid standalone semicolon lines and excess blank lines.
formatJsOutput :: Text -- ^ Generated JavaScript as emitted.
               -> Text -- ^ Same code with layout normalized.
formatJsOutput src =
  let ls0 = T.lines src
      ls1 = moveStandaloneSemicolons ls0
      ls2 = removeBlankBeforeClose ls1
      ls3 = removeBlankAfterOpen ls2
      ls4 = collapseBlankRuns ls3
      ls5 = trimEdgeBlanks ls4
  in T.unlines ls5

-- | Attach lines containing only @;@ to the previous non-empty line.
--
-- Uses a single strict `Seq` fold to avoid repeated list concatenation and
-- end-of-list rewrites during formatting normalization.
moveStandaloneSemicolons :: [Text] -- ^ Lines to normalize.
                         -> [Text] -- ^ Lines with lone semicolons folded into the preceding line.
moveStandaloneSemicolons = F.toList . foldl' step Seq.empty
  where
    step acc line
      | Seq.null acc
      , T.strip line == ";" = Seq.empty
      | Seq.null acc
      = Seq.singleton line
      | T.strip line /= ";" = acc Seq.|> line
      | otherwise =
          case Seq.viewr acc of
            Seq.EmptyR -> acc
            prefix Seq.:> lastLine
              | T.null (T.strip lastLine) -> acc
              | otherwise -> prefix Seq.|> (T.stripEnd lastLine <> ";")

-- | Remove blank lines that are immediately followed by a closing brace line.
removeBlankBeforeClose :: [Text] -- ^ Lines to normalize.
                       -> [Text] -- ^ Lines without blanks directly before a closing brace.
removeBlankBeforeClose [] = []
removeBlankBeforeClose [x] = [x]
removeBlankBeforeClose (x:y:rest)
  | T.null (T.strip x)
  , isCloseLine y = removeBlankBeforeClose (y : rest)
  | otherwise = x : removeBlankBeforeClose (y : rest)

-- | Remove blank lines that are immediately after a line ending with @{@.
removeBlankAfterOpen :: [Text] -- ^ Lines to normalize.
                     -> [Text] -- ^ Lines without blanks directly after an opening brace.
removeBlankAfterOpen [] = []
removeBlankAfterOpen [x] = [x]
removeBlankAfterOpen (x:y:rest)
  | isOpenLine x
  , T.null (T.strip y) = removeBlankAfterOpen (x : rest)
  | otherwise = x : removeBlankAfterOpen (y : rest)

-- | Collapse consecutive blank lines to a single blank line.
--
-- Tracks prior-blank state in one pass and emits directly to a sequence,
-- avoiding multi-pass blank-run cleanup.
collapseBlankRuns :: [Text] -- ^ Lines to normalize.
                  -> [Text] -- ^ Lines with each run of blanks reduced to one.
collapseBlankRuns =
  F.toList . fst . foldl' step (Seq.empty, False)
  where
    step (acc, prevBlank) line =
      let blank = T.null (T.strip line)
      in if blank && prevBlank
           then (acc, True)
           else (acc Seq.|> line, blank)

-- | Trim leading and trailing blank lines.
trimEdgeBlanks :: [Text] -- ^ Lines to normalize.
               -> [Text] -- ^ Lines without leading or trailing blanks.
trimEdgeBlanks =
  reverse . dropWhile (T.null . T.strip) . reverse . dropWhile (T.null . T.strip)

-- | True when the line starts with a closing brace token.
isCloseLine :: Text -- ^ Line to classify.
            -> Bool -- ^ 'True' when it starts with a closing brace.
isCloseLine line = "}" `T.isPrefixOf` T.strip line

-- | True when the line ends with an opening brace token.
isOpenLine :: Text -- ^ Line to classify.
           -> Bool -- ^ 'True' when it ends with an opening brace.
isOpenLine line = "{" `T.isSuffixOf` T.stripEnd line

-- | Render comma-separated JavaScript argument names.
renderArgNames :: [Arg Ann] -- ^ Declared arguments.
               -> Text -- ^ Their JS names, comma separated, for a parameter list.
renderArgNames args =
  T.intercalate ", " (map (toJsIdent . argIdent) args)

-- | Extract the terminal segment of an identifier.
identText :: Identifier -- ^ Possibly qualified identifier.
          -> Text -- ^ Its final segment, without the module qualifier.
identText (_, name) = name

-- | Convert a Kip identifier into a JavaScript-safe identifier.
--
-- Namespace pieces are joined with underscores; dashes are rewritten to
-- underscores; apostrophes are removed; reserved words are prefixed.
toJsIdent :: Identifier -- ^ Kip identifier, possibly qualified.
          -> Text -- ^ A valid JS identifier that is not a reserved word.
toJsIdent ident =
  let raw = baseIdent ident
      sanitized = T.map replaceDash raw
      cleaned = T.filter (/= '\'') sanitized
      prefixed =
        case T.uncons cleaned of
          Nothing -> "_"
          Just (c, _) ->
            if isIdentStart c then cleaned else "_" <> cleaned
      safe =
        if prefixed `elem` jsReserved
          then "_" <> prefixed
          else prefixed
  in safe
  where
    baseIdent (ns, name) =
      let cleanName = T.filter (/= ' ') name
      in case ns of
           [] -> cleanName
           _  -> T.intercalate "_" (map (T.filter (/= ' ')) ns ++ [cleanName])
    replaceDash c = if c == '-' then '_' else c
    isIdentStart c = isLetter c || c == '_' || c == '$'

-- | JavaScript reserved words blocked from raw identifier emission.
jsReserved :: [Text]
jsReserved =
  [ "break", "case", "catch", "class", "const", "continue", "debugger"
  , "default", "delete", "do", "else", "export", "extends", "false"
  , "finally", "for", "function", "if", "import", "in", "instanceof"
  , "new", "null", "return", "super", "switch", "this", "throw"
  , "true", "try", "typeof", "var", "void", "while", "with", "yield"
  , "let", "enum", "await", "implements", "interface", "package"
  , "private", "protected", "public", "static", "undefined"
  ]

-- | Render a JavaScript string literal with escaping.
renderString :: Text -- ^ String contents.
             -> Text -- ^ A double-quoted JS string literal with escapes applied.
renderString txt =
  "\"" <> T.concatMap escapeChar txt <> "\""

-- | Escape one character in JS string context.
escapeChar :: Char -- ^ Character appearing inside a string literal.
           -> Text -- ^ Its escape sequence, or the character itself when safe.
escapeChar c =
  case c of
    '\\' -> "\\\\"
    '"' -> "\\\""
    '\n' -> "\\n"
    '\r' -> "\\r"
    '\t' -> "\\t"
    _ -> T.singleton c

-- | Indent each non-empty line by @n@ spaces.
indent :: Int -- ^ Number of spaces to prepend.
       -> Text -- ^ Text whose lines are indented.
       -> Text -- ^ Indented lines, with blank lines dropped.
indent n =
  T.unlines . map (T.replicate n " " <>) . filter (not . T.null) . T.lines
