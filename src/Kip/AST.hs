{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Core abstract syntax tree definitions for Kip.
module Kip.AST where

import GHC.Generics (Generic)
import Data.List
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Megaparsec.Pos (SourcePos(..), Pos, unPos, mkPos)
import Data.Binary (Binary(..), Get)
import Data.Word (Word8, Word32, Word64)
import Data.Binary.Get (getWord32be, getWord64be)
import Data.Binary.Put (putWord32be, putWord64be)

-- | Binary instance for parser positions.
--
-- ==== Performance note (Optimization 11)
-- Encodes 'Pos' as a fixed-width 'Word32' to reduce decode overhead and
-- allocation pressure compared to generic integer decoding.
instance Binary Pos where
  put p = putWord32be (fromIntegral (unPos p) :: Word32)
  get = do
    w <- getWord32be
    let n = fromIntegral w
    if n <= 0
      then fail "Invalid Pos (must be >= 1)"
      else return (mkPos n)

-- | Fully qualified identifier with namespace parts.
type Identifier = ([Text], Text)

-- | Grammatical cases used by the surface syntax.
data Case =
    Nom -- ^ nominative case (yalın hal)
  | Acc -- ^ accusative case (-i hali)
  | Dat -- ^ dative case (-e hali)
  | Loc -- ^ locative case (-de hali)
  | Abl -- ^ ablative case (-den hali)
  | Gen -- ^ genitive case (-in eki)
  | Ins -- ^ instrumental case (ile, -le)
  | Cond -- ^ conditional case (-se, şart kipi)
  | P3s -- ^ 3rd person possessive (-sI, tamlanan eki)
  deriving (Show, Eq, Ord, Generic, Binary)

-- | Binary instance for source positions.
--
-- ==== Performance note (Optimization 11)
-- Packs line and column into a single 'Word64' payload for faster cache
-- deserialization of span-heavy ASTs.
instance Binary SourcePos where
  put (SourcePos name line col) = do
    put name
    let packedLineCol =
          (fromIntegral (unPos line) `shiftL` 32)
            .|. fromIntegral (unPos col) :: Word64
    putWord64be packedLineCol
  get = do
    name <- get
    packedLineCol <- getWord64be
    let lineW = fromIntegral (packedLineCol `shiftR` 32) :: Int
        colW = fromIntegral (packedLineCol .&. 0xffffffff) :: Int
    if lineW <= 0 || colW <= 0
      then fail "Invalid SourcePos coordinates (must be >= 1)"
      else return (SourcePos name (mkPos lineW) (mkPos colW))

-- | Source span for diagnostics.
data Span =
    Span { spanStart :: SourcePos, spanEnd :: SourcePos, spanPath :: Maybe FilePath } -- ^ Inclusive start and end.
  | NoSpan -- ^ Missing or unknown span.
  deriving (Show, Eq, Ord, Generic, Binary)

-- | Combined annotation of case and span.
type Ann = (Case, Span)

-- | Construct an annotation from case and span.
mkAnn :: Case -- ^ Case to attach.
      -> Span -- ^ Span to attach.
      -> Ann -- ^ Combined annotation.
mkAnn = (,)

-- | Extract the case from an annotation.
annCase :: Ann -- ^ Annotation to inspect.
        -> Case -- ^ Extracted case.
annCase = fst

-- | Extract the span from an annotation.
annSpan :: Ann -- ^ Annotation to inspect.
        -> Span -- ^ Extracted span.
annSpan = snd

-- | Replace the case while keeping the span.
setAnnCase :: Ann -- ^ Annotation to update.
           -> Case -- ^ New case.
           -> Ann -- ^ Updated annotation.
setAnnCase ann cas = (cas, annSpan ann)

-- | Merge two spans to cover both.
mergeSpan :: Span -- ^ First span.
          -> Span -- ^ Second span.
          -> Span -- ^ Merged span.
mergeSpan a b =
  case (a, b) of
    (Span s1 _ f1, Span _ e2 f2) | f1 == f2 -> Span s1 e2 f1
    (Span s1 e1 f1, NoSpan) -> Span s1 e1 f1
    (NoSpan, Span s1 e1 f1) -> Span s1 e1 f1
    (NoSpan, NoSpan) -> NoSpan

-- | Type syntax tree with an annotation payload.
data Ty a =
    TyString { annTy :: a } -- ^ String type.
  | TyInt    { annTy :: a } -- ^ Integer type.
  | TyFloat  { annTy :: a } -- ^ Floating-point type.
  | TyChar   { annTy :: a } -- ^ Character type.
  | Arr      { annTy :: a , dom :: Ty a, img :: Ty a } -- ^ Function type.
  | TyInd    { annTy :: a , indName :: Identifier } -- ^ Named type.
  | TyVar    { annTy :: a , tyVarName :: Identifier } -- ^ Type variable.
  | TySkolem { annTy :: a , tySkolemName :: Identifier } -- ^ Rigid (skolem) type variable.
  | TyApp    { annTy :: a , tyCtor :: Ty a , tyArgs :: [Ty a] } -- ^ Type application.
  deriving (Show, Eq, Ord, Generic, Functor, Binary)

-- | Expression syntax tree with an annotation payload.
data Exp a =
    Var    { annExp :: a , varName :: Identifier , varCandidates :: [(Identifier, Case)] } -- ^ Variable reference.
  | App    { annExp :: a , fn :: Exp a , args :: [Exp a] } -- ^ Function application.
  | SetLit { annExp :: a , setEntries :: Map.Map Text (Exp a) } -- ^ Internal runtime set representation.
  | MapLit { annExp :: a , mapEntries :: Map.Map Text (Exp a, Exp a) } -- ^ Internal runtime map/dictionary representation.
  | StrLit { annExp :: a , lit :: !Text } -- ^ String literal.
  | IntLit { annExp :: a , intVal :: !Integer } -- ^ Integer literal.
  | FloatLit { annExp :: a , floatVal :: !Double } -- ^ Floating-point literal.
  | CharLit { annExp :: a , charVal :: !Char } -- ^ Character literal.
  | Bind   { annExp :: a , bindName :: Identifier , bindNameAnn :: a , bindExp :: Exp a } -- ^ Binding expression.
  | Seq    { annExp :: a , first :: Exp a , second :: Exp a } -- ^ Sequential composition.
  | Match  { annExp :: a , scrutinee :: Exp a , clauses :: [Clause a] } -- ^ Pattern match.
  | Let    { annExp :: a , varName :: Identifier , body :: Exp a } -- ^ Let binding.
  | Ascribe { annExp :: a , ascType :: Ty a , ascExp :: Exp a } -- ^ Type ascription.
  deriving (Show, Eq, Generic, Functor, Binary)

-- | Flatten nested applications into a root function and accumulated arguments.
--
-- ==== Performance note
-- Uses a difference list (@[Exp a] -> [Exp a]@) to collect arguments in
-- a single pass.  The previous version built the result via
-- @prefixArgs ++ appArgs@ at each recursive step, which is O(n^2) for
-- deeply nested applications because @(++)@ traverses its left operand.
-- The difference list composes in O(1) and materializes in O(n).
flattenApplied :: Exp a -- ^ Expression to inspect.
              -> (Exp a, [Exp a]) -- ^ Root function and arguments in call order.
flattenApplied = go id
  where
    go dl e = case e of
      App {fn = appFn, args = appArgs} -> go ((appArgs ++) . dl) appFn
      _ -> (e, dl [])

-- | Reorder values to expected cases, allowing nominative values to fill
-- unmatched expected cases.
-- This is useful for partially applied calls where one side remains nominative.
reorderByCasesNomFallback :: [Case] -- ^ Expected cases.
                         -> [Case] -- ^ Actual cases.
                         -> [a] -- ^ Values to reorder.
                         -> Maybe [a] -- ^ Reordered values when possible.
reorderByCasesNomFallback expected actual xs
  | length expected /= length actual || length actual /= length xs = Nothing
  | otherwise = map snd <$> go (zip actual xs) expected
  where
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
              (_, []) -> Nothing

-- | Typed argument of a function.
-- The annotation on the identifier captures the span of the parameter name for LSP.
type Arg ann = ((Identifier, ann), Ty ann)

-- | Extract the identifier from an argument.
argIdent :: Arg ann -> Identifier
argIdent ((ident, _), _) = ident

-- | Extract the identifier annotation (span) from an argument.
argIdentAnn :: Arg ann -> ann
argIdentAnn ((_, ann), _) = ann

-- | Extract the type from an argument.
argType :: Arg ann -> Ty ann
argType (_, ty) = ty
{- | Constructor with its argument types and case annotation.

The constructor identifier includes a case annotation to track whether
the constructor is in nominative case (e.g., @doğru@) or possessive case
(e.g., @eki@ which is P3s).

Components:
1. @(Identifier, ann)@: Constructor name with its grammatical case annotation
2. @[Ty ann]@: Argument types for the constructor
-}
type Ctor ann = ((Identifier, ann), [Ty ann])

-- | Pattern syntax tree.
data Pat ann =
    PWildcard ann -- ^ Wildcard pattern with annotation.
  | PVar Identifier ann -- ^ Variable pattern (binds a name).
  | PCtor (Identifier, ann) [Pat ann] -- ^ Constructor pattern with nested sub-patterns.
  | PIntLit Integer ann -- ^ Integer literal pattern.
  | PFloatLit Double ann -- ^ Float literal pattern.
  | PStrLit Text ann -- ^ String literal pattern.
  | PCharLit Char ann -- ^ Character literal pattern.
  | PListLit [Pat ann] -- ^ List literal pattern.
  deriving (Show, Eq, Ord, Generic)

-- | Functor instance for pattern annotations.
instance Functor Pat where
  fmap f (PWildcard ann) = PWildcard (f ann)
  fmap f (PVar ident ann) = PVar ident (f ann)
  fmap f (PCtor (ident, ann) pats) = PCtor (ident, f ann) (map (fmap f) pats)
  fmap f (PIntLit n ann) = PIntLit n (f ann)
  fmap f (PFloatLit n ann) = PFloatLit n (f ann)
  fmap f (PStrLit s ann) = PStrLit s (f ann)
  fmap f (PCharLit c ann) = PCharLit c (f ann)
  fmap f (PListLit pats) = PListLit (map (fmap f) pats)

-- | Binary instance for pattern annotations.
instance (Binary ann) => Binary (Pat ann) where
  put (PWildcard ann) = do
    put (0 :: Word8)
    put ann
  put (PVar ident ann) = do
    put (1 :: Word8)
    put ident
    put ann
  put (PCtor ident pats) = do
    put (2 :: Word8)
    put ident
    put pats
  put (PIntLit n ann) = do
    put (3 :: Word8)
    put n
    put ann
  put (PFloatLit n ann) = do
    put (4 :: Word8)
    put n
    put ann
  put (PStrLit s ann) = do
    put (5 :: Word8)
    put s
    put ann
  put (PCharLit c ann) = do
    put (7 :: Word8)
    put c
    put ann
  put (PListLit pats) = do
    put (6 :: Word8)
    put pats
  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> PWildcard <$> get
      1 -> PVar <$> get <*> get
      2 -> PCtor <$> get <*> get
      3 -> PIntLit <$> get <*> get
      4 -> PFloatLit <$> get <*> get
      5 -> PStrLit <$> get <*> get
      6 -> PListLit <$> get
      7 -> PCharLit <$> get <*> get
      _ -> fail "Invalid Pat tag"

-- | Match clause of a pattern and expression.
data Clause ann = Clause (Pat ann) (Exp ann)
  deriving (Show, Eq, Generic, Functor, Binary)

{- | Top-level statements.

Statements are the top-level declarations in a Kip program.

= Statement Types

* __Defn__: Value definition
  @
  name, value.
  @
  Example: @pi, 3.14.@

* __Function__: Function definition with pattern matching
  @
  (args) name, pattern1-ise result1, pattern2-ise result2.
  @
  The @Bool@ indicates whether it's an infinitive (verb form).

* __PrimFunc__: Primitive (built-in) function declaration
  @
  (args) name yerleşiktir.
  @
  The @Bool@ indicates whether it's an infinitive.

* __Load__: Module import
  @
  dir/module-name'i yükle.
  @

* __NewType__: Algebraic data type declaration
  @
  Bir (params type-name) ya ctor1 ya ctor2 olabilir.
  @
  Components:
  1. @Identifier@: Type name (e.g., @"liste"@, @"çift"@)
  2. @[Ty ann]@: Type parameters as type variables with case annotations
     Each parameter is a @TyVar@ that captures both the identifier and its case
  3. @[Ctor ann]@: Constructors with their case annotations and argument types

  Example: @Bir (öğe listesi) ya boş ya da bir öğenin bir öğe listesine eki olabilir.@
  * Type name: @"liste"@
  * Parameters: @[TyVar (Nom, span) "öğe"]@ (single type parameter in nominative)
  * Constructors: @[(("boş", (Nom, span)), []), (("eki", (P3s, span)), [...])]@

  Multi-parameter example: @Bir (a b çifti) ya bir a bir b çifti olabilir.@
  * Type name: @"çift"@
  * Parameters: @[TyVar (Nom, span) "a", TyVar (Nom, span) "b"]@
  * Constructors: @[(("çifti", (Nom, span)), [TyVar _ "a", TyVar _ "b"])]@

  With Turkish case suffixes: @Bir (a'yla b'nin sonucu) ...@
  * Parameters: @[TyVar (Ins, span) "a", TyVar (Gen, span) "b"]@
  * The case of each parameter is preserved in the @TyVar@ annotation

* __PrimType__: Primitive type declaration
  @
  Bir yerleşik type-name olsun.
  @

* __ExpStmt__: Expression statement (evaluate and print)
  @
  expression.
  @
-}
data Stmt ann =
    Defn Identifier (Ty ann) (Exp ann)
  | Function Identifier [Arg ann] (Ty ann) [Clause ann] Bool
  | PrimFunc Identifier [Arg ann] (Ty ann) Bool
  | Load [Text] Identifier
  | NewType Identifier [Ty ann] [Ctor ann]
  | PrimType Identifier [Ty ann]
  | ExpStmt (Exp ann)
  deriving (Show, Eq, Generic, Functor, Binary)

-- | Names introduced by a sequence of statements.
stmtDeclarationNames :: [Stmt Ann] -> [Identifier]
stmtDeclarationNames = concatMap names
  where
    names stmt =
      case stmt of
        Defn name _ _ -> [name]
        Function name _ _ _ _ -> [name]
        PrimFunc name _ _ _ -> [name]
        NewType name _ ctors -> name : map (fst . fst) ctors
        PrimType name _ -> [name]
        _ -> []

-- | Select the most recently recorded span for each identifier.
latestDefSpans :: Map.Map Identifier [Span] -> Map.Map Identifier Span
latestDefSpans =
  Map.mapMaybe (\spans -> case reverse spans of
    span' : _ -> Just span'
    [] -> Nothing)

-- | Filter definition spans to names introduced by the given statements.
defSpansFromStmts :: [Stmt Ann]
                  -> Map.Map Identifier [Span]
                  -> Map.Map Identifier Span
defSpansFromStmts stmts defSpans =
  let allowed = Set.fromList (stmtDeclarationNames stmts)
  in Map.filterWithKey (\ident _ -> Set.member ident allowed) (latestDefSpans defSpans)

-- | Pair function signatures with their corresponding definition spans.
funcSigSpansFromStmts :: [Stmt Ann]
                      -> Map.Map Identifier [Span]
                      -> Map.Map (Identifier, [Ty Ann]) Span
funcSigSpansFromStmts stmts defSpans =
  fst (foldl' step (Map.empty, defSpans) stmts)
  where
    step (acc, spans) stmt =
      case stmt of
        Function name args _ _ _ -> insertSignature name args acc spans
        PrimFunc name args _ _ -> insertSignature name args acc spans
        _ -> (acc, spans)

    insertSignature name args acc spans =
      let (mSpan, spans') = takeSpan name spans
      in (maybe acc (\span' -> Map.insert (name, map snd args) span' acc) mSpan, spans')

    takeSpan name spans =
      case Map.lookup name spans of
        Just (span' : rest) -> (Just span', Map.insert name rest spans)
        _ -> (Nothing, spans)

-- | Pretty-print an expression for fallback rendering.
prettyExp :: Exp a -- ^ Expression to render.
          -> String -- ^ Rendered text.
prettyExp (Var _ name _) = T.unpack (T.intercalate "-" (fst name ++ [snd name]))
prettyExp (StrLit _ s) = show (T.unpack s)
prettyExp (IntLit _ n) = show n
prettyExp (FloatLit _ n) = show n
prettyExp (CharLit _ c) = show c
prettyExp (Bind _ name _ e) =
  T.unpack (T.intercalate "-" (fst name ++ [snd name])) ++ " için " ++ prettyExp e
prettyExp (Seq _ a b) =
  prettyExp a ++ ", " ++ prettyExp b
prettyExp (App _ f xs) =
  unwords (map prettyExp (xs ++ [f]))
prettyExp (SetLit _ entries) =
  "[" ++ intercalate ", " (map prettyExp (Map.elems entries)) ++ "]"
prettyExp (MapLit _ entries) =
  "{" ++ intercalate ", " [prettyExp k ++ " -> " ++ prettyExp v | (k, v) <- Map.elems entries] ++ "}"
prettyExp (Match _ scrut clauses) =
  "(" ++ intercalate ", " (map (prettyClause (prettyExp scrut)) clauses) ++ ")"
  where
    -- | Render an identifier in a clause.
    prettyIdent' :: Identifier -- ^ Identifier to render.
                 -> String -- ^ Rendered identifier.
    prettyIdent' (xs, x) = T.unpack (T.intercalate "-" (xs ++ [x]))
    -- | Render a clause against a scrutinee string.
    prettyClause :: String -- ^ Scrutinee string.
                 -> Clause a -- ^ Clause to render.
                 -> String -- ^ Rendered clause.
    prettyClause scr (Clause pat body) =
      prettyPat scr pat ++ ", " ++ prettyExp body
    -- | Render a pattern for pretty output.
    prettyPat :: String -- ^ Scrutinee string.
              -> Pat a -- ^ Pattern to render.
              -> String -- ^ Rendered pattern.
    prettyPat scr pat =
      case pat of
        PWildcard _ -> "değilse"
        PVar n _ -> prettyIdent' n
        PCtor (ctor, _) pats ->
          let argStrs = scr : map (prettyPat "") pats
          in unwords (argStrs ++ [prettyIdent' ctor])
prettyExp (Let _ name body) =
  "let " ++ T.unpack (T.intercalate "-" (fst name ++ [snd name])) ++ " in " ++ prettyExp body
prettyExp (Ascribe _ ty e) =
  prettyTySimple ty ++ " olarak " ++ prettyExp e
  where
    prettyTySimple (TyString _) = "dizge"
    prettyTySimple (TyInt _) = "tam-sayı"
    prettyTySimple (TyFloat _) = "ondalık-sayı"
    prettyTySimple (TyChar _) = "karakter"
    prettyTySimple (TyInd _ name) = T.unpack (T.intercalate "-" (fst name ++ [snd name]))
    prettyTySimple (TyVar _ name) = T.unpack (T.intercalate "-" (fst name ++ [snd name]))
    prettyTySimple _ = "..."

-- | Pretty-print an expression as an indented tree for debugging.
ppExp :: Int -> Exp a -> String
ppExp n e = indent n ++ case e of
  Var _ name _ -> "Var " ++ ppIdent name
  StrLit _ s -> "StrLit " ++ show s
  IntLit _ i -> "IntLit " ++ show i
  FloatLit _ f -> "FloatLit " ++ show f
  CharLit _ c -> "CharLit " ++ show c
  App _ f args ->
    "App\n" ++ ppExp (n+2) f ++ "\n" ++
    indent (n+2) ++ "args:" ++
    concatMap (\a -> "\n" ++ ppExp (n+4) a) args
  Bind _ name _ body ->
    "Bind " ++ ppIdent name ++ "\n" ++ ppExp (n+2) body
  Seq _ a b ->
    "Seq\n" ++ ppExp (n+2) a ++ "\n" ++ ppExp (n+2) b
  Match _ scrut clauses ->
    "Match\n" ++ ppExp (n+2) scrut ++ "\n" ++
    indent (n+2) ++ "clauses:" ++
    concatMap (\c -> "\n" ++ ppClause (n+4) c) clauses
  Let _ name body ->
    "Let " ++ ppIdent name ++ "\n" ++ ppExp (n+2) body
  Ascribe _ ty body ->
    "Ascribe " ++ ppTy ty ++ "\n" ++ ppExp (n+2) body
  where
    indent i = replicate i ' '

-- | Pretty-print a clause.
ppClause :: Int -> Clause a -> String
ppClause n (Clause pat body) =
  replicate n ' ' ++ ppPat pat ++ " =>\n" ++ ppExp (n+2) body

-- | Pretty-print a pattern.
ppPat :: Pat a -> String
ppPat (PWildcard _) = "_"
ppPat (PVar name _) = ppIdent name
ppPat (PCtor (name, _) pats) =
  ppIdent name ++ "(" ++ intercalate ", " (map ppPat pats) ++ ")"
ppPat (PIntLit i _) = show i
ppPat (PFloatLit f _) = show f
ppPat (PStrLit s _) = show s
ppPat (PCharLit c _) = show c
ppPat (PListLit pats) = "[" ++ intercalate ", " (map ppPat pats) ++ "]"

-- | Pretty-print an identifier.
ppIdent :: Identifier -> String
ppIdent (mods, name) = T.unpack (T.intercalate "." (mods ++ [name]))

-- | Pretty-print a type (simple form).
ppTy :: Ty a -> String
ppTy (TyString _) = "dizge"
ppTy (TyInt _) = "tam-sayı"
ppTy (TyFloat _) = "ondalık-sayı"
ppTy (TyChar _) = "karakter"
ppTy (TyInd _ name) = ppIdent name
ppTy (TyVar _ name) = ppIdent name
ppTy (TySkolem _ name) = "'" ++ ppIdent name
ppTy (Arr _ d i) = "(" ++ ppTy d ++ " -> " ++ ppTy i ++ ")"
ppTy (TyApp _ c args) = ppTy c ++ "(" ++ intercalate ", " (map ppTy args) ++ ")"

-- | Pretty-print a statement as an indented tree.
ppStmt :: Stmt a -> String
ppStmt stmt = case stmt of
  Defn name ty body ->
    "Defn " ++ ppIdent name ++ " : " ++ ppTy ty ++ "\n" ++ ppExp 2 body
  Function name args retTy clauses isInfix ->
    "Function " ++ ppIdent name ++
    " (" ++ intercalate ", " (map ppArg args) ++ ") : " ++ ppTy retTy ++
    (if isInfix then " [infix]" else "") ++ "\n" ++
    concatMap (\c -> ppClause 2 c ++ "\n") clauses
  PrimFunc name args retTy isInfix ->
    "PrimFunc " ++ ppIdent name ++
    " (" ++ intercalate ", " (map ppArg args) ++ ") : " ++ ppTy retTy ++
    (if isInfix then " [infix]" else "")
  Load dirPath name -> "Load " ++ T.unpack (T.intercalate "/" (dirPath ++ [T.pack (ppIdent name)]))
  NewType name params ctors ->
    "NewType " ++ ppIdent name ++
    " [" ++ intercalate ", " (map ppTy params) ++ "]\n" ++
    concatMap (\c -> "  " ++ ppCtor c ++ "\n") ctors
  PrimType name params -> "PrimType " ++ ppIdent name ++ " [" ++ intercalate ", " (map ppTy params) ++ "]"
  ExpStmt e -> "ExpStmt\n" ++ ppExp 2 e
  where
    ppArg ((name, _), ty) = ppIdent name ++ " : " ++ ppTy ty
    ppCtor ((name, _), tys) = ppIdent name ++ "(" ++ intercalate ", " (map ppTy tys) ++ ")"
