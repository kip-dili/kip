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
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec.Pos (SourcePos(..), Pos, unPos, mkPos)
import Data.Binary (Binary(..), Get)
import Data.Word (Word8)

-- | Binary instance for source positions.
instance Binary Pos where
  put = put . unPos
  get = mkPos <$> get

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
instance Binary SourcePos where
  put (SourcePos name line col) = do
    put name
    put line
    put col
  get = SourcePos <$> get <*> get <*> get

-- | Source span for diagnostics.
data Span =
    Span { spanStart :: SourcePos, spanEnd :: SourcePos } -- ^ Inclusive start and end.
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
    (Span s1 _, Span _ e2) -> Span s1 e2
    (Span s1 e1, NoSpan) -> Span s1 e1
    (NoSpan, Span s1 e1) -> Span s1 e1
    (NoSpan, NoSpan) -> NoSpan

-- | Type syntax tree with an annotation payload.
data Ty a =
    TyString { annTy :: a } -- ^ String type.
  | TyInt    { annTy :: a } -- ^ Integer type.
  | TyFloat  { annTy :: a } -- ^ Floating-point type.
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
  | StrLit { annExp :: a , lit :: Text } -- ^ String literal.
  | IntLit { annExp :: a , intVal :: Integer } -- ^ Integer literal.
  | FloatLit { annExp :: a , floatVal :: Double } -- ^ Floating-point literal.
  | Bind   { annExp :: a , bindName :: Identifier , bindExp :: Exp a } -- ^ Binding expression.
  | Seq    { annExp :: a , first :: Exp a , second :: Exp a } -- ^ Sequential composition.
  | Match  { annExp :: a , scrutinee :: Exp a , clauses :: [Clause a] } -- ^ Pattern match.
  | Let    { annExp :: a , varName :: Identifier , body :: Exp a } -- ^ Let binding.
  deriving (Show, Eq, Generic, Functor, Binary)

-- | Typed argument of a function.
type Arg ann = (Identifier, Ty ann)
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
  | PCtor Identifier [Pat ann] -- ^ Constructor pattern with nested sub-patterns.
  deriving (Show, Eq, Generic)

-- | Functor instance for pattern annotations.
instance Functor Pat where
  fmap f (PWildcard ann) = PWildcard (f ann)
  fmap f (PVar ident ann) = PVar ident (f ann)
  fmap f (PCtor ident pats) = PCtor ident (map (fmap f) pats)

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
  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> PWildcard <$> get
      1 -> PVar <$> get <*> get
      2 -> PCtor <$> get <*> get
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
  The @Bool@ indicates whether it's a gerund (verb form).

* __PrimFunc__: Primitive (built-in) function declaration
  @
  (args) name yerleşiktir.
  @
  The @Bool@ indicates whether it's a gerund.

* __Load__: Module import
  @
  module-name'i yükle.
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
  | Load Identifier
  | NewType Identifier [Ty ann] [Ctor ann]
  | PrimType Identifier
  | ExpStmt (Exp ann)
  deriving (Show, Eq, Generic, Functor, Binary)

-- data Ty' a =
--     TyString' { annTy' :: a Ty' }
--   | Arr'     { annTy' :: a Ty' , dom' :: Ty' a , img' :: Ty' a }
--   | TyInd'   { annTy' :: a Ty', indName' :: Identifier }
--   deriving (Show, Eq, Ord)

-- type family TypeCheck (k :: (* -> *) -> *) :: * where
--   TypeCheck Ty' = ()

-- | Check whether a string contains more than one word.
isMultiWord :: String -- ^ Candidate string.
            -> Bool -- ^ True when the string has multiple words.
isMultiWord s = length (words s) /= 1

-- | Pretty-print an expression for fallback rendering.
prettyExp :: Exp a -- ^ Expression to render.
          -> String -- ^ Rendered text.
prettyExp (Var _ name _) = T.unpack (T.intercalate "-" (fst name ++ [snd name]))
prettyExp (StrLit _ s) = show (T.unpack s)
prettyExp (IntLit _ n) = show n
prettyExp (FloatLit _ n) = show n
prettyExp (Bind _ name e) =
  T.unpack (T.intercalate "-" (fst name ++ [snd name])) ++ " olarak " ++ prettyExp e
prettyExp (Seq _ a b) =
  prettyExp a ++ ", " ++ prettyExp b
prettyExp (App _ f xs) =
  unwords (map prettyExp (xs ++ [f]))
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
        PCtor ctor pats ->
          let argStrs = scr : map (prettyPat "") pats
          in unwords (argStrs ++ [prettyIdent' ctor])
prettyExp (Let _ name body) =
  "let " ++ T.unpack (T.intercalate "-" (fst name ++ [snd name])) ++ " in " ++ prettyExp body
