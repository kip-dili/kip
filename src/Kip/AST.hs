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
  | Arr      { annTy :: a , dom :: Ty a, img :: Ty a } -- ^ Function type.
  | TyInd    { annTy :: a , indName :: Identifier } -- ^ Named type.
  | TyVar    { annTy :: a , tyVarName :: Identifier } -- ^ Type variable.
  | TyApp    { annTy :: a , tyCtor :: Ty a , tyArgs :: [Ty a] } -- ^ Type application.
  deriving (Show, Eq, Ord, Generic, Functor, Binary)

-- | Expression syntax tree with an annotation payload.
data Exp a =
    Var    { annExp :: a , varName :: Identifier , varCandidates :: [(Identifier, Case)] } -- ^ Variable reference.
  | App    { annExp :: a , fn :: Exp a , args :: [Exp a] } -- ^ Function application.
  | StrLit { annExp :: a , lit :: Text } -- ^ String literal.
  | IntLit { annExp :: a , intVal :: Integer } -- ^ Integer literal.
  | Bind   { annExp :: a , bindName :: Identifier , bindExp :: Exp a } -- ^ Binding expression.
  | Seq    { annExp :: a , first :: Exp a , second :: Exp a } -- ^ Sequential composition.
  | Match  { annExp :: a , scrutinee :: Exp a , clauses :: [Clause a] } -- ^ Pattern match.
  | Let    { annExp :: a , varName :: Identifier , body :: Exp a } -- ^ Let binding.
  deriving (Show, Eq, Generic, Functor, Binary)

-- | Typed argument of a function.
type Arg ann = (Identifier, Ty ann)
-- | Constructor with its argument types.
type Ctor ann = (Identifier, [Ty ann])

-- | Pattern syntax tree.
data Pat ann =
    PWildcard -- ^ Wildcard pattern.
  | PCtor Identifier [(Identifier, ann)] -- ^ Constructor pattern with named fields.
  deriving (Show, Eq, Generic)

-- | Functor instance for pattern annotations.
instance Functor Pat where
  fmap f PWildcard = PWildcard
  fmap f (PCtor ident vars) = PCtor ident (map (fmap f) vars)

-- | Binary instance for pattern annotations.
instance (Binary ann) => Binary (Pat ann) where
  put PWildcard = put (0 :: Word8)
  put (PCtor ident vars) = do
    put (1 :: Word8)
    put ident
    put vars
  get = do
    tag <- get :: Get Word8
    case tag of
      0 -> return PWildcard
      1 -> PCtor <$> get <*> get
      _ -> fail "Invalid Pat tag"

-- | Match clause of a pattern and expression.
data Clause ann = Clause (Pat ann) (Exp ann)
  deriving (Show, Eq, Generic, Functor, Binary)

-- | Top-level statements.
data Stmt ann =
    Defn Identifier (Ty ann) (Exp ann) -- ^ Value definition.
  | Function Identifier [Arg ann] (Ty ann) [Clause ann] Bool -- ^ Function definition.
  | PrimFunc Identifier [Arg ann] (Ty ann) Bool -- ^ Primitive function declaration.
  | Load Identifier -- ^ Module load statement.
  | NewType Identifier [Identifier] [Ctor ann] -- ^ New type declaration.
  | PrimType Identifier -- ^ Primitive type declaration.
  | ExpStmt (Exp ann) -- ^ Standalone expression statement.
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
        PWildcard -> "değilse"
        PCtor ctor vars ->
          let argStrs = scr : map (prettyIdent' . fst) vars
          in unwords (argStrs ++ [prettyIdent' ctor])
prettyExp (Let _ name body) =
  "let " ++ T.unpack (T.intercalate "-" (fst name ++ [snd name])) ++ " in " ++ prettyExp body
