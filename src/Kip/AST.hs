module Kip.AST where

import Data.List

type Identifier = ([String], String)

data Case =
    Nom -- ^ nominative case (yalın hal)
  | Acc -- ^ accusative case (-i hali)
  | Dat -- ^ dative case (-e hali)
  | Loc -- ^ locative case (-de hali)
  | Abl -- ^ ablative case (-den hali)
  | Gen -- ^ genitive case (-in eki)
  | Ins -- ^ instrumental case (ile, -le)
  | Cond -- ^ conditional case (-se, şart kipi)
  deriving (Show, Eq, Ord)

data Ty a =
    TyString { annTy :: a }
  | Arr      { annTy :: a , dom :: Ty a, img :: Ty a }
  | TyInd    { annTy :: a , indName :: Identifier }
  deriving (Show, Eq, Ord)

data Exp a =
    Var    { annExp :: a , varName :: Identifier }
  | App    { annExp :: a , fn :: Exp a , args :: [Exp a] }
  | StrLit { annExp :: a , lit :: String }
  | Let    { annExp :: a , varName :: Identifier , body :: Exp a }
  deriving (Show, Eq)

type Arg = (Identifier, Ty Case)
type Ctor = (Identifier, [(Identifier, Ty Case)])

data Stmt =
    Defn Identifier (Ty Case) (Exp Case)
  | Function Identifier [Arg] (Ty Case) (Exp Case)
  | NewType Identifier [Ctor]
  | Print (Exp Case)
  | ExpStmt (Exp Case)
  deriving (Show, Eq)

isMultiWord :: String -> Bool
isMultiWord s = length (words s) /= 1

prettyExp :: Exp a -> String
prettyExp (Var _ name) = intercalate "-" (fst name ++ [snd name])
prettyExp (StrLit _ s) = show s
