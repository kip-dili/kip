{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{- | Central registry for primitive functions.

This module provides a single source of truth for all primitive functions in Kip.
Different modules can annotate primitives with their specific information:
- TypeCheck: type checking logic
- Eval: runtime implementation
- Codegen.JS: JavaScript code generation
-}
module Kip.Primitive
  ( PrimitiveDef(..)
  , PrimitiveVariant(..)
  , PrimitiveEvalOps(..)
  , primitiveEvalImpl
  , allPrimitives
  , isImplementedPrimitive
  , primFiles
  , primitiveJsPrelude
  , primitiveJsPrunableSpecs
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Bifunctor as B
import Data.Char (chr, ord, toLower, toUpper, isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)
import Data.Fixed (mod')
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Word (Word32)
import System.FilePath (isRelative, takeBaseName, takeDirectory, (</>))
import Text.Read (readMaybe)
import Kip.AST

-- | A primitive function definition with its variants
data PrimitiveDef = PrimitiveDef
  { primId :: Identifier
    -- ^ The identifier (mods, name) for this primitive
  , primVariants :: [PrimitiveVariant]
    -- ^ Different overloaded variants of this primitive
  , primSourceFiles :: [FilePath]
    -- ^ Library files where this primitive is declared
  } deriving (Show, Eq)

-- | A variant of a primitive function (for overloading)
data PrimitiveVariant = PrimitiveVariant
  { variantArity :: Int
    -- ^ Number of arguments for this variant
  , variantArgTypeCheck :: [Arg Ann] -> Bool
    -- ^ Predicate to check if argument types match this variant
  }

instance Show PrimitiveVariant where
  show v = "PrimitiveVariant { arity = " ++ show (variantArity v) ++ " }"

instance Eq PrimitiveVariant where
  v1 == v2 = variantArity v1 == variantArity v2

-- | Host callbacks needed to evaluate primitives.
data PrimitiveEvalOps m = PrimitiveEvalOps
  { peWriteText :: Text -> m ()
  , peWriteInteger :: Integer -> m ()
  , peWriteDouble :: Double -> m ()
  , peFlushStdout :: m ()
  , peReadLine :: m Text
  , peReadFirstPath :: [FilePath] -> m (Maybe Text)
  , peReadEnv :: Text -> m (Maybe Text)
  , peGetCurrentFile :: m (Maybe FilePath)
  , peGetArgs :: m [Text]
  , peWriteFileText :: FilePath -> Text -> m Bool
  , peGetRandState :: m (Maybe Word32)
  , peSetRandState :: Word32 -> m ()
  , peLookupRandomSeed :: m (Maybe Integer)
  , peRandomRange :: Integer -> Integer -> m Integer
  }

-- | Helper to create a variant that accepts any types
anyTypes :: Int -> PrimitiveVariant
anyTypes n = PrimitiveVariant n (const True)

-- | Helper to create a variant that checks specific type constructors
withTypes :: Int -> ([Ty Ann] -> Bool) -> PrimitiveVariant
withTypes n check = PrimitiveVariant n (check . map snd)

-- | Check if a type is an integer
isIntTy :: Ty Ann -> Bool
isIntTy (TyInt _) = True
isIntTy _ = False

-- | Check if a type is a float
isFloatTy :: Ty Ann -> Bool
isFloatTy (TyFloat _) = True
isFloatTy _ = False

-- | Check if a type is a string
isStringTy :: Ty Ann -> Bool
isStringTy (TyString _) = True
isStringTy _ = False

-- | Check if a type is a character
isCharTy :: Ty Ann -> Bool
isCharTy (TyChar _) = True
isCharTy _ = False

-- | Check for the set type identifier.
isSetIdent :: Identifier -> Bool
isSetIdent (mods, name) = null mods && name == T.pack "küme"

-- | Check for the map/dictionary type identifier.
isMapIdent :: Identifier -> Bool
isMapIdent (mods, name) = null mods && name == T.pack "sözlük"

-- | Check whether a type still contains unresolved type variables.
containsTyVar :: Ty Ann -> Bool
containsTyVar ty =
  case ty of
    TyVar {} -> True
    TyApp _ ctor args -> containsTyVar ctor || any containsTyVar args
    Arr _ d i -> containsTyVar d || containsTyVar i
    _ -> False

-- | Normalize type annotations for structural type equality checks.
normalizeTyAnn :: Ty Ann -> Ty Ann
normalizeTyAnn ty =
  case normalizePrimTy ty of
    TyInt _ -> TyInt (mkAnn Nom NoSpan)
    TyFloat _ -> TyFloat (mkAnn Nom NoSpan)
    TyString _ -> TyString (mkAnn Nom NoSpan)
    TyChar _ -> TyChar (mkAnn Nom NoSpan)
    TyVar _ name -> TyVar (mkAnn Nom NoSpan) name
    TySkolem _ name -> TySkolem (mkAnn Nom NoSpan) name
    TyInd _ name -> TyInd (mkAnn Nom NoSpan) name
    TyApp _ ctor args ->
      TyApp (mkAnn Nom NoSpan) (normalizeTyAnn ctor) (map normalizeTyAnn args)
    Arr _ d i ->
      Arr (mkAnn Nom NoSpan) (normalizeTyAnn d) (normalizeTyAnn i)

-- | Compare types while ignoring source annotations/spans.
sameTy :: Ty Ann -> Ty Ann -> Bool
sameTy a b = normalizeTyAnn a == normalizeTyAnn b

-- | Extract set element type from @öğe küme'si@.
setElemTy :: Ty Ann -> Maybe (Ty Ann)
setElemTy ty =
  case normalizePrimTy ty of
    TyApp _ ctor [elemTy]
      | TyInd _ ident <- normalizePrimTy ctor
      , isSetIdent ident ->
          Just elemTy
    _ -> Nothing

-- | Check whether a type is @öğe küme'si@.
isSetTy :: Ty Ann -> Bool
isSetTy = isJust . setElemTy

-- | Extract key and value types from @anahtar'dan değer'e sözlük@.
mapKeyValTy :: Ty Ann -> Maybe (Ty Ann, Ty Ann)
mapKeyValTy ty =
  case normalizePrimTy ty of
    TyApp _ ctor [keyTy, valTy]
      | TyInd _ ident <- normalizePrimTy ctor
      , isMapIdent ident ->
          Just (keyTy, valTy)
    _ -> Nothing

-- | Check whether a type is @anahtar'dan değer'e sözlük@.
isMapTy :: Ty Ann -> Bool
isMapTy = isJust . mapKeyValTy

-- | All known primitive functions
allPrimitives :: [PrimitiveDef]
allPrimitives =
  [ PrimitiveDef ([], "yaz")
      [ withTypes 1 (\case [t] -> isIntTy t || isFloatTy t || isStringTy t || isCharTy t; _ -> False)
      , anyTypes 2  -- File write
      ]
      ["etki.kip"]

  , PrimitiveDef ([], "oku")
      [ anyTypes 0  -- stdin
      , withTypes 1 (\case [t] -> isStringTy t; _ -> False)  -- file read
      ]
      ["etki.kip"]

  , PrimitiveDef (["argüman"], "oku")
      [ anyTypes 0 ]
      ["etki.kip"]

  , PrimitiveDef (["çevreden"], "oku")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["etki.kip"]

  , PrimitiveDef ([], "uzunluk")
      [ anyTypes 1 ]
      ["dizge.kip"]

  , PrimitiveDef ([], "boyut")
      [ anyTypes 1 ]
      ["küme.kip", "sözlük.kip"]

  , PrimitiveDef ([], "birleşim")
      [ anyTypes 2 ]
      ["dizge.kip", "küme.kip", "sözlük.kip"]

  , PrimitiveDef (["tam", "sayı"], "hal")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False)
      , withTypes 1 (\case [t] -> isCharTy t; _ -> False)
      ]
      ["dizge.kip", "karakter.kip"]

  , PrimitiveDef (["ondalık", "sayı"], "hal")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False)
      , withTypes 1 (\case [t] -> isIntTy t; _ -> False)
      ]
      ["dizge.kip", "tam-sayı.kip"]

  , PrimitiveDef (["karakter"], "hal")
      [ withTypes 1 (\case [t] -> isIntTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef ([], "büyük")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef ([], "küçük")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef ([], "harflik")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef ([], "rakamlık")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef (["harf"], "rakamlık")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef (["büyük"], "harflik")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef (["küçük"], "harflik")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef ([], "boşlukluk")
      [ withTypes 1 (\case [t] -> isCharTy t; _ -> False) ]
      ["karakter.kip"]

  , PrimitiveDef ([], "ters")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "öğe")
      [ withTypes 2 (\case [t1, t2] -> isStringTy t1 && isIntTy t2; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "alış")
      [ withTypes 2 (\case [t1, t2] -> isStringTy t1 && isIntTy t2; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "bırakış")
      [ withTypes 2 (\case [t1, t2] -> isStringTy t1 && isIntTy t2; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "son")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "boşluk")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "satırlar")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "kelimeler")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef (["büyük"], "hal")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef (["küçük"], "hal")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "toplam")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "çarpım")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "fark")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "bölüm")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "kalan")
      [ withTypes 2 (\case [t1, t2] -> isFloatTy t1 || isFloatTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isIntTy t1 && isIntTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["dizge"], "hal")
      [ withTypes 1 (\case [t] -> isFloatTy t || isIntTy t || isCharTy t; _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip", "karakter.kip"]

  , PrimitiveDef ([], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False)
      , withTypes 2 (\case [t1, t2] -> isStringTy t1 && isStringTy t2; _ -> False)
      , withTypes 2 (\case [t1, t2] -> isCharTy t1 && isCharTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip", "dizge.kip", "karakter.kip"]

  , PrimitiveDef ([], "küçüklük")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["küçük"], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "büyüklük")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["büyük"], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["sayı"], "çek")
      [ anyTypes 2 ]
      ["etki.kip"]

  , PrimitiveDef ([], "karekök")
      [ withTypes 1 (\case [t] -> isFloatTy t; _ -> False) ]
      ["ondalık-sayı.kip"]

  , PrimitiveDef ([], "taban")
      [ withTypes 1 (\case [t] -> isFloatTy t; _ -> False) ]
      ["ondalık-sayı.kip"]

  , PrimitiveDef ([], "tavan")
      [ withTypes 1 (\case [t] -> isFloatTy t; _ -> False) ]
      ["ondalık-sayı.kip"]

  , PrimitiveDef ([], "boş-küme")
      [ anyTypes 0 ]
      ["küme.kip"]

  , PrimitiveDef (["boş"], "küme")
      [ anyTypes 0 ]
      ["küme.kip"]


  , PrimitiveDef ([], "ek")
      [ anyTypes 2, anyTypes 3 ]
      ["küme.kip", "sözlük.kip"]

  , PrimitiveDef ([], "çıkarılmış")
      [ anyTypes 2 ]
      ["küme.kip", "sözlük.kip"]

  , PrimitiveDef ([], "üyelik")
      [ anyTypes 2 ]
      ["küme.kip"]

  , PrimitiveDef ([], "liste-hal")
      [ anyTypes 1 ]
      ["küme.kip", "sözlük.kip"]

  , PrimitiveDef (["liste"], "hal")
      [ anyTypes 1 ]
      ["küme.kip", "sözlük.kip"]

  , PrimitiveDef ([], "boş-sözlük")
      [ anyTypes 0 ]
      ["sözlük.kip"]

  , PrimitiveDef (["boş"], "sözlük")
      [ anyTypes 0 ]
      ["sözlük.kip"]

  , PrimitiveDef ([], "karşılık")
      [ anyTypes 2 ]
      ["sözlük.kip"]


  , PrimitiveDef ([], "dur")
      [ anyTypes 0 ]
      []
  ]

-- | Check if a primitive function signature is implemented
isImplementedPrimitive :: Identifier -> [Arg Ann] -> Bool
isImplementedPrimitive name args =
  let normalizedArgs = map (B.second normalizePrimTy) args
      numArgs = length normalizedArgs
      matchingPrims = filter (\p -> primId p == name) allPrimitives
      hasUnknownTyVar = any (containsTyVar . snd) normalizedArgs
  in case matchingPrims of
       [] -> False
       (prim:_) ->
         let typedMatch =
               any (\variant ->
                 variantArity variant == numArgs &&
                 variantArgTypeCheck variant normalizedArgs
               ) (primVariants prim)
             arityOnlyMatch =
               any (\variant -> variantArity variant == numArgs) (primVariants prim)
         in typedMatch || (hasUnknownTyVar && arityOnlyMatch)

-- | Map a primitive identifier to the files that define it.
-- Tries both the direct identifier and the hyphen-joined form
-- since the parser may represent @boş-küme@ as @(["boş"], "küme")@.
primFiles :: Identifier -> [FilePath]
primFiles name =
  case filter (\p -> primId p == name || primId p == joinIdent name) allPrimitives of
    [] -> []
    (prim:_) -> primSourceFiles prim
  where
    joinIdent :: Identifier -> Identifier
    joinIdent (mods, root)
      | null mods = (mods, root)
      | otherwise = ([], T.intercalate "-" (mods ++ [root]))

-- | Resolve a primitive implementation by name/signature.
primitiveEvalImpl :: Monad m => PrimitiveEvalOps m -> Maybe FilePath -> Identifier -> [Arg Ann] -> Maybe ([Exp Ann] -> m (Exp Ann))
primitiveEvalImpl ops mPath ident args = do
  guardPrimFile mPath ident
  case ident of
    ([], "yaz")
      | [(_, TyInt _)] <- args -> Just (primWrite ops)
      | [(_, TyFloat _)] <- args -> Just (primWrite ops)
      | [(_, TyString _)] <- args -> Just (primWrite ops)
      | [(_, TyChar _)] <- args -> Just (primWrite ops)
      | [_, _] <- args -> Just (primWriteFile ops)
      | otherwise -> Nothing
    ([], "oku")
      | [] <- args -> Just (primRead ops)
      | [(_, TyString _)] <- args -> Just (primReadFile ops)
      | otherwise -> Nothing
    (["argüman"], "oku")
      | [] <- args -> Just (primReadArgs ops)
      | otherwise -> Nothing
    (["çevreden"], "oku")
      | [(_, TyString _)] <- args -> Just (primReadEnv ops)
      | otherwise -> Nothing
    ([], "uzunluk")
      | [(_, TyString _)] <- args -> Just (primStringLength "uzunluk")
      | otherwise -> Nothing
    ([], "boyut")
      | [(_, setTy)] <- args, isSetTy setTy -> Just (primSetSize ([], "boyut"))
      | [(_, mapTy)] <- args, isMapTy mapTy -> Just (primMapSize ([], "boyut"))
      | [_] <- args -> Just (primSetSize ([], "boyut"))
      | otherwise -> Nothing
    ([], "birleşim")
      | [(_, TyString _), (_, TyString _)] <- args -> Just (primStringConcat "birleşim")
      | [(_, leftTy), (_, rightTy)] <- args
      , Just leftElemTy <- setElemTy leftTy
      , Just rightElemTy <- setElemTy rightTy
      , sameTy leftElemTy rightElemTy ->
          Just (primSetUnion ([], "birleşim"))
      | [(_, leftTy), (_, rightTy)] <- args
      , isMapTy leftTy, isMapTy rightTy ->
          Just (primMapUnion ([], "birleşim"))
      | [_, _] <- args ->
          Just (primSetUnion ([], "birleşim"))
      | otherwise ->
          Nothing
    ([], "ek")
      | [(_, mapTy), _, _] <- args, isMapTy mapTy ->
          Just (primMapInsert ([], "ek"))
      | [_, _, _] <- args ->
          Just (primMapInsert ([], "ek"))
      | [(_, setTy), (_, elemTy)] <- args
      , Just setElemTy' <- setElemTy setTy
      , sameTy setElemTy' elemTy ->
          Just (primSetInsert ([], "ek"))
      | [_, _] <- args ->
          Just (primSetInsert ([], "ek"))
      | otherwise -> Nothing
    ([], "çıkarılmış")
      | [(_, mapTy), _] <- args, isMapTy mapTy ->
          Just (primMapDelete ([], "çıkarılmış"))
      | [(_, setTy), (_, elemTy)] <- args
      , Just setElemTy' <- setElemTy setTy
      , sameTy setElemTy' elemTy ->
          Just (primSetDelete ([], "çıkarılmış"))
      | [_, _] <- args ->
          Just (primSetDelete ([], "çıkarılmış"))
      | otherwise -> Nothing
    ([], "üyelik")
      | [(_, setTy), (_, elemTy)] <- args
      , Just setElemTy' <- setElemTy setTy
      , sameTy setElemTy' elemTy ->
          Just (primSetMember ([], "üyelik"))
      | [_, _] <- args ->
          Just (primSetMember ([], "üyelik"))
      | otherwise -> Nothing
    ([], "liste-hal")
      | [(_, mapTy)] <- args, isMapTy mapTy -> Just (primMapToList ([], "liste-hal"))
      | [(_, setTy)] <- args, isSetTy setTy -> Just (primSetToList ([], "liste-hal"))
      | [_] <- args -> Just (primSetToList ([], "liste-hal"))
      | otherwise -> Nothing
    (["liste"], "hal")
      | [(_, mapTy)] <- args, isMapTy mapTy -> Just (primMapToList (["liste"], "hal"))
      | [(_, setTy)] <- args, isSetTy setTy -> Just (primSetToList (["liste"], "hal"))
      | [_] <- args -> Just (primSetToList (["liste"], "hal"))
      | otherwise -> Nothing
    (["tam", "sayı"], "hal")
      | [(_, TyString _)] <- args -> Just (primStringToInt "tam-sayı-hali")
      | [(_, TyChar _)] <- args -> Just (primCharToInt "tam-sayı-hal")
      | otherwise -> Nothing
    (["ondalık", "sayı"], "hal")
      | [(_, TyString _)] <- args -> Just (primStringToFloat "ondalık-sayı-hali")
      | [(_, TyInt _)] <- args -> Just (primIntToFloat "tam-sayı-ondalık-sayı-hali")
      | otherwise -> Nothing
    (["karakter"], "hal")
      | [(_, TyInt _)] <- args -> Just (primIntToChar "karakter-hal")
      | otherwise -> Nothing
    ([], "büyük")
      | [(_, TyChar _)] <- args -> Just (primCharUpper "büyük")
      | otherwise -> Nothing
    ([], "küçük")
      | [(_, TyChar _)] <- args -> Just (primCharLower "küçük")
      | otherwise -> Nothing
    ([], "harflik")
      | [(_, TyChar _)] <- args -> Just (primCharIsAlpha "harflik")
      | otherwise -> Nothing
    ([], "rakamlık")
      | [(_, TyChar _)] <- args -> Just (primCharIsDigit "rakamlık")
      | otherwise -> Nothing
    (["harf"], "rakamlık")
      | [(_, TyChar _)] <- args -> Just (primCharIsAlphaNum "harf-rakamlık")
      | otherwise -> Nothing
    (["büyük"], "harflik")
      | [(_, TyChar _)] <- args -> Just (primCharIsUpper "büyük-harflik")
      | otherwise -> Nothing
    (["küçük"], "harflik")
      | [(_, TyChar _)] <- args -> Just (primCharIsLower "küçük-harflik")
      | otherwise -> Nothing
    ([], "boşlukluk")
      | [(_, TyChar _)] <- args -> Just (primCharIsSpace "boşlukluk")
      | otherwise -> Nothing
    ([], "ters")
      | [(_, TyString _)] <- args -> Just (primStringReverse "ters")
      | otherwise -> Nothing
    ([], "öğe")
      | [(_, TyString _), (_, TyInt _)] <- args -> Just (primStringCharAt "öğe")
      | otherwise -> Nothing
    ([], "alış")
      | [(_, TyString _), (_, TyInt _)] <- args -> Just (primStringTake "alış")
      | otherwise -> Nothing
    ([], "bırakış")
      | [(_, TyString _), (_, TyInt _)] <- args -> Just (primStringDrop "bırakış")
      | otherwise -> Nothing
    ([], "son")
      | [(_, TyString _)] <- args -> Just (primStringLastChar "son")
      | otherwise -> Nothing
    ([], "boşluk")
      | [(_, TyString _)] <- args -> Just (primStringIsEmpty "boşluk")
      | otherwise -> Nothing
    ([], "satırlar")
      | [(_, TyString _)] <- args -> Just (primStringLines "satırlar")
      | otherwise -> Nothing
    ([], "kelimeler")
      | [(_, TyString _)] <- args -> Just (primStringWords "kelimeler")
      | otherwise -> Nothing
    (["büyük"], "hal")
      | [(_, TyString _)] <- args -> Just (primStringUpper "büyük-hal")
      | otherwise -> Nothing
    (["küçük"], "hal")
      | [(_, TyString _)] <- args -> Just (primStringLower "küçük-hal")
      | otherwise -> Nothing
    ([], "toplam")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatBin "toplam" (+))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntBin "toplam" (+))
      | otherwise ->
          Nothing
    ([], "çarpım")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatBin "çarpım" (*))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntBin "çarpım" (*))
      | otherwise ->
          Nothing
    ([], "fark")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatBin "fark" (-))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntBin "fark" (-))
      | otherwise ->
          Nothing
    ([], "bölüm")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatDiv "bölüm")
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntDiv "bölüm")
      | otherwise ->
          Nothing
    ([], "kalan")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatMod "kalan")
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntMod "kalan")
      | otherwise ->
          Nothing
    (["dizge"], "hal")
      | [(_, TyFloat _)] <- args ->
          Just (primFloatToString "dizge-hal")
      | [(_, TyInt _)] <- args ->
          Just (primIntToString "dizge-hal")
      | [(_, TyChar _)] <- args ->
          Just (primCharToString "dizge-hal")
      | otherwise ->
          Nothing
    ([], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "eşitlik" (==))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "eşitlik" (==))
      | [(_, TyString _), (_, TyString _)] <- args ->
          Just (primStringEq "eşitlik")
      | [(_, TyChar _), (_, TyChar _)] <- args ->
          Just (primCharEq "eşitlik")
      | otherwise ->
          Nothing
    ([], "küçüklük")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "küçüklük" (<))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "küçüklük" (<))
      | otherwise ->
          Nothing
    (["küçük"], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "küçük-eşitlik" (<=))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "küçük-eşitlik" (<=))
      | otherwise ->
          Nothing
    ([], "büyüklük")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "büyüklük" (>))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "büyüklük" (>))
      | otherwise ->
          Nothing
    (["büyük"], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "büyük-eşitlik" (>=))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "büyük-eşitlik" (>=))
      | otherwise ->
          Nothing
    ([], "karekök")
      | [(_, TyFloat _)] <- args ->
          Just (primFloatSqrt "karekök")
      | otherwise ->
          Nothing
    ([], "taban")
      | [(_, TyFloat _)] <- args ->
          Just (primFloatFloor "taban")
      | otherwise ->
          Nothing
    ([], "tavan")
      | [(_, TyFloat _)] <- args ->
          Just (primFloatCeiling "tavan")
      | otherwise ->
          Nothing
    ([], "boş-küme")
      | [] <- args -> Just (primSetEmpty ([], "boş-küme"))
      | otherwise -> Nothing
    (["boş"], "küme")
      | [] <- args -> Just (primSetEmpty (["boş"], "küme"))
      | otherwise -> Nothing
    ([], "boş-sözlük")
      | [] <- args -> Just (primMapEmpty ([], "boş-sözlük"))
      | otherwise -> Nothing
    (["boş"], "sözlük")
      | [] <- args -> Just (primMapEmpty (["boş"], "sözlük"))
      | otherwise -> Nothing
    ([], "karşılık")
      | [(_, mapTy), _] <- args, isMapTy mapTy ->
          Just (primMapLookup ([], "karşılık"))
      | [_, _] <- args ->
          Just (primMapLookup ([], "karşılık"))
      | otherwise -> Nothing
    (["sayı"], "çek") -> Just (primIntRandom ops ["sayı"] "çek")
    ([], "sayı-çek") -> Just (primIntRandom ops [] "sayı-çek")
    _ -> Nothing
  where
    guardPrimFile mp name =
      case mp of
        Just path ->
          let loadedBase = takeBaseName path
              allowedBases = map takeBaseName (primFiles name)
          in if loadedBase `elem` allowedBases then Just () else Nothing
        _ -> Nothing

primWrite :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primWrite ops args =
  case args of
    [StrLit _ s] -> peWriteText ops s >> peFlushStdout ops >> pure unitExp
    [IntLit _ n] -> peWriteInteger ops n >> peFlushStdout ops >> pure unitExp
    [FloatLit _ n] -> peWriteDouble ops n >> peFlushStdout ops >> pure unitExp
    [CharLit _ c] -> peWriteText ops (T.singleton c) >> peFlushStdout ops >> pure unitExp
    _ -> pure (fallbackApp ([], "yaz") args)

primRead :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primRead ops args =
  case args of
    [] -> StrLit (mkAnn Nom NoSpan) <$> peReadLine ops
    _ -> pure (fallbackApp ([], "oku") args)

primReadFile :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primReadFile ops args =
  case args of
    [StrLit _ path] -> do
      mPath <- peGetCurrentFile ops
      content <- peReadFirstPath ops (resolveReadCandidates mPath path)
      case content of
        Nothing -> pure noneExp
        Just text -> pure (someExp (StrLit (mkAnn Nom NoSpan) text))
    _ -> pure (fallbackApp ([], "oku") args)

primReadArgs :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primReadArgs ops args =
  case args of
    [] -> textListExp (mkAnn Nom NoSpan) <$> peGetArgs ops
    _ -> pure (fallbackApp (["argüman"], "oku") args)

primReadEnv :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primReadEnv ops args =
  case args of
    [StrLit _ key] -> do
      val <- peReadEnv ops key
      case val of
        Nothing -> pure noneExp
        Just txt -> pure (someExp (StrLit (mkAnn Nom NoSpan) txt))
    _ -> pure (fallbackApp (["çevreden"], "oku") args)

primWriteFile :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primWriteFile ops args =
  case args of
    [StrLit _ path, StrLit _ content] -> do
      ok <- peWriteFileText ops (T.unpack path) content
      pure (boolExp ok)
    _ -> pure (fallbackApp ([], "yaz") args)

primStringLength :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringLength fname args =
  case args of
    [StrLit ann s] -> pure (IntLit ann (fromIntegral (T.length s)))
    _ -> pure (fallbackApp ([], fname) args)

primStringConcat :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringConcat fname args =
  case args of
    [StrLit ann a, StrLit _ b] -> pure (StrLit ann (a <> b))
    _ -> pure (fallbackApp ([], fname) args)

primStringReverse :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringReverse fname args =
  case args of
    [StrLit ann s] -> pure (StrLit ann (T.reverse s))
    _ -> pure (fallbackApp ([], fname) args)

primStringCharAt :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringCharAt fname args =
  case args of
    [StrLit ann s, IntLit _ n]
      | n >= 0 && n < fromIntegral (T.length s) ->
          pure (someExp (CharLit ann (T.index s (fromIntegral n))))
      | otherwise -> pure noneExp
    _ -> pure (fallbackApp ([], fname) args)

primStringTake :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringTake fname args =
  case args of
    [StrLit ann s, IntLit _ n] ->
      pure (StrLit ann (T.take (fromIntegral (max 0 n)) s))
    _ -> pure (fallbackApp ([], fname) args)

primStringDrop :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringDrop fname args =
  case args of
    [StrLit ann s, IntLit _ n] ->
      pure (StrLit ann (T.drop (fromIntegral (max 0 n)) s))
    _ -> pure (fallbackApp ([], fname) args)

primStringLastChar :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringLastChar fname args =
  case args of
    [StrLit ann s]
      | T.null s -> pure noneExp
      | otherwise -> pure (someExp (CharLit ann (T.last s)))
    _ -> pure (fallbackApp ([], fname) args)

primStringIsEmpty :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringIsEmpty fname args =
  case args of
    [StrLit _ s] -> pure (boolExp (T.null s))
    _ -> pure (fallbackApp ([], fname) args)

primStringLines :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringLines fname args =
  case args of
    [StrLit ann s] -> pure (textListExp ann (T.lines s))
    _ -> pure (fallbackApp ([], fname) args)

primStringWords :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringWords fname args =
  case args of
    [StrLit ann s] -> pure (textListExp ann (T.words s))
    _ -> pure (fallbackApp ([], fname) args)

primStringUpper :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringUpper _fname args =
  case args of
    [StrLit ann s] -> pure (StrLit ann (T.toUpper s))
    _ -> pure (fallbackApp (["büyük"], "hal") args)

primStringLower :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringLower _fname args =
  case args of
    [StrLit ann s] -> pure (StrLit ann (T.toLower s))
    _ -> pure (fallbackApp (["küçük"], "hal") args)

primStringToInt :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringToInt fname args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n -> pure (someExp (IntLit ann n))
        Nothing -> pure noneExp
    _ -> pure (fallbackApp (["tam", "sayı"], fname) args)

primStringToFloat :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringToFloat fname args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n -> pure (someExp (FloatLit ann n))
        _ -> pure noneExp
    _ -> pure (fallbackApp (["ondalık", "sayı"], fname) args)

primIntBin :: Monad m => Text -> (Integer -> Integer -> Integer) -> [Exp Ann] -> m (Exp Ann)
primIntBin fname op args =
  case args of
    [IntLit ann a, IntLit _ b] -> pure (IntLit ann (op a b))
    _ -> pure (fallbackApp ([], fname) args)

primFloatBin :: Monad m => Text -> (Double -> Double -> Double) -> [Exp Ann] -> m (Exp Ann)
primFloatBin fname op args =
  case args of
    [FloatLit ann a, FloatLit _ b] -> pure (FloatLit ann (op a b))
    _ -> pure (fallbackApp ([], fname) args)

primIntDiv :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primIntDiv fname args =
  case args of
    [IntLit ann a, IntLit _ b] -> pure (IntLit ann (if b == 0 then 0 else a `div` b))
    _ -> pure (fallbackApp ([], fname) args)

primFloatDiv :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primFloatDiv fname args =
  case args of
    [FloatLit ann a, FloatLit _ b] -> pure (FloatLit ann (if b == 0 then 0 else a / b))
    _ -> pure (fallbackApp ([], fname) args)

primIntMod :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primIntMod fname args =
  case args of
    [IntLit ann a, IntLit _ b] -> pure (IntLit ann (if b == 0 then 0 else a `mod` b))
    _ -> pure (fallbackApp ([], fname) args)

primFloatMod :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primFloatMod fname args =
  case args of
    [FloatLit ann a, FloatLit _ b] -> pure (FloatLit ann (if b == 0 then 0 else mod' a b))
    _ -> pure (fallbackApp ([], fname) args)

primIntRandom :: Monad m => PrimitiveEvalOps m -> [Text] -> Text -> [Exp Ann] -> m (Exp Ann)
primIntRandom ops mods name args =
  case args of
    [IntLit ann a, IntLit _ b] -> do
      let lo = min a b
          hi = max a b
      mState <- peGetRandState ops
      n <- case mState of
        Just seed -> do
          let (nextSeed, out) = randRange seed lo hi
          peSetRandState ops nextSeed
          pure out
        Nothing -> do
          mSeed <- peLookupRandomSeed ops
          case mSeed of
            Just seedVal -> do
              let seed = fromIntegral seedVal :: Word32
                  (nextSeed, out) = randRange seed lo hi
              peSetRandState ops nextSeed
              pure out
            Nothing -> peRandomRange ops lo hi
      pure (IntLit ann n)
    _ -> pure (fallbackApp (mods, name) args)
  where
    randRange seed lo hi =
      let nextSeed = seed * 1664525 + 1013904223
          range = hi - lo + 1
          out = lo + (toInteger nextSeed `mod` range)
      in (nextSeed, out)

primIntCmp :: Monad m => Text -> (Integer -> Integer -> Bool) -> [Exp Ann] -> m (Exp Ann)
primIntCmp fname op args =
  case args of
    [IntLit _ a, IntLit _ b] -> pure (boolExp (op a b))
    _ -> pure (fallbackApp ([], fname) args)

primFloatCmp :: Monad m => Text -> (Double -> Double -> Bool) -> [Exp Ann] -> m (Exp Ann)
primFloatCmp fname op args =
  case args of
    [FloatLit _ a, FloatLit _ b] -> pure (boolExp (op a b))
    _ -> pure (fallbackApp ([], fname) args)

primStringEq :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primStringEq fname args =
  case args of
    [StrLit _ a, StrLit _ b] -> pure (boolExp (a == b))
    _ -> pure (fallbackApp ([], fname) args)

primCharEq :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharEq fname args =
  case args of
    [CharLit _ a, CharLit _ b] -> pure (boolExp (a == b))
    _ -> pure (fallbackApp ([], fname) args)

primCharToString :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharToString fname args =
  case args of
    [CharLit ann c] -> pure (StrLit ann (T.singleton c))
    _ -> pure (fallbackApp (["dizge"], fname) args)

primCharToInt :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharToInt fname args =
  case args of
    [CharLit ann c] -> pure (IntLit ann (toInteger (ord c)))
    _ -> pure (fallbackApp (["tam", "sayı"], fname) args)

primIntToChar :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primIntToChar _ args =
  case args of
    [IntLit ann n]
      | n >= 0 && n <= 0x10FFFF ->
          pure (someExp (CharLit ann (chr (fromInteger n))))
      | otherwise ->
          pure noneExp
    _ -> pure (fallbackApp (["karakter"], "hal") args)

primCharUpper :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharUpper fname args =
  case args of
    [CharLit ann c] -> pure (CharLit ann (toUpper c))
    _ -> pure (fallbackApp ([], fname) args)

primCharLower :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharLower fname args =
  case args of
    [CharLit ann c] -> pure (CharLit ann (toLower c))
    _ -> pure (fallbackApp ([], fname) args)

primCharIsAlpha :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharIsAlpha fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isAlpha c))
    _ -> pure (fallbackApp ([], fname) args)

primCharIsDigit :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharIsDigit fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isDigit c))
    _ -> pure (fallbackApp ([], fname) args)

primCharIsAlphaNum :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharIsAlphaNum _fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isAlphaNum c))
    _ -> pure (fallbackApp (["harf"], "rakamlık") args)

primCharIsUpper :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharIsUpper _fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isUpper c))
    _ -> pure (fallbackApp (["büyük"], "harflik") args)

primCharIsLower :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharIsLower _fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isLower c))
    _ -> pure (fallbackApp (["küçük"], "harflik") args)

primCharIsSpace :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primCharIsSpace fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isSpace c))
    _ -> pure (fallbackApp ([], fname) args)

primIntToString :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primIntToString fname args =
  case args of
    [IntLit ann n] -> pure (StrLit ann (T.pack (show n)))
    _ -> pure (fallbackApp (["dizge"], fname) args)

primFloatToString :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primFloatToString fname args =
  case args of
    [FloatLit ann n] -> pure (StrLit ann (T.pack (show n)))
    _ -> pure (fallbackApp (["dizge"], fname) args)

primIntToFloat :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primIntToFloat fname args =
  case args of
    [IntLit ann n] -> pure (FloatLit ann (fromIntegral n))
    _ -> pure (fallbackApp (["tam", "sayı"], fname) args)

primFloatSqrt :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primFloatSqrt fname args =
  case args of
    [FloatLit ann n] -> pure (FloatLit ann (sqrt n))
    _ -> pure (fallbackApp ([], fname) args)

primFloatFloor :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primFloatFloor fname args =
  case args of
    [FloatLit ann n] -> pure (IntLit ann (floor n))
    _ -> pure (fallbackApp ([], fname) args)

primFloatCeiling :: Monad m => Text -> [Exp Ann] -> m (Exp Ann)
primFloatCeiling fname args =
  case args of
    [FloatLit ann n] -> pure (IntLit ann (ceiling n))
    _ -> pure (fallbackApp ([], fname) args)

data SetRepr = SetRepr
  { setKeys :: Set.Set Text
  , setElems :: Map.Map Text (Exp Ann)
  }

primSetEmpty :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetEmpty ident args =
  case args of
    [] -> pure (setExpFromSetRepr emptySetRepr)
    _ -> pure (fallbackApp ident args)

primSetInsert :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetInsert ident args =
  case args of
    [setVal, elemVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          let normalizedElem = normalizeSetValue elemVal
              key = setElemKey normalizedElem
              setRepr' =
                SetRepr
                  { setKeys = Set.insert key (setKeys setRepr)
                  , setElems = Map.insert key normalizedElem (setElems setRepr)
                  }
          in pure (setExpFromSetRepr setRepr')
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primSetDelete :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetDelete ident args =
  case args of
    [setVal, elemVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          let key = setElemKey (normalizeSetValue elemVal)
              setRepr' =
                SetRepr
                  { setKeys = Set.delete key (setKeys setRepr)
                  , setElems = Map.delete key (setElems setRepr)
                  }
          in pure (setExpFromSetRepr setRepr')
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primSetMember :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetMember ident args =
  case args of
    [setVal, elemVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          let key = setElemKey (normalizeSetValue elemVal)
          in pure (boolExp (Set.member key (setKeys setRepr)))
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primSetSize :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetSize ident args =
  case args of
    [setVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          pure (IntLit (mkAnn Nom NoSpan) (fromIntegral (Set.size (setKeys setRepr))))
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primSetUnion :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetUnion ident args =
  case args of
    [leftSetVal, rightSetVal] ->
      case (setReprFromExp leftSetVal, setReprFromExp rightSetVal) of
        (Just leftRepr, Just rightRepr) ->
          let setRepr' =
                SetRepr
                  { setKeys = Set.union (setKeys leftRepr) (setKeys rightRepr)
                  , setElems = Map.union (setElems rightRepr) (setElems leftRepr)
                  }
          in pure (setExpFromSetRepr setRepr')
        _ -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primSetToList :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primSetToList ident args =
  case args of
    [setVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          let values =
                [ elemVal
                | key <- Set.toAscList (setKeys setRepr)
                , Just elemVal <- [Map.lookup key (setElems setRepr)]
                ]
          in pure (foldr listConsExp listNilExp values)
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

emptySetRepr :: SetRepr
emptySetRepr = SetRepr Set.empty Map.empty

setReprFromExp :: Exp Ann -> Maybe SetRepr
setReprFromExp setVal =
  case setVal of
    SetLit _ entries ->
      Just
        SetRepr
          { setKeys = Set.fromList (Map.keys entries)
          , setElems = entries
          }
    _ -> setReprFromElems <$> expToList setVal

setReprFromElems :: [Exp Ann] -> SetRepr
setReprFromElems =
  foldl'
    (\setRepr elemVal ->
      let normalizedElem = normalizeSetValue elemVal
          key = setElemKey normalizedElem
      in SetRepr
           { setKeys = Set.insert key (setKeys setRepr)
           , setElems = Map.insert key normalizedElem (setElems setRepr)
           }
    )
    emptySetRepr

setExpFromSetRepr :: SetRepr -> Exp Ann
setExpFromSetRepr setRepr =
  SetLit
    (mkAnn Nom NoSpan)
    (Map.fromAscList
      [ (key, elemVal)
      | key <- Set.toAscList (setKeys setRepr)
      , Just elemVal <- [Map.lookup key (setElems setRepr)]
      ])

setElemKey :: Exp Ann -> Text
setElemKey = T.pack . show . normalizeSetValue

normalizeSetValue :: Exp Ann -> Exp Ann
normalizeSetValue exp' =
  case exp' of
    Var _ name _ ->
      Var (mkAnn Nom NoSpan) name []
    App _ fnExp argExps ->
      App (mkAnn Nom NoSpan) (normalizeSetValue fnExp) (map normalizeSetValue argExps)
    SetLit _ entries ->
      SetLit (mkAnn Nom NoSpan) (Map.map normalizeSetValue entries)
    MapLit _ entries ->
      MapLit (mkAnn Nom NoSpan) (Map.map (B.bimap normalizeSetValue normalizeSetValue) entries)
    StrLit _ s ->
      StrLit (mkAnn Nom NoSpan) s
    IntLit _ n ->
      IntLit (mkAnn Nom NoSpan) n
    FloatLit _ n ->
      FloatLit (mkAnn Nom NoSpan) n
    CharLit _ c ->
      CharLit (mkAnn Nom NoSpan) c
    Bind _ name _ bindExpr ->
      Bind (mkAnn Nom NoSpan) name (mkAnn Nom NoSpan) (normalizeSetValue bindExpr)
    Seq _ firstExp secondExp ->
      Seq (mkAnn Nom NoSpan) (normalizeSetValue firstExp) (normalizeSetValue secondExp)
    Match _ scrut clauseList ->
      Match (mkAnn Nom NoSpan) (normalizeSetValue scrut) (map normalizeSetClause clauseList)
    Let _ name bodyExpr ->
      Let (mkAnn Nom NoSpan) name (normalizeSetValue bodyExpr)
    Ascribe _ ty ascExpr ->
      Ascribe (mkAnn Nom NoSpan) (normalizeTyAnn ty) (normalizeSetValue ascExpr)

normalizeSetClause :: Clause Ann -> Clause Ann
normalizeSetClause (Clause pat bodyExpr) =
  Clause (normalizeSetPat pat) (normalizeSetValue bodyExpr)

normalizeSetPat :: Pat Ann -> Pat Ann
normalizeSetPat pat =
  case pat of
    PWildcard _ ->
      PWildcard (mkAnn Nom NoSpan)
    PVar name _ ->
      PVar name (mkAnn Nom NoSpan)
    PCtor (name, _) pats ->
      PCtor (name, mkAnn Nom NoSpan) (map normalizeSetPat pats)
    PIntLit n _ ->
      PIntLit n (mkAnn Nom NoSpan)
    PFloatLit n _ ->
      PFloatLit n (mkAnn Nom NoSpan)
    PStrLit s _ ->
      PStrLit s (mkAnn Nom NoSpan)
    PCharLit c _ ->
      PCharLit c (mkAnn Nom NoSpan)
    PListLit pats ->
      PListLit (map normalizeSetPat pats)

expToList :: Exp Ann -> Maybe [Exp Ann]
expToList exp'
  | isListNilExp exp' = Just []
  | otherwise =
      case listConsArgs exp' of
        Just (x, xs) -> (x :) <$> expToList xs
        Nothing -> Nothing

isListNilExp :: Exp Ann -> Bool
isListNilExp exp' =
  case exp' of
    Var _ name candidates ->
      name == ([], "boş") || any ((== ([], "boş")) . fst) candidates
    _ -> False

listConsArgs :: Exp Ann -> Maybe (Exp Ann, Exp Ann)
listConsArgs exp' =
  case exp' of
    App _ (Var _ name candidates) [x, xs]
      | isListConsIdent name || any (isListConsIdent . fst) candidates ->
          Just (x, xs)
    _ -> Nothing
  where
    isListConsIdent ident = ident == ([], "eki") || ident == ([], "ek")

-- | Internal representation for map/dictionary values.
data MapRepr = MapRepr
  { mapKeys :: [Text]
  , mapElems :: Map.Map Text (Exp Ann, Exp Ann)
  }

primMapEmpty :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapEmpty ident args =
  case args of
    [] -> pure (mapExpFromMapRepr emptyMapRepr)
    _ -> pure (fallbackApp ident args)

primMapInsert :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapInsert ident args =
  case args of
    [mapVal, keyVal, valVal] ->
      case mapReprFromExp mapVal of
        Just repr ->
          let normalizedKey = normalizeSetValue keyVal
              key = setElemKey normalizedKey
              repr' = MapRepr
                { mapKeys = if key `elem` mapKeys repr then mapKeys repr else mapKeys repr ++ [key]
                , mapElems = Map.insert key (normalizedKey, valVal) (mapElems repr)
                }
          in pure (mapExpFromMapRepr repr')
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primMapDelete :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapDelete ident args =
  case args of
    [mapVal, keyVal] ->
      case mapReprFromExp mapVal of
        Just repr ->
          let key = setElemKey (normalizeSetValue keyVal)
              repr' = MapRepr
                { mapKeys = filter (/= key) (mapKeys repr)
                , mapElems = Map.delete key (mapElems repr)
                }
          in pure (mapExpFromMapRepr repr')
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primMapLookup :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapLookup ident args =
  case args of
    [mapVal, keyVal] ->
      case mapReprFromExp mapVal of
        Just repr ->
          let key = setElemKey (normalizeSetValue keyVal)
          in case Map.lookup key (mapElems repr) of
               Just (_, v) -> pure (someExp v)
               Nothing -> pure noneExp
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primMapSize :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapSize ident args =
  case args of
    [mapVal] ->
      case mapReprFromExp mapVal of
        Just repr ->
          pure (IntLit (mkAnn Nom NoSpan) (fromIntegral (Map.size (mapElems repr))))
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

primMapUnion :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapUnion ident args =
  case args of
    [leftMapVal, rightMapVal] ->
      case (mapReprFromExp leftMapVal, mapReprFromExp rightMapVal) of
        (Just leftRepr, Just rightRepr) ->
          let -- Right-biased union: right values override left for same keys.
              rightKeys = mapKeys rightRepr
              leftOnlyKeys = filter (\k -> not (Map.member k (mapElems rightRepr))) (mapKeys leftRepr)
              repr' = MapRepr
                { mapKeys = leftOnlyKeys ++ rightKeys
                , mapElems = Map.union (mapElems rightRepr) (mapElems leftRepr)
                }
          in pure (mapExpFromMapRepr repr')
        _ -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)


primMapToList :: Monad m => Identifier -> [Exp Ann] -> m (Exp Ann)
primMapToList ident args =
  case args of
    [mapVal] ->
      case mapReprFromExp mapVal of
        Just repr ->
          let pairs =
                [ pairExp k v
                | key <- mapKeys repr
                , Just (k, v) <- [Map.lookup key (mapElems repr)]
                ]
          in pure (foldr listConsExp listNilExp pairs)
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

pairExp :: Exp Ann -> Exp Ann -> Exp Ann
pairExp a b =
  App
    (mkAnn Nom NoSpan)
    (Var (mkAnn P3s NoSpan) ([], "ikilisi") [(([], "ikilisi"), P3s)])
    [a, b]

emptyMapRepr :: MapRepr
emptyMapRepr = MapRepr [] Map.empty

mapReprFromExp :: Exp Ann -> Maybe MapRepr
mapReprFromExp mapVal =
  case mapVal of
    MapLit _ entries ->
      let keys = Map.keys entries
      in Just MapRepr
           { mapKeys = keys
           , mapElems = entries
           }
    _ -> Nothing

mapExpFromMapRepr :: MapRepr -> Exp Ann
mapExpFromMapRepr repr =
  MapLit
    (mkAnn Nom NoSpan)
    (Map.fromList
      [ (key, entry)
      | key <- mapKeys repr
      , Just entry <- [Map.lookup key (mapElems repr)]
      ])


resolveReadCandidates :: Maybe FilePath -> Text -> [FilePath]
resolveReadCandidates mPath path =
  let raw = T.unpack path
  in case mPath of
       Just base | isRelative raw ->
         let start = takeDirectory base
         in map (</> raw) (parentDirs start)
       _ -> [raw]

parentDirs :: FilePath -> [FilePath]
parentDirs dir =
  let parent = takeDirectory dir
  in if parent == dir then [dir] else dir : parentDirs parent

textListExp :: Ann -> [Text] -> Exp Ann
textListExp ann =
  foldr (listConsExp . StrLit ann) listNilExp

listNilExp :: Exp Ann
listNilExp = Var (mkAnn Nom NoSpan) ([], "boş") [(([], "boş"), Nom)]

listConsExp :: Exp Ann -> Exp Ann -> Exp Ann
listConsExp x xs =
  App
    (mkAnn Nom NoSpan)
    (Var (mkAnn P3s NoSpan) ([], "eki") [(([], "eki"), P3s)])
    [x, xs]

unitExp :: Exp Ann
unitExp = Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)]

noneExp :: Exp Ann
noneExp = Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)]

someExp :: Exp Ann -> Exp Ann
someExp v = App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)]) [v]

boolExp :: Bool -> Exp Ann
boolExp b =
  let name = if b then ([], "doğru") else ([], "yanlış")
  in Var (mkAnn Nom NoSpan) name [(name, Nom)]

fallbackApp :: Identifier -> [Exp Ann] -> Exp Ann
fallbackApp name = App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) name [])

primitiveJsPrelude :: Text
primitiveJsPrelude = T.unlines
  [ "// Kip → JavaScript (async/await for interactive browser support)"
  , ""
  , "// Node.js modules for I/O (lazy loaded)"
  , "var __kip_fs = null;"
  , "var __kip_readline = null;"
  , "var __kip_stdin_queue = [];"
  , "var __kip_stdin_waiters = [];"
  , "var __kip_stdin_closed = false;"
  , "var __kip_stdin_mode = null;"
  , "var __kip_is_browser = (typeof window !== 'undefined');"
  , "var __kip_require = null;"
  , "var __kip_random_seed = null;"
  , "if (!__kip_is_browser && typeof process !== 'undefined' && process.versions && process.versions.node) {"
  , "  const { createRequire } = await import('module');"
  , "  __kip_require = createRequire(import.meta.url);"
  , "}"
  , "if (__kip_is_browser) {"
  , "  if (typeof window.__kip_write !== 'function') {"
  , "    window.__kip_write = (x) => console.log(x);"
  , "  }"
  , "  if (typeof window.__kip_read_line !== 'function') {"
  , "    window.__kip_read_line = async () => {"
  , "      var v = prompt('Input:');"
  , "      return v === null ? '' : v;"
  , "    };"
  , "  }"
  , "}"
  , "if (!__kip_is_browser && typeof process !== 'undefined' && process.env && process.env.KIP_RANDOM_SEED) {"
  , "  const seed = Number(process.env.KIP_RANDOM_SEED);"
  , "  if (!Number.isNaN(seed)) {"
  , "    __kip_random_seed = seed >>> 0;"
  , "  }"
  , "}"
  , "if (__kip_is_browser && typeof window.__kip_random_seed === 'number') {"
  , "  __kip_random_seed = window.__kip_random_seed >>> 0;"
  , "}"
  , "var __kip_rand = () => {"
  , "  if (__kip_random_seed === null) return Math.floor(Math.random() * 4294967296);"
  , "  __kip_random_seed = (Math.imul(__kip_random_seed, 1664525) + 1013904223) >>> 0;"
  , "  return __kip_random_seed;"
  , "};"
  , ""
  , "// Initialize stdin buffer for line-by-line reading (Node.js only)"
  , "var __kip_init_stdin = () => {"
  , "  if (typeof process === 'undefined' || !process.stdin) return;"
  , "  if (__kip_stdin_mode !== null) return;"
  , "  if (!__kip_require) return;"
  , "  if (process.stdin.isTTY === false) {"
  , "    __kip_stdin_mode = 'pipe';"
  , "    __kip_fs = __kip_fs || __kip_require('fs');"
  , "    try {"
  , "      __kip_stdin_queue = __kip_fs.readFileSync(0, 'utf8').split('\\n');"
  , "    } catch (e) {"
  , "      __kip_stdin_queue = [];"
  , "    }"
  , "    __kip_stdin_closed = true;"
  , "    return;"
  , "  }"
  , "  __kip_stdin_mode = 'tty';"
  , "  if (__kip_readline === null) {"
  , "    var readline = __kip_require('readline');"
  , "    __kip_readline = readline.createInterface({ input: process.stdin, crlfDelay: Infinity });"
  , "    __kip_readline.on('line', (line) => {"
  , "      if (__kip_stdin_waiters.length > 0) {"
  , "        __kip_stdin_waiters.shift()(line);"
  , "      } else {"
  , "        __kip_stdin_queue.push(line);"
  , "      }"
  , "    });"
  , "    __kip_readline.on('close', () => {"
  , "      __kip_stdin_closed = true;"
  , "      while (__kip_stdin_waiters.length > 0) {"
  , "        __kip_stdin_waiters.shift()('');"
  , "      }"
  , "    });"
  , "  }"
  , "};"
  , "var __kip_close_stdin = () => {"
  , "  if (__kip_readline && __kip_stdin_mode === 'tty') {"
  , "    __kip_readline.close();"
  , "    __kip_readline = null;"
  , "  }"
  , "};"
  , ""
  , "// Helper to create a tagged value (works whether constructor is function or object)"
  , "var __kip_bool = (tag) => typeof window !== 'undefined' && typeof window[tag] === 'function' ? window[tag]() : { tag, args: [] };"
  , "var __kip_true = () => typeof doğru === 'function' ? doğru() : doğru;"
  , "var __kip_false = () => typeof yanlış === 'function' ? yanlış() : yanlış;"
  , "var __kip_some = (x) => typeof varlık === 'function' ? varlık(x) : { tag: 'varlık', args: [x] };"
  , "var __kip_none = () => typeof yokluk === 'function' ? yokluk() : { tag: 'yokluk', args: [] };"
  , "var __kip_float = (x) => ({ __kip_float: true, value: x });"
  , "var __kip_is_float = (x) => typeof x === 'object' && x && x.__kip_float === true;"
  , "var __kip_num = (x) => __kip_is_float(x) ? x.value : x;"
  , ""
  , "// Primitive boolean constructors (will be overridden by library)"
  , "var doğru = { tag: \"doğru\", args: [] };"
  , "var yanlış = { tag: \"yanlış\", args: [] };"
  , ""
  , "// Option type constructors (will be overridden by library if defined)"
  , "var varlık = (...args) => ({ tag: \"varlık\", args });"
  , "var yokluk = (...args) => ({ tag: \"yokluk\", args });"
  , ""
  , "// Unit type (will be overridden by library if defined)"
  , "var bitimlik = (...args) => ({ tag: \"bitimlik\", args });"
  , ""
  , "// Primitive functions for strings/numbers (may be overloaded by library for other types)"
  , "var __kip_prim_ters = (s) => s.split('').reverse().join('');"
  , "var __kip_prim_birleşim = (a, b) => __kip_num(a) + __kip_num(b);"
  , "var __kip_prim_uzunluk = (s) => s.length;"
  , "var __kip_prim_öğe = (s, n) => n >= 0 && n < s.length ? __kip_some(s[n]) : __kip_none();"
  , "var __kip_prim_alış = (s, n) => s.slice(0, Math.max(0, n));"
  , "var __kip_prim_bırakış = (s, n) => s.slice(Math.max(0, n));"
  , "var __kip_prim_son = (s) => s.length > 0 ? __kip_some(s[s.length - 1]) : __kip_none();"
  , "var __kip_prim_bosluk = (s) => s.length === 0 ? __kip_true() : __kip_false();"
  , "var __kip_prim_satirlar = (s) => {"
  , "  var parts = s.length === 0 ? [] : s.split('\\n');"
  , "  if (parts.length > 0 && parts[parts.length - 1] === '') {"
  , "    parts.pop();"
  , "  }"
  , "  var out = typeof boş === 'function' ? boş() : (typeof boş !== 'undefined' ? boş : { tag: 'boş', args: [] });"
  , "  for (var i = parts.length - 1; i >= 0; i -= 1) {"
  , "    out = typeof eki === 'function' ? eki(parts[i], out) : { tag: 'eki', args: [parts[i], out] };"
  , "  }"
  , "  return out;"
  , "};"
  , "var __kip_prim_kelimeler = (s) => {"
  , "  var trimmed = s.trim();"
  , "  var parts = trimmed.length === 0 ? [] : trimmed.split(/\\s+/u);"
  , "  var out = typeof boş === 'function' ? boş() : (typeof boş !== 'undefined' ? boş : { tag: 'boş', args: [] });"
  , "  for (var i = parts.length - 1; i >= 0; i -= 1) {"
  , "    out = typeof eki === 'function' ? eki(parts[i], out) : { tag: 'eki', args: [parts[i], out] };"
  , "  }"
  , "  return out;"
  , "};"
  , "var __kip_prim_dizge_büyük_hal = (s) => s.toUpperCase();"
  , "var __kip_prim_dizge_küçük_hal = (s) => s.toLowerCase();"
  , "var __kip_prim_toplam = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) + __kip_num(b)) : (__kip_num(a) + __kip_num(b));"
  , "var __kip_prim_fark = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) - __kip_num(b)) : (__kip_num(a) - __kip_num(b));"
  , "var __kip_set_normalize = (v) => {"
  , "  if (__kip_is_float(v)) return { t: 'float', v: v.value };"
  , "  if (typeof v === 'number') return { t: 'int', v };"
  , "  if (typeof v === 'string') return { t: 'str', v };"
  , "  if (v && typeof v === 'object') {"
  , "    if (v.__kip_set === true && Array.isArray(v.entries)) {"
  , "      return { t: 'set', v: v.entries.map((entry) => [entry[0], __kip_set_normalize(entry[1])]) };"
  , "    }"
  , "    if (typeof v.tag === 'string' && Array.isArray(v.args)) {"
  , "      return { t: 'ctor', tag: v.tag, args: v.args.map(__kip_set_normalize) };"
  , "    }"
  , "  }"
  , "  return { t: 'other', v: String(v) };"
  , "};"
  , "var __kip_set_key = (v) => JSON.stringify(__kip_set_normalize(v));"
  , "var __kip_set_from_map = (m) => ({"
  , "  __kip_set: true,"
  , "  entries: Array.from(m.entries()).sort((a, b) => a[0] < b[0] ? -1 : (a[0] > b[0] ? 1 : 0))"
  , "});"
  , "var __kip_set_entries = (s) => (s && s.__kip_set === true && Array.isArray(s.entries)) ? s.entries : null;"
  , "var __kip_array_to_list = (arr) => {"
  , "  var out = typeof boş === 'function' ? boş() : (typeof boş !== 'undefined' ? boş : { tag: 'boş', args: [] });"
  , "  for (var i = arr.length - 1; i >= 0; i -= 1) {"
  , "    out = typeof eki === 'function' ? eki(arr[i], out) : { tag: 'eki', args: [arr[i], out] };"
  , "  }"
  , "  return out;"
  , "};"
  , "var __kip_list_to_array = (list) => {"
  , "  var out = [];"
  , "  var cur = list;"
  , "  while (cur && typeof cur === 'object') {"
  , "    if (cur.tag === 'boş' && Array.isArray(cur.args) && cur.args.length === 0) return out;"
  , "    if (cur.tag === 'eki' && Array.isArray(cur.args) && cur.args.length === 2) {"
  , "      out.push(cur.args[0]);"
  , "      cur = cur.args[1];"
  , "      continue;"
  , "    }"
  , "    return null;"
  , "  }"
  , "  return null;"
  , "};"
  , "var boş_küme = () => ({ __kip_set: true, entries: [] });"
  , "var küme_ilave = (k, x) => {"
  , "  var entries = __kip_set_entries(k);"
  , "  if (!entries) return k;"
  , "  var m = new Map(entries);"
  , "  m.set(__kip_set_key(x), x);"
  , "  return __kip_set_from_map(m);"
  , "};"
  , "var küme_çıkarma = (k, x) => {"
  , "  var entries = __kip_set_entries(k);"
  , "  if (!entries) return k;"
  , "  var m = new Map(entries);"
  , "  m.delete(__kip_set_key(x));"
  , "  return __kip_set_from_map(m);"
  , "};"
  , "var küme_içerik = (k, x) => {"
  , "  var entries = __kip_set_entries(k);"
  , "  if (!entries) return { tag: 'yanlış', args: [] };"
  , "  return (new Map(entries)).has(__kip_set_key(x)) ? { tag: 'doğru', args: [] } : { tag: 'yanlış', args: [] };"
  , "};"
  , "var küme_boyut = (k) => {"
  , "  var entries = __kip_set_entries(k);"
  , "  return entries ? entries.length : 0;"
  , "};"
  , "var küme_birleşim = (a, b) => {"
  , "  var leftEntries = __kip_set_entries(a);"
  , "  var rightEntries = __kip_set_entries(b);"
  , "  if (!leftEntries || !rightEntries) return a;"
  , "  var m = new Map(leftEntries);"
  , "  for (var i = 0; i < rightEntries.length; i += 1) {"
  , "    m.set(rightEntries[i][0], rightEntries[i][1]);"
  , "  }"
  , "  return __kip_set_from_map(m);"
  , "};"
  , "var küme_liste = (k) => {"
  , "  var entries = __kip_set_entries(k);"
  , "  if (!entries) return __kip_array_to_list([]);"
  , "  return __kip_array_to_list(entries.map((entry) => entry[1]));"
  , "};"
  , "var liste_küme = (list) => {"
  , "  var arr = __kip_list_to_array(list);"
  , "  if (arr === null) return boş_küme();"
  , "  var m = new Map();"
  , "  for (var i = 0; i < arr.length; i += 1) {"
  , "    var item = arr[i];"
  , "    m.set(__kip_set_key(item), item);"
  , "  }"
  , "  return __kip_set_from_map(m);"
  , "};"
  , ""
  , "// Map/dictionary primitives"
  , "var __kip_map_entries = (m) => (m && m.__kip_map === true && Array.isArray(m.entries)) ? m.entries : null;"
  , "var boş_sözlük = () => ({ __kip_map: true, entries: [] });"
  , "var sözlük_ek = (m, k, v) => {"
  , "  var entries = __kip_map_entries(m);"
  , "  if (!entries) return m;"
  , "  var key = __kip_set_key(k);"
  , "  var newEntries = entries.filter((e) => e[0] !== key);"
  , "  newEntries.push([key, k, v]);"
  , "  return { __kip_map: true, entries: newEntries };"
  , "};"
  , "var sözlük_çıkarılmış = (m, k) => {"
  , "  var entries = __kip_map_entries(m);"
  , "  if (!entries) return m;"
  , "  var key = __kip_set_key(k);"
  , "  return { __kip_map: true, entries: entries.filter((e) => e[0] !== key) };"
  , "};"
  , "var sözlük_karşılık = (m, k) => {"
  , "  var entries = __kip_map_entries(m);"
  , "  var __none = typeof yokluk === 'function' ? yokluk() : { tag: 'yokluk', args: [] };"
  , "  if (!entries) return __none;"
  , "  var key = __kip_set_key(k);"
  , "  for (var i = 0; i < entries.length; i += 1) {"
  , "    if (entries[i][0] === key) return typeof varlık === 'function' ? varlık(entries[i][2]) : { tag: 'varlık', args: [entries[i][2]] };"
  , "  }"
  , "  return __none;"
  , "};"
  , "var sözlük_boyut = (m) => {"
  , "  var entries = __kip_map_entries(m);"
  , "  return entries ? entries.length : 0;"
  , "};"
  , "var sözlük_birleşim = (a, b) => {"
  , "  var leftEntries = __kip_map_entries(a);"
  , "  var rightEntries = __kip_map_entries(b);"
  , "  if (!leftEntries || !rightEntries) return a;"
  , "  var m = new Map();"
  , "  for (var i = 0; i < leftEntries.length; i += 1) m.set(leftEntries[i][0], leftEntries[i]);"
  , "  for (var i = 0; i < rightEntries.length; i += 1) m.set(rightEntries[i][0], rightEntries[i]);"
  , "  return { __kip_map: true, entries: Array.from(m.values()) };"
  , "};"
  , "var sözlük_liste = (m) => {"
  , "  var entries = __kip_map_entries(m);"
  , "  if (!entries) return __kip_array_to_list([]);"
  , "  return __kip_array_to_list(entries.map((e) => (typeof ikilisi === 'function' ? ikilisi(e[1], e[2]) : { tag: 'ikilisi', args: [e[1], e[2]] })));"
  , "};"
  , ""
  , "// I/O primitives - async to support browser interactivity"
  , "var __kip_prim_oku_stdin = async () => {"
  , "  // Check for browser runtime at call time"
  , "  if (__kip_is_browser && typeof window.__kip_read_line === 'function') {"
  , "    return await window.__kip_read_line();"
  , "  }"
  , "  // Node.js fallback"
  , "  __kip_init_stdin();"
  , "  if (__kip_stdin_queue.length > 0) {"
  , "    return __kip_stdin_queue.shift();"
  , "  }"
  , "  if (__kip_stdin_closed) {"
  , "    return '';"
  , "  }"
  , "  return await new Promise((resolve) => {"
  , "    __kip_stdin_waiters.push(resolve);"
  , "  });"
  , "};"
  , "var __kip_prim_oku_dosya = (path) => {"
  , "  if (!__kip_require) return __kip_none();"
  , "  __kip_fs = __kip_fs || __kip_require('fs');"
  , "  try {"
  , "    return __kip_some(__kip_fs.readFileSync(path, 'utf8'));"
  , "  } catch (e) {"
  , "    return __kip_none();"
  , "  }"
  , "};"
  , "var __kip_prim_arguman_oku = () => {"
  , "  var parts = (typeof process !== 'undefined' && process.argv && process.argv.length > 1) ? process.argv.slice(1) : [];"
  , "  var out = typeof boş === 'function' ? boş() : (typeof boş !== 'undefined' ? boş : { tag: 'boş', args: [] });"
  , "  for (var i = parts.length - 1; i >= 0; i -= 1) {"
  , "    out = typeof eki === 'function' ? eki(parts[i], out) : { tag: 'eki', args: [parts[i], out] };"
  , "  }"
  , "  return out;"
  , "};"
  , "var __kip_prim_cevreden_oku = (name) => {"
  , "  if (typeof process !== 'undefined' && process.env && Object.prototype.hasOwnProperty.call(process.env, name)) {"
  , "    return __kip_some(process.env[name]);"
  , "  }"
  , "  if (__kip_is_browser && typeof window !== 'undefined' && window.__kip_env && Object.prototype.hasOwnProperty.call(window.__kip_env, name)) {"
  , "    return __kip_some(String(window.__kip_env[name]));"
  , "  }"
  , "  return __kip_none();"
  , "};"
  , "var __kip_prim_yaz_dosya = (path, content) => {"
  , "  if (!__kip_require) return __kip_false();"
  , "  __kip_fs = __kip_fs || __kip_require('fs');"
  , "  try {"
  , "    __kip_fs.writeFileSync(path, content);"
  , "    return __kip_true();"
  , "  } catch (e) {"
  , "    return __kip_false();"
  , "  }"
  , "};"
  , ""
  , "// Primitive functions (can be overridden)"
  , "var yaz = (x) => {"
  , "  var val = __kip_is_float(x) ? x.value : x;"
  , "  var output = __kip_is_float(x) && Number.isInteger(val) ? String(val) + '.0' : val;"
  , "  if (__kip_is_browser && typeof window.__kip_write === 'function') {"
  , "    window.__kip_write(output);"
  , "  } else {"
  , "    console.log(output);"
  , "  }"
  , "  return typeof bitimlik === 'function' ? bitimlik() : bitimlik;"
  , "};"
  , "var çarpım = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) * __kip_num(b)) : (__kip_num(a) * __kip_num(b));"
  , "var fark = __kip_prim_fark;"
  , "var bölüm = (a, b) => {"
  , "  var av = __kip_num(a);"
  , "  var bv = __kip_num(b);"
  , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
  , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av / bv) : Math.trunc(av / bv);"
  , "};"
  , "var kalan = (a, b) => {"
  , "  var av = __kip_num(a);"
  , "  var bv = __kip_num(b);"
  , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
  , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av % bv) : (av % bv);"
  , "};"
  , "var karekök = (a) => __kip_float(Math.sqrt(__kip_num(a)) * 1.0);"
  , "var radyan = (a) => __kip_float(__kip_num(a) * Math.PI / 180);"
  , "var derece = (a) => __kip_float(__kip_num(a) * 180 / Math.PI);"
  , "var pi_sayısı = () => __kip_float(Math.PI);"
  , "var taban = (a) => Math.floor(__kip_num(a));"
  , "var tavan = (a) => Math.ceil(__kip_num(a));"
  , "var tam_sayı_ondalık_sayı_hali = (a) => __kip_float(a * 1.0);"
  , "var sayı_çek = (a, b) => {"
  , "  var lo = Math.min(a, b);"
  , "  var hi = Math.max(a, b);"
  , "  var range = hi - lo + 1;"
  , "  return lo + (__kip_rand() % range);"
  , "};"
  , "var __kip_prim_dizge_eşitlik = (a, b) => a === b ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_eşitlik = (a, b) => a === b ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_dizge_hal = (c) => c;"
  , "var __kip_prim_karakter_harflik = (c) => /^\\p{L}$/u.test(c) ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_rakamlık = (c) => /^\\p{Nd}$/u.test(c) ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_harf_rakamlık = (c) => /^[\\p{L}\\p{Nd}]$/u.test(c) ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_buyuk_harflik = (c) => /^\\p{Lu}$/u.test(c) ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_kucuk_harflik = (c) => /^\\p{Ll}$/u.test(c) ? __kip_true() : __kip_false();"
  , "var __kip_prim_karakter_boslukluk = (c) => /^\\s$/u.test(c) ? __kip_true() : __kip_false();"
  , "var eşitlik = (a, b) => __kip_num(a) === __kip_num(b) ? __kip_true() : __kip_false();"
  , "var küçüklük = (a, b) => __kip_num(a) < __kip_num(b) ? __kip_true() : __kip_false();"
  , "var küçük_eşitlik = (a, b) => __kip_num(a) <= __kip_num(b) ? __kip_true() : __kip_false();"
  , "var büyüklük = (a, b) => __kip_num(a) > __kip_num(b) ? __kip_true() : __kip_false();"
  , "var büyük_eşitlik = (a, b) => __kip_num(a) >= __kip_num(b) ? __kip_true() : __kip_false();"
  , "var dizge_hal = (n) => String(__kip_num(n));"
  , "var tam_sayı_hal = (s) => { const n = parseInt(s, 10); return isNaN(n) ? __kip_none() : __kip_some(n); };"
  , "var ondalık_sayı_hal = (s) => { if (typeof s === 'number') return __kip_float(s * 1.0); const n = parseFloat(s); return isNaN(n) ? __kip_none() : __kip_some(__kip_float(n)); };"
  , "var __kip_call = async (fn, args) => {"
  , "  if (typeof fn !== 'function') {"
  , "    throw new TypeError('Attempted to call a non-function');"
  , "  }"
  , "  if (args.length === 0) return await fn();"
  , "  if (fn.length > 0 && args.length < fn.length) {"
  , "    return (...rest) => __kip_call(fn, args.concat(rest));"
  , "  }"
  , "  if (fn.length > 0 && args.length > fn.length) {"
  , "    const head = args.slice(0, fn.length);"
  , "    const tail = args.slice(fn.length);"
  , "    const out = await fn(...head);"
  , "    return await __kip_call(out, tail);"
  , "  }"
  , "  return await fn(...args);"
  , "};"
  ]

-- | Runtime bindings that can be removed when not referenced by generated code.
--
-- Each entry is a binding name and the names of bindings it depends on.
-- Declaration text is taken from 'primitiveJsPrelude', its single source of truth.
primitiveJsPrunableSpecs :: [(Text, [Text], Text)]
primitiveJsPrunableSpecs =
  [ (name, dependencies, runtimeBindingSnippet name)
  | (name, dependencies) <- primitiveJsPrunableDependencies
  ]

primitiveJsPrunableDependencies :: [(Text, [Text])]
primitiveJsPrunableDependencies =
  [ ("doğru", [])
  , ("yanlış", [])
  , ("varlık", [])
  , ("yokluk", [])
  , ("bitimlik", [])
  , ("__kip_prim_ters", [])
  , ("__kip_prim_birleşim", [])
  , ("__kip_prim_uzunluk", [])
  , ("__kip_prim_öğe", ["varlık", "yokluk"])
  , ("__kip_prim_alış", [])
  , ("__kip_prim_bırakış", [])
  , ("__kip_prim_son", ["varlık", "yokluk"])
  , ("__kip_prim_bosluk", ["doğru", "yanlış"])
  , ("__kip_prim_satirlar", [])
  , ("__kip_prim_kelimeler", [])
  , ("__kip_prim_dizge_büyük_hal", [])
  , ("__kip_prim_dizge_küçük_hal", [])
  , ("__kip_prim_toplam", [])
  , ("__kip_prim_fark", [])
  , ("__kip_prim_oku_stdin", [])
  , ("__kip_prim_oku_dosya", ["varlık", "yokluk"])
  , ("__kip_prim_arguman_oku", [])
  , ("__kip_prim_cevreden_oku", ["varlık", "yokluk"])
  , ("__kip_prim_yaz_dosya", ["doğru", "yanlış"])
  , ("yaz", ["bitimlik"])
  , ("çarpım", [])
  , ("fark", ["__kip_prim_fark"])
  , ("bölüm", [])
  , ("kalan", [])
  , ("karekök", [])
  , ("radyan", [])
  , ("derece", [])
  , ("pi_sayısı", [])
  , ("taban", [])
  , ("tavan", [])
  , ("tam_sayı_ondalık_sayı_hali", [])
  , ("sayı_çek", [])
  , ("__kip_prim_dizge_eşitlik", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_eşitlik", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_dizge_hal", [])
  , ("__kip_prim_karakter_harflik", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_rakamlık", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_harf_rakamlık", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_buyuk_harflik", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_kucuk_harflik", ["doğru", "yanlış"])
  , ("__kip_prim_karakter_boslukluk", ["doğru", "yanlış"])
  , ("eşitlik", ["doğru", "yanlış"])
  , ("küçüklük", ["doğru", "yanlış"])
  , ("küçük_eşitlik", ["doğru", "yanlış"])
  , ("büyüklük", ["doğru", "yanlış"])
  , ("büyük_eşitlik", ["doğru", "yanlış"])
  , ("dizge_hal", [])
  , ("tam_sayı_hal", ["varlık", "yokluk"])
  , ("ondalık_sayı_hal", ["varlık", "yokluk"])
  , ("__kip_call", [])
  ]

runtimeBindingSnippet :: Text -> Text
runtimeBindingSnippet name =
  case dropWhile (not . T.isPrefixOf bindingPrefix) (T.lines primitiveJsPrelude) of
    [] -> error ("missing JavaScript runtime binding: " <> T.unpack name)
    firstLine : followingLines ->
      T.unlines (declarationLines firstLine followingLines)
  where
    bindingPrefix = "var " <> name <> " ="

    declarationLines firstLine followingLines
      | declarationEnds firstLine = [firstLine]
      | otherwise = firstLine : takeThroughEnd followingLines

    takeThroughEnd [] =
      error ("unterminated JavaScript runtime binding: " <> T.unpack name)
    takeThroughEnd (line : rest)
      | declarationEnds line = [line]
      | otherwise = line : takeThroughEnd rest

    declarationEnds line =
      line == T.stripStart line
        && ";" `T.isSuffixOf` T.stripEnd line
