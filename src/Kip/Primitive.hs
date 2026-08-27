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
  ( PrimitiveEvalOps(..)
  , primitiveEvalImpl
  , isImplementedPrimitive
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
  }

-- | A variant of a primitive function (for overloading)
data PrimitiveVariant = PrimitiveVariant
  { variantArity :: Int
    -- ^ Number of arguments for this variant
  , variantArgTypeCheck :: [Arg Ann] -> Bool
    -- ^ Predicate to check if argument types match this variant
  }

-- | Host callbacks needed to evaluate primitives.
data PrimitiveEvalOps m = PrimitiveEvalOps
  { peWriteText :: Text -> m ()
    -- ^ Write one text value to standard output.
  , peWriteInteger :: Integer -> m ()
    -- ^ Write one integer to standard output.
  , peWriteDouble :: Double -> m ()
    -- ^ Write one floating-point number to standard output.
  , peFlushStdout :: m ()
    -- ^ Flush standard output after an observable write.
  , peReadLine :: m Text
    -- ^ Read one line from standard input.
  , peReadFirstPath :: [FilePath] -> m (Maybe Text)
    -- ^ Read the first accessible file among a list of candidate paths.
  , peReadEnv :: Text -> m (Maybe Text)
    -- ^ Read an environment variable by name.
  , peGetCurrentFile :: m (Maybe FilePath)
    -- ^ Return the source file currently being evaluated, when known.
  , peGetArgs :: m [Text]
    -- ^ Return command-line arguments visible to the Kip program.
  , peWriteFileText :: FilePath -> Text -> m Bool
    -- ^ Write text to a file and report whether the operation succeeded.
  , peGetRandState :: m (Maybe Word32)
    -- ^ Read the optional deterministic random-generator state.
  , peSetRandState :: Word32 -> m ()
    -- ^ Store the deterministic random-generator state.
  , peLookupRandomSeed :: m (Maybe Integer)
    -- ^ Read a configured random seed when one is available.
  , peRandomRange :: Integer -> Integer -> m Integer
    -- ^ Produce a host-generated random integer in an inclusive range.
  }

-- | Helper to create a variant that accepts any types
anyTypes :: Int -- ^ Required primitive arity.
         -> PrimitiveVariant -- ^ Variant that accepts any argument types.
anyTypes n = PrimitiveVariant n (const True)

-- | Helper to create a variant that checks specific type constructors
withTypes :: Int -- ^ Required primitive arity.
          -> ([Ty Ann] -> Bool) -- ^ Predicate over argument types in source order.
          -> PrimitiveVariant -- ^ Variant using the supplied type predicate.
withTypes n check = PrimitiveVariant n (check . map snd)

-- | A unary variant with a type predicate.
oneType :: (Ty Ann -> Bool) -- ^ Predicate for the single argument type.
        -> PrimitiveVariant -- ^ Unary primitive variant.
oneType check = withTypes 1 (\case [ty] -> check ty; _ -> False)

-- | A binary variant with a predicate over both argument types.
twoTypes :: (Ty Ann -> Ty Ann -> Bool) -- ^ Predicate for the left and right argument types.
         -> PrimitiveVariant -- ^ Binary primitive variant.
twoTypes check = withTypes 2 (\case [left, right] -> check left right; _ -> False)

-- | Check if a type is an integer
isIntTy :: Ty Ann -- ^ Type to classify.
        -> Bool -- ^ 'True' for the primitive integer type.
isIntTy (TyInt _) = True
isIntTy _ = False

-- | Check if a type is a float
isFloatTy :: Ty Ann -- ^ Type to classify.
          -> Bool -- ^ 'True' for the primitive floating-point type.
isFloatTy (TyFloat _) = True
isFloatTy _ = False

-- | Check if a type is a string
isStringTy :: Ty Ann -- ^ Type to classify.
           -> Bool -- ^ 'True' for the primitive string type.
isStringTy (TyString _) = True
isStringTy _ = False

-- | Check if a type is a character
isCharTy :: Ty Ann -- ^ Type to classify.
         -> Bool -- ^ 'True' for the primitive character type.
isCharTy (TyChar _) = True
isCharTy _ = False

-- | Check for the set type identifier.
isSetIdent :: Identifier -- ^ Type identifier to classify.
           -> Bool -- ^ 'True' for the unqualified set type constructor.
isSetIdent (mods, name) = null mods && name == T.pack "küme"

-- | Check for the map/dictionary type identifier.
isMapIdent :: Identifier -- ^ Type identifier to classify.
           -> Bool -- ^ 'True' for the unqualified map type constructor.
isMapIdent (mods, name) = null mods && name == T.pack "sözlük"

-- | Check whether a type still contains unresolved type variables.
containsTyVar :: Ty Ann -- ^ Type tree to inspect.
              -> Bool -- ^ 'True' when any unresolved type variable occurs.
containsTyVar ty =
  case ty of
    TyVar {} -> True
    TyApp _ ctor args -> containsTyVar ctor || any containsTyVar args
    Arr _ d i -> containsTyVar d || containsTyVar i
    _ -> False

-- | Normalize type annotations for structural type equality checks.
normalizeTyAnn :: Ty Ann -- ^ Type whose annotations and primitive aliases are normalized.
               -> Ty Ann -- ^ Structurally comparable normalized type.
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
sameTy :: Ty Ann -- ^ First type to compare.
       -> Ty Ann -- ^ Second type to compare.
       -> Bool -- ^ 'True' when the normalized structures agree.
sameTy a b = normalizeTyAnn a == normalizeTyAnn b

-- | Extract set element type from @öğe küme'si@.
setElemTy :: Ty Ann -- ^ Possible set type.
          -> Maybe (Ty Ann) -- ^ Element type when the input is a set.
setElemTy ty =
  case normalizePrimTy ty of
    TyApp _ ctor [elemTy]
      | TyInd _ ident <- normalizePrimTy ctor
      , isSetIdent ident ->
          Just elemTy
    _ -> Nothing

-- | Check whether a type is @öğe küme'si@.
isSetTy :: Ty Ann -- ^ Type to classify.
        -> Bool -- ^ 'True' when the type is a set application.
isSetTy = isJust . setElemTy

-- | Extract key and value types from @anahtar'dan değer'e sözlük@.
mapKeyValTy :: Ty Ann -- ^ Possible map type.
            -> Maybe (Ty Ann, Ty Ann) -- ^ Key and value types when the input is a map.
mapKeyValTy ty =
  case normalizePrimTy ty of
    TyApp _ ctor [keyTy, valTy]
      | TyInd _ ident <- normalizePrimTy ctor
      , isMapIdent ident ->
          Just (keyTy, valTy)
    _ -> Nothing

-- | Check whether a type is @anahtar'dan değer'e sözlük@.
isMapTy :: Ty Ann -- ^ Type to classify.
        -> Bool -- ^ 'True' when the type is a map application.
isMapTy = isJust . mapKeyValTy

-- | Shared overload variants for common primitive argument shapes.
stringVariants, charVariants, intVariants, floatVariants, stringIntVariants, numericVariants :: [PrimitiveVariant]
stringVariants = [oneType isStringTy]
charVariants = [oneType isCharTy]
intVariants = [oneType isIntTy]
floatVariants = [oneType isFloatTy]
stringIntVariants = [twoTypes (\left right -> isStringTy left && isIntTy right)]
numericVariants =
  [ twoTypes (\left right -> isFloatTy left || isFloatTy right)
  , twoTypes (\left right -> isIntTy left && isIntTy right)
  ]

-- | All known primitive functions
allPrimitives :: [PrimitiveDef]
allPrimitives =
  [ PrimitiveDef ([], "yaz")
      [ oneType (\ty -> isIntTy ty || isFloatTy ty || isStringTy ty || isCharTy ty)
      , anyTypes 2  -- File write
      ]
      ["etki.kip"]

  , PrimitiveDef ([], "oku")
      [ anyTypes 0  -- stdin
      , oneType isStringTy  -- file read
      ]
      ["etki.kip"]

  , PrimitiveDef (["argüman"], "oku")
      [ anyTypes 0 ]
      ["etki.kip"]

  , PrimitiveDef (["çevreden"], "oku")
      stringVariants
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
      [ oneType isStringTy
      , oneType isCharTy
      ]
      ["dizge.kip", "karakter.kip"]

  , PrimitiveDef (["ondalık", "sayı"], "hal")
      [ oneType isStringTy
      , oneType isIntTy
      ]
      ["dizge.kip", "tam-sayı.kip"]

  , PrimitiveDef (["karakter"], "hal")
      intVariants
      ["karakter.kip"]

  , PrimitiveDef ([], "büyük")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef ([], "küçük")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef ([], "harflik")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef ([], "rakamlık")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef (["harf"], "rakamlık")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef (["büyük"], "harflik")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef (["küçük"], "harflik")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef ([], "boşlukluk")
      charVariants
      ["karakter.kip"]

  , PrimitiveDef ([], "ters")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "öğe")
      stringIntVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "alış")
      stringIntVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "bırakış")
      stringIntVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "son")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "boşluk")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "satırlar")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "kelimeler")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef (["büyük"], "hal")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef (["küçük"], "hal")
      stringVariants
      ["dizge.kip"]

  , PrimitiveDef ([], "toplam")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "çarpım")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "fark")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "bölüm")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "kalan")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["dizge"], "hal")
      [oneType (\ty -> isFloatTy ty || isIntTy ty || isCharTy ty)]
      ["tam-sayı.kip", "ondalık-sayı.kip", "karakter.kip"]

  , PrimitiveDef ([], "eşitlik")
      [ twoTypes (\left right -> isFloatTy left || isFloatTy right || isIntTy left && isIntTy right)
      , twoTypes (\left right -> isStringTy left && isStringTy right)
      , twoTypes (\left right -> isCharTy left && isCharTy right)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip", "dizge.kip", "karakter.kip"]

  , PrimitiveDef ([], "küçüklük")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["küçük"], "eşitlik")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "büyüklük")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["büyük"], "eşitlik")
      numericVariants
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef (["sayı"], "çek")
      [ anyTypes 2 ]
      ["etki.kip"]

  , PrimitiveDef ([], "karekök")
      floatVariants
      ["ondalık-sayı.kip"]

  , PrimitiveDef ([], "taban")
      floatVariants
      ["ondalık-sayı.kip"]

  , PrimitiveDef ([], "tavan")
      floatVariants
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

-- | Check if a primitive function signature is implemented.
isImplementedPrimitive :: Identifier -- ^ Primitive identifier to query.
                       -> [Arg Ann] -- ^ Typed arguments in source order.
                       -> Bool -- ^ 'True' when a matching implementation exists.
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
primFiles :: Identifier -- ^ Primitive identifier or its hyphenated spelling.
          -> [FilePath] -- ^ Standard-library files that declare the primitive.
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
primitiveEvalImpl :: Monad m
                  => PrimitiveEvalOps m -- ^ Host operations available to effectful primitives.
                  -> Maybe FilePath -- ^ File currently being evaluated, used to scope primitives.
                  -> Identifier -- ^ Primitive identifier to resolve.
                  -> [Arg Ann] -- ^ Typed arguments used for overload selection.
                  -> Maybe ([Exp Ann] -> m (Exp Ann)) -- ^ Runtime implementation when the signature is supported.
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
      | [(_, TyInt _)] <- args -> Just primIntToChar
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
      | [(_, TyChar _)] <- args -> Just primCharIsAlphaNum
      | otherwise -> Nothing
    (["büyük"], "harflik")
      | [(_, TyChar _)] <- args -> Just primCharIsUpper
      | otherwise -> Nothing
    (["küçük"], "harflik")
      | [(_, TyChar _)] <- args -> Just primCharIsLower
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
      | [(_, TyString _)] <- args -> Just primStringUpper
      | otherwise -> Nothing
    (["küçük"], "hal")
      | [(_, TyString _)] <- args -> Just primStringLower
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

-- | Implement the output primitive for supported literal values.
primWrite :: Monad m
          => PrimitiveEvalOps m -- ^ Host callbacks used for output and flushing.
          -> [Exp Ann] -- ^ Evaluated primitive arguments.
          -> m (Exp Ann) -- ^ Unit value or an unresolved fallback application.
primWrite ops args =
  case args of
    [StrLit _ s] -> peWriteText ops s >> peFlushStdout ops >> pure unitExp
    [IntLit _ n] -> peWriteInteger ops n >> peFlushStdout ops >> pure unitExp
    [FloatLit _ n] -> peWriteDouble ops n >> peFlushStdout ops >> pure unitExp
    [CharLit _ c] -> peWriteText ops (T.singleton c) >> peFlushStdout ops >> pure unitExp
    _ -> pure (fallbackApp ([], "yaz") args)

-- | Implement the zero-argument standard-input primitive.
primRead :: Monad m
         => PrimitiveEvalOps m -- ^ Host callbacks used to read input.
         -> [Exp Ann] -- ^ Evaluated primitive arguments.
         -> m (Exp Ann) -- ^ Read string or an unresolved fallback application.
primRead ops args =
  case args of
    [] -> StrLit (mkAnn Nom NoSpan) <$> peReadLine ops
    _ -> pure (fallbackApp ([], "oku") args)

-- | Read a text file relative to the current module and its parents.
primReadFile :: Monad m
             => PrimitiveEvalOps m -- ^ Host callbacks for path context and file reads.
             -> [Exp Ann] -- ^ Evaluated path argument.
             -> m (Exp Ann) -- ^ Optional string value or an unresolved fallback.
primReadFile ops args =
  case args of
    [StrLit _ path] -> do
      mPath <- peGetCurrentFile ops
      content <- peReadFirstPath ops (resolveReadCandidates mPath path)
      case content of
        Nothing -> pure noneExp
        Just text -> pure (someExp (StrLit (mkAnn Nom NoSpan) text))
    _ -> pure (fallbackApp ([], "oku") args)

-- | Return command-line arguments as a Kip list of strings.
primReadArgs :: Monad m
             => PrimitiveEvalOps m -- ^ Host callback that supplies process arguments.
             -> [Exp Ann] -- ^ Expected empty primitive argument list.
             -> m (Exp Ann) -- ^ Argument list or an unresolved fallback.
primReadArgs ops args =
  case args of
    [] -> textListExp (mkAnn Nom NoSpan) <$> peGetArgs ops
    _ -> pure (fallbackApp (["argüman"], "oku") args)

-- | Read an environment variable into a Kip optional string.
primReadEnv :: Monad m
            => PrimitiveEvalOps m -- ^ Host callback used to query the environment.
            -> [Exp Ann] -- ^ Evaluated environment-variable name.
            -> m (Exp Ann) -- ^ Optional string value or an unresolved fallback.
primReadEnv ops args =
  case args of
    [StrLit _ key] -> do
      val <- peReadEnv ops key
      case val of
        Nothing -> pure noneExp
        Just txt -> pure (someExp (StrLit (mkAnn Nom NoSpan) txt))
    _ -> pure (fallbackApp (["çevreden"], "oku") args)

-- | Write text to a file and return a Kip Boolean indicating success.
primWriteFile :: Monad m
              => PrimitiveEvalOps m -- ^ Host callback used to write the file.
              -> [Exp Ann] -- ^ Evaluated path and content arguments.
              -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primWriteFile ops args =
  case args of
    [StrLit _ path, StrLit _ content] -> do
      ok <- peWriteFileText ops (T.unpack path) content
      pure (boolExp ok)
    _ -> pure (fallbackApp ([], "yaz") args)

-- | Compute the character length of a string literal.
primStringLength :: Monad m
                 => Text -- ^ Primitive name retained for fallback reconstruction.
                 -> [Exp Ann] -- ^ Evaluated string argument.
                 -> m (Exp Ann) -- ^ Integer length or an unresolved fallback.
primStringLength fname args =
  case args of
    [StrLit ann s] -> pure (IntLit ann (fromIntegral (T.length s)))
    _ -> pure (fallbackApp ([], fname) args)

-- | Concatenate two string literals.
primStringConcat :: Monad m
                 => Text -- ^ Primitive name retained for fallback reconstruction.
                 -> [Exp Ann] -- ^ Evaluated left and right strings.
                 -> m (Exp Ann) -- ^ Concatenated string or an unresolved fallback.
primStringConcat fname args =
  case args of
    [StrLit ann a, StrLit _ b] -> pure (StrLit ann (a <> b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Reverse the characters in a string literal.
primStringReverse :: Monad m
                  => Text -- ^ Primitive name retained for fallback reconstruction.
                  -> [Exp Ann] -- ^ Evaluated string argument.
                  -> m (Exp Ann) -- ^ Reversed string or an unresolved fallback.
primStringReverse fname args =
  case args of
    [StrLit ann s] -> pure (StrLit ann (T.reverse s))
    _ -> pure (fallbackApp ([], fname) args)

-- | Look up a character by zero-based string index.
primStringCharAt :: Monad m
                 => Text -- ^ Primitive name retained for fallback reconstruction.
                 -> [Exp Ann] -- ^ Evaluated string and integer index.
                 -> m (Exp Ann) -- ^ Optional character or an unresolved fallback.
primStringCharAt fname args =
  case args of
    [StrLit ann s, IntLit _ n]
      | n >= 0 && n < fromIntegral (T.length s) ->
          pure (someExp (CharLit ann (T.index s (fromIntegral n))))
      | otherwise -> pure noneExp
    _ -> pure (fallbackApp ([], fname) args)

-- | Take a nonnegative number of leading characters from a string.
primStringTake :: Monad m
               => Text -- ^ Primitive name retained for fallback reconstruction.
               -> [Exp Ann] -- ^ Evaluated string and requested count.
               -> m (Exp Ann) -- ^ Resulting prefix or an unresolved fallback.
primStringTake fname args =
  case args of
    [StrLit ann s, IntLit _ n] ->
      pure (StrLit ann (T.take (fromIntegral (max 0 n)) s))
    _ -> pure (fallbackApp ([], fname) args)

-- | Drop a nonnegative number of leading characters from a string.
primStringDrop :: Monad m
               => Text -- ^ Primitive name retained for fallback reconstruction.
               -> [Exp Ann] -- ^ Evaluated string and requested count.
               -> m (Exp Ann) -- ^ Remaining suffix or an unresolved fallback.
primStringDrop fname args =
  case args of
    [StrLit ann s, IntLit _ n] ->
      pure (StrLit ann (T.drop (fromIntegral (max 0 n)) s))
    _ -> pure (fallbackApp ([], fname) args)

-- | Return the final character of a nonempty string.
primStringLastChar :: Monad m
                   => Text -- ^ Primitive name retained for fallback reconstruction.
                   -> [Exp Ann] -- ^ Evaluated string argument.
                   -> m (Exp Ann) -- ^ Optional final character or an unresolved fallback.
primStringLastChar fname args =
  case args of
    [StrLit ann s]
      | T.null s -> pure noneExp
      | otherwise -> pure (someExp (CharLit ann (T.last s)))
    _ -> pure (fallbackApp ([], fname) args)

-- | Test whether a string is empty.
primStringIsEmpty :: Monad m
                  => Text -- ^ Primitive name retained for fallback reconstruction.
                  -> [Exp Ann] -- ^ Evaluated string argument.
                  -> m (Exp Ann) -- ^ Kip Boolean or an unresolved fallback.
primStringIsEmpty fname args =
  case args of
    [StrLit _ s] -> pure (boolExp (T.null s))
    _ -> pure (fallbackApp ([], fname) args)

-- | Split a string into a Kip list of lines.
primStringLines :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated string argument.
                -> m (Exp Ann) -- ^ List of line strings or an unresolved fallback.
primStringLines fname args =
  case args of
    [StrLit ann s] -> pure (textListExp ann (T.lines s))
    _ -> pure (fallbackApp ([], fname) args)

-- | Split a string into a Kip list of whitespace-delimited words.
primStringWords :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated string argument.
                -> m (Exp Ann) -- ^ List of word strings or an unresolved fallback.
primStringWords fname args =
  case args of
    [StrLit ann s] -> pure (textListExp ann (T.words s))
    _ -> pure (fallbackApp ([], fname) args)

-- | Convert a string to uppercase.
primStringUpper :: Monad m
                => [Exp Ann] -- ^ Evaluated string argument.
                -> m (Exp Ann) -- ^ Uppercase string or an unresolved fallback.
primStringUpper args =
  case args of
    [StrLit ann s] -> pure (StrLit ann (T.toUpper s))
    _ -> pure (fallbackApp (["büyük"], "hal") args)

-- | Convert a string to lowercase.
primStringLower :: Monad m
                => [Exp Ann] -- ^ Evaluated string argument.
                -> m (Exp Ann) -- ^ Lowercase string or an unresolved fallback.
primStringLower args =
  case args of
    [StrLit ann s] -> pure (StrLit ann (T.toLower s))
    _ -> pure (fallbackApp (["küçük"], "hal") args)

-- | Parse a decimal integer from a string.
primStringToInt :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated string argument.
                -> m (Exp Ann) -- ^ Optional integer or an unresolved fallback.
primStringToInt fname args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n -> pure (someExp (IntLit ann n))
        Nothing -> pure noneExp
    _ -> pure (fallbackApp (["tam", "sayı"], fname) args)

-- | Parse a floating-point number from a string.
primStringToFloat :: Monad m
                  => Text -- ^ Primitive name retained for fallback reconstruction.
                  -> [Exp Ann] -- ^ Evaluated string argument.
                  -> m (Exp Ann) -- ^ Optional float or an unresolved fallback.
primStringToFloat fname args =
  case args of
    [StrLit ann s] ->
      case readMaybe (T.unpack s) of
        Just n -> pure (someExp (FloatLit ann n))
        _ -> pure noneExp
    _ -> pure (fallbackApp (["ondalık", "sayı"], fname) args)

-- | Apply a strict binary operation to integer literals.
primIntBin :: Monad m
           => Text -- ^ Primitive name retained for fallback reconstruction.
           -> (Integer -> Integer -> Integer) -- ^ Host operation on the two values.
           -> [Exp Ann] -- ^ Evaluated integer arguments.
           -> m (Exp Ann) -- ^ Integer result or an unresolved fallback.
primIntBin fname op args =
  case args of
    [IntLit ann a, IntLit _ b] -> pure (IntLit ann (op a b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Apply a strict binary operation to floating-point literals.
primFloatBin :: Monad m
             => Text -- ^ Primitive name retained for fallback reconstruction.
             -> (Double -> Double -> Double) -- ^ Host operation on the two values.
             -> [Exp Ann] -- ^ Evaluated floating-point arguments.
             -> m (Exp Ann) -- ^ Floating-point result or an unresolved fallback.
primFloatBin fname op args =
  case args of
    [FloatLit ann a, FloatLit _ b] -> pure (FloatLit ann (op a b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Divide integer literals, defining division by zero as zero.
primIntDiv :: Monad m
           => Text -- ^ Primitive name retained for fallback reconstruction.
           -> [Exp Ann] -- ^ Evaluated dividend and divisor.
           -> m (Exp Ann) -- ^ Integer quotient or an unresolved fallback.
primIntDiv fname args =
  case args of
    [IntLit ann a, IntLit _ b] -> pure (IntLit ann (if b == 0 then 0 else a `div` b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Divide floating-point literals, defining division by zero as zero.
primFloatDiv :: Monad m
             => Text -- ^ Primitive name retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Evaluated dividend and divisor.
             -> m (Exp Ann) -- ^ Floating-point quotient or an unresolved fallback.
primFloatDiv fname args =
  case args of
    [FloatLit ann a, FloatLit _ b] -> pure (FloatLit ann (if b == 0 then 0 else a / b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Compute integer remainder, defining a zero divisor result as zero.
primIntMod :: Monad m
           => Text -- ^ Primitive name retained for fallback reconstruction.
           -> [Exp Ann] -- ^ Evaluated dividend and divisor.
           -> m (Exp Ann) -- ^ Integer remainder or an unresolved fallback.
primIntMod fname args =
  case args of
    [IntLit ann a, IntLit _ b] -> pure (IntLit ann (if b == 0 then 0 else a `mod` b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Compute floating-point modulus, defining a zero divisor result as zero.
primFloatMod :: Monad m
             => Text -- ^ Primitive name retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Evaluated dividend and divisor.
             -> m (Exp Ann) -- ^ Floating-point modulus or an unresolved fallback.
primFloatMod fname args =
  case args of
    [FloatLit ann a, FloatLit _ b] -> pure (FloatLit ann (if b == 0 then 0 else mod' a b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Generate an integer in an inclusive range using deterministic state when configured.
primIntRandom :: Monad m
              => PrimitiveEvalOps m -- ^ Host random-state and entropy callbacks.
              -> [Text] -- ^ Namespace components of the primitive name.
              -> Text -- ^ Root component of the primitive name.
              -> [Exp Ann] -- ^ Evaluated lower and upper bounds.
              -> m (Exp Ann) -- ^ Random integer or an unresolved fallback.
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

-- | Compare two integer literals and return a Kip Boolean.
primIntCmp :: Monad m
           => Text -- ^ Primitive name retained for fallback reconstruction.
           -> (Integer -> Integer -> Bool) -- ^ Host comparison to perform.
           -> [Exp Ann] -- ^ Evaluated integer operands.
           -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primIntCmp fname op args =
  case args of
    [IntLit _ a, IntLit _ b] -> pure (boolExp (op a b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Compare two floating-point literals and return a Kip Boolean.
primFloatCmp :: Monad m
             => Text -- ^ Primitive name retained for fallback reconstruction.
             -> (Double -> Double -> Bool) -- ^ Host comparison to perform.
             -> [Exp Ann] -- ^ Evaluated floating-point operands.
             -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primFloatCmp fname op args =
  case args of
    [FloatLit _ a, FloatLit _ b] -> pure (boolExp (op a b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Compare two string literals for equality.
primStringEq :: Monad m
             => Text -- ^ Primitive name retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Evaluated string operands.
             -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primStringEq fname args =
  case args of
    [StrLit _ a, StrLit _ b] -> pure (boolExp (a == b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Compare two character literals for equality.
primCharEq :: Monad m
           => Text -- ^ Primitive name retained for fallback reconstruction.
           -> [Exp Ann] -- ^ Evaluated character operands.
           -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharEq fname args =
  case args of
    [CharLit _ a, CharLit _ b] -> pure (boolExp (a == b))
    _ -> pure (fallbackApp ([], fname) args)

-- | Convert a character literal to a singleton string.
primCharToString :: Monad m
                 => Text -- ^ Primitive name retained for fallback reconstruction.
                 -> [Exp Ann] -- ^ Evaluated character argument.
                 -> m (Exp Ann) -- ^ Singleton string or an unresolved fallback.
primCharToString fname args =
  case args of
    [CharLit ann c] -> pure (StrLit ann (T.singleton c))
    _ -> pure (fallbackApp (["dizge"], fname) args)

-- | Convert a character literal to its Unicode code point.
primCharToInt :: Monad m
              => Text -- ^ Primitive name retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated character argument.
              -> m (Exp Ann) -- ^ Integer code point or an unresolved fallback.
primCharToInt fname args =
  case args of
    [CharLit ann c] -> pure (IntLit ann (toInteger (ord c)))
    _ -> pure (fallbackApp (["tam", "sayı"], fname) args)

-- | Convert a valid Unicode code point to a character.
primIntToChar :: Monad m
              => [Exp Ann] -- ^ Evaluated integer code point.
              -> m (Exp Ann) -- ^ Optional character or an unresolved fallback.
primIntToChar args =
  case args of
    [IntLit ann n]
      | n >= 0 && n <= 0x10FFFF ->
          pure (someExp (CharLit ann (chr (fromInteger n))))
      | otherwise ->
          pure noneExp
    _ -> pure (fallbackApp (["karakter"], "hal") args)

-- | Convert a character to uppercase.
primCharUpper :: Monad m
              => Text -- ^ Primitive name retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated character argument.
              -> m (Exp Ann) -- ^ Uppercase character or an unresolved fallback.
primCharUpper fname args =
  case args of
    [CharLit ann c] -> pure (CharLit ann (toUpper c))
    _ -> pure (fallbackApp ([], fname) args)

-- | Convert a character to lowercase.
primCharLower :: Monad m
              => Text -- ^ Primitive name retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated character argument.
              -> m (Exp Ann) -- ^ Lowercase character or an unresolved fallback.
primCharLower fname args =
  case args of
    [CharLit ann c] -> pure (CharLit ann (toLower c))
    _ -> pure (fallbackApp ([], fname) args)

-- | Test whether a character is alphabetic.
primCharIsAlpha :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated character argument.
                -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharIsAlpha fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isAlpha c))
    _ -> pure (fallbackApp ([], fname) args)

-- | Test whether a character is a decimal digit.
primCharIsDigit :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated character argument.
                -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharIsDigit fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isDigit c))
    _ -> pure (fallbackApp ([], fname) args)

-- | Test whether a character is alphanumeric.
primCharIsAlphaNum :: Monad m
                   => [Exp Ann] -- ^ Evaluated character argument.
                   -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharIsAlphaNum args =
  case args of
    [CharLit _ c] -> pure (boolExp (isAlphaNum c))
    _ -> pure (fallbackApp (["harf"], "rakamlık") args)

-- | Test whether a character is uppercase.
primCharIsUpper :: Monad m
                => [Exp Ann] -- ^ Evaluated character argument.
                -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharIsUpper args =
  case args of
    [CharLit _ c] -> pure (boolExp (isUpper c))
    _ -> pure (fallbackApp (["büyük"], "harflik") args)

-- | Test whether a character is lowercase.
primCharIsLower :: Monad m
                => [Exp Ann] -- ^ Evaluated character argument.
                -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharIsLower args =
  case args of
    [CharLit _ c] -> pure (boolExp (isLower c))
    _ -> pure (fallbackApp (["küçük"], "harflik") args)

-- | Test whether a character is whitespace.
primCharIsSpace :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated character argument.
                -> m (Exp Ann) -- ^ Boolean result or an unresolved fallback.
primCharIsSpace fname args =
  case args of
    [CharLit _ c] -> pure (boolExp (isSpace c))
    _ -> pure (fallbackApp ([], fname) args)

-- | Render an integer literal as text.
primIntToString :: Monad m
                => Text -- ^ Primitive name retained for fallback reconstruction.
                -> [Exp Ann] -- ^ Evaluated integer argument.
                -> m (Exp Ann) -- ^ String representation or an unresolved fallback.
primIntToString fname args =
  case args of
    [IntLit ann n] -> pure (StrLit ann (T.pack (show n)))
    _ -> pure (fallbackApp (["dizge"], fname) args)

-- | Render a floating-point literal as text.
primFloatToString :: Monad m
                  => Text -- ^ Primitive name retained for fallback reconstruction.
                  -> [Exp Ann] -- ^ Evaluated floating-point argument.
                  -> m (Exp Ann) -- ^ String representation or an unresolved fallback.
primFloatToString fname args =
  case args of
    [FloatLit ann n] -> pure (StrLit ann (T.pack (show n)))
    _ -> pure (fallbackApp (["dizge"], fname) args)

-- | Convert an integer literal to floating point.
primIntToFloat :: Monad m
               => Text -- ^ Primitive name retained for fallback reconstruction.
               -> [Exp Ann] -- ^ Evaluated integer argument.
               -> m (Exp Ann) -- ^ Floating-point value or an unresolved fallback.
primIntToFloat fname args =
  case args of
    [IntLit ann n] -> pure (FloatLit ann (fromIntegral n))
    _ -> pure (fallbackApp (["tam", "sayı"], fname) args)

-- | Compute the square root of a floating-point literal.
primFloatSqrt :: Monad m
              => Text -- ^ Primitive name retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated floating-point argument.
              -> m (Exp Ann) -- ^ Square root or an unresolved fallback.
primFloatSqrt fname args =
  case args of
    [FloatLit ann n] -> pure (FloatLit ann (sqrt n))
    _ -> pure (fallbackApp ([], fname) args)

-- | Round a floating-point literal down to an integer.
primFloatFloor :: Monad m
               => Text -- ^ Primitive name retained for fallback reconstruction.
               -> [Exp Ann] -- ^ Evaluated floating-point argument.
               -> m (Exp Ann) -- ^ Integer floor or an unresolved fallback.
primFloatFloor fname args =
  case args of
    [FloatLit ann n] -> pure (IntLit ann (floor n))
    _ -> pure (fallbackApp ([], fname) args)

-- | Round a floating-point literal up to an integer.
primFloatCeiling :: Monad m
                 => Text -- ^ Primitive name retained for fallback reconstruction.
                 -> [Exp Ann] -- ^ Evaluated floating-point argument.
                 -> m (Exp Ann) -- ^ Integer ceiling or an unresolved fallback.
primFloatCeiling fname args =
  case args of
    [FloatLit ann n] -> pure (IntLit ann (ceiling n))
    _ -> pure (fallbackApp ([], fname) args)

-- | Canonicalized keys and representative values for a runtime set.
data SetRepr = SetRepr
  { setKeys :: Set.Set Text
    -- ^ Ordered membership keys derived from normalized element structure.
  , setElems :: Map.Map Text (Exp Ann)
    -- ^ Representative normalized expression for each membership key.
  }

-- | Construct an empty runtime set.
primSetEmpty :: Monad m
             => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Expected empty primitive argument list.
             -> m (Exp Ann) -- ^ Empty set or an unresolved fallback.
primSetEmpty ident args =
  case args of
    [] -> pure (setExpFromSetRepr emptySetRepr)
    _ -> pure (fallbackApp ident args)

-- | Insert or replace a normalized element in a runtime set.
primSetInsert :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated set and element arguments.
              -> m (Exp Ann) -- ^ Updated set or an unresolved fallback.
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

-- | Delete a normalized element from a runtime set.
primSetDelete :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated set and element arguments.
              -> m (Exp Ann) -- ^ Updated set or an unresolved fallback.
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

-- | Test membership of a normalized element in a runtime set.
primSetMember :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated set and element arguments.
              -> m (Exp Ann) -- ^ Boolean membership result or an unresolved fallback.
primSetMember ident args =
  case args of
    [setVal, elemVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          let key = setElemKey (normalizeSetValue elemVal)
          in pure (boolExp (Set.member key (setKeys setRepr)))
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

-- | Return the number of distinct elements in a runtime set.
primSetSize :: Monad m
            => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
            -> [Exp Ann] -- ^ Evaluated set argument.
            -> m (Exp Ann) -- ^ Integer size or an unresolved fallback.
primSetSize ident args =
  case args of
    [setVal] ->
      case setReprFromExp setVal of
        Just setRepr ->
          pure (IntLit (mkAnn Nom NoSpan) (fromIntegral (Set.size (setKeys setRepr))))
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

-- | Form the union of two runtime sets.
primSetUnion :: Monad m
             => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Evaluated left and right sets.
             -> m (Exp Ann) -- ^ Union set or an unresolved fallback.
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

-- | Convert a runtime set to a deterministically ordered Kip list.
primSetToList :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated set argument.
              -> m (Exp Ann) -- ^ List of elements or an unresolved fallback.
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

-- | Empty canonical runtime-set representation.
emptySetRepr :: SetRepr
emptySetRepr = SetRepr Set.empty Map.empty

-- | Decode either an internal set literal or a Kip list into set form.
setReprFromExp :: Exp Ann -- ^ Runtime expression to decode.
               -> Maybe SetRepr -- ^ Canonical set representation when recognized.
setReprFromExp setVal =
  case setVal of
    SetLit _ entries ->
      Just
        SetRepr
          { setKeys = Set.fromList (Map.keys entries)
          , setElems = entries
          }
    _ -> setReprFromElems <$> expToList setVal

-- | Build a canonical set representation from element expressions.
setReprFromElems :: [Exp Ann] -- ^ Elements to normalize and deduplicate.
                 -> SetRepr -- ^ Canonical set representation.
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

-- | Encode a canonical set representation as an internal set literal.
setExpFromSetRepr :: SetRepr -- ^ Canonical set representation.
                  -> Exp Ann -- ^ Runtime set literal.
setExpFromSetRepr setRepr =
  SetLit
    (mkAnn Nom NoSpan)
    (Map.fromAscList
      [ (key, elemVal)
      | key <- Set.toAscList (setKeys setRepr)
      , Just elemVal <- [Map.lookup key (setElems setRepr)]
      ])

-- | Derive a stable structural key for one set or map key expression.
setElemKey :: Exp Ann -- ^ Element expression to normalize and key.
           -> Text -- ^ Textual structural key.
setElemKey = T.pack . show . normalizeSetValue

-- | Remove source annotations and candidate metadata before structural keying.
normalizeSetValue :: Exp Ann -- ^ Runtime value to canonicalize.
                  -> Exp Ann -- ^ Structurally equivalent annotation-free value.
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

-- | Normalize every annotation-bearing component of a match clause.
normalizeSetClause :: Clause Ann -- ^ Clause nested in a structural value.
                   -> Clause Ann -- ^ Canonicalized clause.
normalizeSetClause (Clause pat bodyExpr) =
  Clause (normalizeSetPat pat) (normalizeSetValue bodyExpr)

-- | Normalize every annotation in a pattern used by a structural value.
normalizeSetPat :: Pat Ann -- ^ Pattern to canonicalize.
                -> Pat Ann -- ^ Annotation-free structural pattern.
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

-- | Decode a runtime expression encoded with Kip list constructors.
expToList :: Exp Ann -- ^ Possible list expression.
          -> Maybe [Exp Ann] -- ^ Elements when the complete list shape is recognized.
expToList exp'
  | isListNilExp exp' = Just []
  | otherwise =
      case listConsArgs exp' of
        Just (x, xs) -> (x :) <$> expToList xs
        Nothing -> Nothing

-- | Recognize the empty-list constructor through its resolved or candidate name.
isListNilExp :: Exp Ann -- ^ Expression to classify.
             -> Bool -- ^ 'True' for the empty-list constructor.
isListNilExp exp' =
  case exp' of
    Var _ name candidates ->
      name == ([], "boş") || any ((== ([], "boş")) . fst) candidates
    _ -> False

-- | Extract the head and tail of a list-constructor application.
listConsArgs :: Exp Ann -- ^ Possible list-cons expression.
             -> Maybe (Exp Ann, Exp Ann) -- ^ Head and tail when recognized.
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
    -- ^ Stable key order used when converting the map to a list.
  , mapElems :: Map.Map Text (Exp Ann, Exp Ann)
    -- ^ Normalized key expressions and associated values by structural key.
  }

-- | Construct an empty runtime map.
primMapEmpty :: Monad m
             => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Expected empty primitive argument list.
             -> m (Exp Ann) -- ^ Empty map or an unresolved fallback.
primMapEmpty ident args =
  case args of
    [] -> pure (mapExpFromMapRepr emptyMapRepr)
    _ -> pure (fallbackApp ident args)

-- | Insert or replace a key/value entry in a runtime map.
primMapInsert :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated map, key, and value arguments.
              -> m (Exp Ann) -- ^ Updated map or an unresolved fallback.
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

-- | Delete a key and its value from a runtime map.
primMapDelete :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated map and key arguments.
              -> m (Exp Ann) -- ^ Updated map or an unresolved fallback.
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

-- | Look up a normalized key in a runtime map.
primMapLookup :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated map and key arguments.
              -> m (Exp Ann) -- ^ Optional value or an unresolved fallback.
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

-- | Return the number of entries in a runtime map.
primMapSize :: Monad m
            => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
            -> [Exp Ann] -- ^ Evaluated map argument.
            -> m (Exp Ann) -- ^ Integer size or an unresolved fallback.
primMapSize ident args =
  case args of
    [mapVal] ->
      case mapReprFromExp mapVal of
        Just repr ->
          pure (IntLit (mkAnn Nom NoSpan) (fromIntegral (Map.size (mapElems repr))))
        Nothing -> pure (fallbackApp ident args)
    _ -> pure (fallbackApp ident args)

-- | Form a right-biased union of two runtime maps.
primMapUnion :: Monad m
             => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
             -> [Exp Ann] -- ^ Evaluated left and right maps.
             -> m (Exp Ann) -- ^ Union map or an unresolved fallback.
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


-- | Convert a runtime map to a Kip list of key/value pairs.
primMapToList :: Monad m
              => Identifier -- ^ Primitive identifier retained for fallback reconstruction.
              -> [Exp Ann] -- ^ Evaluated map argument.
              -> m (Exp Ann) -- ^ Ordered list of pairs or an unresolved fallback.
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

-- | Construct a Kip pair expression from a key and value.
pairExp :: Exp Ann -- ^ First pair component.
        -> Exp Ann -- ^ Second pair component.
        -> Exp Ann -- ^ Pair-constructor application.
pairExp a b =
  App
    (mkAnn Nom NoSpan)
    (Var (mkAnn P3s NoSpan) ([], "ikilisi") [(([], "ikilisi"), P3s)])
    [a, b]

-- | Empty canonical runtime-map representation.
emptyMapRepr :: MapRepr
emptyMapRepr = MapRepr [] Map.empty

-- | Decode an internal map literal into its ordered representation.
mapReprFromExp :: Exp Ann -- ^ Runtime expression to decode.
               -> Maybe MapRepr -- ^ Canonical map representation when recognized.
mapReprFromExp mapVal =
  case mapVal of
    MapLit _ entries ->
      let keys = Map.keys entries
      in Just MapRepr
           { mapKeys = keys
           , mapElems = entries
           }
    _ -> Nothing

-- | Encode a canonical map representation as an internal map literal.
mapExpFromMapRepr :: MapRepr -- ^ Canonical map representation.
                  -> Exp Ann -- ^ Runtime map literal.
mapExpFromMapRepr repr =
  MapLit
    (mkAnn Nom NoSpan)
    (Map.fromList
      [ (key, entry)
      | key <- mapKeys repr
      , Just entry <- [Map.lookup key (mapElems repr)]
      ])


-- | Resolve a requested read path against the current module and parent directories.
resolveReadCandidates :: Maybe FilePath -- ^ Current source file, when known.
                      -> Text -- ^ Requested path from the Kip program.
                      -> [FilePath] -- ^ Candidate filesystem paths in lookup order.
resolveReadCandidates mPath path =
  let raw = T.unpack path
  in case mPath of
       Just base | isRelative raw ->
         let start = takeDirectory base
         in map (</> raw) (parentDirs start)
       _ -> [raw]

-- | Enumerate a directory and all of its ancestors through the filesystem root.
parentDirs :: FilePath -- ^ Starting directory.
           -> [FilePath] -- ^ Starting directory followed by successive parents.
parentDirs dir =
  let parent = takeDirectory dir
  in if parent == dir then [dir] else dir : parentDirs parent

-- | Encode host text values as a Kip list of string literals.
textListExp :: Ann -- ^ Annotation assigned to each string literal.
            -> [Text] -- ^ Text values to encode.
            -> Exp Ann -- ^ Kip list expression.
textListExp ann =
  foldr (listConsExp . StrLit ann) listNilExp

-- | Runtime expression for the empty-list constructor.
listNilExp :: Exp Ann
listNilExp = Var (mkAnn Nom NoSpan) ([], "boş") [(([], "boş"), Nom)]

-- | Construct a Kip list cell.
listConsExp :: Exp Ann -- ^ Head element.
            -> Exp Ann -- ^ Tail list.
            -> Exp Ann -- ^ List-constructor application.
listConsExp x xs =
  App
    (mkAnn Nom NoSpan)
    (Var (mkAnn P3s NoSpan) ([], "eki") [(([], "eki"), P3s)])
    [x, xs]

-- | Runtime expression for Kip's unit value.
unitExp :: Exp Ann
unitExp = Var (mkAnn Nom NoSpan) ([], "bitimlik") [(([], "bitimlik"), Nom)]

-- | Runtime expression for Kip's absent optional value.
noneExp :: Exp Ann
noneExp = Var (mkAnn Nom NoSpan) ([], "yokluk") [(([], "yokluk"), Nom)]

-- | Wrap a runtime expression in Kip's present optional constructor.
someExp :: Exp Ann -- ^ Value to wrap.
        -> Exp Ann -- ^ Present optional expression.
someExp v = App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) ([], "varlık") [(([], "varlık"), Nom)]) [v]

-- | Convert a host Boolean to Kip's Boolean constructor.
boolExp :: Bool -- ^ Host truth value.
        -> Exp Ann -- ^ Kip Boolean expression.
boolExp b =
  let name = if b then ([], "doğru") else ([], "yanlış")
  in Var (mkAnn Nom NoSpan) name [(name, Nom)]

-- | Reconstruct an unresolved primitive application when runtime shapes do not match.
fallbackApp :: Identifier -- ^ Primitive function name.
            -> [Exp Ann] -- ^ Evaluated arguments to retain.
            -> Exp Ann -- ^ Ordinary application expression for later handling.
fallbackApp name = App (mkAnn Nom NoSpan) (Var (mkAnn Nom NoSpan) name [])

-- | Complete JavaScript runtime prelude used by generated Kip programs.
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

-- | Dependency graph for runtime bindings that the JavaScript backend may prune.
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

-- | Extract one complete runtime binding declaration from the JavaScript prelude.
runtimeBindingSnippet :: Text -- ^ JavaScript binding name to locate.
                      -> Text -- ^ Declaration text, including continuation lines.
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
