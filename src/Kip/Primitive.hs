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
import Data.Fixed (mod')
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Word (Word32)
import System.FilePath (isRelative, takeDirectory, takeFileName, (</>))
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
  , peGetCurrentFile :: m (Maybe FilePath)
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

-- | Check for the integer type identifier.
isIntIdent :: Identifier -> Bool
isIntIdent (mods, name) = mods == [T.pack "tam"] && name == T.pack "sayı"

-- | Check if a type is a float
isFloatTy :: Ty Ann -> Bool
isFloatTy (TyFloat _) = True
isFloatTy _ = False

-- | Check for the floating-point type identifier.
isFloatIdent :: Identifier -> Bool
isFloatIdent (mods, name) = mods == [T.pack "ondalık"] && name == T.pack "sayı"

-- | Check if a type is a string
isStringTy :: Ty Ann -> Bool
isStringTy (TyString _) = True
isStringTy _ = False

-- | Check for the string type identifier.
isStringIdent :: Identifier -> Bool
isStringIdent (mods, name) = null mods && name == T.pack "dizge"

-- | Normalize primitive aliases to canonical primitive constructors.
normalizePrimTy :: Ty Ann -> Ty Ann
normalizePrimTy ty =
  case ty of
    TyInd ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | otherwise -> TyInd ann name
    TyVar ann name
      | isIntIdent name -> TyInt ann
      | isFloatIdent name -> TyFloat ann
      | isStringIdent name -> TyString ann
      | otherwise -> TyVar ann name
    TyApp ann ctor args ->
      TyApp ann (normalizePrimTy ctor) (map normalizePrimTy args)
    Arr ann d i ->
      Arr ann (normalizePrimTy d) (normalizePrimTy i)
    TySkolem ann name ->
      TySkolem ann name
    _ -> ty

-- | Check whether a type still contains unresolved type variables.
containsTyVar :: Ty Ann -> Bool
containsTyVar ty =
  case ty of
    TyVar {} -> True
    TyApp _ ctor args -> containsTyVar ctor || any containsTyVar args
    Arr _ d i -> containsTyVar d || containsTyVar i
    _ -> False

-- | All known primitive functions
allPrimitives :: [PrimitiveDef]
allPrimitives =
  [ PrimitiveDef ([], "yaz")
      [ withTypes 1 (\case [t] -> isIntTy t || isFloatTy t || isStringTy t; _ -> False)
      , anyTypes 2  -- File write
      ]
      ["etki.kip"]

  , PrimitiveDef ([], "oku")
      [ anyTypes 0  -- stdin
      , withTypes 1 (\case [t] -> isStringTy t; _ -> False)  -- file read
      ]
      ["etki.kip"]

  , PrimitiveDef ([], "uzunluk")
      [ anyTypes 1 ]
      ["dizge.kip"]

  , PrimitiveDef ([], "birleşim")
      [ anyTypes 2 ]
      ["dizge.kip"]

  , PrimitiveDef (["tam", "sayı"], "hal")
      [ anyTypes 1 ]
      ["dizge.kip"]

  , PrimitiveDef (["ondalık", "sayı"], "hal")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False)
      , withTypes 1 (\case [t] -> isIntTy t; _ -> False)
      ]
      ["dizge.kip", "tam-sayı.kip"]

  , PrimitiveDef ([], "ters")
      [ withTypes 1 (\case [t] -> isStringTy t; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "karakter")
      [ withTypes 2 (\case [t1, t2] -> isStringTy t1 && isIntTy t2; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "alış")
      [ withTypes 2 (\case [t1, t2] -> isStringTy t1 && isIntTy t2; _ -> False) ]
      ["dizge.kip"]

  , PrimitiveDef ([], "bırakış")
      [ withTypes 2 (\case [t1, t2] -> isStringTy t1 && isIntTy t2; _ -> False) ]
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
      [ withTypes 1 (\case [t] -> isFloatTy t || isIntTy t; _ -> False) ]
      ["tam-sayı.kip", "ondalık-sayı.kip"]

  , PrimitiveDef ([], "eşitlik")
      [ withTypes 2 (\case [t1, t2] -> (isFloatTy t1 || isFloatTy t2) || (isIntTy t1 && isIntTy t2); _ -> False)
      , withTypes 2 (\case [t1, t2] -> isStringTy t1 && isStringTy t2; _ -> False)
      ]
      ["tam-sayı.kip", "ondalık-sayı.kip", "dizge.kip"]

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

-- | Map a primitive identifier to the files that define it
primFiles :: Identifier -> [FilePath]
primFiles name =
  case filter (\p -> primId p == name) allPrimitives of
    [] -> []
    (prim:_) -> primSourceFiles prim

-- | Resolve a primitive implementation by name/signature.
primitiveEvalImpl :: Monad m => PrimitiveEvalOps m -> Maybe FilePath -> Identifier -> [Arg Ann] -> Maybe ([Exp Ann] -> m (Exp Ann))
primitiveEvalImpl ops mPath ident args = do
  guardPrimFile mPath ident
  case ident of
    ([], "yaz")
      | [(_, TyInt _)] <- args -> Just (primWrite ops)
      | [(_, TyFloat _)] <- args -> Just (primWrite ops)
      | [(_, TyString _)] <- args -> Just (primWrite ops)
      | [_, _] <- args -> Just (primWriteFile ops)
      | otherwise -> Nothing
    ([], "oku")
      | [] <- args -> Just (primRead ops)
      | [(_, TyString _)] <- args -> Just (primReadFile ops)
      | otherwise -> Nothing
    ([], "uzunluk") -> Just (primStringLength "uzunluk")
    ([], "birleşim") -> Just (primStringConcat "birleşim")
    (["tam", "sayı"], "hal") -> Just (primStringToInt "tam-sayı-hali")
    (["ondalık", "sayı"], "hal")
      | [(_, TyString _)] <- args -> Just (primStringToFloat "ondalık-sayı-hali")
      | [(_, TyInt _)] <- args -> Just (primIntToFloat "tam-sayı-ondalık-sayı-hali")
      | otherwise -> Nothing
    ([], "ters")
      | [(_, TyString _)] <- args -> Just (primStringReverse "ters")
      | otherwise -> Nothing
    ([], "karakter")
      | [(_, TyString _), (_, TyInt _)] <- args -> Just (primStringCharAt "karakter")
      | otherwise -> Nothing
    ([], "alış")
      | [(_, TyString _), (_, TyInt _)] <- args -> Just (primStringTake "alış")
      | otherwise -> Nothing
    ([], "bırakış")
      | [(_, TyString _), (_, TyInt _)] <- args -> Just (primStringDrop "bırakış")
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
      | otherwise ->
          Nothing
    ([], "eşitlik")
      | [(_, TyFloat _), (_, TyFloat _)] <- args ->
          Just (primFloatCmp "eşitlik" (==))
      | [(_, TyInt _), (_, TyInt _)] <- args ->
          Just (primIntCmp "eşitlik" (==))
      | [(_, TyString _), (_, TyString _)] <- args ->
          Just (primStringEq "eşitlik")
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
    (["sayı"], "çek") -> Just (primIntRandom ops ["sayı"] "çek")
    ([], "sayı-çek") -> Just (primIntRandom ops [] "sayı-çek")
    _ -> Nothing
  where
    guardPrimFile mp name =
      case mp of
        Just path | takeFileName path `elem` primFiles name -> Just ()
        _ -> Nothing

primWrite :: Monad m => PrimitiveEvalOps m -> [Exp Ann] -> m (Exp Ann)
primWrite ops args =
  case args of
    [StrLit _ s] -> peWriteText ops s >> peFlushStdout ops >> pure unitExp
    [IntLit _ n] -> peWriteInteger ops n >> peFlushStdout ops >> pure unitExp
    [FloatLit _ n] -> peWriteDouble ops n >> peFlushStdout ops >> pure unitExp
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
          pure (someExp (StrLit ann (T.singleton (T.index s (fromIntegral n)))))
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
  , "var __kip_prim_karakter = (s, n) => n >= 0 && n < s.length ? __kip_some(s[n]) : __kip_none();"
  , "var __kip_prim_alış = (s, n) => s.slice(0, Math.max(0, n));"
  , "var __kip_prim_bırakış = (s, n) => s.slice(Math.max(0, n));"
  , "var __kip_prim_toplam = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) + __kip_num(b)) : (__kip_num(a) + __kip_num(b));"
  , "var __kip_prim_fark = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) - __kip_num(b)) : (__kip_num(a) - __kip_num(b));"
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
-- Each triple is: binding name, dependency names, exact snippet text in
-- 'primitiveJsPrelude'. Dependencies are traversed transitively.
primitiveJsPrunableSpecs :: [(Text, [Text], Text)]
primitiveJsPrunableSpecs =
  [ ("doğru", [], "var doğru = { tag: \"doğru\", args: [] };\n")
  , ("yanlış", [], "var yanlış = { tag: \"yanlış\", args: [] };\n")
  , ("varlık", [], "var varlık = (...args) => ({ tag: \"varlık\", args });\n")
  , ("yokluk", [], "var yokluk = (...args) => ({ tag: \"yokluk\", args });\n")
  , ("bitimlik", [], "var bitimlik = (...args) => ({ tag: \"bitimlik\", args });\n")
  , ("__kip_prim_ters", [], "var __kip_prim_ters = (s) => s.split('').reverse().join('');\n")
  , ("__kip_prim_birleşim", [], "var __kip_prim_birleşim = (a, b) => __kip_num(a) + __kip_num(b);\n")
  , ("__kip_prim_uzunluk", [], "var __kip_prim_uzunluk = (s) => s.length;\n")
  , ("__kip_prim_karakter", ["varlık", "yokluk"], "var __kip_prim_karakter = (s, n) => n >= 0 && n < s.length ? __kip_some(s[n]) : __kip_none();\n")
  , ("__kip_prim_alış", [], "var __kip_prim_alış = (s, n) => s.slice(0, Math.max(0, n));\n")
  , ("__kip_prim_bırakış", [], "var __kip_prim_bırakış = (s, n) => s.slice(Math.max(0, n));\n")
  , ("__kip_prim_toplam", [], "var __kip_prim_toplam = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) + __kip_num(b)) : (__kip_num(a) + __kip_num(b));\n")
  , ("__kip_prim_fark", [], "var __kip_prim_fark = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) - __kip_num(b)) : (__kip_num(a) - __kip_num(b));\n")
  , ("__kip_prim_oku_stdin", [], T.unlines
      [ "var __kip_prim_oku_stdin = async () => {"
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
      ])
  , ("__kip_prim_oku_dosya", ["varlık", "yokluk"], T.unlines
      [ "var __kip_prim_oku_dosya = (path) => {"
      , "  if (!__kip_require) return __kip_none();"
      , "  __kip_fs = __kip_fs || __kip_require('fs');"
      , "  try {"
      , "    return __kip_some(__kip_fs.readFileSync(path, 'utf8'));"
      , "  } catch (e) {"
      , "    return __kip_none();"
      , "  }"
      , "};"
      ])
  , ("__kip_prim_yaz_dosya", ["doğru", "yanlış"], T.unlines
      [ "var __kip_prim_yaz_dosya = (path, content) => {"
      , "  if (!__kip_require) return __kip_false();"
      , "  __kip_fs = __kip_fs || __kip_require('fs');"
      , "  try {"
      , "    __kip_fs.writeFileSync(path, content);"
      , "    return __kip_true();"
      , "  } catch (e) {"
      , "    return __kip_false();"
      , "  }"
      , "};"
      ])
  , ("yaz", ["bitimlik"], T.unlines
      [ "var yaz = (x) => {"
      , "  var val = __kip_is_float(x) ? x.value : x;"
      , "  var output = __kip_is_float(x) && Number.isInteger(val) ? String(val) + '.0' : val;"
      , "  if (__kip_is_browser && typeof window.__kip_write === 'function') {"
      , "    window.__kip_write(output);"
      , "  } else {"
      , "    console.log(output);"
      , "  }"
      , "  return typeof bitimlik === 'function' ? bitimlik() : bitimlik;"
      , "};"
      ])
  , ("çarpım", [], "var çarpım = (a, b) => __kip_is_float(a) || __kip_is_float(b) ? __kip_float(__kip_num(a) * __kip_num(b)) : (__kip_num(a) * __kip_num(b));\n")
  , ("fark", ["__kip_prim_fark"], "var fark = __kip_prim_fark;\n")
  , ("bölüm", [], T.unlines
      [ "var bölüm = (a, b) => {"
      , "  var av = __kip_num(a);"
      , "  var bv = __kip_num(b);"
      , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
      , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av / bv) : Math.trunc(av / bv);"
      , "};"
      ])
  , ("kalan", [], T.unlines
      [ "var kalan = (a, b) => {"
      , "  var av = __kip_num(a);"
      , "  var bv = __kip_num(b);"
      , "  if (bv === 0) return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(0) : 0;"
      , "  return __kip_is_float(a) || __kip_is_float(b) ? __kip_float(av % bv) : (av % bv);"
      , "};"
      ])
  , ("karekök", [], "var karekök = (a) => __kip_float(Math.sqrt(__kip_num(a)) * 1.0);\n")
  , ("radyan", [], "var radyan = (a) => __kip_float(__kip_num(a) * Math.PI / 180);\n")
  , ("derece", [], "var derece = (a) => __kip_float(__kip_num(a) * 180 / Math.PI);\n")
  , ("pi_sayısı", [], "var pi_sayısı = () => __kip_float(Math.PI);\n")
  , ("taban", [], "var taban = (a) => Math.floor(__kip_num(a));\n")
  , ("tavan", [], "var tavan = (a) => Math.ceil(__kip_num(a));\n")
  , ("tam_sayı_ondalık_sayı_hali", [], "var tam_sayı_ondalık_sayı_hali = (a) => __kip_float(a * 1.0);\n")
  , ("sayı_çek", [], T.unlines
      [ "var sayı_çek = (a, b) => {"
      , "  var lo = Math.min(a, b);"
      , "  var hi = Math.max(a, b);"
      , "  var range = hi - lo + 1;"
      , "  return lo + (__kip_rand() % range);"
      , "};"
      ])
  , ("__kip_prim_dizge_eşitlik", ["doğru", "yanlış"], "var __kip_prim_dizge_eşitlik = (a, b) => a === b ? __kip_true() : __kip_false();\n")
  , ("eşitlik", ["doğru", "yanlış"], "var eşitlik = (a, b) => __kip_num(a) === __kip_num(b) ? __kip_true() : __kip_false();\n")
  , ("küçüklük", ["doğru", "yanlış"], "var küçüklük = (a, b) => __kip_num(a) < __kip_num(b) ? __kip_true() : __kip_false();\n")
  , ("küçük_eşitlik", ["doğru", "yanlış"], "var küçük_eşitlik = (a, b) => __kip_num(a) <= __kip_num(b) ? __kip_true() : __kip_false();\n")
  , ("büyüklük", ["doğru", "yanlış"], "var büyüklük = (a, b) => __kip_num(a) > __kip_num(b) ? __kip_true() : __kip_false();\n")
  , ("büyük_eşitlik", ["doğru", "yanlış"], "var büyük_eşitlik = (a, b) => __kip_num(a) >= __kip_num(b) ? __kip_true() : __kip_false();\n")
  , ("dizge_hal", [], "var dizge_hal = (n) => String(__kip_num(n));\n")
  , ("tam_sayı_hal", ["varlık", "yokluk"], "var tam_sayı_hal = (s) => { const n = parseInt(s, 10); return isNaN(n) ? __kip_none() : __kip_some(n); };\n")
  , ("ondalık_sayı_hal", ["varlık", "yokluk"], "var ondalık_sayı_hal = (s) => { if (typeof s === 'number') return __kip_float(s * 1.0); const n = parseFloat(s); return isNaN(n) ? __kip_none() : __kip_some(__kip_float(n)); };\n")
  , ("__kip_call", [], T.unlines
      [ "var __kip_call = async (fn, args) => {"
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
      ])
  ]
