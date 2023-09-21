{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Integral(
-- * Binary
  integralBinaryNoZero
, integralBinary
, integralBinDigits
, binDigitsIntegral
-- * Octal
, integralOctalNoZero
, integralOctal
, integralOctDigits
, octDigitsIntegral
-- * Decimal
, integralDecimal
, integralDecimalNoZero
, integralDecDigits
, decDigitsIntegral
-- * Hexadecimal
, integralHexadecimalNoZero
, integralHexadecimal
, integralHexDigits
, hexDigitsIntegral
-- * HEXADECIMAL
, integralHEXADECIMALNoZero
, integralHEXADECIMAL
, integralHEXDigits
, _HEXDigitsIntegral
-- * HeXaDeCiMaL
, integralHeXaDeCiMaLNoZero
, integralHeXaDeCiMaL
, _HeXDigitsIntegral
, mod10
, addDecDigit
, addDecDigit'
) where

import           Prelude                (Eq, Integral, error, fst, lookup,
                                         quotRem, (*), (+), (-), (==), (>=), mod, divMod)

import           Control.Applicative    (Applicative)
import           Control.Category       (id, (.))
import           Control.Lens           (APrism, Choice, Prism', Review,
                                         clonePrism, outside, prism', unto, over, _1,
                                         ( # ), (.~), (^?!), (^?))
import           Control.Lens.Extras    (is)

import           Data.Bool              (Bool, bool)
import           Data.Either            (Either (..), either)
import           Data.Foldable          (find, foldl')
import           Data.Function          (($),const)
import           Data.Functor           ((<$>))
import           Data.Int               (Int)
import           Data.List.NonEmpty     (NonEmpty)
import           Data.Maybe             (fromMaybe)
import           Data.Ord               ((>))

import           Data.Digit.Binary
import           Data.Digit.Decimal
import           Data.Digit.Hexadecimal.LowerCase
import           Data.Digit.Hexadecimal.UpperCase
import           Data.Digit.Hexadecimal.MixedCase
import           Data.Digit.Octal
import qualified Data.List.NonEmpty     as NonEmpty

-- $setup
-- >>> import Data.Digit

-- |
--
-- >>> 1 ^? integralBinaryNoZero
-- Just BinDigit1
--
-- >>> integralBinaryNoZero # BinDigit1 :: Integer
-- 1
integralBinaryNoZero ::
  (Integral a, BinaryNoZero d) =>
  Prism'
    a
    d
integralBinaryNoZero =
  associatePrism (1, d1) []

-- |
--
-- >>> 0 ^? integralBinary :: Maybe BinDigit
-- Just BinDigit0
--
-- >>> integralBinary # BinDigit0 :: Integer
-- 0
integralBinary ::
  (Integral a, Binary d) =>
  Prism'
    a
    d
integralBinary =
  associatePrism (0, d0) [(1, d1)]

-- |
-- >>> integralBinDigits (4 :: Int)
-- Right (BinDigit1 :| [BinDigit0, BinDigit0])
--
-- >>> integralBinDigits (0 :: Int)
-- Right (BinDigit0 :| [])
--
-- >>> integralBinDigits (-1 :: Int)
-- Left (BinDigit0 :| [])
--
-- >>> integralBinDigits (-4 :: Int)
-- Left (BinDigit1 :| [BinDigit1])
integralBinDigits :: Integral a => a -> Either (NonEmpty BinDigit) (NonEmpty BinDigit)
integralBinDigits n =
  if n >= 0
  then Right . NonEmpty.fromList $ go n []
  else Left . NonEmpty.fromList $ go (-n - 1) []
  where
    go k =
      let
        (q, r) = quotRem k 2
      in
        (if q == 0 then id else go q) . ((r ^?! integralBinary) :)

-- |
-- >>> binDigitsIntegral (Right (BinDigit1 :| [BinDigit0, BinDigit0])) :: Int
-- 4
--
-- >>> binDigitsIntegral (Right (BinDigit0 :| [])) :: Int
-- 0
--
-- >>> binDigitsIntegral (Left (BinDigit0 :| [])) :: Int
-- 0
--
-- >>> binDigitsIntegral (Left (BinDigit1 :| [BinDigit1])) :: Int
-- -3
binDigitsIntegral :: Integral a => Either (NonEmpty BinDigit) (NonEmpty BinDigit) -> a
binDigitsIntegral = either (\n -> -(go n)) go
  where
    go = foldl' (\b a -> (integralBinary # a) + 2 * b) 0

-- |
--
-- >>> 7 ^? integralOctalNoZero :: Maybe OctDigit
-- Just OctDigit7
--
-- >>> integralOctalNoZero # OctDigit7 :: Integer
-- 7
integralOctalNoZero ::
  (Integral a, OctalNoZero d) =>
  Prism'
    a
    d
integralOctalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7)]

-- |
--
-- >>> 7 ^? integralOctal :: Maybe OctDigit
-- Just OctDigit7
--
-- >>> integralOctal # OctDigit7 :: Integer
-- 7
integralOctal ::
  (Integral a, Octal d) =>
  Prism'
    a
    d
integralOctal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7)]

-- |
-- >>> integralOctDigits (64 :: Int)
-- Right (OctDigit1 :| [OctDigit0, OctDigit0])
--
-- >>> integralOctDigits (0 :: Int)
-- Right (OctDigit0 :| [])
--
-- >>> integralOctDigits (-1 :: Int)
-- Left (OctDigit0 :| [])
--
-- >>> integralOctDigits (-64 :: Int)
-- Left (OctDigit7 :| [OctDigit7])
integralOctDigits :: Integral a => a -> Either (NonEmpty OctDigit) (NonEmpty OctDigit)
integralOctDigits n =
  if n >= 0
  then Right . NonEmpty.fromList $ go n []
  else Left . NonEmpty.fromList $ go (-n - 1) []
  where
    go k =
      let
        (q, r) = quotRem k 8
      in
        (if q == 0 then id else go q) . ((r ^?! integralOctal) :)

-- |
-- >>> octDigitsIntegral (Right (OctDigit1 :| [OctDigit0, OctDigit0])) :: Int
-- 64
--
-- >>> octDigitsIntegral (Right (OctDigit0 :| [])) :: Int
-- 0
--
-- >>> octDigitsIntegral (Left (OctDigit0 :| [])) :: Int
-- 0
--
-- >>> octDigitsIntegral (Left (OctDigit7 :| [OctDigit7])) :: Int
-- -63
octDigitsIntegral :: Integral a => Either (NonEmpty OctDigit) (NonEmpty OctDigit) -> a
octDigitsIntegral = either (\n -> -(go n)) go
  where
    go = foldl' (\b a -> (integralOctal # a) + 8 * b) 0

-- |
-- >>> 9 ^? integralDecimalNoZero :: Maybe DecDigit
-- Just DecDigit9
--
-- >>> integralDecimalNoZero # DecDigit9 :: Integer
-- 9
integralDecimalNoZero ::
  (Integral a, DecimalNoZero d) =>
  Prism'
    a
    d
integralDecimalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9)]

-- |
-- >>> 9 ^? integralDecimal :: Maybe DecDigit
-- Just DecDigit9
--
-- >>> integralDecimal # DecDigit9 :: Integer
-- 9
integralDecimal ::
  (Integral a, Decimal d) =>
  Prism'
    a
    d
integralDecimal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9)]

-- |
-- >>> integralDecDigits (100 :: Int)
-- Right (DecDigit1 :| [DecDigit0, DecDigit0])
--
-- >>> integralDecDigits (0 :: Int)
-- Right (DecDigit0 :| [])
--
-- >>> integralDecDigits (-1 :: Int)
-- Left (DecDigit0 :| [])
--
-- >>> integralDecDigits (-100 :: Int)
-- Left (DecDigit9 :| [DecDigit9])
integralDecDigits :: Integral a => a -> Either (NonEmpty DecDigit) (NonEmpty DecDigit)
integralDecDigits n =
  if n >= 0
  then Right . NonEmpty.fromList $ go n []
  else Left . NonEmpty.fromList $ go (-n - 1) []
  where
    go k =
      let
        (q, r) = quotRem k 10
      in
        (if q == 0 then id else go q) . ((r ^?! integralDecimal) :)

-- |
-- >>> decDigitsIntegral (Right (DecDigit1 :| [DecDigit0, DecDigit0])) :: Int
-- 100
--
-- >>> decDigitsIntegral (Right (DecDigit0 :| [])) :: Int
-- 0
--
-- >>> decDigitsIntegral (Left (DecDigit0 :| [])) :: Int
-- 0
--
-- >>> decDigitsIntegral (Left (DecDigit9 :| [DecDigit9])) :: Int
-- -9
decDigitsIntegral :: Integral a => Either (NonEmpty DecDigit) (NonEmpty DecDigit) -> a
decDigitsIntegral = either (\n -> -(go n)) go
  where
    go = foldl' (\b a -> (integralDecimal # a) + 10 * b) 0

-- |
--
-- >>> 15 ^? integralHexadecimalNoZero :: Maybe HexDigit
-- Just HexDigitf
--
-- >>> integralHexadecimalNoZero # HexDigitf :: Integer
-- 15
integralHexadecimalNoZero ::
  (Integral a, HexadecimalNoZero d) =>
  Prism'
    a
    d
integralHexadecimalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, da), (11, db), (12, dc), (13, dd), (14, de), (15, df)]

-- |
--
-- >>> 15 ^? integralHexadecimal :: Maybe HexDigit
-- Just HexDigitf
--
-- >>> integralHexadecimal # HexDigitf :: Integer
-- 15
integralHexadecimal ::
  (Integral a, Hexadecimal d) =>
  Prism'
    a
    d
integralHexadecimal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, da), (11, db), (12, dc), (13, dd), (14, de), (15, df)]

-- |
-- >>> integralHexDigits (256 :: Int)
-- Right (HexDigit1 :| [HexDigit0, HexDigit0])
--
-- >>> integralHexDigits (0 :: Int)
-- Right (HexDigit0 :| [])
--
-- >>> integralHexDigits (-1 :: Int)
-- Left (HexDigit0 :| [])
--
-- >>> integralHexDigits (-256 :: Int)
-- Left (HexDigitf :| [HexDigitf])
integralHexDigits :: Integral a => a -> Either (NonEmpty HexDigit) (NonEmpty HexDigit)
integralHexDigits n =
  if n >= 0
  then Right . NonEmpty.fromList $ go n []
  else Left . NonEmpty.fromList $ go (-n - 1) []
  where
    go k =
      let
        (q, r) = quotRem k 16
      in
        (if q == 0 then id else go q) . ((r ^?! integralHexadecimal) :)

-- |
-- >>> hexDigitsIntegral (Right (HexDigit1 :| [HexDigit0, HexDigit0])) :: Int
-- 256
--
-- >>> hexDigitsIntegral (Right (HexDigit0 :| [])) :: Int
-- 0
--
-- >>> hexDigitsIntegral (Left (HexDigit0 :| [])) :: Int
-- 0
--
-- >>> hexDigitsIntegral (Left (HexDigitf :| [HexDigitf])) :: Int
-- -255
hexDigitsIntegral :: Integral a => Either (NonEmpty HexDigit) (NonEmpty HexDigit) -> a
hexDigitsIntegral = either (\n -> -(go n)) go
  where
    go = foldl' (\b a -> (integralHexadecimal # a) + 16 * b) 0

-- |
--
-- >>> 15 ^? integralHEXADECIMALNoZero :: Maybe HEXDigit
-- Just HEXDigitF
--
-- >>> integralHEXADECIMALNoZero # HEXDigitF :: Integer
-- 15
integralHEXADECIMALNoZero ::
  (Integral a, HEXADECIMALNoZero d) =>
  Prism'
    a
    d
integralHEXADECIMALNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, dA), (11, dB), (12, dC), (13, dD), (14, dE), (15, dF)]


-- |
--
-- >>> 15 ^? integralHEXADECIMAL :: Maybe HEXDigit
-- Just HEXDigitF
--
-- >>> integralHEXADECIMAL # HEXDigitF :: Integer
-- 15
integralHEXADECIMAL ::
  (Integral a, HEXADECIMAL d) =>
  Prism'
    a
    d
integralHEXADECIMAL =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, dA), (11, dB), (12, dC), (13, dD), (14, dE), (15, dF)]

-- |
-- >>> integralHEXDigits (256 :: Int)
-- Right (HEXDigit1 :| [HEXDigit0, HEXDigit0])
--
-- >>> integralHEXDigits (0 :: Int)
-- Right (HEXDigit0 :| [])
--
-- >>> integralHEXDigits (-1 :: Int)
-- Left (HEXDigit0 :| [])
--
-- >>> integralHEXDigits (-256 :: Int)
-- Left (HEXDigitF :| [HEXDigitF])
integralHEXDigits :: Integral a => a -> Either (NonEmpty HEXDigit) (NonEmpty HEXDigit)
integralHEXDigits n =
  if n >= 0
  then Right . NonEmpty.fromList $ go n []
  else Left . NonEmpty.fromList $ go (-n - 1) []
  where
    go k =
      let
        (q, r) = quotRem k 16
      in
        (if q == 0 then id else go q) . ((r ^?! integralHEXADECIMAL) :)

-- |
-- >>> _HEXDigitsIntegral (Right (HEXDigit1 :| [HEXDigit0, HEXDigit0])) :: Int
-- 256
--
-- >>> _HEXDigitsIntegral (Right (HEXDigit0 :| [])) :: Int
-- 0
--
-- >>> _HEXDigitsIntegral (Left (HEXDigit0 :| [])) :: Int
-- 0
--
-- >>> _HEXDigitsIntegral (Left (HEXDigitF :| [HEXDigitF])) :: Int
-- -255
_HEXDigitsIntegral :: Integral a => Either (NonEmpty HEXDigit) (NonEmpty HEXDigit) -> a
_HEXDigitsIntegral = either (\n -> -(go n)) go
  where
    go = foldl' (\b a -> (integralHEXADECIMAL # a) + 16 * b) 0

-- |
--
-- >>> 15 ^? integralHeXaDeCiMaLNoZero :: Maybe HeXDigit
-- Just HeXDigitF
--
-- >>> integralHeXaDeCiMaLNoZero # HeXDigitF :: Integer
-- 15
integralHeXaDeCiMaLNoZero ::
  (Integral a, HeXaDeCiMaLNoZero d) =>
  Review
    a
    d
integralHeXaDeCiMaLNoZero =
  unto
    (outside d1 .~ const 1 $
     outside d2 .~ const 2 $
     outside d3 .~ const 3 $
     outside d4 .~ const 4 $
     outside d5 .~ const 5 $
     outside d6 .~ const 6 $
     outside d7 .~ const 7 $
     outside d8 .~ const 8 $
     outside d9 .~ const 9 $
     outside da .~ const 10 $
     outside dA .~ const 10 $
     outside db .~ const 11 $
     outside dB .~ const 11 $
     outside dc .~ const 12 $
     outside dC .~ const 12 $
     outside dd .~ const 13 $
     outside dD .~ const 13 $
     outside de .~ const 14 $
     outside dE .~ const 14 $
     outside df .~ const 15 $
     outside dF .~ const 15 $
     error "incomplete pattern")

-- |
--
-- >>> 15 ^? integralHeXaDeCiMaL :: Maybe HeXDigit
-- Just HeXDigitF
--
-- >>> integralHeXaDeCiMaL # HeXDigitF :: Integer
-- 15
integralHeXaDeCiMaL ::
  (Integral a, HeXaDeCiMaL d) =>
  Review
    a
    d
integralHeXaDeCiMaL =
  unto
    (outside d0 .~ const 0 $
     outside d1 .~ const 1 $
     outside d2 .~ const 2 $
     outside d3 .~ const 3 $
     outside d4 .~ const 4 $
     outside d5 .~ const 5 $
     outside d6 .~ const 6 $
     outside d7 .~ const 7 $
     outside d8 .~ const 8 $
     outside d9 .~ const 9 $
     outside da .~ const 10 $
     outside dA .~ const 10 $
     outside db .~ const 11 $
     outside dB .~ const 11 $
     outside dc .~ const 12 $
     outside dC .~ const 12 $
     outside dd .~ const 13 $
     outside dD .~ const 13 $
     outside de .~ const 14 $
     outside dE .~ const 14 $
     outside df .~ const 15 $
     outside dF .~ const 15 $
     error "incomplete pattern")

-- |
-- >>> _HeXDigitsIntegral (Right (HeXDigit1 :| [HeXDigit0, HeXDigit0])) :: Int
-- 256
--
-- >>> _HeXDigitsIntegral (Right (HeXDigit0 :| [])) :: Int
-- 0
--
-- >>> _HeXDigitsIntegral (Left (HeXDigit0 :| [])) :: Int
-- 0
--
-- >>> _HeXDigitsIntegral (Left (HeXDigitF :| [HeXDigitF])) :: Int
-- -255
_HeXDigitsIntegral :: Integral a => Either (NonEmpty HeXDigit) (NonEmpty HeXDigit) -> a
_HeXDigitsIntegral = either (\n -> -(go n)) go
  where
    go = foldl' (\b a -> (integralHeXaDeCiMaL # a) + 16 * b) 0

mod10 ::
  Integral a =>
  a
  -> DecDigit
mod10 n =
  let r = n `mod` 10
  in fromMaybe (mod10 r) (r ^? integralDecimal)

addDecDigit ::
  DecDigit
  -> DecDigit
  -> (Bool, DecDigit)
addDecDigit a b =
  let (x, r) =
        (integralDecimal # a + integralDecimal # b) `divMod` 10
  in  (x > 0, mod10 (r :: Int))

addDecDigit' ::
  DecDigit
  -> DecDigit
  -> (DecDigit, DecDigit)
addDecDigit' a b =
  over _1 (bool x0 x1) (addDecDigit a b)

---- not exported
associatePrism ::
  (Eq b, Choice p, Applicative f) =>
  (b, APrism a a () ())
  -> [(b, APrism a a () ())]
  -> p a (f a)
  -> p b (f b)
associatePrism def z =
  prism'
    (\d -> fst (fromMaybe def (find (\(_, w) -> is w d) z)))
    (\i -> (\p -> clonePrism p # ()) <$> lookup i (def:z))
