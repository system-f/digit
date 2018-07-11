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
, _HeXDigitsIntegral
) where

import Control.Lens.Extras(is)
import Data.Digit.Binary
import Data.Digit.Decimal
import Data.Digit.Octal
import Data.Digit.Hexadecimal
import Data.Digit.HEXADECIMAL
import Data.Digit.HeXaDeCiMaL
import qualified Data.List.NonEmpty as NonEmpty
import Papa

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
-- >>> integralBinDigits (8 :: Int)
-- BinDigit1 :| [BinDigit0, BinDigit0, BinDigit0]
integralBinDigits :: Integral a => a -> NonEmpty BinDigit
integralBinDigits n = NonEmpty.fromList $ go n []
  where
    go 0 = id
    go k =
      let
        (q, r) = quotRem k 2
      in
        go q . ((r ^?! integralBinary) :)

-- |
-- >>> binDigitsIntegral (BinDigit1 :| [BinDigit0, BinDigit0, BinDigit0]) :: Int
-- 8
binDigitsIntegral :: Integral a => NonEmpty BinDigit -> a
binDigitsIntegral =
  foldl' (\b a -> (integralBinary # a) + 2 * b) 0

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
-- OctDigit1 :| [OctDigit0, Octdigit0]
integralOctDigits :: Integral a => a -> NonEmpty OctDigit
integralOctDigits n = NonEmpty.fromList $ go n []
  where
    go 0 = id
    go k =
      let
        (q, r) = quotRem k 8
      in
        go q . ((r ^?! integralOctal) :)

-- |
-- >>> octDigitsIntegral (OctDigit1 :| [OctDigit0, OctDigit0]) :: Int
-- 64
octDigitsIntegral :: Integral a => NonEmpty OctDigit -> a
octDigitsIntegral =
  foldl' (\b a -> (integralOctal # a) + 8 * b) 0

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
-- DecDigit1 :| [DecDigit0, Decdigit0]
integralDecDigits :: Integral a => a -> NonEmpty DecDigit
integralDecDigits n = NonEmpty.fromList $ go n []
  where
    go 0 = id
    go k =
      let
        (q, r) = quotRem k 10
      in
        go q . ((r ^?! integralDecimal) :)

-- |
-- >>> decDigitsIntegral (DecDigit1 :| [DecDigit0, DecDigit0]) :: Int
-- 100
decDigitsIntegral :: Integral a => NonEmpty DecDigit -> a
decDigitsIntegral =
  foldl' (\b a -> (integralDecimal # a) + 10 * b) 0

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
-- HexDigit1 :| [HexDigit0, Hexdigit0]
integralHexDigits :: Integral a => a -> NonEmpty HexDigit
integralHexDigits n = NonEmpty.fromList $ go n []
  where
    go 0 = id
    go k =
      let
        (q, r) = quotRem k 16
      in
        go q . ((r ^?! integralHexadecimal) :)

-- |
-- >>> hexDigitsIntegral (HexDigit1 :| [HexDigit0, HexDigit0]) :: Int
-- 256
hexDigitsIntegral :: Integral a => NonEmpty HexDigit -> a
hexDigitsIntegral =
  foldl' (\b a -> (integralHexadecimal # a) + 16 * b) 0

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
-- HEXDigit1 :| [HEXDigit0, HEXdigit0]
integralHEXDigits :: Integral a => a -> NonEmpty HEXDigit
integralHEXDigits n = NonEmpty.fromList $ go n []
  where
    go 0 = id
    go k =
      let
        (q, r) = quotRem k 16
      in
        go q . ((r ^?! integralHEXADECIMAL) :)

-- |
-- >>> _HEXDigitsIntegral (HEXDigit1 :| [HEXDigit0, HEXDigit0]) :: Int
-- 256
_HEXDigitsIntegral :: Integral a => NonEmpty HEXDigit -> a
_HEXDigitsIntegral =
  foldl' (\b a -> (integralHEXADECIMAL # a) + 16 * b) 0

-- |
-- >>> _HeXDigitsIntegral (HEXDigit1 :| [HEXDigit0, HEXDigit0]) :: Int
-- 256
_HeXDigitsIntegral :: Integral a => NonEmpty HeXDigit -> a
_HeXDigitsIntegral =
  foldl' (\b a -> (integralHEXADECIMAL # a) + 16 * b) 0

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
