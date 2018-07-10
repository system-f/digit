{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Integral(
  integralBinaryNoZero
, integralBinary
, integralOctalNoZero
, integralOctal
, integralDecimal
, integralDecimalNoZero
, integralHexadecimalNoZero
, integralHexadecimal
, integralHEXADECIMALNoZero
, integralHEXADECIMAL
) where

import Control.Lens.Extras(is)
import Data.Digit.Binary as D
import Data.Digit.Decimal as D
import Data.Digit.Octal as D
import Data.Digit.Hexadecimal as D
import Data.Digit.HEXADECIMAL as D
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
