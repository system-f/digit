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
import Data.Digit.D0 as D
import Data.Digit.D1 as D
import Data.Digit.D2 as D
import Data.Digit.D3 as D
import Data.Digit.D4 as D
import Data.Digit.D5 as D
import Data.Digit.D6 as D
import Data.Digit.D7 as D
import Data.Digit.D8 as D
import Data.Digit.D9 as D
import Data.Digit.Da as D
import Data.Digit.DA as D
import Data.Digit.Db as D
import Data.Digit.DB as D
import Data.Digit.Dc as D
import Data.Digit.DC as D
import Data.Digit.Dd as D
import Data.Digit.DD as D
import Data.Digit.De as D
import Data.Digit.DE as D
import Data.Digit.Df as D
import Data.Digit.DF as D
import Papa

-- $setup
-- >>> import Data.Digit.Digit

-- |
--
-- >>> 1 ^? integralBinaryNoZero
-- Just ()
--
-- >>> integralBinaryNoZero # Digit1 :: Integer
-- 1
--
-- prop> \c -> c /= 1 ==> (c ^? integralBinaryNoZero == Nothing)
integralBinaryNoZero ::
  (Integral a, BinaryNoZero d) =>
  Prism'
    a
    d
integralBinaryNoZero =
  associatePrism (1, d1) []
  
-- |
--
-- >>> 0 ^? integralBinary :: Maybe Digit
-- Just 0
--
--
-- >>> 1 ^? integralBinary :: Maybe Digit
-- Just 1
--
-- >>> integralBinary # Digit0 :: Integer
-- 0
--
-- >>> integralBinary # Digit1 :: Integer
-- 1
--
-- prop> \c -> (c `notElem` [0, 1]) ==> (c ^? integralBinary == Nothing)
integralBinary ::
  (Integral a, Binary d) =>
  Prism'
    a
    d
integralBinary =
  associatePrism (0, d0) [(1, d1)]
  
-- |
--
-- >>> 1 ^? integralOctalNoZero :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralOctalNoZero :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralOctalNoZero :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralOctalNoZero :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralOctalNoZero :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralOctalNoZero :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralOctalNoZero :: Maybe Digit
-- Just 7
--
-- >>> integralOctalNoZero # Digit1 :: Integer
-- 1
--
-- >>> integralOctalNoZero # Digit2 :: Integer
-- 2
--
-- >>> integralOctalNoZero # Digit3 :: Integer
-- 3
--
-- >>> integralOctalNoZero # Digit4 :: Integer
-- 4
--
-- >>> integralOctalNoZero # Digit5 :: Integer
-- 5
--
-- >>> integralOctalNoZero # Digit6 :: Integer
-- 6
--
-- >>> integralOctalNoZero # Digit7 :: Integer
-- 7
--
-- prop> \c -> (c `notElem` [1..7]) ==> (c ^? integralOctalNoZero == Nothing)
integralOctalNoZero ::
  (Integral a, OctalNoZero d) =>
  Prism'
    a
    d
integralOctalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7)]

-- |
--
-- >>> 0 ^? integralOctal :: Maybe Digit
-- Just 0
--
-- >>> 1 ^? integralOctal :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralOctal :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralOctal :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralOctal :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralOctal :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralOctal :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralOctal :: Maybe Digit
-- Just 7
--
-- >>> integralOctal # Digit0 :: Integer
-- 0
--
-- >>> integralOctal # Digit1 :: Integer
-- 1
--
-- >>> integralOctal # Digit2 :: Integer
-- 2
--
-- >>> integralOctal # Digit3 :: Integer
-- 3
--
-- >>> integralOctal # Digit4 :: Integer
-- 4
--
-- >>> integralOctal # Digit5 :: Integer
-- 5
--
-- >>> integralOctal # Digit6 :: Integer
-- 6
--
-- >>> integralOctal # Digit7 :: Integer
-- 7
--
-- prop> \c -> (c `notElem` [0..7]) ==> (c ^? integralOctal == Nothing)
integralOctal ::
  (Integral a, Octal d) =>
  Prism'
    a
    d
integralOctal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7)]
  
-- |
--
-- >>> 1 ^? integralDecimalNoZero :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralDecimalNoZero :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralDecimalNoZero :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralDecimalNoZero :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralDecimalNoZero :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralDecimalNoZero :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralDecimalNoZero :: Maybe Digit
-- Just 7
--
-- >>> 8 ^? integralDecimalNoZero :: Maybe Digit
-- Just 8
--
-- >>> 9 ^? integralDecimalNoZero :: Maybe Digit
-- Just 9
--
-- >>> integralDecimalNoZero # Digit1 :: Integer
-- 1
--
-- >>> integralDecimalNoZero # Digit2 :: Integer
-- 2
--
-- >>> integralDecimalNoZero # Digit3 :: Integer
-- 3
--
-- >>> integralDecimalNoZero # Digit4 :: Integer
-- 4
--
-- >>> integralDecimalNoZero # Digit5 :: Integer
-- 5
--
-- >>> integralDecimalNoZero # Digit6 :: Integer
-- 6
--
-- >>> integralDecimalNoZero # Digit7 :: Integer
-- 7
--
-- >>> integralDecimalNoZero # Digit8 :: Integer
-- 8
--
-- >>> integralDecimalNoZero # Digit9 :: Integer
-- 9
--
-- prop> \c -> (c `notElem` [1..9]) ==> (c ^? integralDecimalNoZero == Nothing)
integralDecimalNoZero ::
  (Integral a, DecimalNoZero d) =>
  Prism'
    a
    d
integralDecimalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9)]
  
-- |
--
-- >>> 0 ^? integralDecimal :: Maybe Digit
-- Just 0
--
-- >>> 1 ^? integralDecimal :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralDecimal :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralDecimal :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralDecimal :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralDecimal :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralDecimal :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralDecimal :: Maybe Digit
-- Just 7
--
-- >>> 8 ^? integralDecimal :: Maybe Digit
-- Just 8
--
-- >>> 9 ^? integralDecimal :: Maybe Digit
-- Just 9
--
-- >>> integralDecimal # Digit0 :: Integer
-- 0
--
-- >>> integralDecimal # Digit1 :: Integer
-- 1
--
-- >>> integralDecimal # Digit2 :: Integer
-- 2
--
-- >>> integralDecimal # Digit3 :: Integer
-- 3
--
-- >>> integralDecimal # Digit4 :: Integer
-- 4
--
-- >>> integralDecimal # Digit5 :: Integer
-- 5
--
-- >>> integralDecimal # Digit6 :: Integer
-- 6
--
-- >>> integralDecimal # Digit7 :: Integer
-- 7
--
-- >>> integralDecimal # Digit8 :: Integer
-- 8
--
-- >>> integralDecimal # Digit9 :: Integer
-- 9
--
-- prop> \c -> (c `notElem` [0..9]) ==> (c ^? integralDecimal == Nothing)
integralDecimal ::
  (Integral a, Decimal d) =>
  Prism'
    a
    d
integralDecimal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9)]
  
-- |
--
-- >>> 1 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 7
--
-- >>> 8 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 8
--
-- >>> 9 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just 9
--
-- >>> 10 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just a
--
-- >>> 11 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just b
--
-- >>> 12 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just c
--
-- >>> 13 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just d
--
-- >>> 14 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just e
--
-- >>> 15 ^? integralHexadecimalNoZero :: Maybe Digit
-- Just f
--
-- >>> integralHexadecimalNoZero # Digit1 :: Integer
-- 1
--
-- >>> integralHexadecimalNoZero # Digit2 :: Integer
-- 2
--
-- >>> integralHexadecimalNoZero # Digit3 :: Integer
-- 3
--
-- >>> integralHexadecimalNoZero # Digit4 :: Integer
-- 4
--
-- >>> integralHexadecimalNoZero # Digit5 :: Integer
-- 5
--
-- >>> integralHexadecimalNoZero # Digit6 :: Integer
-- 6
--
-- >>> integralHexadecimalNoZero # Digit7 :: Integer
-- 7
--
-- >>> integralHexadecimalNoZero # Digit8 :: Integer
-- 8
--
-- >>> integralHexadecimalNoZero # Digit9 :: Integer
-- 9
--
-- >>> integralHexadecimalNoZero # Digita :: Integer
-- 10
--
-- >>> integralHexadecimalNoZero # Digitb :: Integer
-- 11
--
-- >>> integralHexadecimalNoZero # Digitc :: Integer
-- 12
--
-- >>> integralHexadecimalNoZero # Digitd :: Integer
-- 13
--
-- >>> integralHexadecimalNoZero # Digite :: Integer
-- 14
--
-- >>> integralHexadecimalNoZero # Digitf :: Integer
-- 15
--
-- prop> \c -> (c `notElem` [1..15]) ==> (c ^? integralHexadecimalNoZero == Nothing)
integralHexadecimalNoZero ::
  (Integral a, HexadecimalNoZero d) =>
  Prism'
    a
    d
integralHexadecimalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, da), (11, db), (12, dc), (13, dd), (14, de), (15, df)]
  
-- |
--
-- >>> 0 ^? integralHexadecimal :: Maybe Digit
-- Just 0
--
-- >>> 1 ^? integralHexadecimal :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralHexadecimal :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralHexadecimal :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralHexadecimal :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralHexadecimal :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralHexadecimal :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralHexadecimal :: Maybe Digit
-- Just 7
--
-- >>> 8 ^? integralHexadecimal :: Maybe Digit
-- Just 8
--
-- >>> 9 ^? integralHexadecimal :: Maybe Digit
-- Just 9
--
-- >>> 10 ^? integralHexadecimal :: Maybe Digit
-- Just a
--
-- >>> 11 ^? integralHexadecimal :: Maybe Digit
-- Just b
--
-- >>> 12 ^? integralHexadecimal :: Maybe Digit
-- Just c
--
-- >>> 13 ^? integralHexadecimal :: Maybe Digit
-- Just d
--
-- >>> 14 ^? integralHexadecimal :: Maybe Digit
-- Just e
--
-- >>> 15 ^? integralHexadecimal :: Maybe Digit
-- Just f
--
-- >>> integralHexadecimal # Digit0 :: Integer
-- 0
--
-- >>> integralHexadecimal # Digit1 :: Integer
-- 1
--
-- >>> integralHexadecimal # Digit2 :: Integer
-- 2
--
-- >>> integralHexadecimal # Digit3 :: Integer
-- 3
--
-- >>> integralHexadecimal # Digit4 :: Integer
-- 4
--
-- >>> integralHexadecimal # Digit5 :: Integer
-- 5
--
-- >>> integralHexadecimal # Digit6 :: Integer
-- 6
--
-- >>> integralHexadecimal # Digit7 :: Integer
-- 7
--
-- >>> integralHexadecimal # Digit8 :: Integer
-- 8
--
-- >>> integralHexadecimal # Digit9 :: Integer
-- 9
--
-- >>> integralHexadecimal # Digita :: Integer
-- 10
--
-- >>> integralHexadecimal # Digitb :: Integer
-- 11
--
-- >>> integralHexadecimal # Digitc :: Integer
-- 12
--
-- >>> integralHexadecimal # Digitd :: Integer
-- 13
--
-- >>> integralHexadecimal # Digite :: Integer
-- 14
--
-- >>> integralHexadecimal # Digitf :: Integer
-- 15
--
-- prop> \c -> (c `notElem` [0..15]) ==> (c ^? integralHexadecimal == Nothing)
integralHexadecimal ::
  (Integral a, Hexadecimal d) =>
  Prism'
    a
    d
integralHexadecimal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, da), (11, db), (12, dc), (13, dd), (14, de), (15, df)]

-- |
--
-- >>> 1 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 7
--
-- >>> 8 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 8
--
-- >>> 9 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just 9
--
-- >>> 10 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just A
--
-- >>> 11 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just B
--
-- >>> 12 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just C
--
-- >>> 13 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just D
--
-- >>> 14 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just E
--
-- >>> 15 ^? integralHEXADECIMALNoZero :: Maybe Digit
-- Just F
--
-- >>> integralHEXADECIMALNoZero # Digit1 :: Integer
-- 1
--
-- >>> integralHEXADECIMALNoZero # Digit2 :: Integer
-- 2
--
-- >>> integralHEXADECIMALNoZero # Digit3 :: Integer
-- 3
--
-- >>> integralHEXADECIMALNoZero # Digit4 :: Integer
-- 4
--
-- >>> integralHEXADECIMALNoZero # Digit5 :: Integer
-- 5
--
-- >>> integralHEXADECIMALNoZero # Digit6 :: Integer
-- 6
--
-- >>> integralHEXADECIMALNoZero # Digit7 :: Integer
-- 7
--
-- >>> integralHEXADECIMALNoZero # Digit8 :: Integer
-- 8
--
-- >>> integralHEXADECIMALNoZero # Digit9 :: Integer
-- 9
--
-- >>> integralHEXADECIMALNoZero # DigitA :: Integer
-- 10
--
-- >>> integralHEXADECIMALNoZero # DigitB :: Integer
-- 11
--
-- >>> integralHEXADECIMALNoZero # DigitC :: Integer
-- 12
--
-- >>> integralHEXADECIMALNoZero # DigitD :: Integer
-- 13
--
-- >>> integralHEXADECIMALNoZero # DigitE :: Integer
-- 14
--
-- >>> integralHEXADECIMALNoZero # DigitF :: Integer
-- 15
--
-- prop> \c -> (c `notElem` [1..15]) ==> (c ^? integralHEXADECIMALNoZero == Nothing)
integralHEXADECIMALNoZero ::
  (Integral a, HEXADECIMALNoZero d) =>
  Prism'
    a
    d
integralHEXADECIMALNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, dA), (11, dB), (12, dC), (13, dD), (14, dE), (15, dF)]
  

-- |
--
-- >>> 0 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 0
--
-- >>> 1 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 1
--
-- >>> 2 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 2
--
-- >>> 3 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 3
--
-- >>> 4 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 4
--
-- >>> 5 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 5
--
-- >>> 6 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 6
--
-- >>> 7 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 7
--
-- >>> 8 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 8
--
-- >>> 9 ^? integralHEXADECIMAL :: Maybe Digit
-- Just 9
--
-- >>> 10 ^? integralHEXADECIMAL :: Maybe Digit
-- Just A
--
-- >>> 11 ^? integralHEXADECIMAL :: Maybe Digit
-- Just B
--
-- >>> 12 ^? integralHEXADECIMAL :: Maybe Digit
-- Just C
--
-- >>> 13 ^? integralHEXADECIMAL :: Maybe Digit
-- Just D
--
-- >>> 14 ^? integralHEXADECIMAL :: Maybe Digit
-- Just E
--
-- >>> 15 ^? integralHEXADECIMAL :: Maybe Digit
-- Just F
--
-- >>> integralHEXADECIMAL # Digit0 :: Integer
-- 0
--
-- >>> integralHEXADECIMAL # Digit1 :: Integer
-- 1
--
-- >>> integralHEXADECIMAL # Digit2 :: Integer
-- 2
--
-- >>> integralHEXADECIMAL # Digit3 :: Integer
-- 3
--
-- >>> integralHEXADECIMAL # Digit4 :: Integer
-- 4
--
-- >>> integralHEXADECIMAL # Digit5 :: Integer
-- 5
--
-- >>> integralHEXADECIMAL # Digit6 :: Integer
-- 6
--
-- >>> integralHEXADECIMAL # Digit7 :: Integer
-- 7
--
-- >>> integralHEXADECIMAL # Digit8 :: Integer
-- 8
--
-- >>> integralHEXADECIMAL # Digit9 :: Integer
-- 9
--
-- >>> integralHEXADECIMAL # DigitA :: Integer
-- 10
--
-- >>> integralHEXADECIMAL # DigitB :: Integer
-- 11
--
-- >>> integralHEXADECIMAL # DigitC :: Integer
-- 12
--
-- >>> integralHEXADECIMAL # DigitD :: Integer
-- 13
--
-- >>> integralHEXADECIMAL # DigitE :: Integer
-- 14
--
-- >>> integralHEXADECIMAL # DigitF :: Integer
-- 15
--
-- prop> \c -> (c `notElem` [0..15]) ==> (c ^? integralHEXADECIMAL == Nothing)
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
