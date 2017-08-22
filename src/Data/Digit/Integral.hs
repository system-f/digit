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

import Control.Lens.Extras(is)
import Papa

-- |
--
-- >>> 1 ^? integralBinaryNoZero
-- Just ()
--
-- >>> integralBinaryNoZero # Left () :: Integer
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
-- >>> 0 ^? integralBinary
-- Just ()
--
--
-- >>> 1 ^? integralBinary
-- Just ()
--
-- >>> integralBinary # Left () :: Integer
-- 1
--
-- >>> integralBinary # (Right (Left ()) :: Either () (Either () Void)) :: Integer
-- 0
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
-- >>> 1 ^? integralOctalNoZero
-- Just ()
--
-- >>> 2 ^? integralOctalNoZero
-- Just ()
--
-- >>> 3 ^? integralOctalNoZero
-- Just ()
--
-- >>> 4 ^? integralOctalNoZero
-- Just ()
--
-- >>> 5 ^? integralOctalNoZero
-- Just ()
--
-- >>> 6 ^? integralOctalNoZero
-- Just ()
--
-- >>> 7 ^? integralOctalNoZero
-- Just ()
--
-- >>> integralOctalNoZero # Left () :: Integer
-- 1
--
-- prop> \c -> (c `notElem` [0..7]) ==> (c ^? integralOctalNoZero == Nothing)
integralOctalNoZero ::
  (Integral a, OctalNoZero d) =>
  Prism'
    a
    d
integralOctalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7)]
  
integralOctal ::
  (Integral a, Octal d) =>
  Prism'
    a
    d
integralOctal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7)]
  
integralDecimalNoZero ::
  (Integral a, DecimalNoZero d) =>
  Prism'
    a
    d
integralDecimalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9)]
  
integralDecimal ::
  (Integral a, Decimal d) =>
  Prism'
    a
    d
integralDecimal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9)]
  
integralHexadecimalNoZero ::
  (Integral a, HexadecimalNoZero d) =>
  Prism'
    a
    d
integralHexadecimalNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, da), (11, db), (12, dc), (13, dd), (14, de), (15, df)]
  
integralHexadecimal ::
  (Integral a, Hexadecimal d) =>
  Prism'
    a
    d
integralHexadecimal =
  associatePrism (0, d0) [(1, d1), (2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, da), (11, db), (12, dc), (13, dd), (14, de), (15, df)]
  
integralHEXADECIMALNoZero ::
  (Integral a, HEXADECIMALNoZero d) =>
  Prism'
    a
    d
integralHEXADECIMALNoZero =
  associatePrism (1, d1) [(2, d2), (3, d3), (4, d4), (5, d5), (6, d6), (7, d7), (8, d8), (9, d9), (10, dA), (11, dB), (12, dC), (13, dD), (14, dE), (15, dF)]
  
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
