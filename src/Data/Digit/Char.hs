{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Char(
-- * Binary
  charBinaryNoZero
, charBinary
-- * Octal
, charOctalNoZero
, charOctal
-- * Decimal
, charDecimalNoZero
, charDecimal
-- * Hexadecimal
, charHexadecimalNoZero
, charHexadecimal
-- * HEXADECIMAL
, charHEXADECIMALNoZero
, charHEXADECIMAL
-- * HeXaDeCiMaL
, charHeXaDeCiMaLNoZero
, charHeXaDeCiMaL
) where

import           Prelude                (Char, Eq, fst, lookup)

import           Control.Applicative    (Applicative)
import           Control.Lens           (APrism, Choice, Prism', clonePrism,
                                         prism', ( # ))
import           Control.Lens.Extras    (is)

import           Data.Foldable          (find)
import           Data.Functor           ((<$>))
import           Data.Maybe             (fromMaybe)

import           Data.Digit.Binary      as D
import           Data.Digit.Decimal     as D
import           Data.Digit.Hexadecimal.LowerCase as D
import           Data.Digit.Hexadecimal.UpperCase as D
import           Data.Digit.Hexadecimal.MixedCase as D
import           Data.Digit.Octal       as D

-- $setup
-- >>> import Data.Digit

-- |
--
-- >>> '1' ^? charBinaryNoZero :: Maybe BinDigit
-- Just BinDigit1
--
-- >>> charBinaryNoZero # BinDigit1
-- '1'
charBinaryNoZero ::
  BinaryNoZero d =>
  Prism'
    Char
    d
charBinaryNoZero =
  associatePrism ('1', d1) []

-- |
--
-- >>> '0' ^? charBinary :: Maybe BinDigit
-- Just BinDigit0
--
-- >>> charBinary # BinDigit0 :: Char
-- '0'
charBinary ::
  Binary d =>
  Prism'
    Char
    d
charBinary =
  associatePrism ('0', d0) [('1', d1)]

-- |
--
-- >>> '6' ^? charOctalNoZero :: Maybe OctDigit
-- Just OctDigit6
--
-- >>> charOctalNoZero # OctDigit5 :: Char
-- '5'
charOctalNoZero ::
  OctalNoZero d =>
  Prism'
    Char
    d
charOctalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7)]

-- |
-- >>> '7' ^? charOctal :: Maybe OctDigit
-- Just OctDigit7
--
-- >>> charOctal # OctDigit7 :: Char
-- '7'
charOctal ::
  Octal d =>
  Prism'
    Char
    d
charOctal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7)]

-- |
-- >>> '9' ^? charDecimalNoZero :: Maybe DecDigit
-- Just DecDigit9
--
-- >>> charDecimalNoZero # DecDigit9 :: Char
-- '9'
charDecimalNoZero ::
  DecimalNoZero d =>
  Prism'
    Char
    d
charDecimalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9)]

-- |
-- >>> '9' ^? charDecimal :: Maybe DecDigit
-- Just DecDigit9
--
-- >>> charDecimal # DecDigit9 :: Char
-- '9'
charDecimal ::
  Decimal d =>
  Prism'
    Char
    d
charDecimal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9)]

-- |
-- >>> 'f' ^? charHexadecimalNoZero :: Maybe HexDigit
-- Just HexDigitf
--
-- >>> charHexadecimalNoZero # HexDigitf :: Char
-- 'f'
charHexadecimalNoZero ::
  HexadecimalNoZero d =>
  Prism'
    Char
    d
charHexadecimalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df)]

-- |
-- >>> 'f' ^? charHexadecimal :: Maybe HexDigit
-- Just HexDigitf
--
-- >>> charHexadecimal # HexDigitf :: Char
-- 'f'
charHexadecimal ::
  Hexadecimal d =>
  Prism'
    Char
    d
charHexadecimal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df)]

-- |
-- >>> 'F' ^? charHEXADECIMALNoZero :: Maybe HEXDigit
-- Just HEXDigitF
--
-- >>> charHEXADECIMALNoZero # HEXDigitF :: Char
-- 'F'
charHEXADECIMALNoZero ::
  HEXADECIMALNoZero d =>
  Prism'
    Char
    d
charHEXADECIMALNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]

-- |
-- >>> 'F' ^? charHEXADECIMAL :: Maybe HEXDigit
-- Just HEXDigitF
--
-- >>> charHEXADECIMAL # HEXDigitF :: Char
-- 'F'
charHEXADECIMAL ::
  HEXADECIMAL d =>
  Prism'
    Char
    d
charHEXADECIMAL =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]

-- |
-- >>> 'f' ^? charHeXaDeCiMaLNoZero :: Maybe HeXDigit
-- Just HeXDigitf
--
-- >>> 'F' ^? charHeXaDeCiMaLNoZero :: Maybe HeXDigit
-- Just HeXDigitF
--
-- >>> charHeXaDeCiMaLNoZero # HeXDigitf :: Char
-- 'f'
--
-- >>> charHeXaDeCiMaLNoZero # HeXDigitF :: Char
-- 'F'
charHeXaDeCiMaLNoZero ::
  HeXaDeCiMaLNoZero d =>
  Prism'
    Char
    d
charHeXaDeCiMaLNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]


-- |
-- >>> 'f' ^? charHeXaDeCiMaL :: Maybe HeXDigit
-- Just HeXDigitf
--
-- >>> 'F' ^? charHeXaDeCiMaL :: Maybe HeXDigit
-- Just HeXDigitF
--
-- >>> charHeXaDeCiMaL # HeXDigitf :: Char
-- 'f'
--
-- >>> charHeXaDeCiMaL # HeXDigitF :: Char
-- 'F'
charHeXaDeCiMaL ::
  HeXaDeCiMaL d =>
  Prism'
    Char
    d
charHeXaDeCiMaL =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]

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
