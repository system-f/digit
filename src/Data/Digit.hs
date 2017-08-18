{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit(
  module D
) where

import Data.Digit.Binary as D
import Data.Digit.Decimal as D
import Data.Digit.Octal as D
import Data.Digit.Hexadecimal as D
import Data.Digit.HeXaDeCiMaL as D
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
import Data.Digit.DAa as D
import Data.Digit.DBb as D
import Data.Digit.DCc as D
import Data.Digit.DDd as D
import Data.Digit.DEe as D
import Data.Digit.DFf as D
import Data.Digit.Digit0 as D
import Data.Digit.Digit1 as D
import Data.Digit.Digit2 as D
import Data.Digit.Digit3 as D
import Data.Digit.Digit4 as D
import Data.Digit.Digit5 as D
import Data.Digit.Digit6 as D
import Data.Digit.Digit7 as D
import Data.Digit.Digit8 as D
import Data.Digit.Digit9 as D
import Data.Digit.Digita as D
import Data.Digit.DigitA as D
import Data.Digit.Digitb as D
import Data.Digit.DigitB as D
import Data.Digit.Digitc as D
import Data.Digit.DigitC as D
import Data.Digit.Digitd as D
import Data.Digit.DigitD as D
import Data.Digit.Digite as D
import Data.Digit.DigitE as D
import Data.Digit.Digitf as D
import Data.Digit.DigitF as D

import Control.Lens
import Control.Lens.Extras(is)
import Data.List
import Data.Maybe
import Prelude

-- Integral:
--   BinaryNotZero, Binary, Octal, OctalNotZero, Decimal, DecimalNotZero, Hexadecimal, HexadecimalNotZero, HeXaDeCiMaL, HeXaDeCiMaLNotZero, HEXADECIMAL, HEXADECIMALNotZero
-- Char:
--   BinaryNotZero, Binary, Octal, OctalNotZero, Decimal, DecimalNotZero, Hexadecimal, HexadecimalNotZero, HeXaDeCiMaL, HeXaDeCiMaLNotZero, HEXADECIMAL, HEXADECIMALNotZero

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

integralBinaryNoZero ::
  (Integral a, BinaryNoZero d) =>
  Prism'
    a
    d
integralBinaryNoZero =
  associatePrism (1, d1) []
  
integralBinary ::
  (Integral a, Binary d) =>
  Prism'
    a
    d
integralBinary =
  associatePrism (0, d0) [(1, d1)]
  
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
  
charBinaryNoZero ::
  BinaryNoZero d =>
  Prism'
    Char
    d
charBinaryNoZero =
  associatePrism ('1', d1) []
  
charBinary ::
  Binary d =>
  Prism'
    Char
    d
charBinary =
  associatePrism ('0', d0) [('1', d1)]
  
charOctalNoZero ::
  OctalNoZero d =>
  Prism'
    Char
    d
charOctalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7)]
  
charOctal ::
  Octal d =>
  Prism'
    Char
    d
charOctal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7)]
  
charDecimalNoZero ::
  DecimalNoZero d =>
  Prism'
    Char
    d
charDecimalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9)]
  
charDecimal ::
  Decimal d =>
  Prism'
    Char
    d
charDecimal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9)]
  
charHexadecimalNoZero ::
  HexadecimalNoZero d =>
  Prism'
    Char
    d
charHexadecimalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df)]
  
charHexadecimal ::
  Hexadecimal d =>
  Prism'
    Char
    d
charHexadecimal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df)]
  
charHEXADECIMALNoZero ::
  HEXADECIMALNoZero d =>
  Prism'
    Char
    d
charHEXADECIMALNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  
charHEXADECIMAL ::
  HEXADECIMAL d =>
  Prism'
    Char
    d
charHEXADECIMAL =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  
charHeXaDeCiMaLNoZero ::
  HeXaDeCiMaLNoZero d =>
  Prism'
    Char
    d
charHeXaDeCiMaLNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  
charHeXaDeCiMaL ::
  HeXaDeCiMaL d =>
  Prism'
    Char
    d
charHeXaDeCiMaL =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  