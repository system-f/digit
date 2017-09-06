{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Char(
  charBinaryNoZero
, charBinary
, charOctalNoZero
, charOctal
, charDecimalNoZero
, charDecimal
, charHexadecimalNoZero
, charHexadecimal
, charHEXADECIMALNoZero
, charHEXADECIMAL
, charHeXaDeCiMaLNoZero
, charHeXaDeCiMaL
) where

import Control.Lens.Extras(is)
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
import Papa


-- $setup
-- >>> import Data.Digit.Digit

-- |
--
-- >>> '1' ^? charBinaryNoZero :: Maybe Digit
-- Just 1
--
-- >>> charBinaryNoZero # Digit1
-- '1'
--
-- prop> \c -> c /= '1' ==> (c ^? charBinaryNoZero == Nothing)
charBinaryNoZero ::
  BinaryNoZero d =>
  Prism'
    Char
    d
charBinaryNoZero =
  associatePrism ('1', d1) []
  
-- |
--
-- >>> '0' ^? charBinary :: Maybe Digit
-- Just 0
--
-- >>> '1' ^? charBinary :: Maybe Digit
-- Just 1
--
-- >>> charBinary # Digit0 :: Char
-- '0'
--
-- >>> charBinary # Digit1 :: Char
-- '1'
--
-- prop> \c -> (c `notElem` "01") ==> (c ^? charBinary == Nothing)
charBinary ::
  Binary d =>
  Prism'
    Char
    d
charBinary =
  associatePrism ('0', d0) [('1', d1)]
  
-- |
--
-- >>> '1' ^? charOctalNoZero :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charOctalNoZero :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charOctalNoZero :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charOctalNoZero :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charOctalNoZero :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charOctalNoZero :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charOctalNoZero :: Maybe Digit
-- Just 7
--
-- >>> charOctalNoZero # Digit1 :: Char
-- '1'
--
-- >>> charOctalNoZero # Digit2 :: Char
-- '2'
--
-- >>> charOctalNoZero # Digit3 :: Char
-- '3'
--
-- >>> charOctalNoZero # Digit4 :: Char
-- '4'
--
-- >>> charOctalNoZero # Digit5 :: Char
-- '5'
--
-- >>> charOctalNoZero # Digit6 :: Char
-- '6'
--
-- >>> charOctalNoZero # Digit7 :: Char
-- '7'
--
-- prop> \c -> (c `notElem` "1234567") ==> (c ^? charOctalNoZero == Nothing)
charOctalNoZero ::
  OctalNoZero d =>
  Prism'
    Char
    d
charOctalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7)]
  
-- |
--
-- >>> '0' ^? charOctal :: Maybe Digit
-- Just 0
--
-- >>> '1' ^? charOctal :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charOctal :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charOctal :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charOctal :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charOctal :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charOctal :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charOctal :: Maybe Digit
-- Just 7
--
-- >>> charOctal # Digit0 :: Char
-- '0'
--
-- >>> charOctal # Digit1 :: Char
-- '1'
--
-- >>> charOctal # Digit2 :: Char
-- '2'
--
-- >>> charOctal # Digit3 :: Char
-- '3'
--
-- >>> charOctal # Digit4 :: Char
-- '4'
--
-- >>> charOctal # Digit5 :: Char
-- '5'
--
-- >>> charOctal # Digit6 :: Char
-- '6'
--
-- >>> charOctal # Digit7 :: Char
-- '7'
--
-- prop> \c -> (c `notElem` "01234567") ==> (c ^? charOctal == Nothing)
charOctal ::
  Octal d =>
  Prism'
    Char
    d
charOctal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7)]
  
-- |
--
-- >>> '1' ^? charDecimalNoZero :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charDecimalNoZero :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charDecimalNoZero :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charDecimalNoZero :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charDecimalNoZero :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charDecimalNoZero :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charDecimalNoZero :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charDecimalNoZero :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charDecimalNoZero :: Maybe Digit
-- Just 9
--
-- >>> charDecimalNoZero # Digit1 :: Char
-- '1'
--
-- >>> charDecimalNoZero # Digit2 :: Char
-- '2'
--
-- >>> charDecimalNoZero # Digit3 :: Char
-- '3'
--
-- >>> charDecimalNoZero # Digit4 :: Char
-- '4'
--
-- >>> charDecimalNoZero # Digit5 :: Char
-- '5'
--
-- >>> charDecimalNoZero # Digit6 :: Char
-- '6'
--
-- >>> charDecimalNoZero # Digit7 :: Char
-- '7'
--
-- >>> charDecimalNoZero # Digit8 :: Char
-- '8'
--
-- >>> charDecimalNoZero # Digit9 :: Char
-- '9'
--
-- prop> \c -> (c `notElem` "123456789") ==> (c ^? charDecimalNoZero == Nothing)
charDecimalNoZero ::
  DecimalNoZero d =>
  Prism'
    Char
    d
charDecimalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9)]
  
-- |
--
-- >>> '0' ^? charDecimal :: Maybe Digit
-- Just 0
--
-- >>> '1' ^? charDecimal :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charDecimal :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charDecimal :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charDecimal :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charDecimal :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charDecimal :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charDecimal :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charDecimal :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charDecimal :: Maybe Digit
-- Just 9
--
-- >>> charDecimal # Digit0 :: Char
-- '0'
--
-- >>> charDecimal # Digit1 :: Char
-- '1'
--
-- >>> charDecimal # Digit2 :: Char
-- '2'
--
-- >>> charDecimal # Digit3 :: Char
-- '3'
--
-- >>> charDecimal # Digit4 :: Char
-- '4'
--
-- >>> charDecimal # Digit5 :: Char
-- '5'
--
-- >>> charDecimal # Digit6 :: Char
-- '6'
--
-- >>> charDecimal # Digit7 :: Char
-- '7'
--
-- >>> charDecimal # Digit8 :: Char
-- '8'
--
-- >>> charDecimal # Digit9 :: Char
-- '9'
--
-- prop> \c -> (c `notElem` "0123456789") ==> (c ^? charDecimal == Nothing)
charDecimal ::
  Decimal d =>
  Prism'
    Char
    d
charDecimal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9)]
  
-- |
--
-- >>> '1' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charHexadecimalNoZero :: Maybe Digit
-- Just 9
--
-- >>> 'a' ^? charHexadecimalNoZero :: Maybe Digit
-- Just a
--
-- >>> 'b' ^? charHexadecimalNoZero :: Maybe Digit
-- Just b
--
-- >>> 'c' ^? charHexadecimalNoZero :: Maybe Digit
-- Just c
--
-- >>> 'd' ^? charHexadecimalNoZero :: Maybe Digit
-- Just d
--
-- >>> 'e' ^? charHexadecimalNoZero :: Maybe Digit
-- Just e
--
-- >>> 'f' ^? charHexadecimalNoZero :: Maybe Digit
-- Just f
--
-- >>> charHexadecimalNoZero # Digit1 :: Char
-- '1'
--
-- >>> charHexadecimalNoZero # Digit2 :: Char
-- '2'
--
-- >>> charHexadecimalNoZero # Digit3 :: Char
-- '3'
--
-- >>> charHexadecimalNoZero # Digit4 :: Char
-- '4'
--
-- >>> charHexadecimalNoZero # Digit5 :: Char
-- '5'
--
-- >>> charHexadecimalNoZero # Digit6 :: Char
-- '6'
--
-- >>> charHexadecimalNoZero # Digit7 :: Char
-- '7'
--
-- >>> charHexadecimalNoZero # Digit8 :: Char
-- '8'
--
-- >>> charHexadecimalNoZero # Digit9 :: Char
-- '9'
--
-- >>> charHexadecimalNoZero # Digita :: Char
-- 'a'
--
-- >>> charHexadecimalNoZero # Digitb :: Char
-- 'b'
--
-- >>> charHexadecimalNoZero # Digitc :: Char
-- 'c'
--
-- >>> charHexadecimalNoZero # Digitd :: Char
-- 'd'
--
-- >>> charHexadecimalNoZero # Digite :: Char
-- 'e'
--
-- >>> charHexadecimalNoZero # Digitf :: Char
-- 'f'
--
-- prop> \c -> (c `notElem` "123456789abcdef") ==> (c ^? charHexadecimalNoZero == Nothing)
charHexadecimalNoZero ::
  HexadecimalNoZero d =>
  Prism'
    Char
    d
charHexadecimalNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df)]
  
-- |
--
-- >>> '0' ^? charHexadecimal :: Maybe Digit
-- Just 0
--
-- >>> '1' ^? charHexadecimal :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charHexadecimal :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charHexadecimal :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charHexadecimal :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charHexadecimal :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charHexadecimal :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charHexadecimal :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charHexadecimal :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charHexadecimal :: Maybe Digit
-- Just 9
--
-- >>> 'a' ^? charHexadecimal :: Maybe Digit
-- Just a
--
-- >>> 'b' ^? charHexadecimal :: Maybe Digit
-- Just b
--
-- >>> 'c' ^? charHexadecimal :: Maybe Digit
-- Just c
--
-- >>> 'd' ^? charHexadecimal :: Maybe Digit
-- Just d
--
-- >>> 'e' ^? charHexadecimal :: Maybe Digit
-- Just e
--
-- >>> 'f' ^? charHexadecimal :: Maybe Digit
-- Just f
--
-- >>> charHexadecimal # Digit0 :: Char
-- '0'
--
-- >>> charHexadecimal # Digit1 :: Char
-- '1'
--
-- >>> charHexadecimal # Digit2 :: Char
-- '2'
--
-- >>> charHexadecimal # Digit3 :: Char
-- '3'
--
-- >>> charHexadecimal # Digit4 :: Char
-- '4'
--
-- >>> charHexadecimal # Digit5 :: Char
-- '5'
--
-- >>> charHexadecimal # Digit6 :: Char
-- '6'
--
-- >>> charHexadecimal # Digit7 :: Char
-- '7'
--
-- >>> charHexadecimal # Digit8 :: Char
-- '8'
--
-- >>> charHexadecimal # Digit9 :: Char
-- '9'
--
-- >>> charHexadecimal # Digita :: Char
-- 'a'
--
-- >>> charHexadecimal # Digitb :: Char
-- 'b'
--
-- >>> charHexadecimal # Digitc :: Char
-- 'c'
--
-- >>> charHexadecimal # Digitd :: Char
-- 'd'
--
-- >>> charHexadecimal # Digite :: Char
-- 'e'
--
-- >>> charHexadecimal # Digitf :: Char
-- 'f'
--
-- prop> \c -> (c `notElem` "0123456789abcdef") ==> (c ^? charHexadecimal == Nothing)
charHexadecimal ::
  Hexadecimal d =>
  Prism'
    Char
    d
charHexadecimal =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df)]
  
-- |
--
-- >>> '1' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just 9
--
-- >>> 'A' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just A
--
-- >>> 'B' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just B
--
-- >>> 'C' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just C
--
-- >>> 'D' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just D
--
-- >>> 'E' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just E
--
-- >>> 'F' ^? charHEXADECIMALNoZero :: Maybe Digit
-- Just F
--
-- >>> charHEXADECIMALNoZero # Digit1 :: Char
-- '1'
--
-- >>> charHEXADECIMALNoZero # Digit2 :: Char
-- '2'
--
-- >>> charHEXADECIMALNoZero # Digit3 :: Char
-- '3'
--
-- >>> charHEXADECIMALNoZero # Digit4 :: Char
-- '4'
--
-- >>> charHEXADECIMALNoZero # Digit5 :: Char
-- '5'
--
-- >>> charHEXADECIMALNoZero # Digit6 :: Char
-- '6'
--
-- >>> charHEXADECIMALNoZero # Digit7 :: Char
-- '7'
--
-- >>> charHEXADECIMALNoZero # Digit8 :: Char
-- '8'
--
-- >>> charHEXADECIMALNoZero # Digit9 :: Char
-- '9'
--
-- >>> charHEXADECIMALNoZero # DigitA :: Char
-- 'A'
--
-- >>> charHEXADECIMALNoZero # DigitB :: Char
-- 'B'
--
-- >>> charHEXADECIMALNoZero # DigitC :: Char
-- 'C'
--
-- >>> charHEXADECIMALNoZero # DigitD :: Char
-- 'D'
--
-- >>> charHEXADECIMALNoZero # DigitE :: Char
-- 'E'
--
-- >>> charHEXADECIMALNoZero # DigitF :: Char
-- 'F'
--
-- prop> \c -> (c `notElem` "123456789ABCDEF") ==> (c ^? charHEXADECIMALNoZero == Nothing)
charHEXADECIMALNoZero ::
  HEXADECIMALNoZero d =>
  Prism'
    Char
    d
charHEXADECIMALNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  
-- |
--
-- >>> '0' ^? charHEXADECIMAL :: Maybe Digit
-- Just 0
--
-- >>> '1' ^? charHEXADECIMAL :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charHEXADECIMAL :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charHEXADECIMAL :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charHEXADECIMAL :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charHEXADECIMAL :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charHEXADECIMAL :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charHEXADECIMAL :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charHEXADECIMAL :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charHEXADECIMAL :: Maybe Digit
-- Just 9
--
-- >>> 'A' ^? charHEXADECIMAL :: Maybe Digit
-- Just A
--
-- >>> 'B' ^? charHEXADECIMAL :: Maybe Digit
-- Just B
--
-- >>> 'C' ^? charHEXADECIMAL :: Maybe Digit
-- Just C
--
-- >>> 'D' ^? charHEXADECIMAL :: Maybe Digit
-- Just D
--
-- >>> 'E' ^? charHEXADECIMAL :: Maybe Digit
-- Just E
--
-- >>> 'F' ^? charHEXADECIMAL :: Maybe Digit
-- Just F
--
-- >>> charHEXADECIMAL # Digit0 :: Char
-- '0'
--
-- >>> charHEXADECIMAL # Digit1 :: Char
-- '1'
--
-- >>> charHEXADECIMAL # Digit2 :: Char
-- '2'
--
-- >>> charHEXADECIMAL # Digit3 :: Char
-- '3'
--
-- >>> charHEXADECIMAL # Digit4 :: Char
-- '4'
--
-- >>> charHEXADECIMAL # Digit5 :: Char
-- '5'
--
-- >>> charHEXADECIMAL # Digit6 :: Char
-- '6'
--
-- >>> charHEXADECIMAL # Digit7 :: Char
-- '7'
--
-- >>> charHEXADECIMAL # Digit8 :: Char
-- '8'
--
-- >>> charHEXADECIMAL # Digit9 :: Char
-- '9'
--
-- >>> charHEXADECIMAL # DigitA :: Char
-- 'A'
--
-- >>> charHEXADECIMAL # DigitB :: Char
-- 'B'
--
-- >>> charHEXADECIMAL # DigitC :: Char
-- 'C'
--
-- >>> charHEXADECIMAL # DigitD :: Char
-- 'D'
--
-- >>> charHEXADECIMAL # DigitE :: Char
-- 'E'
--
-- >>> charHEXADECIMAL # DigitF :: Char
-- 'F'
--
-- prop> \c -> (c `notElem` "0123456789ABCDEF") ==> (c ^? charHEXADECIMAL == Nothing)
charHEXADECIMAL ::
  HEXADECIMAL d =>
  Prism'
    Char
    d
charHEXADECIMAL =
  associatePrism ('0', d0) [('1', d1), ('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  
-- |
--
-- >>> '1' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just 9
--
-- >>> 'a' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just a
--
-- >>> 'b' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just b
--
-- >>> 'c' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just c
--
-- >>> 'd' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just d
--
-- >>> 'e' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just e
--
-- >>> 'f' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just f
--
-- >>> 'A' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just A
--
-- >>> 'B' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just B
--
-- >>> 'C' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just C
--
-- >>> 'D' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just D
--
-- >>> 'E' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just E
--
-- >>> 'F' ^? charHeXaDeCiMaLNoZero :: Maybe Digit
-- Just F
--
-- >>> charHeXaDeCiMaLNoZero # Digit1 :: Char
-- '1'
--
-- >>> charHeXaDeCiMaLNoZero # Digit2 :: Char
-- '2'
--
-- >>> charHeXaDeCiMaLNoZero # Digit3 :: Char
-- '3'
--
-- >>> charHeXaDeCiMaLNoZero # Digit4 :: Char
-- '4'
--
-- >>> charHeXaDeCiMaLNoZero # Digit5 :: Char
-- '5'
--
-- >>> charHeXaDeCiMaLNoZero # Digit6 :: Char
-- '6'
--
-- >>> charHeXaDeCiMaLNoZero # Digit7 :: Char
-- '7'
--
-- >>> charHeXaDeCiMaLNoZero # Digit8 :: Char
-- '8'
--
-- >>> charHeXaDeCiMaLNoZero # Digit9 :: Char
-- '9'
--
-- >>> charHeXaDeCiMaLNoZero # Digita :: Char
-- 'a'
--
-- >>> charHeXaDeCiMaLNoZero # Digitb :: Char
-- 'b'
--
-- >>> charHeXaDeCiMaLNoZero # Digitc :: Char
-- 'c'
--
-- >>> charHeXaDeCiMaLNoZero # Digitd :: Char
-- 'd'
--
-- >>> charHeXaDeCiMaLNoZero # Digite :: Char
-- 'e'
--
-- >>> charHeXaDeCiMaLNoZero # Digitf :: Char
-- 'f'
--
-- >>> charHeXaDeCiMaLNoZero # DigitA :: Char
-- 'A'
--
-- >>> charHeXaDeCiMaLNoZero # DigitB :: Char
-- 'B'
--
-- >>> charHeXaDeCiMaLNoZero # DigitC :: Char
-- 'C'
--
-- >>> charHeXaDeCiMaLNoZero # DigitD :: Char
-- 'D'
--
-- >>> charHeXaDeCiMaLNoZero # DigitE :: Char
-- 'E'
--
-- >>> charHeXaDeCiMaLNoZero # DigitF :: Char
-- 'F'
--
-- prop> \c -> (c `notElem` "123456789abcdefABCDEF") ==> (c ^? charHeXaDeCiMaLNoZero == Nothing)
charHeXaDeCiMaLNoZero ::
  HeXaDeCiMaLNoZero d =>
  Prism'
    Char
    d
charHeXaDeCiMaLNoZero =
  associatePrism ('1', d1) [('2', d2), ('3', d3), ('4', d4), ('5', d5), ('6', d6), ('7', d7), ('8', d8), ('9', d9), ('a', da), ('b', db), ('c', dc), ('d', dd), ('e', de), ('f', df), ('A', dA), ('B', dB), ('C', dC), ('D', dD), ('E', dE), ('F', dF)]
  

-- |
--
-- >>> '0' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 0
--
-- >>> '1' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 1
--
-- >>> '2' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 2
--
-- >>> '3' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 3
--
-- >>> '4' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 4
--
-- >>> '5' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 5
--
-- >>> '6' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 6
--
-- >>> '7' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 7
--
-- >>> '8' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 8
--
-- >>> '9' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just 9
--
-- >>> 'a' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just a
--
-- >>> 'b' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just b
--
-- >>> 'c' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just c
--
-- >>> 'd' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just d
--
-- >>> 'e' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just e
--
-- >>> 'f' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just f
--
-- >>> 'A' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just A
--
-- >>> 'B' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just B
--
-- >>> 'C' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just C
--
-- >>> 'D' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just D
--
-- >>> 'E' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just E
--
-- >>> 'F' ^? charHeXaDeCiMaL :: Maybe Digit
-- Just F
--
-- >>> charHeXaDeCiMaL # Digit0 :: Char
-- '0'
--
-- >>> charHeXaDeCiMaL # Digit1 :: Char
-- '1'
--
-- >>> charHeXaDeCiMaL # Digit2 :: Char
-- '2'
--
-- >>> charHeXaDeCiMaL # Digit3 :: Char
-- '3'
--
-- >>> charHeXaDeCiMaL # Digit4 :: Char
-- '4'
--
-- >>> charHeXaDeCiMaL # Digit5 :: Char
-- '5'
--
-- >>> charHeXaDeCiMaL # Digit6 :: Char
-- '6'
--
-- >>> charHeXaDeCiMaL # Digit7 :: Char
-- '7'
--
-- >>> charHeXaDeCiMaL # Digit8 :: Char
-- '8'
--
-- >>> charHeXaDeCiMaL # Digit9 :: Char
-- '9'
--
-- >>> charHeXaDeCiMaL # Digita :: Char
-- 'a'
--
-- >>> charHeXaDeCiMaL # Digitb :: Char
-- 'b'
--
-- >>> charHeXaDeCiMaL # Digitc :: Char
-- 'c'
--
-- >>> charHeXaDeCiMaL # Digitd :: Char
-- 'd'
--
-- >>> charHeXaDeCiMaL # Digite :: Char
-- 'e'
--
-- >>> charHeXaDeCiMaL # Digitf :: Char
-- 'f'
--
-- >>> charHeXaDeCiMaL # DigitA :: Char
-- 'A'
--
-- >>> charHeXaDeCiMaL # DigitB :: Char
-- 'B'
--
-- >>> charHeXaDeCiMaL # DigitC :: Char
-- 'C'
--
-- >>> charHeXaDeCiMaL # DigitD :: Char
-- 'D'
--
-- >>> charHeXaDeCiMaL # DigitE :: Char
-- 'E'
--
-- >>> charHeXaDeCiMaL # DigitF :: Char
-- 'F'
--
-- prop> \c -> (c `notElem` "0123456789abcdefABCDEF") ==> (c ^? charHeXaDeCiMaL == Nothing)
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
