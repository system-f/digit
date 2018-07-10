{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.HEXADECIMAL(
  HEXDigit(..)
, HEXADECIMALNoZero
, HEXADECIMAL
, parseHEXADECIMALNoZero
, parseHEXADECIMAL
-- * Prisms
, _HEXDigit0
, _HEXDigit1
, _HEXDigit2
, _HEXDigit3
, _HEXDigit4
, _HEXDigit5
, _HEXDigit6
, _HEXDigit7
, _HEXDigit8
, _HEXDigit9
, _HEXDigitA
, _HEXDigitB
, _HEXDigitC
, _HEXDigitD
, _HEXDigitE
, _HEXDigitF
-- * Re-exports
, module Data.Digit.D0
, module Data.Digit.D1
, module Data.Digit.D2
, module Data.Digit.D3
, module Data.Digit.D4
, module Data.Digit.D5
, module Data.Digit.D6
, module Data.Digit.D7
, module Data.Digit.D8
, module Data.Digit.D9
, module Data.Digit.DA
, module Data.Digit.DB
, module Data.Digit.DC
, module Data.Digit.DD
, module Data.Digit.DE
, module Data.Digit.DF
) where

import Prelude (Eq, Show, Ord, Enum, Bounded)
import Control.Lens.TH (makePrisms)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0
import Data.Digit.D1
import Data.Digit.D2
import Data.Digit.D3
import Data.Digit.D4
import Data.Digit.D5
import Data.Digit.D6
import Data.Digit.D7
import Data.Digit.D8
import Data.Digit.D9
import Data.Digit.DA
import Data.Digit.DB
import Data.Digit.DC
import Data.Digit.DD
import Data.Digit.DE
import Data.Digit.DF
import Data.Digit.Decimal(parseDecimalNoZero)

data HEXDigit
  = HEXDigit0
  | HEXDigit1
  | HEXDigit2
  | HEXDigit3
  | HEXDigit4
  | HEXDigit5
  | HEXDigit6
  | HEXDigit7
  | HEXDigit8
  | HEXDigit9
  | HEXDigitA
  | HEXDigitB
  | HEXDigitC
  | HEXDigitD
  | HEXDigitE
  | HEXDigitF
  deriving (Show, Eq, Ord, Enum, Bounded)

makePrisms ''HEXDigit

instance D0 HEXDigit where; d0 = _HEXDigit0
instance D1 HEXDigit where; d1 = _HEXDigit1
instance D2 HEXDigit where; d2 = _HEXDigit2
instance D3 HEXDigit where; d3 = _HEXDigit3
instance D4 HEXDigit where; d4 = _HEXDigit4
instance D5 HEXDigit where; d5 = _HEXDigit5
instance D6 HEXDigit where; d6 = _HEXDigit6
instance D7 HEXDigit where; d7 = _HEXDigit7
instance D8 HEXDigit where; d8 = _HEXDigit8
instance D9 HEXDigit where; d9 = _HEXDigit9
instance DA HEXDigit where; dA = _HEXDigitA
instance DB HEXDigit where; dB = _HEXDigitB
instance DC HEXDigit where; dC = _HEXDigitC
instance DD HEXDigit where; dD = _HEXDigitD
instance DE HEXDigit where; dE = _HEXDigitE
instance DF HEXDigit where; dF = _HEXDigitF

type HEXADECIMALNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d)

parseHEXADECIMALNoZero ::
  (HEXADECIMALNoZero d, CharParsing p) =>
  p d
parseHEXADECIMALNoZero =
  choice
    [
      parseDecimalNoZero
    , parseA
    , parseB
    , parseC
    , parseD
    , parseE
    , parseF
    ] <?> "HEXADECIMALNoZero"

type HEXADECIMAL d =
  (D0 d, HEXADECIMALNoZero d)

parseHEXADECIMAL ::
  (HEXADECIMAL d, CharParsing p) =>
  p d
parseHEXADECIMAL =
  choice
    [
      parse0
    , parseHEXADECIMALNoZero
    ] <?> "HEXADECIMAL"
