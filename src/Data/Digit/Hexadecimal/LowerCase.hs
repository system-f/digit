{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.Hexadecimal.LowerCase(
  HexDigit(..)
, HexadecimalNoZero
, Hexadecimal
, parseHexadecimalNoZero
, parseHexadecimal
-- * Prisms
, _HexDigit0
, _HexDigit1
, _HexDigit2
, _HexDigit3
, _HexDigit4
, _HexDigit5
, _HexDigit6
, _HexDigit7
, _HexDigit8
, _HexDigit9
, _HexDigita
, _HexDigitb
, _HexDigitc
, _HexDigitd
, _HexDigite
, _HexDigitf
-- * Re-exports
, module Data.Digit.Class.D0
, module Data.Digit.Class.D1
, module Data.Digit.Class.D2
, module Data.Digit.Class.D3
, module Data.Digit.Class.D4
, module Data.Digit.Class.D5
, module Data.Digit.Class.D6
, module Data.Digit.Class.D7
, module Data.Digit.Class.D8
, module Data.Digit.Class.D9

, module Data.Digit.Class.Lower
) where

import Prelude (Eq, Show, Ord)
import Control.Lens.TH (makePrisms)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)

import Data.Digit.Class.D0
import Data.Digit.Class.D1
import Data.Digit.Class.D2
import Data.Digit.Class.D3
import Data.Digit.Class.D4
import Data.Digit.Class.D5
import Data.Digit.Class.D6
import Data.Digit.Class.D7
import Data.Digit.Class.D8
import Data.Digit.Class.D9
import Data.Digit.Class.Lower

import Data.Digit.Decimal(parseDecimalNoZero)

data HexDigit
  = HexDigit0
  | HexDigit1
  | HexDigit2
  | HexDigit3
  | HexDigit4
  | HexDigit5
  | HexDigit6
  | HexDigit7
  | HexDigit8
  | HexDigit9
  | HexDigita
  | HexDigitb
  | HexDigitc
  | HexDigitd
  | HexDigite
  | HexDigitf
  deriving (Show, Eq, Ord)

makePrisms ''HexDigit

instance D0 HexDigit where; d0 = _HexDigit0
instance D1 HexDigit where; d1 = _HexDigit1
instance D2 HexDigit where; d2 = _HexDigit2
instance D3 HexDigit where; d3 = _HexDigit3
instance D4 HexDigit where; d4 = _HexDigit4
instance D5 HexDigit where; d5 = _HexDigit5
instance D6 HexDigit where; d6 = _HexDigit6
instance D7 HexDigit where; d7 = _HexDigit7
instance D8 HexDigit where; d8 = _HexDigit8
instance D9 HexDigit where; d9 = _HexDigit9
instance Da HexDigit where; da = _HexDigita
instance Db HexDigit where; db = _HexDigitb
instance Dc HexDigit where; dc = _HexDigitc
instance Dd HexDigit where; dd = _HexDigitd
instance De HexDigit where; de = _HexDigite
instance Df HexDigit where; df = _HexDigitf

type HexadecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

parseHexadecimalNoZero ::
  (HexadecimalNoZero d, CharParsing p) =>
  p d
parseHexadecimalNoZero =
  choice
    [
      parseDecimalNoZero
    , parsea
    , parseb
    , parsec
    , parsed
    , parsee
    , parsef
    ] <?> "HexadecimalNoZero"

type Hexadecimal d =
  (D0 d, HexadecimalNoZero d)

parseHexadecimal ::
  (Hexadecimal d, CharParsing p) =>
  p d
parseHexadecimal =
  choice
    [
      parse0
    , parseHexadecimalNoZero
    ] <?> "Hexadecimal"
