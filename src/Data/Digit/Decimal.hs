{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.Decimal(
  DecDigit(..)
, DecimalNoZero
, Decimal
, parseDecimalNoZero
, parseDecimal
-- * Prisms
, _DecDigit0
, _DecDigit1
, _DecDigit2
, _DecDigit3
, _DecDigit4
, _DecDigit5
, _DecDigit6
, _DecDigit7
, _DecDigit8
, _DecDigit9
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
) where

import Prelude (Eq, Show, Ord)
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

data DecDigit
  = DecDigit0
  | DecDigit1
  | DecDigit2
  | DecDigit3
  | DecDigit4
  | DecDigit5
  | DecDigit6
  | DecDigit7
  | DecDigit8
  | DecDigit9
  deriving (Show, Eq, Ord)

makePrisms ''DecDigit

instance D0 DecDigit where; d0 = _DecDigit0
instance D1 DecDigit where; d1 = _DecDigit1
instance D2 DecDigit where; d2 = _DecDigit2
instance D3 DecDigit where; d3 = _DecDigit3
instance D4 DecDigit where; d4 = _DecDigit4
instance D5 DecDigit where; d5 = _DecDigit5
instance D6 DecDigit where; d6 = _DecDigit6
instance D7 DecDigit where; d7 = _DecDigit7
instance D8 DecDigit where; d8 = _DecDigit8
instance D9 DecDigit where; d9 = _DecDigit9

type DecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d)

parseDecimalNoZero ::
  (DecimalNoZero d, CharParsing p) =>
  p d
parseDecimalNoZero =
  choice
    [
      parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    , parse8
    , parse9
    ] <?> "DecimalNoZero"

type Decimal d =
  (D0 d, DecimalNoZero d)

parseDecimal ::
  (Decimal d, CharParsing p) =>
  p d
parseDecimal =
  choice
    [
      parse0
    , parseDecimalNoZero
    ] <?> "Decimal"
