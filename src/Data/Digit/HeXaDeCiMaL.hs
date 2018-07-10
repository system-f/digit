{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.HeXaDeCiMaL(
  module Data.Digit.D0
, module Data.Digit.D1
, module Data.Digit.D2
, module Data.Digit.D3
, module Data.Digit.D4
, module Data.Digit.D5
, module Data.Digit.D6
, module Data.Digit.D7
, module Data.Digit.D8
, module Data.Digit.D9
, module Data.Digit.DAa
, module Data.Digit.DBb
, module Data.Digit.DCc
, module Data.Digit.DDd
, module Data.Digit.DEe
, module Data.Digit.DFf
, HeXDigit(..)
, HeXaDeCiMaLNoZero
, HeXaDeCiMaL
, parseHeXaDeCiMaLNoZero
, parseHeXaDeCiMaL
-- * Prisms
, _HeXDigit0
, _HeXDigit1
, _HeXDigit2
, _HeXDigit3
, _HeXDigit4
, _HeXDigit5
, _HeXDigit6
, _HeXDigit7
, _HeXDigit8
, _HeXDigit9
, _HeXDigita
, _HeXDigitb
, _HeXDigitc
, _HeXDigitd
, _HeXDigite
, _HeXDigitf
, _HeXDigitA
, _HeXDigitB
, _HeXDigitC
, _HeXDigitD
, _HeXDigitE
, _HeXDigitF
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
import Data.Digit.Decimal(parseDecimalNoZero)
import Data.Digit.DAa
import Data.Digit.DBb
import Data.Digit.DCc
import Data.Digit.DDd
import Data.Digit.DEe
import Data.Digit.DFf

data HeXDigit
  = HeXDigit0
  | HeXDigit1
  | HeXDigit2
  | HeXDigit3
  | HeXDigit4
  | HeXDigit5
  | HeXDigit6
  | HeXDigit7
  | HeXDigit8
  | HeXDigit9
  | HeXDigita
  | HeXDigitb
  | HeXDigitc
  | HeXDigitd
  | HeXDigite
  | HeXDigitf
  | HeXDigitA
  | HeXDigitB
  | HeXDigitC
  | HeXDigitD
  | HeXDigitE
  | HeXDigitF
  deriving (Show, Eq, Ord)

makePrisms ''HeXDigit

instance D0 HeXDigit where; d0 = _HeXDigit0
instance D1 HeXDigit where; d1 = _HeXDigit1
instance D2 HeXDigit where; d2 = _HeXDigit2
instance D3 HeXDigit where; d3 = _HeXDigit3
instance D4 HeXDigit where; d4 = _HeXDigit4
instance D5 HeXDigit where; d5 = _HeXDigit5
instance D6 HeXDigit where; d6 = _HeXDigit6
instance D7 HeXDigit where; d7 = _HeXDigit7
instance D8 HeXDigit where; d8 = _HeXDigit8
instance D9 HeXDigit where; d9 = _HeXDigit9
instance DA HeXDigit where; dA = _HeXDigitA
instance Da HeXDigit where; da = _HeXDigita
instance DB HeXDigit where; dB = _HeXDigitB
instance Db HeXDigit where; db = _HeXDigitb
instance DC HeXDigit where; dC = _HeXDigitC
instance Dc HeXDigit where; dc = _HeXDigitc
instance DD HeXDigit where; dD = _HeXDigitD
instance Dd HeXDigit where; dd = _HeXDigitd
instance DE HeXDigit where; dE = _HeXDigitE
instance De HeXDigit where; de = _HeXDigite
instance DF HeXDigit where; dF = _HeXDigitF
instance Df HeXDigit where; df = _HeXDigitf

type HeXaDeCiMaLNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

parseHeXaDeCiMaLNoZero ::
  (HeXaDeCiMaLNoZero d, CharParsing p) =>
  p d
parseHeXaDeCiMaLNoZero =
  choice
    [
      parseDecimalNoZero
    , parseAa
    , parseBb
    , parseCc
    , parseDd
    , parseEe
    , parseFf
    ] <?> "HeXaDeCiMaLNoZero"

type HeXaDeCiMaL d =
  (D0 d, HeXaDeCiMaLNoZero d)

parseHeXaDeCiMaL ::
  (HeXaDeCiMaL d, CharParsing p) =>
  p d
parseHeXaDeCiMaL =
  choice
    [
      parse0
    , parseHeXaDeCiMaLNoZero
    ] <?> "HeXaDeCiMaL"
