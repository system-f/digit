{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.HEXADECIMAL(
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
, module Data.Digit.DA
, module Data.Digit.DB
, module Data.Digit.DC
, module Data.Digit.DD
, module Data.Digit.DE
, module Data.Digit.DF
, HEXDigit(..)
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
  deriving (Show, Eq, Ord)

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

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type HEXADECIMALNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d)

-- |
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHEXADECIMALNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHEXADECIMALNoZero "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHEXADECIMALNoZero "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHEXADECIMALNoZero "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHEXADECIMALNoZero "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHEXADECIMALNoZero "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHEXADECIMALNoZero "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHEXADECIMALNoZero "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHEXADECIMALNoZero "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseHEXADECIMALNoZero "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseHEXADECIMALNoZero "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseHEXADECIMALNoZero "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseHEXADECIMALNoZero "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseHEXADECIMALNoZero "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseHEXADECIMALNoZero "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseHEXADECIMALNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "123456789ABCDEF") ==> isn't _Right (parse parseHEXADECIMALNoZero "test" [c] :: Either ParseError Digit)
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

-- |
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseHEXADECIMAL "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHEXADECIMAL "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHEXADECIMAL "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHEXADECIMAL "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHEXADECIMAL "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHEXADECIMAL "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHEXADECIMAL "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHEXADECIMAL "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHEXADECIMAL "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHEXADECIMAL "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseHEXADECIMAL "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseHEXADECIMAL "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseHEXADECIMAL "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseHEXADECIMAL "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseHEXADECIMAL "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseHEXADECIMAL "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseHEXADECIMAL "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "0123456789ABCDEF") ==> isn't _Right (parse parseHEXADECIMAL "test" [c] :: Either ParseError Digit)
parseHEXADECIMAL ::
  (HEXADECIMAL d, CharParsing p) =>
  p d
parseHEXADECIMAL =
  choice
    [
      parse0
    , parseHEXADECIMALNoZero
    ] <?> "HEXADECIMAL"
