{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.Hexadecimal(
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
, module Data.Digit.Da
, module Data.Digit.Db
, module Data.Digit.Dc
, module Data.Digit.Dd
, module Data.Digit.De
, module Data.Digit.Df
, HexDigit(..)
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
import Data.Digit.Da
import Data.Digit.Db
import Data.Digit.Dc
import Data.Digit.Dd
import Data.Digit.De
import Data.Digit.Df
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

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type HexadecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

-- |
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHexadecimalNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHexadecimalNoZero "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHexadecimalNoZero "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHexadecimalNoZero "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHexadecimalNoZero "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHexadecimalNoZero "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHexadecimalNoZero "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHexadecimalNoZero "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHexadecimalNoZero "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseHexadecimalNoZero "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseHexadecimalNoZero "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseHexadecimalNoZero "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseHexadecimalNoZero "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseHexadecimalNoZero "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseHexadecimalNoZero "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> isn't _Right (parse parseHexadecimalNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "123456789abcdef") ==> isn't _Right (parse parseHexadecimalNoZero "test" [c] :: Either ParseError Digit)
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

-- |
--
-- >>> parse (parseHexadecimal <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseHexadecimal "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseHexadecimal <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHexadecimal "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHexadecimal <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHexadecimal "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHexadecimal <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHexadecimal "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHexadecimal <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHexadecimal "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHexadecimal <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHexadecimal "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHexadecimal <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHexadecimal "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHexadecimal <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHexadecimal "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHexadecimal <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHexadecimal "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHexadecimal <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHexadecimal "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHexadecimal <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseHexadecimal "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> parse (parseHexadecimal <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseHexadecimal "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> parse (parseHexadecimal <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseHexadecimal "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> parse (parseHexadecimal <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseHexadecimal "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> parse (parseHexadecimal <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseHexadecimal "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> parse (parseHexadecimal <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseHexadecimal "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> isn't _Right (parse parseHexadecimal "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "0123456789abcdef") ==> isn't _Right (parse parseHexadecimal "test" [c] :: Either ParseError Digit)
parseHexadecimal ::
  (Hexadecimal d, CharParsing p) =>
  p d
parseHexadecimal =
  choice
    [
      parse0
    , parseHexadecimalNoZero
    ] <?> "Hexadecimal"
