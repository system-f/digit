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

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type HeXaDeCiMaLNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

-- |
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseHeXaDeCiMaLNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "123456789abcdefABCDEF") ==> isn't _Right (parse parseHeXaDeCiMaLNoZero "test" [c] :: Either ParseError Digit)
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

-- |
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseHeXaDeCiMaL "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHeXaDeCiMaL "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHeXaDeCiMaL "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHeXaDeCiMaL "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHeXaDeCiMaL "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHeXaDeCiMaL "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHeXaDeCiMaL "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHeXaDeCiMaL "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHeXaDeCiMaL "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHeXaDeCiMaL "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseHeXaDeCiMaL "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseHeXaDeCiMaL "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseHeXaDeCiMaL "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseHeXaDeCiMaL "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseHeXaDeCiMaL "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseHeXaDeCiMaL "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseHeXaDeCiMaL "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseHeXaDeCiMaL "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseHeXaDeCiMaL "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseHeXaDeCiMaL "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseHeXaDeCiMaL "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseHeXaDeCiMaL "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseHeXaDeCiMaL "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "0123456789abcdefABCDEF") ==> isn't _Right (parse parseHeXaDeCiMaL "test" [c] :: Either ParseError Digit)
parseHeXaDeCiMaL ::
  (HeXaDeCiMaL d, CharParsing p) =>
  p d
parseHeXaDeCiMaL =
  choice
    [
      parse0
    , parseHeXaDeCiMaLNoZero
    ] <?> "HeXaDeCiMaL"
