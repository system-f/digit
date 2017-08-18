{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.HEXADECIMAL(
  HEXADECIMALNoZero
, HEXADECIMAL
, parseHEXADECIMALNoZero
, parseHEXADECIMAL
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0(D0, parse0)
import Data.Digit.D1(D1)
import Data.Digit.D2(D2)
import Data.Digit.D3(D3)
import Data.Digit.D4(D4)
import Data.Digit.D5(D5)
import Data.Digit.D6(D6)
import Data.Digit.D7(D7)
import Data.Digit.D8(D8)
import Data.Digit.D9(D9)
import Data.Digit.DA(DA, parseA)
import Data.Digit.DB(DB, parseB)
import Data.Digit.DC(DC, parseC)
import Data.Digit.DD(DD, parseD)
import Data.Digit.DE(DE, parseE)
import Data.Digit.DF(DF, parseF)
import Data.Digit.Decimal(parseDecimalNoZero)

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
