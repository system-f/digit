{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.HEXADECIMAL where

import Data.Void
import Text.Parser.Char
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
import Data.Digit.Decimal

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Control.Lens(isn't, _Right)

type HEXADECIMALNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d)

-- |
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "1" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "1xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "2" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "2xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "3" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "3xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "4" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "4xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "5" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "5xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "6" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "6xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "7" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "7xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "8" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "8xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "9" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "9xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "A" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Axyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "B" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Bxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "C" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Cxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "D" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Dxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "E" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Exyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "F" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Fxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHEXADECIMALNoZero "test" "xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "123456789ABCDEF") ==> isn't _Right (parse parseHEXADECIMALNoZero "test" [c] :: Either ParseError (HEXADECIMALNoZeroDigit' ()))
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

type HEXADECIMALNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 dA dB dC dD dE dF =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 (Either dA (Either dB (Either dC (Either dD (Either dE (Either dF Void)))))))))))))))

type HEXADECIMALNoZeroDigit' d =
  HEXADECIMALNoZeroDigit d d d d d d d d d d d d d d d
  

type HEXADECIMAL d =
  (D0 d, HEXADECIMALNoZero d)

-- |
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "0" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "0xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "1" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "1xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "2" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "2xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "3" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "3xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "4" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "4xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "5" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "5xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "6" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "6xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "7" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "7xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "8" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "8xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "9" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "9xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "A" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Axyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "B" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Bxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "C" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Cxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "D" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Dxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "E" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Exyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "F" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Fxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHEXADECIMAL "test" "xyz" :: Either ParseError (HEXADECIMALDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "0123456789ABCDEF") ==> isn't _Right (parse parseHEXADECIMAL "test" [c] :: Either ParseError (HEXADECIMALDigit' ()))
parseHEXADECIMAL ::
  (HEXADECIMAL d, CharParsing p) =>
  p d
parseHEXADECIMAL =
  choice
    [
      parse0
    , parseHEXADECIMALNoZero
    ] <?> "HEXADECIMAL"

type HEXADECIMALDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 dA dB dC dD dE dF =
  Either d0 (HEXADECIMALNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 dA dB dC dD dE dF)

type HEXADECIMALDigit' d =
  HEXADECIMALDigit d d d d d d d d d d d d d d d d
