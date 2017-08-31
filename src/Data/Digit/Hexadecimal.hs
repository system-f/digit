{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.Hexadecimal(
  HexadecimalNoZero
, Hexadecimal
, parseHexadecimalNoZero
, parseHexadecimal
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
import Data.Digit.Da(Da, parsea)
import Data.Digit.Db(Db, parseb)
import Data.Digit.Dc(Dc, parsec)
import Data.Digit.Dd(Dd, parsed)
import Data.Digit.De(De, parsee)
import Data.Digit.Df(Df, parsef)
import Data.Digit.Decimal(parseDecimalNoZero)

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
