{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.Decimal(
  DecimalNoZero
, Decimal
, parseDecimalNoZero
, parseDecimal
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0(D0, parse0)
import Data.Digit.D1(D1, parse1)
import Data.Digit.D2(D2, parse2)
import Data.Digit.D3(D3, parse3)
import Data.Digit.D4(D4, parse4)
import Data.Digit.D5(D5, parse5)
import Data.Digit.D6(D6, parse6)
import Data.Digit.D7(D7, parse7)
import Data.Digit.D8(D8, parse8)
import Data.Digit.D9(D9, parse9)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d)

-- |
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseDecimalNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseDecimalNoZero "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseDecimalNoZero "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseDecimalNoZero "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseDecimalNoZero "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseDecimalNoZero "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseDecimalNoZero "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseDecimalNoZero "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseDecimalNoZero "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> isn't _Right (parse parseDecimalNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "123456789") ==> isn't _Right (parse parseDecimalNoZero "test" [c] :: Either ParseError Digit)
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

-- |
--
-- >>> parse (parseDecimal <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseDecimal "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseDecimal <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseDecimal "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseDecimal <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseDecimal "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseDecimal <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseDecimal "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseDecimal <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseDecimal "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseDecimal <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseDecimal "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseDecimal <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseDecimal "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseDecimal <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseDecimal "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseDecimal <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseDecimal "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseDecimal <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseDecimal "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> isn't _Right (parse parseDecimal "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "0123456789") ==> isn't _Right (parse parseDecimal "test" [c] :: Either ParseError Digit)
parseDecimal ::
  (Decimal d, CharParsing p) =>
  p d
parseDecimal =
  choice
    [
      parse0
    , parseDecimalNoZero
    ] <?> "Decimal"
