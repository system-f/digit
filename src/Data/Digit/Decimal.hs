{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.Decimal where

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

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Control.Lens(isn't, _Right)

type DecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d)

-- |
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "1" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "1xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "2" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "2xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "3" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "3xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "4" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "4xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "5" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "5xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "6" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "6xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "7" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "7xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "8" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "8xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "9" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "9xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseDecimalNoZero "test" "xyz" :: Either ParseError (DecimalNoZeroDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "123456789") ==> isn't _Right (parse parseDecimalNoZero "test" [c] :: Either ParseError (DecimalNoZeroDigit' ()))
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

type DecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 Void)))))))))

type DecimalNoZeroDigit' d =
  DecimalNoZeroDigit d d d d d d d d d

type Decimal d =
  (D0 d, DecimalNoZero d)

-- |
--
-- >>> parse (parseDecimal <* eof) "test" "0" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "0xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "1" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "1xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "2" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "2xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "3" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "3xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "4" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "4xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "5" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "5xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "6" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "6xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "7" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "7xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "8" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "8xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "9" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "9xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseDecimal "test" "xyz" :: Either ParseError (DecimalDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "0123456789") ==> isn't _Right (parse parseDecimal "test" [c] :: Either ParseError (DecimalDigit' ()))
parseDecimal ::
  (Decimal d, CharParsing p) =>
  p d
parseDecimal =
  choice
    [
      parse0
    , parseDecimalNoZero
    ] <?> "Decimal"

type DecimalDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 =
  Either d0 (DecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9)

type DecimalDigit' d =
  DecimalDigit d d d d d d d d d d
  