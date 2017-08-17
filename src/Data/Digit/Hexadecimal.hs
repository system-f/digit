{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Hexadecimal where

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
import Data.Digit.Da
import Data.Digit.Db
import Data.Digit.Dc
import Data.Digit.Dd
import Data.Digit.De
import Data.Digit.Df
import Data.Digit.Decimal

type HexadecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

-- |
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "1" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "1xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "2" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "2xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "3" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "3xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "4" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "4xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "5" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "5xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "6" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "6xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "7" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "7xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "8" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "8xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "9" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "9xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "a" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "axyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "b" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "bxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "c" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "cxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "d" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "dxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "e" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "exyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "f" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "fxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHexadecimalNoZero "test" "xyz" :: Either ParseError (HexadecimalNoZeroDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "123456789abcdef") ==> isn't _Right (parse parseHexadecimalNoZero "test" [c] :: Either ParseError (HexadecimalNoZeroDigit' ()))
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

type HexadecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 (Either da (Either db (Either dc (Either dd (Either de (Either df Void)))))))))))))))

type HexadecimalNoZeroDigit' d =
  HexadecimalNoZeroDigit d d d d d d d d d d d d d d d
  

type Hexadecimal d =
  (D0 d, HexadecimalNoZero d)

-- |
--
-- >>> parse (parseHexadecimal <* eof) "test" "0" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "0xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "0" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "0xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "2" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "2xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "3" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "3xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "4" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "4xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "5" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "5xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "6" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "6xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "7" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "7xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "8" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "8xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "9" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "9xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "a" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "axyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "b" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "bxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "c" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "cxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "d" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "dxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "e" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "exyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "f" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "fxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHexadecimal "test" "xyz" :: Either ParseError (HexadecimalDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "0123456789abcdef") ==> isn't _Right (parse parseHexadecimal "test" [c] :: Either ParseError (HexadecimalDigit' ()))
parseHexadecimal ::
  (Hexadecimal d, CharParsing p) =>
  p d
parseHexadecimal =
  choice
    [
      parse0
    , parseHexadecimalNoZero
    ] <?> "Hexadecimal"

type HexadecimalDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df =
  Either d0 (HexadecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df)

type HexadecimalDigit' d =
  HexadecimalDigit d d d d d d d d d d d d d d d d
