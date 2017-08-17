{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.Octal where

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

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Control.Lens(isn't, _Right)

type OctalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d)

-- |
--
-- >>> parse (parseOctalNoZero <* eof) "test" "1" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "1xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "2" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "2xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "3" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "3xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "4" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "4xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "5" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "5xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "6" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "6xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "7" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "7xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseOctalNoZero "test" "xyz" :: Either ParseError (OctalNoZeroDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "1234567") ==> isn't _Right (parse parseOctalNoZero "test" [c] :: Either ParseError (OctalNoZeroDigit' ()))
parseOctalNoZero ::
  (OctalNoZero d, CharParsing p) =>
  p d
parseOctalNoZero =
  choice
    [
      parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    ] <?> "OctalNoZero"

type OctalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 Void)))))))

type OctalNoZeroDigit' d =
  OctalNoZeroDigit d d d d d d d 

type Octal d =
  (D0 d, OctalNoZero d)

-- |
--
-- >>> parse (parseOctal <* eof) "test" "0" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "1xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "0" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "1xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "2" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "2xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "3" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "3xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "4" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "4xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "5" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "5xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "6" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "6xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "7" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "7xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseOctal "test" "xyz" :: Either ParseError (OctalDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "01234567") ==> isn't _Right (parse parseOctal "test" [c] :: Either ParseError (OctalDigit' ()))
parseOctal ::
  (Octal d, CharParsing p) =>
  p d
parseOctal =
  choice
    [
      parse0
    , parseOctalNoZero
    ] <?> "Octal"

type OctalDigit d0 d1 d2 d3 d4 d5 d6 d7 =
  Either d0 (OctalNoZeroDigit d1 d2 d3 d4 d5 d6 d7)

type OctalDigit' d =
  OctalDigit d d d d d d d d
