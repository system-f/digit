{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.Octal(
  OctalNoZero
, Octal
, parseOctalNoZero
, parseOctal
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

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type OctalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d)

-- |
--
-- >>> parse (parseOctalNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseOctalNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseOctalNoZero <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseOctalNoZero "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseOctalNoZero <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseOctalNoZero "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseOctalNoZero <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseOctalNoZero "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseOctalNoZero <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseOctalNoZero "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseOctalNoZero <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseOctalNoZero "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseOctalNoZero <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseOctalNoZero "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> isn't _Right (parse parseOctalNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "1234567") ==> isn't _Right (parse parseOctalNoZero "test" [c] :: Either ParseError Digit)
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

type Octal d =
  (D0 d, OctalNoZero d)

-- |
--
-- >>> parse (parseOctal <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseOctal <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseOctal "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseOctal "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseOctal <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseOctal "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseOctal <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseOctal "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseOctal <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseOctal "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseOctal <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseOctal "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseOctal <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseOctal "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseOctal <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseOctal "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> isn't _Right (parse parseOctal "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "01234567") ==> isn't _Right (parse parseOctal "test" [c] :: Either ParseError Digit)
parseOctal ::
  (Octal d, CharParsing p) =>
  p d
parseOctal =
  choice
    [
      parse0
    , parseOctalNoZero
    ] <?> "Octal"
