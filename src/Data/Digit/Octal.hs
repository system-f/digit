{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.Octal(
  module Data.Digit.D0
, module Data.Digit.D1
, module Data.Digit.D2
, module Data.Digit.D3
, module Data.Digit.D4
, module Data.Digit.D5
, module Data.Digit.D6
, module Data.Digit.D7
, OctDigit(..)
, OctalNoZero
, Octal
, parseOctalNoZero
, parseOctal
-- * Prisms
, _OctDigit0
, _OctDigit1
, _OctDigit2
, _OctDigit3
, _OctDigit4
, _OctDigit5
, _OctDigit6
, _OctDigit7
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

data OctDigit
  = OctDigit0
  | OctDigit1
  | OctDigit2
  | OctDigit3
  | OctDigit4
  | OctDigit5
  | OctDigit6
  | OctDigit7
  deriving (Show, Eq, Ord)

makePrisms ''OctDigit

instance D0 OctDigit where; d0 = _OctDigit0
instance D1 OctDigit where; d1 = _OctDigit1
instance D2 OctDigit where; d2 = _OctDigit2
instance D3 OctDigit where; d3 = _OctDigit3
instance D4 OctDigit where; d4 = _OctDigit4
instance D5 OctDigit where; d5 = _OctDigit5
instance D6 OctDigit where; d6 = _OctDigit6
instance D7 OctDigit where; d7 = _OctDigit7

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
