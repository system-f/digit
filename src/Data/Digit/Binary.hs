{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.Binary where

import Papa
import Data.Void(Void)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0(D0, parse0)
import Data.Digit.D1(D1, parse1)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit1

type BinaryNoZero d =
  D1 d

-- |
--
-- >>> parse (parseBinaryNoZero <* eof) "test" "1" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> parse parseBinaryNoZero "test" "1xyz" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> isn't _Right (parse parseBinaryNoZero "test" "xyz" :: Either ParseError (Digit1 ()))
-- True
--
-- prop> \c -> (c `notElem` "1") ==> isn't _Right (parse parseBinaryNoZero "test" [c] :: Either ParseError (Digit1 ()))
parseBinaryNoZero ::
  (BinaryNoZero d, CharParsing p) =>
  p d
parseBinaryNoZero =
  parse1 <?> "BinaryNoZero"

type BinaryNoZeroDigit d1 =
  Either d1 Void

type Binary d =
  (D0 d, BinaryNoZero d)

-- |
--
-- >>> parse (parseBinary <* eof) "test" "0" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> parse parseBinary "test" "0xyz" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> parse (parseBinary <* eof) "test" "1" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> parse parseBinary "test" "1xyz" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseBinary "test" "xyz" :: Either ParseError (BinaryDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "01") ==> isn't _Right (parse parseBinary "test" [c] :: Either ParseError (BinaryDigit' ()))
parseBinary ::
  (Binary d, CharParsing p) =>
  p d
parseBinary =
  choice
    [
      parse0
    , parseBinaryNoZero
    ] <?> "Binary"

type BinaryDigit d0 d1 =
  Either d0 (BinaryNoZeroDigit d1)

type BinaryDigit' d =
  BinaryDigit d d
