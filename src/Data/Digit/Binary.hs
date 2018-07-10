{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Digit.Binary(
  module Data.Digit.D0
, module Data.Digit.D1
, BinDigit(..)
, BinaryNoZero
, Binary
, parseBinaryNoZero
, parseBinary
-- * Prisms
, _BinDigit0
, _BinDigit1
) where

import Prelude (Eq, Show, Ord)
import Control.Lens.TH (makePrisms)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0
import Data.Digit.D1

data BinDigit
  = BinDigit0
  | BinDigit1
  deriving (Show, Eq, Ord)

makePrisms ''BinDigit

instance D0 BinDigit where; d0 = _BinDigit0
instance D1 BinDigit where; d1 = _BinDigit1

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type BinaryNoZero d =
  D1 d

-- |
--
-- >>> parse (parseBinaryNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseBinaryNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> isn't _Right (parse parseBinaryNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "1") ==> isn't _Right (parse parseBinaryNoZero "test" [c] :: Either ParseError Digit)
parseBinaryNoZero ::
  (BinaryNoZero d, CharParsing p) =>
  p d
parseBinaryNoZero =
  parse1 <?> "BinaryNoZero"

type Binary d =
  (D0 d, BinaryNoZero d)

-- |
--
-- >>> parse (parseBinary <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseBinary "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseBinary <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseBinary "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> isn't _Right (parse parseBinary "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "01") ==> isn't _Right (parse parseBinary "test" [c] :: Either ParseError Digit)
parseBinary ::
  (Binary d, CharParsing p) =>
  p d
parseBinary =
  choice
    [
      parse0
    , parseBinaryNoZero
    ] <?> "Binary"
