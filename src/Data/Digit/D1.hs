{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D1(
  D1(..)
, parse1
) where

import Control.Category (id)
import Control.Lens (Prism', (#))

import Data.Functor ((<$))

import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D1 d where
  d1 ::
    Prism'
      d
      ()
  x1 ::
    d
  x1 =
    d1 # ()

instance D1 () where
  d1 =
    id

-- |
--
-- >>> parse (parse1 <* eof) "test" "1" :: Either ParseError BinDigit
-- Right BinDigit1
--
-- >>> parse parse1 "test" "1xyz" :: Either ParseError BinDigit
-- Right BinDigit1
--
-- >>> isn't _Right (parse parse1 "test" "xyz" :: Either ParseError BinDigit)
-- True
parse1 ::
  (D1 d, CharParsing p) =>
  p d
parse1 =
  x1 <$ char '1' <?> "1"
