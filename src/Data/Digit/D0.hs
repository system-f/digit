{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Digit.D0(
  D0(..)
, parse0
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

class D0 d where
  d0 ::
    Prism'
      d
      ()
  x0 ::
    d
  x0 =
    d0 # ()

instance D0 () where
  d0 =
    id

-- |
--
-- >>> parse (parse0 <* eof) "test" "0" :: Either ParseError BinDigit
-- Right BinDigit0
--
-- >>> parse parse0 "test" "0xyz" :: Either ParseError BinDigit
-- Right BinDigit0
--
-- >>> isn't _Right (parse parse0 "test" "xyz" :: Either ParseError BinDigit)
-- True
parse0 ::
  (D0 d, CharParsing p) =>
  p d
parse0 =
  x0 <$ char '0' <?> "0"
