{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Db(
  Db(..)
, parseb
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

class Db d where
  db ::
    Prism'
      d
      ()
  xb ::
    d
  xb =
    db # ()

instance Db () where
  db =
    id
    
-- |
--
-- >>> parse (parseb <* eof) "test" "b" :: Either ParseError HexDigit
-- Right HexDigitb
--
-- >>> parse parseb "test" "bxyz" :: Either ParseError HexDigit
-- Right HexDigitb
--
-- >>> isn't _Right (parse parseb "test" "xyz" :: Either ParseError HexDigit)
-- True
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"
