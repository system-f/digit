{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DB(
  DB(..)
, parseB
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

class DB d where
  dB ::
    Prism'
      d
      ()
  xB ::
    d
  xB =
    dB # ()

instance DB () where
  dB =
    id
    
-- |
--
-- >>> parse (parseB <* eof) "test" "B" :: Either ParseError HEXDigit
-- Right HEXDigitB
--
-- >>> parse parseB "test" "Bxyz" :: Either ParseError HEXDigit
-- Right HEXDigitB
--
-- >>> isn't _Right (parse parseB "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseB ::
  (DB d, CharParsing p) =>
  p d
parseB =
  xB <$ char 'B' <?> "B"
