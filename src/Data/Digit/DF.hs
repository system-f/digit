{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DF(
  DF(..)
, parseF
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

class DF d where
  dF ::
    Prism'
      d
      ()
  xF ::
    d
  xF =
    dF # ()

instance DF () where
  dF =
    id
    
-- |
--
-- >>> parse (parseF <* eof) "test" "F" :: Either ParseError HEXDigit
-- Right HEXDigitF
--
-- >>> parse parseF "test" "Fxyz" :: Either ParseError HEXDigit
-- Right HEXDigitF
--
-- >>> isn't _Right (parse parseF "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseF ::
  (DF d, CharParsing p) =>
  p d
parseF =
  xF <$ char 'F' <?> "F"
