{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DC(
  DC(..)
, parseC
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

class DC d where
  dC ::
    Prism'
      d
      ()
  xC ::
    d
  xC =
    dC # ()

instance DC () where
  dC =
    id
    
-- |
--
-- >>> parse (parseC <* eof) "test" "C" :: Either ParseError HEXDigit
-- Right HEXDigitC
--
-- >>> parse parseC "test" "Cxyz" :: Either ParseError HEXDigit
-- Right HEXDigitC
--
-- >>> isn't _Right (parse parseC "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseC ::
  (DC d, CharParsing p) =>
  p d
parseC =
  xC <$ char 'C' <?> "C"
