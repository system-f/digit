{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Df(
  Df(..)
, parsef
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

class Df d where
  df ::
    Prism'
      d
      ()
  xf ::
    d
  xf =
    df # ()

instance Df () where
  df =
    id
    
-- |
--
-- >>> parse (parsef <* eof) "test" "f" :: Either ParseError HexDigit
-- Right HexDigitf
--
-- >>> parse parsef "test" "fxyz" :: Either ParseError HexDigit
-- Right HexDigitf
--
-- >>> isn't _Right (parse parsef "test" "xyz" :: Either ParseError HexDigit)
-- True
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"
