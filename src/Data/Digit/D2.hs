{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D2(
  D2(..)
, parse2
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

class D2 d where
  d2 ::
    Prism'
      d
      ()
  x2 ::
    d
  x2 =
    d2 # ()

instance D2 () where
  d2 =
    id
    
-- |
--
-- >>> parse (parse2 <* eof) "test" "2" :: Either ParseError DecDigit
-- Right DecDigit2
--
-- >>> parse parse2 "test" "2xyz" :: Either ParseError DecDigit
-- Right DecDigit2
--
-- >>> isn't _Right (parse parse2 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse2 ::
  (D2 d, CharParsing p) =>
  p d
parse2 =
  x2 <$ char '2' <?> "2"
