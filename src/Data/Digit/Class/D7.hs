{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Class.D7(
  D7(..)
, parse7
) where

import Control.Category (id)
import Control.Lens (Prism', (#))

import Data.Functor ((<$))

import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit.Class
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D7 d where
  d7 ::
    Prism'
      d
      ()
  x7 ::
    d
  x7 =
    d7 # ()

instance D7 () where
  d7 =
    id
    
-- |
--
-- >>> parse (parse7 <* eof) "test" "7" :: Either ParseError DecDigit
-- Right DecDigit7
--
-- >>> parse parse7 "test" "7xyz" :: Either ParseError DecDigit
-- Right DecDigit7
--
-- >>> isn't _Right (parse parse7 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse7 ::
  (D7 d, CharParsing p) =>
  p d
parse7 =
  x7 <$ char '7' <?> "7"
