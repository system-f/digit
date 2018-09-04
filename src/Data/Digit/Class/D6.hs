{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Class.D6(
  D6(..)
, parse6
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

class D6 d where
  d6 ::
    Prism'
      d
      ()
  x6 ::
    d
  x6 =
    d6 # ()

instance D6 () where
  d6 =
    id
    
-- |
--
-- >>> parse (parse6 <* eof) "test" "6" :: Either ParseError DecDigit
-- Right DecDigit6
--
-- >>> parse parse6 "test" "6xyz" :: Either ParseError DecDigit
-- Right DecDigit6
--
-- >>> isn't _Right (parse parse6 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse6 ::
  (D6 d, CharParsing p) =>
  p d
parse6 =
  x6 <$ char '6' <?> "6"
