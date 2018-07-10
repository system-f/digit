{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D3(
  D3(..)
, parse3
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D3 d where
  d3 ::
    Prism'
      d
      ()
  x3 ::
    d
  x3 =
    d3 # ()

instance D3 () where
  d3 =
    id

-- |
--
-- >>> parse (parse3 <* eof) "test" "3" :: Either ParseError DecDigit
-- Right DecDigit3
--
-- >>> parse parse3 "test" "3xyz" :: Either ParseError DecDigit
-- Right DecDigit3
--
-- >>> isn't _Right (parse parse3 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse3 ::
  (D3 d, CharParsing p) =>
  p d
parse3 =
  x3 <$ char '3' <?> "3"
