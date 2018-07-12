{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Dc(
  Dc(..)
, parsec
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class Dc d where
  dc ::
    Prism'
      d
      ()
  xc ::
    d
  xc =
    dc # ()

instance Dc () where
  dc =
    id
   
-- |
--
-- >>> parse (parsec <* eof) "test" "c" :: Either ParseError HexDigit
-- Right HexDigitc
--
-- >>> parse parsec "test" "cxyz" :: Either ParseError HexDigit
-- Right HexDigitc
--
-- >>> isn't _Right (parse parsec "test" "xyz" :: Either ParseError HexDigit)
-- True
parsec ::
  (Dc d, CharParsing p) =>
  p d
parsec =
  xc <$ char 'c' <?> "c"
