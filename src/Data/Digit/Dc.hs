{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Dc(
  Dc(..)
, parsec
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
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
-- >>> parse (parsec <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parsec "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> isn't _Right (parse parsec "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'c' ==> isn't _Right (parse parsec "test" [c] :: Either ParseError Digit)
parsec ::
  (Dc d, CharParsing p) =>
  p d
parsec =
  xc <$ char 'c' <?> "c"
