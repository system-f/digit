{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DE(
  DE(..)
, parseE
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class DE d where
  dE ::
    Prism'
      d
      ()
  xE ::
    d
  xE =
    dE # ()

instance DE () where
  dE =
    id
    
-- |
--
-- >>> parse (parseE <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseE "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> isn't _Right (parse parseE "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'E' ==> isn't _Right (parse parseE "test" [c] :: Either ParseError Digit)
parseE ::
  (DE d, CharParsing p) =>
  p d
parseE =
  xE <$ char 'E' <?> "E"
