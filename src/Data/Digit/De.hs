{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.De(
  De(..)
, parsee
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class De d where
  de ::
    Prism'
      d
      ()
  xe ::
    d
  xe =
    de # ()

instance De () where
  de =
    id
    
-- |
--
-- >>> parse (parsee <* eof) "test" "e" :: Either ParseError HexDigit
-- Right HexDigite
--
-- >>> parse parsee "test" "exyz" :: Either ParseError HexDigit
-- Right HexDigite
--
-- >>> isn't _Right (parse parsee "test" "xyz" :: Either ParseError HexDigit)
-- True
parsee ::
  (De d, CharParsing p) =>
  p d
parsee =
  xe <$ char 'e' <?> "e"
