{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.De(
  De(..)
, parsee
) where

import Data.Digit.Digit(Digit(Digite))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
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
-- >>> parse (parsee <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parsee "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> isn't _Right (parse parsee "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'e' ==> isn't _Right (parse parsee "test" [c] :: Either ParseError Digit)
parsee ::
  (De d, CharParsing p) =>
  p d
parsee =
  xe <$ char 'e' <?> "e"

instance De Digit where
  de =
    prism'
      (\() -> Digite)
      (\d ->  case d of
                Digite ->
                  Just ()
                _ ->
                  Nothing)
