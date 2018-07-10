{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Da(
  Da(..)
, parsea
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class Da d where
  da ::
    Prism'
      d
      ()
  xa ::
    d
  xa =
    da # ()

instance Da () where
  da =
    id
    
-- |
--
-- >>> parse (parsea <* eof) "test" "a" :: Either ParseError HexDigit
-- Right HexDigita
--
-- >>> parse parsea "test" "axyz" :: Either ParseError HexDigit
-- Right HexDigita
--
-- >>> isn't _Right (parse parsea "test" "xyz" :: Either ParseError HexDigit)
-- True
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"
