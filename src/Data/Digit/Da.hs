{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Da(
  Da(..)
, parsea
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
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
-- >>> parse (parsea <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parsea "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> isn't _Right (parse parsea "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'a' ==> isn't _Right (parse parsea "test" [c] :: Either ParseError Digit)
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"
