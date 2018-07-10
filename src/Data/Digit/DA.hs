{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DA(
  DA(..)
, parseA
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class DA d where
  dA ::
    Prism'
      d
      ()
  xA ::
    d
  xA =
    dA # ()

instance DA () where
  dA =
    id

-- |
--
-- >>> parse (parseA <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseA "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> isn't _Right (parse parseA "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'A' ==> isn't _Right (parse parseA "test" [c] :: Either ParseError Digit)
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"
