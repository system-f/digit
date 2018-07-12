{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DA(
  DA(..)
, parseA
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
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
-- >>> parse (parseA <* eof) "test" "A" :: Either ParseError HEXDigit
-- Right HEXDigitA
--
-- >>> parse parseA "test" "Axyz" :: Either ParseError HEXDigit
-- Right HEXDigitA
--
-- >>> isn't _Right (parse parseA "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"
