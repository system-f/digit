{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DA where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.DigitA

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
-- >>> parse (parseA <* eof) "test" "A" :: Either ParseError (DigitA ())
-- Right (DigitA ())
--
-- >>> parse parseA "test" "Axyz" :: Either ParseError (DigitA ())
-- Right (DigitA ())
--
-- >>> isn't _Right (parse parseA "test" "xyz" :: Either ParseError (DigitA ()))
-- True
--
-- prop> \c -> c /= 'A' ==> isn't _Right (parse parseA "test" [c] :: Either ParseError (DigitA ()))
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"

instance DA d => DA (Either d x) where
  dA =
    _Left . dA
