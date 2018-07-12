{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D4(
  D4(..)
, parse4
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D4 d where
  d4 ::
    Prism'
      d
      ()
  x4 ::
    d
  x4 =
    d4 # ()

instance D4 () where
  d4 =
    id
   
-- |
--
-- >>> parse (parse4 <* eof) "test" "4" :: Either ParseError DecDigit
-- Right DecDigit4
--
-- >>> parse parse4 "test" "4xyz" :: Either ParseError DecDigit
-- Right DecDigit4
--
-- >>> isn't _Right (parse parse4 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse4 ::
  (D4 d, CharParsing p) =>
  p d
parse4 =
  x4 <$ char '4' <?> "4"
