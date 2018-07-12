{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D5(
  D5(..)
, parse5
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D5 d where
  d5 ::
    Prism'
      d
      ()
  x5 ::
    d
  x5 =
    d5 # ()

instance D5 () where
  d5 =
    id
    
-- |
--
-- >>> parse (parse5 <* eof) "test" "5" :: Either ParseError DecDigit
-- Right DecDigit5
--
-- >>> parse parse5 "test" "5xyz" :: Either ParseError DecDigit
-- Right DecDigit5
--
-- >>> isn't _Right (parse parse5 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse5 ::
  (D5 d, CharParsing p) =>
  p d
parse5 =
  x5 <$ char '5' <?> "5"
