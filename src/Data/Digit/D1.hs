{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D1(
  D1(..)
, parse1
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D1 d where
  d1 ::
    Prism'
      d
      ()
  x1 ::
    d
  x1 =
    d1 # ()

instance D1 () where
  d1 =
    id

-- |
--
-- >>> parse (parse1 <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parse1 "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> isn't _Right (parse parse1 "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= '1' ==> isn't _Right (parse parse1 "test" [c] :: Either ParseError Digit)
parse1 ::
  (D1 d, CharParsing p) =>
  p d
parse1 =
  x1 <$ char '1' <?> "1"
