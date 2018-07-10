{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D2(
  D2(..)
, parse2
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D2 d where
  d2 ::
    Prism'
      d
      ()
  x2 ::
    d
  x2 =
    d2 # ()

instance D2 () where
  d2 =
    id
    
-- |
--
-- >>> parse (parse2 <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parse2 "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> isn't _Right (parse parse2 "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= '2' ==> isn't _Right (parse parse2 "test" [c] :: Either ParseError Digit)
parse2 ::
  (D2 d, CharParsing p) =>
  p d
parse2 =
  x2 <$ char '2' <?> "2"
