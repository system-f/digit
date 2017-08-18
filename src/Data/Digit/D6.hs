{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D6(
  D6(..)
, parse6
) where

import Data.Digit.Digit(Digit(Digit6))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D6 d where
  d6 ::
    Prism'
      d
      ()
  x6 ::
    d
  x6 =
    d6 # ()

instance D6 () where
  d6 =
    id
    
-- |
--
-- >>> parse (parse6 <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parse6 "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> isn't _Right (parse parse6 "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= '6' ==> isn't _Right (parse parse6 "test" [c] :: Either ParseError Digit)
parse6 ::
  (D6 d, CharParsing p) =>
  p d
parse6 =
  x6 <$ char '6' <?> "6"

instance D6 Digit where
  d6 =
    prism'
      (\() -> Digit6)
      (\d ->  case d of
                Digit6 ->
                  Just ()
                _ ->
                  Nothing)
