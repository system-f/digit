{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D9(
  D9(..)
, parse9
) where

import Data.Digit.Digit(Digit(Digit9))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D9 d where
  d9 ::
    Prism'
      d
      ()
  x9 ::
    d
  x9 =
    d9 # ()

instance D9 () where
  d9 =
    id
    
-- |
--
-- >>> parse (parse9 <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parse9 "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> isn't _Right (parse parse9 "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= '9' ==> isn't _Right (parse parse9 "test" [c] :: Either ParseError Digit)
parse9 ::
  (D9 d, CharParsing p) =>
  p d
parse9 =
  x9 <$ char '9' <?> "9"

instance D9 Digit where
  d9 =
    prism'
      (\() -> Digit9)
      (\d ->  case d of
                Digit9 ->
                  Just ()
                _ ->
                  Nothing)
