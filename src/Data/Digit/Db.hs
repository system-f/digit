{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Db(
  Db(..)
, parseb
) where

import Data.Digit.Digit(Digit(Digitb))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class Db d where
  db ::
    Prism'
      d
      ()
  xb ::
    d
  xb =
    db # ()

instance Db () where
  db =
    id
    
-- |
--
-- >>> parse (parseb <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseb "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> isn't _Right (parse parseb "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'b' ==> isn't _Right (parse parseb "test" [c] :: Either ParseError Digit)
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"

instance Db Digit where
  db =
    prism'
      (\() -> Digitb)
      (\d ->  case d of
                Digitb ->
                  Just ()
                _ ->
                  Nothing)
