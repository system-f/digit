{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Dd(
  Dd(..)
, parsed
) where

import Data.Digit.Digit(Digit(Digitd))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class Dd d where
  dd ::
    Prism'
      d
      ()
  xd ::
    d
  xd =
    dd # ()

instance Dd () where
  dd =
    id
    
-- |
--
-- >>> parse (parsed <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parsed "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> isn't _Right (parse parsed "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'd' ==> isn't _Right (parse parsed "test" [c] :: Either ParseError Digit)
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"

instance Dd Digit where
  dd =
    prism'
      (\() -> Digitd)
      (\d ->  case d of
                Digitd ->
                  Just ()
                _ ->
                  Nothing)

