{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Dd where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digitd

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
-- >>> parse (parsed <* eof) "test" "d" :: Either ParseError (Digitd ())
-- Right (Digitd ())
--
-- >>> parse parsed "test" "dxyz" :: Either ParseError (Digitd ())
-- Right (Digitd ())
--
-- >>> isn't _Right (parse parsed "test" "xyz" :: Either ParseError (Digitd ()))
-- True
--
-- prop> \c -> c /= 'd' ==> isn't _Right (parse parsed "test" [c] :: Either ParseError (Digitd ()))
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"

instance Dd d => Dd (Either d x) where
  dd =
    _Left . dd
