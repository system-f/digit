{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Dd(
  Dd(..)
, parsed
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
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
-- >>> parse (parsed <* eof) "test" "d" :: Either ParseError HexDigit
-- Right HexDigitd
--
-- >>> parse parsed "test" "dxyz" :: Either ParseError HexDigit
-- Right HexDigitd
--
-- >>> isn't _Right (parse parsed "test" "xyz" :: Either ParseError HexDigit)
-- True
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"
