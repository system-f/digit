{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DB(
  DB(..)
, parseB
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class DB d where
  dB ::
    Prism'
      d
      ()
  xB ::
    d
  xB =
    dB # ()

instance DB () where
  dB =
    id
    
-- |
--
-- >>> parse (parseB <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseB "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> isn't _Right (parse parseB "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'B' ==> isn't _Right (parse parseB "test" [c] :: Either ParseError Digit)
parseB ::
  (DB d, CharParsing p) =>
  p d
parseB =
  xB <$ char 'B' <?> "B"
