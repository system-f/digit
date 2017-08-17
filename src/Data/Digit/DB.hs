module Data.Digit.DB where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.DigitB

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
-- >>> parse (parseB <* eof) "test" "B" :: Either ParseError (DigitB ())
-- Right (DigitB ())
--
-- >>> parse parseB "test" "Bxyz" :: Either ParseError (DigitB ())
-- Right (DigitB ())
--
-- >>> isn't _Right (parse parseB "test" "xyz" :: Either ParseError (DigitB ()))
-- True
--
-- prop> \c -> c /= 'B' ==> isn't _Right (parse parseB "test" [c] :: Either ParseError (DigitB ()))
parseB ::
  (DB d, CharParsing p) =>
  p d
parseB =
  xB <$ char 'B' <?> "B"

instance DB d => DB (Either d x) where
  dB =
    _Left . dB
