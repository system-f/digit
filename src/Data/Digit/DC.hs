module Data.Digit.DC where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.DigitC

class DC d where
  dC ::
    Prism'
      d
      ()
  xC ::
    d
  xC =
    dC # ()

instance DC () where
  dC =
    id
    
-- |
--
-- >>> parse (parseC <* eof) "test" "C" :: Either ParseError (DigitC ())
-- Right (DigitC ())
--
-- >>> parse parseC "test" "Cxyz" :: Either ParseError (DigitC ())
-- Right (DigitC ())
--
-- >>> isn't _Right (parse parseC "test" "xyz" :: Either ParseError (DigitC ()))
-- True
--
-- prop> \c -> c /= 'C' ==> isn't _Right (parse parseC "test" [c] :: Either ParseError (DigitC ()))
parseC ::
  (DC d, CharParsing p) =>
  p d
parseC =
  xC <$ char 'C' <?> "C"

instance DC d => DC (Either d x) where
  dC =
    _Left . dC