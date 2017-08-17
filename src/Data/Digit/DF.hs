module Data.Digit.DF where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.DigitF

class DF d where
  dF ::
    Prism'
      d
      ()
  xF ::
    d
  xF =
    dF # ()

instance DF () where
  dF =
    id
    
-- |
--
-- >>> parse (parseF <* eof) "test" "F" :: Either ParseError (DigitF ())
-- Right (DigitF ())
--
-- >>> parse parseF "test" "Fxyz" :: Either ParseError (DigitF ())
-- Right (DigitF ())
--
-- >>> isn't _Right (parse parseF "test" "xyz" :: Either ParseError (DigitF ()))
-- True
--
-- prop> \c -> c /= 'F' ==> isn't _Right (parse parseF "test" [c] :: Either ParseError (DigitF ()))
parseF ::
  (DF d, CharParsing p) =>
  p d
parseF =
  xF <$ char 'F' <?> "F"

instance DF d => DF (Either d x) where
  dF =
    _Left . dF
