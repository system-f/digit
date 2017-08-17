module Data.Digit.Db where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digitb

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
-- >>> parse (parseb <* eof) "test" "b" :: Either ParseError (Digitb ())
-- Right (Digitb ())
--
-- >>> parse parseb "test" "bxyz" :: Either ParseError (Digitb ())
-- Right (Digitb ())
--
-- >>> isn't _Right (parse parseb "test" "xyz" :: Either ParseError (Digitb ()))
-- True
--
-- prop> \c -> c /= 'b' ==> isn't _Right (parse parseb "test" [c] :: Either ParseError (Digitb ()))
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"

instance Db d => Db (Either d x) where
  db =
    _Left . db
