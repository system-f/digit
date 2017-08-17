module Data.Digit.Da where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digita

class Da d where
  da ::
    Prism'
      d
      ()
  xa ::
    d
  xa =
    da # ()

instance Da () where
  da =
    id
    
-- |
--
-- >>> parse (parsea <* eof) "test" "a" :: Either ParseError (Digita ())
-- Right (Digita ())
--
-- >>> parse parsea "test" "axyz" :: Either ParseError (Digita ())
-- Right (Digita ())
--
-- >>> isn't _Right (parse parsea "test" "xyz" :: Either ParseError (Digita ()))
-- True
--
-- prop> \c -> c /= 'a' ==> isn't _Right (parse parsea "test" [c] :: Either ParseError (Digita ()))
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"

instance Da d => Da (Either d x) where
  da =
    _Left . da