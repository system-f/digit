{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D6 where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit6

class D6 d where
  d6 ::
    Prism'
      d
      ()
  x6 ::
    d
  x6 =
    d6 # ()

instance D6 () where
  d6 =
    id
    
-- |
--
-- >>> parse (parse6 <* eof) "test" "6" :: Either ParseError (Digit6 ())
-- Right (Digit6 ())
--
-- >>> parse parse6 "test" "6xyz" :: Either ParseError (Digit6 ())
-- Right (Digit6 ())
--
-- >>> isn't _Right (parse parse6 "test" "xyz" :: Either ParseError (Digit6 ()))
-- True
--
-- prop> \c -> c /= '6' ==> isn't _Right (parse parse6 "test" [c] :: Either ParseError (Digit6 ()))
parse6 ::
  (D6 d, CharParsing p) =>
  p d
parse6 =
  x6 <$ char '6' <?> "6"

instance D6 d => D6 (Either d x) where
  d6 =
    _Left . d6
