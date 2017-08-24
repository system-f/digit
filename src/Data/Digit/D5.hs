{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D5(
  D5(..)
, parse5
) where

import Data.Digit.D4(D4)
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit5

class D5 d where
  d5 ::
    Prism'
      d
      ()
  x5 ::
    d
  x5 =
    d5 # ()

instance D5 () where
  d5 =
    id
    
-- |
--
-- >>> parse (parse5 <* eof) "test" "5" :: Either ParseError (Digit5 ())
-- Right (Digit5 ())
--
-- >>> parse parse5 "test" "5xyz" :: Either ParseError (Digit5 ())
-- Right (Digit5 ())
--
-- >>> isn't _Right (parse parse5 "test" "xyz" :: Either ParseError (Digit5 ()))
-- True
--
-- prop> \c -> c /= '5' ==> isn't _Right (parse parse5 "test" [c] :: Either ParseError (Digit5 ()))
parse5 ::
  (D5 d, CharParsing p) =>
  p d
parse5 =
  x5 <$ char '5' <?> "5"

instance (D4 x, D5 d) => D5 (Either d x) where
  d5 =
    _Left . d5
