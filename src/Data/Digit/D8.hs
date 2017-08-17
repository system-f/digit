{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D8 where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit8

class D8 d where
  d8 ::
    Prism'
      d
      ()
  x8 ::
    d
  x8 =
    d8 # ()

instance D8 () where
  d8 =
    id
    
-- |
--
-- >>> parse (parse8 <* eof) "test" "8" :: Either ParseError (Digit8 ())
-- Right (Digit8 ())
--
-- >>> parse parse8 "test" "8xyz" :: Either ParseError (Digit8 ())
-- Right (Digit8 ())
--
-- >>> isn't _Right (parse parse8 "test" "xyz" :: Either ParseError (Digit8 ()))
-- True
--
-- prop> \c -> c /= '8' ==> isn't _Right (parse parse8 "test" [c] :: Either ParseError (Digit8 ()))
parse8 ::
  (D8 d, CharParsing p) =>
  p d
parse8 =
  x8 <$ char '8' <?> "8"

instance D8 d => D8 (Either d x) where
  d8 =
    _Left . d8
