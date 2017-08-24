{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Df(
  Df(..)
, parsef
) where

import Data.Digit.De(De)
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digitf

class Df d where
  df ::
    Prism'
      d
      ()
  xf ::
    d
  xf =
    df # ()

instance Df () where
  df =
    id
    
-- |
--
-- >>> parse (parsef <* eof) "test" "f" :: Either ParseError (Digitf ())
-- Right (Digitf ())
--
-- >>> parse parsef "test" "fxyz" :: Either ParseError (Digitf ())
-- Right (Digitf ())
--
-- >>> isn't _Right (parse parsef "test" "xyz" :: Either ParseError (Digitf ()))
-- True
--
-- prop> \c -> c /= 'f' ==> isn't _Right (parse parsef "test" [c] :: Either ParseError (Digitf ()))
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"

instance (De x, Df d) => Df (Either d x) where
  df =
    _Left . df
