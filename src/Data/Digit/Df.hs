{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Df(
  Df(..)
, parsef
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

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
-- >>> parse (parsef <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parsef "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> isn't _Right (parse parsef "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'f' ==> isn't _Right (parse parsef "test" [c] :: Either ParseError Digit)
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"
