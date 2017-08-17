{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DD where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.DigitD

class DD d where
  dD ::
    Prism'
      d
      ()
  xD ::
    d
  xD =
    dD # ()

instance DD () where
  dD =
    id
    
-- |
--
-- >>> parse (parseD <* eof) "test" "D" :: Either ParseError (DigitD ())
-- Right (DigitD ())
--
-- >>> parse parseD "test" "Dxyz" :: Either ParseError (DigitD ())
-- Right (DigitD ())
--
-- >>> isn't _Right (parse parseD "test" "xyz" :: Either ParseError (DigitD ()))
-- True
--
-- prop> \c -> c /= 'D' ==> isn't _Right (parse parseD "test" [c] :: Either ParseError (DigitD ()))
parseD ::
  (DD d, CharParsing p) =>
  p d
parseD =
  xD <$ char 'D' <?> "D"

instance DD d => DD (Either d x) where
  dD =
    _Left . dD
