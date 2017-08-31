{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DF(
  DF(..)
, parseF
) where

import Data.Digit.Digit(Digit(DigitF))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

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
-- >>> parse (parseF <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseF "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseF "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'F' ==> isn't _Right (parse parseF "test" [c] :: Either ParseError Digit)
parseF ::
  (DF d, CharParsing p) =>
  p d
parseF =
  xF <$ char 'F' <?> "F"

instance DF Digit where
  dF =
    prism'
      (\() -> DigitF)
      (\d ->  case d of
                DigitF ->
                  Just ()
                _ ->
                  Nothing)
