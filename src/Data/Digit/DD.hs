{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DD(
  DD(..)
, parseD
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

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
-- >>> parse (parseD <* eof) "test" "D" :: Either ParseError HEXDigit
-- Right HEXDigitD
--
-- >>> parse parseD "test" "Dxyz" :: Either ParseError HEXDigit
-- Right HEXDigitD
--
-- >>> isn't _Right (parse parseD "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseD ::
  (DD d, CharParsing p) =>
  p d
parseD =
  xD <$ char 'D' <?> "D"
