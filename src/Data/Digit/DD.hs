{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DD(
  DD(..)
, parseD
) where

import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
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
-- >>> parse (parseD <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseD "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> isn't _Right (parse parseD "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'D' ==> isn't _Right (parse parseD "test" [c] :: Either ParseError Digit)
parseD ::
  (DD d, CharParsing p) =>
  p d
parseD =
  xD <$ char 'D' <?> "D"
