{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.DC(
  DC(..)
, parseC
) where

import Data.Digit.Digit(Digit(DigitC))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class DC d where
  dC ::
    Prism'
      d
      ()
  xC ::
    d
  xC =
    dC # ()

instance DC () where
  dC =
    id
    
-- |
--
-- >>> parse (parseC <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseC "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> isn't _Right (parse parseC "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= 'C' ==> isn't _Right (parse parseC "test" [c] :: Either ParseError Digit)
parseC ::
  (DC d, CharParsing p) =>
  p d
parseC =
  xC <$ char 'C' <?> "C"

instance DC Digit where
  dC =
    prism'
      (\() -> DigitC)
      (\d ->  case d of
                DigitC ->
                  Just ()
                _ ->
                  Nothing)
