{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DFf(
  DFf
, parseFf
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DF(DF, parseF)
import Data.Digit.Df(Df, parsef)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DFf a =
  (DF a, Df a)
  
-- |
--
-- >>> parse (parseFf <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseFf "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> parse (parseFf <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseFf "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> isn't _Right (parse parseFf "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "Ff") ==> isn't _Right (parse parseFf "test" [c] :: Either ParseError Digit)
parseFf ::
  (DFf d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef] <?> "Ff"
