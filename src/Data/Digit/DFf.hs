{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DFf(
  module Data.Digit.DF
, module Data.Digit.Df
, DFf
, parseFf
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DF
import Data.Digit.Df

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit
-- >>> import Papa

type DFf a =
  (DF a, Df a)
  
-- |
--
-- >>> parse (parseFf <* eof) "test" "F" :: Either ParseError HeXDigit
-- Right HeXDigitF
--
-- >>> parse parseFf "test" "Fxyz" :: Either ParseError HeXDigit
-- Right HeXDigitF
--
-- >>> parse (parseFf <* eof) "test" "f" :: Either ParseError HeXDigit
-- Right HeXDigitf
--
-- >>> parse parseFf "test" "fxyz" :: Either ParseError HeXDigit
-- Right HeXDigitf
--
-- >>> isn't _Right (parse parseFf "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseFf ::
  (DFf d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef] <?> "Ff"
