{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DCc(
  module Data.Digit.DC
, module Data.Digit.Dc
, DCc
, parseCc
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DC
import Data.Digit.Dc

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit
-- >>> import Papa

type DCc a =
  (DC a, Dc a)
  
-- |
--
-- >>> parse (parseCc <* eof) "test" "C" :: Either ParseError HeXDigit
-- Right HeXDigitC
--
-- >>> parse parseCc "test" "Cxyz" :: Either ParseError HeXDigit
-- Right HeXDigitC
--
-- >>> parse (parseCc <* eof) "test" "c" :: Either ParseError HeXDigit
-- Right HeXDigitc
--
-- >>> parse parseCc "test" "cxyz" :: Either ParseError HeXDigit
-- Right HeXDigitc
--
-- >>> isn't _Right (parse parseCc "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseCc ::
  (DCc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec] <?> "Cc"
