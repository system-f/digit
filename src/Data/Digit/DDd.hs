{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DDd(
  module Data.Digit.DD
, module Data.Digit.Dd
, DDd
, parseDd
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DD
import Data.Digit.Dd

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit
-- >>> import Papa

type DDd a =
  (DD a, Dd a)
  
-- |
--
-- >>> parse (parseDd <* eof) "test" "D" :: Either ParseError HeXDigit
-- Right HeXDigitD
--
-- >>> parse parseDd "test" "Dxyz" :: Either ParseError HeXDigit
-- Right HeXDigitD
--
-- >>> parse (parseDd <* eof) "test" "d" :: Either ParseError HeXDigit
-- Right HeXDigitd
--
-- >>> parse parseDd "test" "dxyz" :: Either ParseError HeXDigit
-- Right HeXDigitd
--
-- >>> isn't _Right (parse parseDd "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseDd ::
  (DDd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed] <?> "Dd"
