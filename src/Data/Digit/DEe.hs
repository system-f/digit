{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DEe(
  module Data.Digit.DE
, module Data.Digit.De
, DEe
, parseEe
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DE
import Data.Digit.De

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DEe a =
  (DE a, De a)
  
-- |
--
-- >>> parse (parseEe <* eof) "test" "E" :: Either ParseError HeXDigit
-- Right HeXDigitE
--
-- >>> parse parseEe "test" "Exyz" :: Either ParseError HeXDigit
-- Right HeXDigitE
--
-- >>> parse (parseEe <* eof) "test" "e" :: Either ParseError HeXDigit
-- Right HeXDigite
--
-- >>> parse parseEe "test" "exyz" :: Either ParseError HeXDigit
-- Right HeXDigite
--
-- >>> isn't _Right (parse parseEe "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseEe ::
  (DEe d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee] <?> "Ee"
