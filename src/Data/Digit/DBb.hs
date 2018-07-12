{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DBb(
  module Data.Digit.DB
, module Data.Digit.Db
, DBb
, parseBb
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DB
import Data.Digit.Db

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit
-- >>> import Papa

type DBb a =
  (DB a, Db a)
  
-- |
--
-- >>> parse (parseBb <* eof) "test" "B" :: Either ParseError HeXDigit
-- Right HeXDigitB
--
-- >>> parse parseBb "test" "Bxyz" :: Either ParseError HeXDigit
-- Right HeXDigitB
--
-- >>> parse (parseBb <* eof) "test" "b" :: Either ParseError HeXDigit
-- Right HeXDigitb
--
-- >>> parse parseBb "test" "bxyz" :: Either ParseError HeXDigit
-- Right HeXDigitb
--
-- >>> isn't _Right (parse parseBb "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseBb ::
  (DBb d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb] <?> "Bb"
