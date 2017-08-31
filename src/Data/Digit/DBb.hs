{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DBb(
  DBb
, parseBb
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DB(DB, parseB)
import Data.Digit.Db(Db, parseb)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DBb a =
  (DB a, Db a)
  
-- |
--
-- >>> parse (parseBb <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseBb "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseBb <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseBb "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> isn't _Right (parse parseBb "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "Bb") ==> isn't _Right (parse parseBb "test" [c] :: Either ParseError Digit)
parseBb ::
  (DBb d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb] <?> "Bb"
