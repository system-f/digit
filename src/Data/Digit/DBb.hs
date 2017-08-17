{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DBb where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DB(DB, parseB)
import Data.Digit.Db(Db, parseb)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Control.Lens(isn't, _Right)
-- >>> import Data.Digit.HeXaDeCiMaL

type DBb a =
  (DB a, Db a)
  
-- |
--
-- >>> parse (parseBb <* eof) "test" "B" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseBb <* eof) "test" "b" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseBb "test" "Bxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseBb "test" "bxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseBb "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Bb") ==> isn't _Right (parse parseBb "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseBb ::
  (DBb d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb] <?> "Bb"

