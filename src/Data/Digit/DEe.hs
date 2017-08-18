{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DEe(
  DEe
, parseEe
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DE(DE, parseE)
import Data.Digit.De(De, parsee)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DEe a =
  (DE a, De a)
  
-- |
--
-- >>> parse (parseEe <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseEe "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseEe <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseEe "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> isn't _Right (parse parseEe "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "Ee") ==> isn't _Right (parse parseEe "test" [c] :: Either ParseError Digit)
parseEe ::
  (DEe d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee] <?> "Ee"
