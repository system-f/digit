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
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DDd a =
  (DD a, Dd a)
  
-- |
--
-- >>> parse (parseDd <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseDd "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseDd <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseDd "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> isn't _Right (parse parseDd "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "Dd") ==> isn't _Right (parse parseDd "test" [c] :: Either ParseError Digit)
parseDd ::
  (DDd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed] <?> "Dd"
