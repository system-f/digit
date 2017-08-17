{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DDd where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DD
import Data.Digit.Dd

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Control.Lens(isn't, _Right)
-- >>> import Data.Digit.HeXaDeCiMaL

type DDd a =
  (DD a, Dd a)
  
-- |
--
-- >>> parse (parseDd <* eof) "test" "D" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDd <* eof) "test" "d" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseDd "test" "Dxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseDd "test" "dxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseDd "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Dd") ==> isn't _Right (parse parseDd "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseDd ::
  (DDd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed] <?> "Dd"
