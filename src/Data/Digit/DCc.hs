{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.DCc(
  DCc
, parseCc
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DC(DC, parseC)
import Data.Digit.Dc(Dc, parsec)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DCc a =
  (DC a, Dc a)
  
-- |
--
-- >>> parse (parseCc <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseCc "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseCc <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseCc "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> isn't _Right (parse parseCc "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "Cc") ==> isn't _Right (parse parseCc "test" [c] :: Either ParseError Digit)
parseCc ::
  (DCc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec] <?> "Cc"
