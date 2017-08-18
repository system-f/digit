{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Digit.D0(
  D0(..)
, parse0
) where
  
import Data.Digit.Digit(Digit(Digit0))
import Papa
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

class D0 d where
  d0 ::
    Prism'
      d
      ()
  x0 ::
    d
  x0 =
    d0 # ()

instance D0 () where
  d0 =
    id

-- |
--
-- >>> parse (parse0 <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parse0 "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> isn't _Right (parse parse0 "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> c /= '0' ==> isn't _Right (parse parse0 "test" [c] :: Either ParseError Digit)
parse0 ::
  (D0 d, CharParsing p) =>
  p d
parse0 =
  x0 <$ char '0' <?> "0"

instance D0 Digit where
  d0 =
    prism'
      (\() -> Digit0)
      (\d ->  case d of
                Digit0 ->
                  Just ()
                _ ->
                  Nothing)
