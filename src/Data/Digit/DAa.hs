{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DAa where

import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DA
import Data.Digit.Da

type DAa a =
  (DA a, Da a)
  
-- |
--
-- >>> parse (parseAa <* eof) "test" "A" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseAa <* eof) "test" "a" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseAa "test" "Axyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseAa "test" "axyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseAa "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Aa") ==> isn't _Right (parse parseAa "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseAa ::
  (DAa d, CharParsing p) =>
  p d
parseAa =
  choice [parseA, parsea] <?> "Aa"
