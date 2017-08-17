{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DEe where

import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DE
import Data.Digit.De

type DEe a =
  (DE a, De a)
  
-- |
--
-- >>> parse (parseEe <* eof) "test" "E" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseEe <* eof) "test" "e" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseEe "test" "Exyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseEe "test" "exyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseEe "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Ee") ==> isn't _Right (parse parseEe "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseEe ::
  (DEe d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee] <?> "Ee"
