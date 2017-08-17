{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DFf where

import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DF
import Data.Digit.Df

type DFf a =
  (DF a, Df a)
  
-- |
--
-- >>> parse (parseFf <* eof) "test" "F" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseFf <* eof) "test" "f" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseFf "test" "Fxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseFf "test" "fxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseFf "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Ff") ==> isn't _Right (parse parseFf "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseFf ::
  (DFf d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef] <?> "Ff"
