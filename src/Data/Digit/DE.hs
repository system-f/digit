{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DE where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

class DE d where
  dE ::
    Prism'
      d
      ()
  xE ::
    d
  xE =
    dE # ()

instance DE () where
  dE =
    id
    
-- |
--
-- >>> parse (parseE <* eof) "test" "E" :: Either ParseError (DigitE ())
-- Right (DigitE ())
--
-- >>> parse parseE "test" "Exyz" :: Either ParseError (DigitE ())
-- Right (DigitE ())
--
-- >>> isn't _Right (parse parseE "test" "xyz" :: Either ParseError (DigitE ()))
-- True
--
-- prop> \c -> c /= 'E' ==> isn't _Right (parse parseE "test" [c] :: Either ParseError (DigitE ()))
parseE ::
  (DE d, CharParsing p) =>
  p d
parseE =
  xE <$ char 'E' <?> "E"

instance DE d => DE (Either d x) where
  dE =
    _Left . dE
