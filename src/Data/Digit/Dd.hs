{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Dd where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

class Dd d where
  dd ::
    Prism'
      d
      ()
  xd ::
    d
  xd =
    dd # ()

instance Dd () where
  dd =
    id
    
-- |
--
-- >>> parse (parsed <* eof) "test" "d" :: Either ParseError (Digitd ())
-- Right (Digitd ())
--
-- >>> parse parsed "test" "dxyz" :: Either ParseError (Digitd ())
-- Right (Digitd ())
--
-- >>> isn't _Right (parse parsed "test" "xyz" :: Either ParseError (Digitd ()))
-- True
--
-- prop> \c -> c /= 'd' ==> isn't _Right (parse parsed "test" [c] :: Either ParseError (Digitd ()))
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"

instance Dd d => Dd (Either d x) where
  dd =
    _Left . dd
