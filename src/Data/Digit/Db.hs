{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Db where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

class Db d where
  db ::
    Prism'
      d
      ()
  xb ::
    d
  xb =
    db # ()

instance Db () where
  db =
    id
    
-- |
--
-- >>> parse (parseb <* eof) "test" "b" :: Either ParseError (Digitb ())
-- Right (Digitb ())
--
-- >>> parse parseb "test" "bxyz" :: Either ParseError (Digitb ())
-- Right (Digitb ())
--
-- >>> isn't _Right (parse parseb "test" "xyz" :: Either ParseError (Digitb ()))
-- True
--
-- prop> \c -> c /= 'b' ==> isn't _Right (parse parseb "test" [c] :: Either ParseError (Digitb ()))
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"

instance Db d => Db (Either d x) where
  db =
    _Left . db
