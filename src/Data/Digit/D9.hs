{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.D9 where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

class D9 d where
  d9 ::
    Prism'
      d
      ()
  x9 ::
    d
  x9 =
    d9 # ()

instance D9 () where
  d9 =
    id
    
-- |
--
-- >>> parse (parse9 <* eof) "test" "9" :: Either ParseError (Digit9 ())
-- Right (Digit9 ())
--
-- >>> parse parse9 "test" "9xyz" :: Either ParseError (Digit9 ())
-- Right (Digit9 ())
--
-- >>> isn't _Right (parse parse9 "test" "xyz" :: Either ParseError (Digit9 ()))
-- True
--
-- prop> \c -> c /= '9' ==> isn't _Right (parse parse9 "test" [c] :: Either ParseError (Digit9 ()))
parse9 ::
  (D9 d, CharParsing p) =>
  p d
parse9 =
  x9 <$ char '9' <?> "9"

instance D9 d => D9 (Either d x) where
  d9 =
    _Left . d9
