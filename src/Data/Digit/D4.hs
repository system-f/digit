{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.D4 where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
  
class D4 d where
  d4 ::
    Prism'
      d
      ()
  x4 ::
    d
  x4 =
    d4 # ()

instance D4 () where
  d4 =
    id
   
-- |
--
-- >>> parse (parse4 <* eof) "test" "4" :: Either ParseError (Digit4 ())
-- Right (Digit4 ())
--
-- >>> parse parse4 "test" "4xyz" :: Either ParseError (Digit4 ())
-- Right (Digit4 ())
--
-- >>> isn't _Right (parse parse4 "test" "xyz" :: Either ParseError (Digit4 ()))
-- True 
--
-- prop> \c -> c /= '4' ==> isn't _Right (parse parse4 "test" [c] :: Either ParseError (Digit4 ()))
parse4 ::
  (D4 d, CharParsing p) =>
  p d
parse4 =
  x4 <$ char '4' <?> "4"

instance D4 d => D4 (Either d x) where
  d4 =
    _Left . d4
