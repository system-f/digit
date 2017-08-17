{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Da where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

class Da d where
  da ::
    Prism'
      d
      ()
  xa ::
    d
  xa =
    da # ()

instance Da () where
  da =
    id
    
-- |
--
-- >>> parse (parsea <* eof) "test" "a" :: Either ParseError (Digita ())
-- Right (Digita ())
--
-- >>> parse parsea "test" "axyz" :: Either ParseError (Digita ())
-- Right (Digita ())
--
-- >>> isn't _Right (parse parsea "test" "xyz" :: Either ParseError (Digita ()))
-- True
--
-- prop> \c -> c /= 'a' ==> isn't _Right (parse parsea "test" [c] :: Either ParseError (Digita ()))
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"

instance Da d => Da (Either d x) where
  da =
    _Left . da
