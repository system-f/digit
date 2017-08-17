{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.De where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

class De d where
  de ::
    Prism'
      d
      ()
  xe ::
    d
  xe =
    de # ()

instance De () where
  de =
    id
    
-- |
--
-- >>> parse (parsee <* eof) "test" "e" :: Either ParseError (Digite ())
-- Right (Digite ())
--
-- >>> parse parsee "test" "exyz" :: Either ParseError (Digite ())
-- Right (Digite ())
--
-- >>> isn't _Right (parse parsee "test" "xyz" :: Either ParseError (Digite ()))
-- True
--
-- prop> \c -> c /= 'e' ==> isn't _Right (parse parsee "test" [c] :: Either ParseError (Digite ()))
parsee ::
  (De d, CharParsing p) =>
  p d
parsee =
  xe <$ char 'e' <?> "e"

instance De d => De (Either d x) where
  de =
    _Left . de
