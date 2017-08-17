{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Dc where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digitc

class Dc d where
  dc ::
    Prism'
      d
      ()
  xc ::
    d
  xc =
    dc # ()

instance Dc () where
  dc =
    id
   
-- |
--
-- >>> parse (parsec <* eof) "test" "c" :: Either ParseError (Digitc ())
-- Right (Digitc ())
--
-- >>> parse parsec "test" "cxyz" :: Either ParseError (Digitc ())
-- Right (Digitc ())
--
-- >>> isn't _Right (parse parsec "test" "xyz" :: Either ParseError (Digitc ()))
-- True
--
-- prop> \c -> c /= 'c' ==> isn't _Right (parse parsec "test" [c] :: Either ParseError (Digitc ()))
parsec ::
  (Dc d, CharParsing p) =>
  p d
parsec =
  xc <$ char 'c' <?> "c"

instance Dc d => Dc (Either d x) where
  dc =
    _Left . dc
