{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.D0 where
  
import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

class D0 d where
  d0 ::
    Prism'
      d
      ()
  x0 ::
    d
  x0 =
    d0 # ()

instance D0 () where
  d0 =
    id

-- |
--
-- >>> parse (parse0 <* eof) "test" "0" :: Either ParseError (Digit0 ())
-- Right (Digit0 ())
--
-- >>> parse parse0 "test" "0xyz" :: Either ParseError (Digit0 ())
-- Right (Digit0 ())
--
-- >>> isn't _Right (parse parse0 "test" "xyz" :: Either ParseError (Digit0 ()))
-- True
--
-- prop> \c -> c /= '0' ==> isn't _Right (parse parse0 "test" [c] :: Either ParseError (Digit0 ()))
parse0 ::
  (D0 d, CharParsing p) =>
  p d
parse0 =
  x0 <$ char '0' <?> "0"

instance D0 d => D0 (Either d x) where
  d0 =
    _Left . d0
