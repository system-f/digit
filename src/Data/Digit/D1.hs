{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.D1 where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit1

class D1 d where
  d1 ::
    Prism'
      d
      ()
  x1 ::
    d
  x1 =
    d1 # ()

instance D1 () where
  d1 =
    id

-- |
--
-- >>> parse (parse1 <* eof) "test" "1" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> parse parse1 "test" "1xyz" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> isn't _Right (parse parse1 "test" "xyz" :: Either ParseError (Digit1 ()))
-- True
--
-- prop> \c -> c /= '1' ==> isn't _Right (parse parse1 "test" [c] :: Either ParseError (Digit1 ()))
parse1 ::
  (D1 d, CharParsing p) =>
  p d
parse1 =
  x1 <$ char '1' <?> "1"
    
instance D1 d => D1 (Either d x) where
  d1 =
    _Left . d1
