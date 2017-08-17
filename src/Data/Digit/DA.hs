{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DA where

import Control.Lens hiding ((<.>))
import Text.Parser.Char
import Text.Parser.Combinators((<?>))

class DA d where
  dA ::
    Prism'
      d
      ()
  xA ::
    d
  xA =
    dA # ()

instance DA () where
  dA =
    id

-- |
--
-- >>> parse (parseA <* eof) "test" "A" :: Either ParseError (DigitA ())
-- Right (DigitA ())
--
-- >>> parse parseA "test" "Axyz" :: Either ParseError (DigitA ())
-- Right (DigitA ())
--
-- >>> isn't _Right (parse parseA "test" "xyz" :: Either ParseError (DigitA ()))
-- True
--
-- prop> \c -> c /= 'A' ==> isn't _Right (parse parseA "test" [c] :: Either ParseError (DigitA ()))
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"

instance DA d => DA (Either d x) where
  dA =
    _Left . dA
