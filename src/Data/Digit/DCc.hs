{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DCc where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DC
import Data.Digit.Dc

type DCc a =
  (DC a, Dc a)
  
-- |
--
-- >>> parse (parseCc <* eof) "test" "C" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseCc <* eof) "test" "c" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseCc "test" "Cxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseCc "test" "cxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseCc "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Cc") ==> isn't _Right (parse parseCc "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseCc ::
  (DCc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec]
