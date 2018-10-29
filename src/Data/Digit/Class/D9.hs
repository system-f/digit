{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit.Class.D9
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Class.D9(
  D9(..)
, parse9
) where

import Control.Category (id)
import Control.Lens (Prism', (#))

import Data.Functor ((<$))

import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))

-- $setup
-- >>> import Data.Digit.Class
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

-- | Classy prism for the digit 9
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
-- >>> parse (parse9 <* eof) "test" "9" :: Either ParseError DecDigit
-- Right DecDigit9
--
-- >>> parse parse9 "test" "9xyz" :: Either ParseError DecDigit
-- Right DecDigit9
--
-- >>> isn't _Right (parse parse9 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse9 ::
  (D9 d, CharParsing p) =>
  p d
parse9 =
  x9 <$ char '9' <?> "9"
