{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit.Class.D8
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Class.D8(
  D8(..)
, parse8
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

-- | Classy prism for the digit 8
class D8 d where
  d8 ::
    Prism'
      d
      ()
  x8 ::
    d
  x8 =
    d8 # ()

instance D8 () where
  d8 =
    id
    
-- |
--
-- >>> parse (parse8 <* eof) "test" "8" :: Either ParseError DecDigit
-- Right DecDigit8
--
-- >>> parse parse8 "test" "8xyz" :: Either ParseError DecDigit
-- Right DecDigit8
--
-- >>> isn't _Right (parse parse8 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse8 ::
  (D8 d, CharParsing p) =>
  p d
parse8 =
  x8 <$ char '8' <?> "8"
