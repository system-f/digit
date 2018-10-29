{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit.Class.D3
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Class.D3(
  D3(..)
, parse3
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

-- | Classy prism for the digit 3
class D3 d where
  d3 ::
    Prism'
      d
      ()
  x3 ::
    d
  x3 =
    d3 # ()

instance D3 () where
  d3 =
    id

-- |
--
-- >>> parse (parse3 <* eof) "test" "3" :: Either ParseError DecDigit
-- Right DecDigit3
--
-- >>> parse parse3 "test" "3xyz" :: Either ParseError DecDigit
-- Right DecDigit3
--
-- >>> isn't _Right (parse parse3 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse3 ::
  (D3 d, CharParsing p) =>
  p d
parse3 =
  x3 <$ char '3' <?> "3"
