{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit.Class.D5
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Class.D5(
  D5(..)
, parse5
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

-- | Classy prism for the digit 5
class D5 d where
  d5 ::
    Prism'
      d
      ()
  x5 ::
    d
  x5 =
    d5 # ()

instance D5 () where
  d5 =
    id
    
-- |
--
-- >>> parse (parse5 <* eof) "test" "5" :: Either ParseError DecDigit
-- Right DecDigit5
--
-- >>> parse parse5 "test" "5xyz" :: Either ParseError DecDigit
-- Right DecDigit5
--
-- >>> isn't _Right (parse parse5 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse5 ::
  (D5 d, CharParsing p) =>
  p d
parse5 =
  x5 <$ char '5' <?> "5"
