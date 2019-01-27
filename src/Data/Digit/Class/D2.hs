{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit.Class.D2
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Class.D2(
  D2(..)
, parse2
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

-- | Classy prism for the digit 2
class D2 d where
  d2 ::
    Prism'
      d
      ()
  x2 ::
    d
  x2 =
    d2 # ()

instance D2 () where
  d2 =
    id
    
-- |
--
-- >>> parse (parse2 <* eof) "test" "2" :: Either ParseError DecDigit
-- Right DecDigit2
--
-- >>> parse parse2 "test" "2xyz" :: Either ParseError DecDigit
-- Right DecDigit2
--
-- >>> isn't _Right (parse parse2 "test" "xyz" :: Either ParseError DecDigit)
-- True
parse2 ::
  (D2 d, CharParsing p) =>
  p d
parse2 =
  x2 <$ char '2' <?> "2"
