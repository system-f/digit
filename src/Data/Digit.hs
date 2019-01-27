{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit (
  module Data.Digit.Binary
, module Data.Digit.Decimal
, module Data.Digit.Octal
, module Data.Digit.Hexadecimal.LowerCase
, module Data.Digit.Hexadecimal.UpperCase
, module Data.Digit.Hexadecimal.MixedCase
-- * Utilities
, module Data.Digit.Char
, module Data.Digit.Enum
, module Data.Digit.Integral
, module Data.Digit.Natural
) where

import Data.Digit.Binary
import Data.Digit.Decimal
import Data.Digit.Octal
import Data.Digit.Hexadecimal.LowerCase
import Data.Digit.Hexadecimal.UpperCase
import Data.Digit.Hexadecimal.MixedCase
import Data.Digit.Char
import Data.Digit.Enum
import Data.Digit.Integral
import Data.Digit.Natural
