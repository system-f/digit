{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Data.Digit.Binary
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Binary(
  BinDigit(..)
, BinaryNoZero
, Binary
, parseBinaryNoZero
, parseBinary
-- * Prisms
, _BinDigit0
, _BinDigit1
-- * Re-exports
, module Data.Digit.Class.D0
, module Data.Digit.Class.D1
) where

import Prelude (Eq, Show, Ord)
import Control.Lens.TH (makePrisms)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)

import Data.Digit.Class.D0
import Data.Digit.Class.D1

-- | A single bit. Either zero or one.
data BinDigit
  = BinDigit0
  | BinDigit1
  deriving (Show, Eq, Ord)

makePrisms ''BinDigit

instance D0 BinDigit where; d0 = _BinDigit0
instance D1 BinDigit where; d1 = _BinDigit1

type BinaryNoZero =
  D1

parseBinaryNoZero ::
  (BinaryNoZero d, CharParsing p) =>
  p d
parseBinaryNoZero =
  parse1 <?> "BinaryNoZero"

-- | A constraint for bits
type Binary d =
  (D0 d, BinaryNoZero d)

parseBinary ::
  (Binary d, CharParsing p) =>
  p d
parseBinary =
  choice
    [
      parse0
    , parseBinaryNoZero
    ] <?> "Binary"
