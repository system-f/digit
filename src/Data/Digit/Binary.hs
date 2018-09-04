{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

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

data BinDigit
  = BinDigit0
  | BinDigit1
  deriving (Show, Eq, Ord)

makePrisms ''BinDigit

instance D0 BinDigit where; d0 = _BinDigit0
instance D1 BinDigit where; d1 = _BinDigit1

type BinaryNoZero d =
  D1 d

parseBinaryNoZero ::
  (BinaryNoZero d, CharParsing p) =>
  p d
parseBinaryNoZero =
  parse1 <?> "BinaryNoZero"

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
