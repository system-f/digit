{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Data.Digit.Octal
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Octal(
  OctDigit(..)
, OctalNoZero
, Octal
, parseOctalNoZero
, parseOctal
-- * Prisms
, _OctDigit0
, _OctDigit1
, _OctDigit2
, _OctDigit3
, _OctDigit4
, _OctDigit5
, _OctDigit6
, _OctDigit7
-- * Re-exports
, module Data.Digit.Class.D0
, module Data.Digit.Class.D1
, module Data.Digit.Class.D2
, module Data.Digit.Class.D3
, module Data.Digit.Class.D4
, module Data.Digit.Class.D5
, module Data.Digit.Class.D6
, module Data.Digit.Class.D7
) where

import Prelude (Eq, Show, Ord, Enum, Bounded)
import Control.Lens.TH (makePrisms)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.Class.D0
import Data.Digit.Class.D1
import Data.Digit.Class.D2
import Data.Digit.Class.D3
import Data.Digit.Class.D4
import Data.Digit.Class.D5
import Data.Digit.Class.D6
import Data.Digit.Class.D7

data OctDigit
  = OctDigit0
  | OctDigit1
  | OctDigit2
  | OctDigit3
  | OctDigit4
  | OctDigit5
  | OctDigit6
  | OctDigit7
  deriving (Show, Eq, Ord, Enum, Bounded)

makePrisms ''OctDigit

instance D0 OctDigit where; d0 = _OctDigit0
instance D1 OctDigit where; d1 = _OctDigit1
instance D2 OctDigit where; d2 = _OctDigit2
instance D3 OctDigit where; d3 = _OctDigit3
instance D4 OctDigit where; d4 = _OctDigit4
instance D5 OctDigit where; d5 = _OctDigit5
instance D6 OctDigit where; d6 = _OctDigit6
instance D7 OctDigit where; d7 = _OctDigit7

type OctalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d)

parseOctalNoZero ::
  (OctalNoZero d, CharParsing p) =>
  p d
parseOctalNoZero =
  choice
    [
      parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    ] <?> "OctalNoZero"

type Octal d =
  (D0 d, OctalNoZero d)

parseOctal ::
  (Octal d, CharParsing p) =>
  p d
parseOctal =
  choice
    [
      parse0
    , parseOctalNoZero
    ] <?> "Octal"
