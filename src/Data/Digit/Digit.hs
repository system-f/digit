{-# LANGUAGE NoImplicitPrelude #-}

-- | A data type with ten nullary constructors [0-9] and combinators.
module Data.Digit.Digit
(
-- * Data type
  Digit
-- * Destructors
, foldDigit
-- * Prisms
, digit
, digitC
) where

import Prelude(Show(..), Read(..), Eq, Enum(..), Maybe(..), Bounded, Ord, Int, Char, (.))
import Data.Digit.D0
import Data.Digit.D1
import Data.Digit.D2
import Data.Digit.D3
import Data.Digit.D4
import Data.Digit.D5
import Data.Digit.D6
import Data.Digit.D7
import Data.Digit.D8
import Data.Digit.D9
import Control.Lens

-- $setup
-- >>> import Prelude

-- | A data type with ten nullary constructors.
data Digit =
  D0
  | D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  | D9
  deriving (Eq, Ord, Enum, Bounded)

-- | Catamorphism for @Digit@.
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d0 == x0
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d1 == x1
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d2 == x2
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d3 == x3
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d4 == x4
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d5 == x5
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d6 == x6
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d7 == x7
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d8 == x8
--
-- prop> foldDigit x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 d9 == x9
foldDigit ::
  a -- ^ Zero.
  -> a -- ^ One.
  -> a -- ^ Two.
  -> a -- ^ Three.
  -> a -- ^ Four.
  -> a -- ^ Five.
  -> a -- ^ Six.
  -> a -- ^ Seven.
  -> a -- ^ Eight.
  -> a -- ^ Nine.
  -> Digit -- ^ The digit to fold.
  -> a
foldDigit x0 _  _  _  _  _  _  _  _  _  D0 =
  x0
foldDigit _  x1 _  _  _  _  _  _  _  _  D1 =
  x1
foldDigit _  _  x2 _  _  _  _  _  _  _  D2 =
  x2
foldDigit _  _  _  x3 _  _  _  _  _  _  D3 =
  x3
foldDigit _  _  _  _  x4 _  _  _  _  _  D4 =
  x4
foldDigit _  _  _  _  _  x5 _  _  _  _  D5 =
  x5
foldDigit _  _  _  _  _  _  x6 _  _  _  D6 =
  x6
foldDigit _  _  _  _  _  _  _  x7 _  _  D7 =
  x7
foldDigit _  _  _  _  _  _  _  _  x8 _  D8 =
  x8
foldDigit _  _  _  _  _  _  _  _  _  x9 D9 =
  x9

instance D0 Digit where
  d0 =
    D0

instance D1 Digit where
  d1 =
    D1

instance D2 Digit where
  d2 =
    D2

instance D3 Digit where
  d3 =
    D3

instance D4 Digit where
  d4 =
    D4

instance D5 Digit where
  d5 =
    D5

instance D6 Digit where
  d6 =
    D6

instance D7 Digit where
  d7 =
    D7

instance D8 Digit where
  d8 =
    D8

instance D9 Digit where
  d9 =
    D9

-- | A prism for using @Int@ as @Digit@.
--
-- >>> 5 ^? digit
-- Just 5
--
-- >>> 0 ^? digit
-- Just 0
--
-- >>> 9 ^? digit
-- Just 9
--
-- >>> 10 ^? digit
-- Nothing
--
-- >>> (-5) ^? digit
-- Nothing
digit ::
  Prism' Int Digit
digit =
  prism'
    fromEnum
    (\n -> case n of 0 -> Just D0
                     1 -> Just D1
                     2 -> Just D2
                     3 -> Just D3
                     4 -> Just D4
                     5 -> Just D5
                     6 -> Just D6
                     7 -> Just D7
                     8 -> Just D8
                     9 -> Just D9
                     _ -> Nothing)

-- | A prism for using @Char@ as @Digit@.
--
-- >>> '5' ^? digitC
-- Just 5
--
-- >>> '0' ^? digitC
-- Just 0
--
-- >>> '9' ^? digitC
-- Just 9
--
-- >>> 'a' ^? digitC
-- Nothing
--
-- >>> '@' ^? digitC
-- Nothing
digitC ::
  Prism' Char Digit
digitC =
  prism'
    (let f = f in f)
    (\n -> case n of '0' -> Just D0
                     '1' -> Just D1
                     '2' -> Just D2
                     '3' -> Just D3
                     '4' -> Just D4
                     '5' -> Just D5
                     '6' -> Just D6
                     '7' -> Just D7
                     '8' -> Just D8
                     '9' -> Just D9
                     _ -> Nothing)

instance Show Digit where
  show = show . fromEnum

instance Read Digit where
  readsPrec _ ('0':t) =
    [(D0, t)]
  readsPrec _ ('1':t) =
    [(D1, t)]
  readsPrec _ ('2':t) =
    [(D2, t)]
  readsPrec _ ('3':t) =
    [(D3, t)]
  readsPrec _ ('4':t) =
    [(D4, t)]
  readsPrec _ ('5':t) =
    [(D5, t)]
  readsPrec _ ('6':t) =
    [(D6, t)]
  readsPrec _ ('7':t) =
    [(D7, t)]
  readsPrec _ ('8':t) =
    [(D8, t)]
  readsPrec _ ('9':t) =
    [(D9, t)]
  readsPrec _ _       =
    []
