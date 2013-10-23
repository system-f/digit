{-# LANGUAGE NoImplicitPrelude #-}

-- | A data type with eight nullary constructors [1-8] and combinators.
module Data.Digit.Digit1_8
(
-- * Data type
  Digit1_8
-- * Destructors
, foldDigit1_8
-- * Prisms
, digit1_8
, digitC1_8
, digit1_8digit
, digit1_8digit1_9
) where

import Prelude(Show(..), Read(..), Eq, Enum(..), Maybe(..), Bounded, Ord, Int, Char, (.))
import Data.Digit.Digit(Digit, digit, digitC)
import Data.Digit.Digit1_9(Digit1_9, foldDigit1_9, digit1_9digit)
import Data.Digit.D1
import Data.Digit.D2
import Data.Digit.D3
import Data.Digit.D4
import Data.Digit.D5
import Data.Digit.D6
import Data.Digit.D7
import Data.Digit.D8
import Control.Lens

-- $setup
-- >>> import Prelude
-- >>> import Data.Digit.D0
-- >>> import Data.Digit.D9

-- | A data type with eight nullary constructors.
data Digit1_8 =
  D1
  | D2
  | D3
  | D4
  | D5
  | D6
  | D7
  | D8
  deriving (Eq, Ord, Bounded)

-- | Catamorphism for @Digit1_8@.
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d1 == x1
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d2 == x2
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d3 == x3
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d4 == x4
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d5 == x5
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d6 == x6
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d7 == x7
--
-- prop> foldDigit1_8 x1 x2 x3 x4 x5 x6 x7 x8 d8 == x8
foldDigit1_8 ::
  a -- ^ One.
  -> a -- ^ Two.
  -> a -- ^ Three.
  -> a -- ^ Four.
  -> a -- ^ Five.
  -> a -- ^ Six.
  -> a -- ^ Seven.
  -> a -- ^ Eight.
  -> Digit1_8 -- ^ The digit to fold.
  -> a
foldDigit1_8 x1 _  _  _  _  _  _  _  D1 =
  x1
foldDigit1_8 _  x2 _  _  _  _  _  _  D2 =
  x2
foldDigit1_8 _  _  x3 _  _  _  _  _  D3 =
  x3
foldDigit1_8 _  _  _  x4 _  _  _  _  D4 =
  x4
foldDigit1_8 _  _  _  _  x5 _  _  _  D5 =
  x5
foldDigit1_8 _  _  _  _  _  x6 _  _  D6 =
  x6
foldDigit1_8 _  _  _  _  _  _  x7 _  D7 =
  x7
foldDigit1_8 _  _  _  _  _  _  _  x8 D8 =
  x8

instance D1 Digit1_8 where
  d1 =
    D1

instance D2 Digit1_8 where
  d2 =
    D2

instance D3 Digit1_8 where
  d3 =
    D3

instance D4 Digit1_8 where
  d4 =
    D4

instance D5 Digit1_8 where
  d5 =
    D5

instance D6 Digit1_8 where
  d6 =
    D6

instance D7 Digit1_8 where
  d7 =
    D7

instance D8 Digit1_8 where
  d8 =
    D8

-- | A prism for using @Int@ as @Digit1_8@.
--
-- >>> 5 ^? digit1_8
-- Just 5
--
-- >>> 0 ^? digit1_8
-- Nothing
--
-- >>> 8 ^? digit1_8
-- Just 8
--
-- >>> 9 ^? digit1_8
-- Nothing
--
-- >>> 10 ^? digit1_8
-- Nothing
--
-- >>> (-5) ^? digit1_8
-- Nothing
digit1_8 ::
  Prism' Int Digit1_8
digit1_8 =
  digit . digit1_8digit

-- | A prism for using @Char@ as @Digit1_8@.
--
-- >>> '5' ^? digitC1_8
-- Just 5
--
-- >>> '0' ^? digitC1_8
-- Nothing
--
-- >>> '8' ^? digitC1_8
-- Just 8
--
-- >>> '9' ^? digitC1_8
-- Nothing
--
-- >>> 'x' ^? digitC1_8
-- Nothing
--
-- >>> '*' ^? digitC1_8
-- Nothing
digitC1_8 ::
  Prism' Char Digit1_8
digitC1_8 =
  digitC . digit1_8digit

-- | A prism for using @Digit@ as @Digit1_8@.
--
-- >>> d5 ^? digit1_8digit
-- Just 5
--
-- >>> d0 ^? digit1_8digit
-- Nothing
--
-- >>> d8 ^? digit1_8digit
-- Just 8
--
-- >>> d9 ^? digit1_8digit
-- Nothing
--
-- >>> d1 ^? digit1_8digit
-- Just 1
digit1_8digit ::
  Prism' Digit Digit1_8
digit1_8digit =
  digit1_9digit . digit1_8digit1_9

-- | A prism for using @Digit1_9@ as @Digit1_8@.
--
-- >>> d5 ^? digit1_8digit1_9
-- Just 5
--
-- >>> d8 ^? digit1_8digit1_9
-- Just 8
--
-- >>> d9 ^? digit1_8digit1_9
-- Nothing
--
-- >>> d1 ^? digit1_8digit1_9
-- Just 1
digit1_8digit1_9 ::
  Prism' Digit1_9 Digit1_8
digit1_8digit1_9 =
  prism'
    (foldDigit1_8
           d1
           d2
           d3
           d4
           d5
           d6
           d7
           d8)
    (foldDigit1_9
      (Just D1)
      (Just D2)
      (Just D3)
      (Just D4)
      (Just D5)
      (Just D6)
      (Just D7)
      (Just D8)
      Nothing)


instance Show Digit1_8 where
  show d =
    show (digit1_8 # d)

instance Read Digit1_8 where
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
  readsPrec _ _       =
    []
