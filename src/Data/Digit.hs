{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A data type with ten nullary constructors [0-9] and combinators.
module Data.Digit
(
-- * Data type
  Digit
-- * Destructors
, foldDigit
-- * Prisms
, D0(..)
, x0
, D1(..)
, x1
, D2(..)
, x2
, D3(..)
, x3
, D4(..)
, x4
, D5(..)
, x5
, D6(..)
, x6
, D7(..)
, x7
, D8(..)
, x8
, D9(..)
, x9
, digit
, digitC
, digitQ
) where

import Control.Category((.))
import Control.Lens(Prism', prism', (^?), ( # ))
import Data.Char(Char)
import Data.Data (Data)
import Data.Eq(Eq)
import Data.Function(const)
import Data.Int(Int)
import Data.Maybe(Maybe(Nothing, Just), maybe)
import Data.Ord(Ord)
import Data.Typeable (Typeable)
import Language.Haskell.TH(ExpQ, PatQ, varE, varP, mkName)
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter), quotePat, quoteExp, quoteDec, dataToExpQ, dataToPatQ, quoteType)
import Prelude(Show(..), Read(..), Enum(..), Bounded, error)

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
  deriving (Eq, Ord, Enum, Bounded, Data, Typeable)

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
foldDigit q0 _  _  _  _  _  _  _  _  _  D0 =
  q0
foldDigit _  q1 _  _  _  _  _  _  _  _  D1 =
  q1
foldDigit _  _  q2 _  _  _  _  _  _  _  D2 =
  q2
foldDigit _  _  _  q3 _  _  _  _  _  _  D3 =
  q3
foldDigit _  _  _  _  q4 _  _  _  _  _  D4 =
  q4
foldDigit _  _  _  _  _  q5 _  _  _  _  D5 =
  q5
foldDigit _  _  _  _  _  _  q6 _  _  _  D6 =
  q6
foldDigit _  _  _  _  _  _  _  q7 _  _  D7 =
  q7
foldDigit _  _  _  _  _  _  _  _  q8 _  D8 =
  q8
foldDigit _  _  _  _  _  _  _  _  _  q9 D9 =
  q9

class D0 d where
  d0 ::
    Prism'
      d
      ()

instance D0 Digit where
  d0 =
    prism'
      (\() -> D0)
      (\d -> case d of
               D0 -> Just ()
               _ -> Nothing)

x0 ::
  D0 d =>
  d
x0 =
  d0 # ()

class D1 d where
  d1 ::
    Prism'
      d
      ()

instance D1 Digit where
  d1 =
    prism'
      (\() -> D1)
      (\d -> case d of
               D1 -> Just ()
               _ -> Nothing)

x1 ::
  D1 d =>
  d
x1 =
  d1 # ()

class D2 d where
  d2 ::
    Prism'
      d
      ()

instance D2 Digit where
  d2 =
    prism'
      (\() -> D2)
      (\d -> case d of
               D2 -> Just ()
               _ -> Nothing)

x2 ::
  D2 d =>
  d
x2 =
  d2 # ()

class D3 d where
  d3 ::
    Prism'
      d
      ()

instance D3 Digit where
  d3 =
    prism'
      (\() -> D3)
      (\d -> case d of
               D3 -> Just ()
               _ -> Nothing)

x3 ::
  D3 d =>
  d
x3 =
  d3 # ()

class D4 d where
  d4 ::
    Prism'
      d
      ()

instance D4 Digit where
  d4 =
    prism'
      (\() -> D4)
      (\d -> case d of
               D4 -> Just ()
               _ -> Nothing)

x4 ::
  D4 d =>
  d
x4 =
  d4 # ()

class D5 d where
  d5 ::
    Prism'
      d
      ()

instance D5 Digit where
  d5 =
    prism'
      (\() -> D5)
      (\d -> case d of
               D5 -> Just ()
               _ -> Nothing)

x5 ::
  D5 d =>
  d
x5 =
  d5 # ()

class D6 d where
  d6 ::
    Prism'
      d
      ()

instance D6 Digit where
  d6 =
    prism'
      (\() -> D6)
      (\d -> case d of
               D6 -> Just ()
               _ -> Nothing)

x6 ::
  D6 d =>
  d
x6 =
  d6 # ()

class D7 d where
  d7 ::
    Prism'
      d
      ()

instance D7 Digit where
  d7 =
    prism'
      (\() -> D7)
      (\d -> case d of
               D7 -> Just ()
               _ -> Nothing)

x7 ::
  D7 d =>
  d
x7 =
  d7 # ()

class D8 d where
  d8 ::
    Prism'
      d
      ()

instance D8 Digit where
  d8 =
    prism'
      (\() -> D8)
      (\d -> case d of
               D8 -> Just ()
               _ -> Nothing)

x8 ::
  D8 d =>
  d
x8 =
  d8 # ()

class D9 d where
  d9 ::
    Prism'
      d
      ()

instance D9 Digit where
  d9 =
    prism'
      (\() -> D9)
      (\d -> case d of
               D9 -> Just ()
               _ -> Nothing)

x9 ::
  D9 d =>
  d
x9 =
  d9 # ()

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
    (\d -> case d of D0 -> '0'
                     D1 -> '1'
                     D2 -> '2'
                     D3 -> '3'
                     D4 -> '4'
                     D5 -> '5'
                     D6 -> '6'
                     D7 -> '7'
                     D8 -> '8'
                     D9 -> '9')
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

-- | A QuasiQuoter for any range of @Digit@.
--
-- [digitQ|4|] :: Digit
-- 4
--
-- named [digitQ|4|]  = "four"
-- named [digitQ|$x|] = "not four, " ++ show x ++ " instead"
--
-- mod10D x = let y = mod x 10 in [digitQ|$y|]
digitQ :: QuasiQuoter
digitQ = QuasiQuoter {
    quoteExp = dexp
  , quotePat = dpat
  , quoteType = error "not quotable"
  , quoteDec = error "not quotable"
  }

dexp :: [Char] -> ExpQ
dexp ('$':vn) = varE (mkName vn)
dexp (d:[])   = maybe (error "not a digit") (dataToExpQ (const Nothing)) (d ^? digitC)
dexp _        = error "not a digit"

dpat :: [Char] -> PatQ
dpat ('$':vn) = varP (mkName vn)
dpat (d:[])   = maybe (error "not a digit") (dataToPatQ (const Nothing)) (d ^? digitC)
dpat _        = error "not a digit"
