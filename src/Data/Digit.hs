{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A data type with ten nullary constructors [0-9] and combinators.
module Data.Digit
(
  Digit
, foldDigit
, HasDigit(..)
, AsDigit(..)
, ManyDigit(..)
, D0(..)
, D1(..)
, D2(..)
, D3(..)
, D4(..)
, D5(..)
, D6(..)
, D7(..)
, D8(..)
, D9(..)
, mod10
, absrem10
, divMod10
, parsedigit
, digitQ
, (/+/)
) where

import Data.Data (Data)
import Data.Maybe(fromMaybe)
import Data.Typeable (Typeable)
import Language.Haskell.TH(ExpQ, PatQ, varE, varP, mkName)
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter), quotePat, quoteExp, quoteDec, dataToExpQ, dataToPatQ, quoteType)
import Prelude(fromEnum, Bounded, error) -- livin on the edge
import Text.Parser.Char(CharParsing, char)
import Text.Parser.Combinators((<?>))
import Papa

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

instance Show Digit where
  show =
    show . fromEnum

-- | Catamorphism for @Digit@.
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D0 == q0
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D1 == q1
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D2 == q2
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D3 == q3
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D4 == q4
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D5 == q5
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D6 == q6
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D7 == q7
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D8 == q8
--
-- prop> foldDigit q0 q1 q2 q3 q4 q5 q6 q7 q8 q9 D9 == q9
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

class HasDigit a where
  hasdigit ::
    Lens' a Digit

instance HasDigit Digit where
  hasdigit =
    id

class D0 d where
  d0 ::
    Prism'
      d
      ()
  x0 ::
    D0 d =>
    d
  x0 =
    d0 # ()

instance D0 Digit where
  d0 =
    prism'
      (\() -> D0)
      (\d -> case d of
               D0 -> Just ()
               _ -> Nothing)

class D1 d where
  d1 ::
    Prism'
      d
      ()
  x1 ::
    D1 d =>
    d
  x1 =
    d1 # ()

instance D1 Digit where
  d1 =
    prism'
      (\() -> D1)
      (\d -> case d of
               D1 -> Just ()
               _ -> Nothing)

class D2 d where
  d2 ::
    Prism'
      d
      ()
  x2 ::
    D2 d =>
    d
  x2 =
    d2 # ()

instance D2 Digit where
  d2 =
    prism'
      (\() -> D2)
      (\d -> case d of
               D2 -> Just ()
               _ -> Nothing)

class D3 d where
  d3 ::
    Prism'
      d
      ()
  x3 ::
    D3 d =>
    d
  x3 =
    d3 # ()

instance D3 Digit where
  d3 =
    prism'
      (\() -> D3)
      (\d -> case d of
               D3 -> Just ()
               _ -> Nothing)

class D4 d where
  d4 ::
    Prism'
      d
      ()
  x4 ::
    D4 d =>
    d
  x4 =
    d4 # ()

instance D4 Digit where
  d4 =
    prism'
      (\() -> D4)
      (\d -> case d of
               D4 -> Just ()
               _ -> Nothing)

class D5 d where
  d5 ::
    Prism'
      d
      ()
  x5 ::
    D5 d =>
    d
  x5 =
    d5 # ()

instance D5 Digit where
  d5 =
    prism'
      (\() -> D5)
      (\d -> case d of
               D5 -> Just ()
               _ -> Nothing)

class D6 d where
  d6 ::
    Prism'
      d
      ()
  x6 ::
    D6 d =>
    d
  x6 =
    d6 # ()

instance D6 Digit where
  d6 =
    prism'
      (\() -> D6)
      (\d -> case d of
               D6 -> Just ()
               _ -> Nothing)

class D7 d where
  d7 ::
    Prism'
      d
      ()
  x7 ::
    D7 d =>
    d
  x7 =
    d7 # ()

instance D7 Digit where
  d7 =
    prism'
      (\() -> D7)
      (\d -> case d of
               D7 -> Just ()
               _ -> Nothing)

class D8 d where
  d8 ::
    Prism'
      d
      ()
  x8 ::
    D8 d =>
    d
  x8 =
    d8 # ()

instance D8 Digit where
  d8 =
    prism'
      (\() -> D8)
      (\d -> case d of
               D8 -> Just ()
               _ -> Nothing)

class D9 d where
  d9 ::
    Prism'
      d
      ()
  x9 ::
    D9 d =>
    d
  x9 =
    d9 # ()

instance D9 Digit where
  d9 =
    prism'
      (\() -> D9)
      (\d -> case d of
               D9 -> Just ()
               _ -> Nothing)

class AsDigit a where
  digit ::
    Prism'
      a
      Digit

instance AsDigit Digit where
  digit =
    id      

-- | 
--
-- >>> (5 :: Int) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Int) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Int) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Int) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Int) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Int
-- 5
--
-- >>> digitIntegral # D9 :: Int
-- 9
--
-- >>> digitIntegral # D0 :: Int
-- 0
instance AsDigit Int where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Int8) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Int8) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Int8) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Int8) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Int8) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Int8
-- 5
--
-- >>> digitIntegral # D9 :: Int8
-- 9
--
-- >>> digitIntegral # D0 :: Int8
-- 0
instance AsDigit Int8 where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Int16) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Int16) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Int16) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Int16) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Int16) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Int16
-- 5
--
-- >>> digitIntegral # D9 :: Int16
-- 9
--
-- >>> digitIntegral # D0 :: Int16
-- 0   
instance AsDigit Int16 where
  digit =
    digitIntegral
    
-- | 
--
-- >>> (5 :: Int32) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Int32) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Int32) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Int32) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Int32) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Int32
-- 5
--
-- >>> digitIntegral # D9 :: Int32
-- 9
--
-- >>> digitIntegral # D0 :: Int32
-- 0
instance AsDigit Int32 where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Int64) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Int64) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Int64) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Int64) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Int64) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Int64
-- 5
--
-- >>> digitIntegral # D9 :: Int64
-- 9
--
-- >>> digitIntegral # D0 :: Int64
-- 0
instance AsDigit Int64 where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Integer) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Integer) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Integer) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Integer) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Integer) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Integer
-- 5
--
-- >>> digitIntegral # D9 :: Integer
-- 9
--
-- >>> digitIntegral # D0 :: Integer
-- 0
instance AsDigit Integer where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Identity Int) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Identity Int) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Identity Int) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Identity Int) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Identity Int) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Identity Int
-- Identity 5
--
-- >>> digitIntegral # D9 :: Identity Int
-- Identity 9
--
-- >>> digitIntegral # D0 :: Identity Int
-- Identity 0
instance Integral a => AsDigit (Identity a) where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Const Int String) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Const Int String) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Const Int String) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Const Int String) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Const Int String) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Const Int String
-- Const 5
--
-- >>> digitIntegral # D9 :: Const Int String
-- Const 9
--
-- >>> digitIntegral # D0 :: Const Int String
-- Const 0
instance Integral a => AsDigit (Const a b) where
  digit =
    digitIntegral

-- | 
--
-- >>> (5 :: Word) ^? digitIntegral
-- Just 5
--
-- >>> (0 :: Word) ^? digitIntegral
-- Just 0
--
-- >>> (9 :: Word) ^? digitIntegral
-- Just 9
--
-- >>> (10 :: Word) ^? digitIntegral
-- Nothing
--
-- >>> ((-5) :: Word) ^? digitIntegral
-- Nothing
--
-- >>> digitIntegral # D5 :: Word
-- 5
--
-- >>> digitIntegral # D9 :: Word
-- 9
--
-- >>> digitIntegral # D0 :: Word
-- 0
instance AsDigit Word where
  digit =
    digitIntegral

-- | 
--
-- >>> '5' ^? digit
-- Just 5
--
-- >>> '0' ^? digit
-- Just 0
--
-- >>> '9' ^? digit
-- Just 9
--
-- >>> 'a' ^? digit
-- Nothing
--
-- >>> '@' ^? digit
-- Nothing
--
-- >>> digit # D5 :: Char
-- '5'
--
-- >>> digit # D9 :: Char
-- '9'
--
-- >>> digit # D0 :: Char
-- '0'
instance AsDigit Char where
  digit =
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

class ManyDigit a where
  digitT ::
    Traversal'
      a
      Digit

instance ManyDigit Digit where
  digitT =
    id
    
-- | Modulus with 10.
--
-- >>> mod10 0
-- 0
--
-- >>> mod10 1
-- 1
--
-- >>> mod10 8
-- 8
--
-- >>> mod10 9
-- 9
--
-- >>> mod10 10
-- 0
--
-- >>> mod10 90
-- 0
--
-- >>> mod10 91
-- 1
--
-- >>> mod10 (-1)
-- 9
--
-- >>> mod10 (-12)
-- 8
mod10 ::
  Integral a =>
  a
  -> Digit
mod10 n =
  let r = n `mod` 10
  in fromMaybe (mod10 r) (r ^? digitIntegral)

-- | Absolute value of remainder 10.
--
-- >>> absrem10 0
-- 0
--
-- >>> absrem10 1
-- 1
--
-- >>> absrem10 8
-- 8
--
-- >>> absrem10 9
-- 9
--
-- >>> absrem10 10
-- 0
--
-- >>> absrem10 90
-- 0
--
-- >>> absrem10 91
-- 1
--
-- >>> absrem10 (-1)
-- 1
--
-- >>> absrem10 (-12)
-- 2
absrem10 ::
  Integral a =>
  a
  -> Digit
absrem10 n =
  let r = abs n `rem` 10
  in fromMaybe (absrem10 r) (r ^? digitIntegral)

-- | Division/modulus with 10.
--
-- >>> divMod10 0
-- (0,0)
--
-- >>> divMod10 1
-- (0,1)
--
-- >>> divMod10 8
-- (0,8)
--
-- >>> divMod10 9
-- (0,9)
--
-- >>> divMod10 10
-- (1,0)
--
-- >>> divMod10 90
-- (9,0)
--
-- >>> divMod10 91
-- (9,1)
--
-- >>> divMod10 (-1)
-- (-1,9)
divMod10 ::
  Integral a =>
  a
  -> (a, Digit)
divMod10 n =
  let (x, r) = n `divMod` 10
  in (x, mod10 r)

parsedigit ::
  CharParsing p =>
  p Digit
parsedigit =
  let p = asum ((\d -> d <$ char (digit # d)) <$> [D0 .. D9])
  in p <?> "Digit"

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
    quoteExp =  let dexp ::
                      [Char]
                      -> ExpQ
                    dexp ('$':vn) =
                      varE (mkName vn)
                    dexp (d:[]) =
                      maybe (error "not a digit") (dataToExpQ (const Nothing)) (d ^? digit)
                    dexp _ =
                      error "not a digit"
                in  dexp
  , quotePat =  let dpat ::
                      [Char]
                      -> PatQ
                    dpat ('$':vn) =
                      varP (mkName vn)
                    dpat (d:[]) =
                      maybe (error "not a digit") (dataToPatQ (const Nothing)) (d ^? digit)
                    dpat _ =
                      error "not a digit"
                in  dpat
  , quoteType =
      error "not quotable"
  , quoteDec =
      error "not quotable"
  }

-- |
--
-- >>> D0 /+/ D0
-- (0,0)
--
-- >>> D0 /+/ D1
-- (0,1)
--
-- >>> D1 /+/ D0
-- (0,1)
--
-- >>> D4 /+/ D5
-- (0,9)
--
-- >>> D5 /+/ D5
-- (1,0)
--
-- >>> D5 /+/ D6
-- (1,1)
--
-- >>> D8 /+/ D7
-- (1,5)
--
-- >>> D9 /+/ D9
-- (1,8)
(/+/) ::
  Digit
  -> Digit
  -> (Digit, Digit)
a /+/ b =
  let (x, r) = divMod10 (digitIntegral # a + digitIntegral # b)
  in (mod10 (x :: Integer), r) 

---- not exported
digitIntegral ::
  Integral a =>
  Prism' a Digit
digitIntegral =
  prism'
    (\n -> case n of D0 -> 0
                     D1 -> 1
                     D2 -> 2
                     D3 -> 3
                     D4 -> 4
                     D5 -> 5
                     D6 -> 6
                     D7 -> 7
                     D8 -> 8
                     D9 -> 9)
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
