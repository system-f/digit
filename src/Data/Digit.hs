{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
, digitlist
-- * mod operations
, mod10
, divMod10
-- * Parsers
, parsedigit
, parsedigitlist
, parsedigits
, parsedigitlist1
, skipdigitlist
, skipdigitlist1
, parsenotdigit
, parsenotdigits
, parsenotdigits1
, skipnotdigits
, skipnotdigits1
-- * Quasi-Quoters
, digitQ
-- * Digits
, Digits
, digits
, digitsI
, digitsS
, (/+/)
, (.+.)
, (.*.)
, mantissa
) where

import Control.Applicative(many, some)
import Control.Category((.))
import Control.Lens(Prism', prism', Iso', iso, Cons(_Cons), Snoc(_Snoc), AsEmpty(_Empty), Each(each), Ixed(ix), Index, IxValue, Plated(plate), Reversing(reversing), (^?), ( # ))
import Control.Monad(Monad)
import Data.Char(Char)
import Data.Data (Data)
import Data.Eq(Eq((==)))
import Data.Foldable(foldl', asum)
import Data.Function(const)
import Data.Functor((<$), (<$>))
import Data.Int(Int)
import Data.List(unfoldr, reverse, notElem, zip, (++))
import Data.List.NonEmpty(NonEmpty, some1)
import Data.Maybe(Maybe(Nothing, Just), maybe, fromMaybe)
import Data.Monoid(Monoid(mempty, mappend))
import Data.Ord(Ord((<)))
import Data.Semigroup(Semigroup((<>)))
import Data.String(String)
import Data.Traversable(traverse)
import Data.Typeable (Typeable)
import Language.Haskell.TH(ExpQ, PatQ, varE, varP, mkName)
import Language.Haskell.TH.Quote(QuasiQuoter(QuasiQuoter), quotePat, quoteExp, quoteDec, dataToExpQ, dataToPatQ, quoteType)
import Prelude(Show(..), Read(..), Enum(..), Floating((**)), Bounded, Num(..), Integral, Integer, fromIntegral, error, divMod, mod)
import Text.Parser.Char(CharParsing, char, satisfy)
import Text.Parser.Combinators(skipMany, skipSome, (<?>))

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
--
-- >>> digit # D5
-- 5
--
-- >>> digit # D9
-- 9
--
-- >>> digit # D0
-- 0
digit ::
  Integral a =>
  Prism' a Digit
digit =
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
--
-- >>> digitC # D5
-- '5'
--
-- >>> digitC # D9
-- '9'
--
-- >>> digitC # D0
-- '0'
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

-- | A prism for the list of digits in an integer
--
-- >>> 1234 ^? digitlist
-- Just [1,2,3,4]
-- 
-- >>> 0 ^? digitlist
-- Just []
-- 
-- >>> 1 ^? digitlist
-- Just [1]
-- 
-- >>> 90 ^? digitlist
-- Just [9,0]
-- 
-- >>> 05 ^? digitlist
-- Just [5]
-- 
-- >>> 105 ^? digitlist
-- Just [1,0,5]
-- 
-- >>> (-1) ^? digitlist
-- Nothing
--
-- λ> digitlist # [D0]
-- 0
--
-- >>> digitlist # [D0, D1]
-- 1
--
-- >>> digitlist # [D1]
-- 1
--
-- >>> digitlist # [D1, D2, D3]
-- 123
--
-- >>> digitlist # [D1, D0, D3]
-- 103
--
-- >>> digitlist # [D1, D0, D3, D0]
-- 1030
digitlist ::
  Integral a =>
  Prism'
    a
    [Digit]
digitlist =
  prism'
    (foldl' (\a b -> a * 10 + digit # b) 0)
    (\i ->  if  i < 0
              then
                Nothing
              else 
                Just (reverse (unfoldr (\n -> 
                  let (x, r) = divMod10 n
                  in  if x == 0
                        then
                          if r == D0
                            then
                              Nothing
                            else
                              Just (r, 0)
                        else
                              Just (r, x)) i))
    )

digits ::
  Integral a =>
  Prism'
    a
    Digits
digits =
  digitlist . digitsI

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
mod10 ::
  Integral a =>
  a
  -> Digit
mod10 n =
  let r = n `mod` 10
  in fromMaybe (mod10 r) (r ^? digit)

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
  (Monad p, CharParsing p) =>
  p Digit
parsedigit =
  let p = asum ((\d -> d <$ char (digitC # d)) <$> [D0 .. D9])
  in p <?> "digit"

parsedigitlist ::
  (Monad p, CharParsing p) =>
  p [Digit]
parsedigitlist =
  many parsedigit

parsedigits ::
  (Monad p, CharParsing p) =>
  p Digits
parsedigits = 
  Digits <$> parsedigitlist

parsedigitlist1 ::
  (Monad p, CharParsing p) =>
  p (NonEmpty Digit)
parsedigitlist1 =
  some1 parsedigit

skipdigitlist ::
  (Monad p, CharParsing p) =>
  p ()
skipdigitlist =
  skipMany parsedigit

skipdigitlist1 ::
  (Monad p, CharParsing p) =>
  p ()
skipdigitlist1 =
  skipSome parsedigit

parsenotdigit ::
  (Monad p, CharParsing p) =>
  p Char
parsenotdigit =
  let p = satisfy (`notElem` ['0' .. '9'])
  in p <?> "not digit"

parsenotdigits ::
  (Monad p, CharParsing p) =>
  p String
parsenotdigits =
  many parsenotdigit

parsenotdigits1 ::
  (Monad p, CharParsing p) =>
  p String
parsenotdigits1 =
  some parsenotdigit

skipnotdigits ::
  (Monad p, CharParsing p) =>
  p ()
skipnotdigits =
  skipMany parsenotdigit

skipnotdigits1 ::
  (Monad p, CharParsing p) =>
  p ()
skipnotdigits1 =
  skipSome parsenotdigit

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

newtype Digits =
  Digits [Digit]
  deriving (Eq, Ord, Show, Data, Typeable)

digitsI ::
  Iso'
    [Digit]
    Digits
digitsI =
  iso
    Digits
    (\(Digits x) -> x)

digitsS ::
  Prism'
    String
    Digits
digitsS =
  prism'
    (\(Digits d) -> (digitC #) <$> d)
    (\s -> Digits <$> traverse (^? digitC) s)

instance Cons Digits Digits Digit Digit where
  _Cons =
    prism'
      (\(h, Digits t) -> Digits (h:t))
      (\(Digits d) -> case d of 
                        [] ->
                          Nothing
                        (h:t) ->
                          Just (h, Digits t))


instance Snoc Digits Digits Digit Digit where
  _Snoc =
    prism'
      (\(Digits t, z) -> Digits (t ++ [z]))
      (\(Digits d) -> (\(a, b) -> (Digits a, b)) <$> d ^? _Snoc)

instance AsEmpty Digits where
  _Empty =
    prism'
      (\() -> Digits [])
      (\(Digits d) -> case d of
                        [] ->
                          Just ()
                        (_:_) ->
                          Nothing)

instance Each Digits Digits Digit Digit where
  each f (Digits d) =
    Digits <$> each f d

type instance IxValue Digits = Digit
type instance Index Digits = Int
instance Ixed Digits where
  ix i f (Digits d) =
    Digits <$> ix i f d

instance Plated Digits where
  plate f (Digits d) =
    Digits <$> plate (\x -> (\(Digits e) -> e) <$> f (Digits x)) d

instance Reversing Digits where
  reversing (Digits d) =
    Digits (reversing d)

instance Semigroup Digits where
  Digits d <> Digits e =
    Digits (d <> e)

instance Monoid Digits where
  mempty =
    Digits mempty
  mappend =
    (<>)

(/+/) ::
  Digit
  -> Digit
  -> (Digit, Digit)
a /+/ b =
  let (x, r) = divMod10 (digit # a + digit # b)
  in (mod10 (x :: Integer), r) 

(.+.) ::
  Digits
  -> Digits
  -> Digits
Digits d .+. Digits e =
  Digits (fromMaybe [] ((digitlist # d + (digitlist # e :: Integer)) ^? digitlist))

(.*.) ::
  Digits
  -> Digits
  -> Digits
Digits d .*. Digits e =
  Digits (fromMaybe [] ((digitlist # d * (digitlist # e :: Integer)) ^? digitlist))

mantissa ::
  Floating a =>
  Digits
  -> a
mantissa d =
  let acc a (e, x) = 
        a + fromIntegral (digit # x :: Int) * 10 ** fromIntegral (negate e :: Int)
  in foldl' acc 0 (zip [1..] (digitsI # d))
