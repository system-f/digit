{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit2(
  DecimalNoZero
, Decimal
, OctalNoZero
, Octal
, HeXaDeCiMaLNoZero
, HeXaDeCiMaL
, HEXADECIMALNoZero
, HEXADECIMAL
, HexadecimalNoZero
, Hexadecimal
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
, DA(..)
, DB(..)
, DC(..)
, DD(..)
, DE(..)
, DF(..)
, Da(..)
, Db(..)
, Dc(..)
, Dd(..)
, De(..)
, Df(..)
, parse0
, parse1
, parse2
, parse3
, parse4
, parse5
, parse6
, parse7
, parse8
, parse9
, parseA
, parseB
, parseC
, parseD
, parseE
, parseF
, parsea
, parseb
, parsec
, parsed
, parsee
, parsef
, parseAa
, parseBb
, parseCc
, parseDd
, parseEe
, parseFf
, parseBinaryNoZero
, parseBinary
, parseOctalNoZero
, parseOctal
, parseDecimalNoZero
, parseDecimal
, parseHEXADECIMALNoZero
, parseHEXADECIMAL
, parseHexadecimalNoZero
, parseHexadecimal
, parseHeXaDeCiMaLNoZero
, parseHeXaDeCiMaL
) where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

type BinaryNoZero d =
  D1 d

type Binary d =
  (D0 d, BinaryNoZero d)

type OctalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d)

type Octal d =
  (D0 d, OctalNoZero d)

type DecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d)

type Decimal d =
  (D0 d, DecimalNoZero d)

type HeXaDeCiMaLNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

type HeXaDeCiMaL d =
  (D0 d, HeXaDeCiMaLNoZero d)

type HEXADECIMALNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d)

type HEXADECIMAL d =
  (D0 d, HEXADECIMALNoZero d)

type HexadecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

type Hexadecimal d =
  (D0 d, HexadecimalNoZero d)

class D0 d where
  d0 ::
    Prism'
      d
      ()
  x0 ::
    d
  x0 =
    d0 # ()

instance D0 () where
  d0 =
    id

parse0 ::
  (D0 d, CharParsing p) =>
  p d
parse0 =
  x0 <$ char '0' <?> "0"

class D1 d where
  d1 ::
    Prism'
      d
      ()
  x1 ::
    d
  x1 =
    d1 # ()

instance D1 () where
  d1 =
    id

parse1 ::
  (D1 d, CharParsing p) =>
  p d
parse1 =
  x1 <$ char '1' <?> "1"

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
    
parse2 ::
  (D2 d, CharParsing p) =>
  p d
parse2 =
  x2 <$ char '2' <?> "2"

class D3 d where
  d3 ::
    Prism'
      d
      ()
  x3 ::
    d
  x3 =
    d3 # ()

instance D3 () where
  d3 =
    id
    
parse3 ::
  (D3 d, CharParsing p) =>
  p d
parse3 =
  x3 <$ char '3' <?> "3"

class D4 d where
  d4 ::
    Prism'
      d
      ()
  x4 ::
    d
  x4 =
    d4 # ()

instance D4 () where
  d4 =
    id
    
parse4 ::
  (D4 d, CharParsing p) =>
  p d
parse4 =
  x4 <$ char '4' <?> "4"

class D5 d where
  d5 ::
    Prism'
      d
      ()
  x5 ::
    d
  x5 =
    d5 # ()

instance D5 () where
  d5 =
    id
    
parse5 ::
  (D5 d, CharParsing p) =>
  p d
parse5 =
  x5 <$ char '5' <?> "5"

class D6 d where
  d6 ::
    Prism'
      d
      ()
  x6 ::
    d
  x6 =
    d6 # ()

instance D6 () where
  d6 =
    id
    
parse6 ::
  (D6 d, CharParsing p) =>
  p d
parse6 =
  x6 <$ char '6' <?> "6"

class D7 d where
  d7 ::
    Prism'
      d
      ()
  x7 ::
    d
  x7 =
    d7 # ()

instance D7 () where
  d7 =
    id
    
parse7 ::
  (D7 d, CharParsing p) =>
  p d
parse7 =
  x7 <$ char '7' <?> "7"

class D8 d where
  d8 ::
    Prism'
      d
      ()
  x8 ::
    d
  x8 =
    d8 # ()

instance D8 () where
  d8 =
    id
    
parse8 ::
  (D8 d, CharParsing p) =>
  p d
parse8 =
  x8 <$ char '8' <?> "8"

class D9 d where
  d9 ::
    Prism'
      d
      ()
  x9 ::
    d
  x9 =
    d9 # ()

instance D9 () where
  d9 =
    id
    
parse9 ::
  (D9 d, CharParsing p) =>
  p d
parse9 =
  x9 <$ char '9' <?> "9"

class DA d where
  dA ::
    Prism'
      d
      ()
  xA ::
    d
  xA =
    dA # ()

instance DA () where
  dA =
    id
    
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"

class DB d where
  dB ::
    Prism'
      d
      ()
  xB ::
    d
  xB =
    dB # ()

instance DB () where
  dB =
    id
    
parseB ::
  (DB d, CharParsing p) =>
  p d
parseB =
  xB <$ char 'B' <?> "B"

class DC d where
  dC ::
    Prism'
      d
      ()
  xC ::
    d
  xC =
    dC # ()

instance DC () where
  dC =
    id
    
parseC ::
  (DC d, CharParsing p) =>
  p d
parseC =
  xC <$ char 'C' <?> "C"

class DD d where
  dD ::
    Prism'
      d
      ()
  xD ::
    d
  xD =
    dD # ()

instance DD () where
  dD =
    id
    
parseD ::
  (DD d, CharParsing p) =>
  p d
parseD =
  xD <$ char 'D' <?> "D"

class DE d where
  dE ::
    Prism'
      d
      ()
  xE ::
    d
  xE =
    dE # ()

instance DE () where
  dE =
    id
    
parseE ::
  (DE d, CharParsing p) =>
  p d
parseE =
  xE <$ char 'E' <?> "E"

class DF d where
  dF ::
    Prism'
      d
      ()
  xF ::
    d
  xF =
    dF # ()

instance DF () where
  dF =
    id
    
parseF ::
  (DF d, CharParsing p) =>
  p d
parseF =
  xF <$ char 'F' <?> "F"

class Da d where
  da ::
    Prism'
      d
      ()
  xa ::
    d
  xa =
    da # ()

instance Da () where
  da =
    id
    
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"

class Db d where
  db ::
    Prism'
      d
      ()
  xb ::
    d
  xb =
    db # ()

instance Db () where
  db =
    id
    
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"

class Dc d where
  dc ::
    Prism'
      d
      ()
  xc ::
    d
  xc =
    dc # ()

instance Dc () where
  dc =
    id
    
parsec ::
  (Dc d, CharParsing p) =>
  p d
parsec =
  xc <$ char 'c' <?> "c"

class Dd d where
  dd ::
    Prism'
      d
      ()
  xd ::
    d
  xd =
    dd # ()

instance Dd () where
  dd =
    id
    
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"

class De d where
  de ::
    Prism'
      d
      ()
  xe ::
    d
  xe =
    de # ()

instance De () where
  de =
    id
    
parsee ::
  (De d, CharParsing p) =>
  p d
parsee =
  xe <$ char 'e' <?> "e"

class Df d where
  df ::
    Prism'
      d
      ()
  xf ::
    d
  xf =
    df # ()

instance Df () where
  df =
    id
    
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"

parseAa ::
  (DA d, Da d, CharParsing p) =>
  p d
parseAa =
  choice [parseA, parsea]

parseBb ::
  (DB d, Db d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb]

parseCc ::
  (DC d, Dc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec]

parseDd ::
  (DD d, Dd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed]

parseEe ::
  (DE d, De d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee]

parseFf ::
  (DF d, Df d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef]


----

parseBinaryNoZero ::
  (BinaryNoZero d, CharParsing p) =>
  p d
parseBinaryNoZero =
  parse1 <?> "BinaryNoZero"

parseBinary ::
  (Binary d, CharParsing p) =>
  p d
parseBinary =
  choice
    [
      parse0
    , parseBinaryNoZero
    ] <?> "Binary"

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

parseOctal ::
  (Octal d, CharParsing p) =>
  p d
parseOctal =
  choice
    [
      parse0
    , parseOctalNoZero
    ] <?> "Octal"

parseDecimalNoZero ::
  (DecimalNoZero d, CharParsing p) =>
  p d
parseDecimalNoZero =
  choice
    [
      parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    , parse8
    , parse9
    ] <?> "DecimalNoZero"

parseDecimal ::
  (Decimal d, CharParsing p) =>
  p d
parseDecimal =
  choice
    [
      parse0
    , parseDecimalNoZero
    ] <?> "Decimal"

parseHEXADECIMALNoZero ::
  (HEXADECIMALNoZero d, CharParsing p) =>
  p d
parseHEXADECIMALNoZero =
  choice
    [
      parseDecimalNoZero
    , parseA
    , parseB
    , parseC
    , parseD
    , parseE
    , parseF
    ] <?> "HEXADECIMALNoZero"

parseHEXADECIMAL ::
  (HEXADECIMAL d, CharParsing p) =>
  p d
parseHEXADECIMAL =
  choice
    [
      parse0
    , parseHEXADECIMALNoZero
    ] <?> "HEXADECIMAL"

parseHexadecimalNoZero ::
  (HexadecimalNoZero d, CharParsing p) =>
  p d
parseHexadecimalNoZero =
  choice
    [
      parseDecimalNoZero
    , parsea
    , parseb
    , parsec
    , parsed
    , parsee
    , parsef
    ] <?> "HexadecimalNoZero"

parseHexadecimal ::
  (Hexadecimal d, CharParsing p) =>
  p d
parseHexadecimal =
  choice
    [
      parse0
    , parseHexadecimalNoZero
    ] <?> "Hexadecimal"

parseHeXaDeCiMaLNoZero ::
  (HeXaDeCiMaLNoZero d, CharParsing p) =>
  p d
parseHeXaDeCiMaLNoZero =
  choice
    [
      parseDecimalNoZero
    , parseAa
    , parseBb
    , parseCc
    , parseDd
    , parseEe
    , parseFf
    ] <?> "HeXaDeCiMaLNoZero"

parseHeXaDeCiMaL ::
  (HeXaDeCiMaL d, CharParsing p) =>
  p d
parseHeXaDeCiMaL =
  choice
    [
      parse0
    , parseHeXaDeCiMaLNoZero
    ] <?> "HeXaDeCiMaL"

----

newtype Digit0 a =
  Digit0 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D0 a => D0 (Digit0 a) where
  d0 =
    _Wrapped . d0

instance Functor Digit0 where
  fmap f (Digit0 a) =
    Digit0 (f a)
    
instance Apply Digit0 where
  Digit0 f <.> Digit0 a =
    Digit0 (f a)

instance Applicative Digit0 where
  pure =
    Digit0
  (<*>) =
    (<.>)

instance Bind Digit0 where
  Digit0 a >>- f =
    f a

instance Monad Digit0 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit0 where
  foldMap f (Digit0 a) = 
    f a

instance Foldable1 Digit0 where
  foldMap1 f (Digit0 a) = 
    f a

instance Traversable Digit0 where
  traverse f (Digit0 a) = 
    Digit0 <$> f a

instance Traversable1 Digit0 where
  traverse1 f (Digit0 a) = 
    Digit0 <$> f a

instance Semigroup a => Semigroup (Digit0 a) where
  Digit0 x <> Digit0 y =
    Digit0 (x <> y)

instance Monoid a => Monoid (Digit0 a) where
  Digit0 x `mappend` Digit0 y =
    Digit0 (x `mappend` y)
  mempty =
    Digit0 mempty

instance Field1 (Digit0 a) (Digit0 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit0 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit0 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit0 where
  itraverse f =
    traverse (f ())

instance Each (Digit0 a) (Digit0 b) a b where
  each =
    traverse

type instance Index (Digit0 a) = 
  ()
type instance IxValue (Digit0 a) =
  a
instance Ixed (Digit0 a) where
  ix () f (Digit0 a) =
    Digit0 <$> f a

makeWrapped ''Digit0


newtype Digit1 a =
  Digit1 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D1 a => D1 (Digit1 a) where
  d1 =
    _Wrapped . d1

instance Functor Digit1 where
  fmap f (Digit1 a) =
    Digit1 (f a)
    
instance Apply Digit1 where
  Digit1 f <.> Digit1 a =
    Digit1 (f a)

instance Applicative Digit1 where
  pure =
    Digit1
  (<*>) =
    (<.>)

instance Bind Digit1 where
  Digit1 a >>- f =
    f a

instance Monad Digit1 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit1 where
  foldMap f (Digit1 a) = 
    f a

instance Foldable1 Digit1 where
  foldMap1 f (Digit1 a) = 
    f a

instance Traversable Digit1 where
  traverse f (Digit1 a) = 
    Digit1 <$> f a

instance Traversable1 Digit1 where
  traverse1 f (Digit1 a) = 
    Digit1 <$> f a

instance Semigroup a => Semigroup (Digit1 a) where
  Digit1 x <> Digit1 y =
    Digit1 (x <> y)

instance Monoid a => Monoid (Digit1 a) where
  Digit1 x `mappend` Digit1 y =
    Digit1 (x `mappend` y)
  mempty =
    Digit1 mempty

instance Field1 (Digit1 a) (Digit1 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit1 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit1 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit1 where
  itraverse f =
    traverse (f ())

instance Each (Digit1 a) (Digit1 b) a b where
  each =
    traverse

type instance Index (Digit1 a) = 
  ()
type instance IxValue (Digit1 a) =
  a
instance Ixed (Digit1 a) where
  ix () f (Digit1 a) =
    Digit1 <$> f a

makeWrapped ''Digit1

-- todo: All other digits

instance D0 d => D0 (Either d x) where
  d0 =
    _Left . d0
    
instance D1 d => D1 (Either d x) where
  d1 =
    _Left . d1

instance D2 d => D2 (Either d x) where
  d2 =
    _Left . d2

instance D3 d => D3 (Either d x) where
  d3 =
    _Left . d3

instance D4 d => D4 (Either d x) where
  d4 =
    _Left . d4

instance D5 d => D5 (Either d x) where
  d5 =
    _Left . d5

instance D6 d => D6 (Either d x) where
  d6 =
    _Left . d6

instance D7 d => D7 (Either d x) where
  d7 =
    _Left . d7

instance D8 d => D8 (Either d x) where
  d8 =
    _Left . d8

instance D9 d => D9 (Either d x) where
  d9 =
    _Left . d9

instance Da d => Da (Either d x) where
  da =
    _Left . da

instance Db d => Db (Either d x) where
  db =
    _Left . db

instance Dc d => Dc (Either d x) where
  dc =
    _Left . dc

instance Dd d => Dd (Either d x) where
  dd =
    _Left . dd

instance De d => De (Either d x) where
  de =
    _Left . de

instance Df d => Df (Either d x) where
  df =
    _Left . df

instance DA d => DA (Either d x) where
  dA =
    _Left . dA

instance DB d => DB (Either d x) where
  dB =
    _Left . dB

instance DC d => DC (Either d x) where
  dC =
    _Left . dC

instance DD d => DD (Either d x) where
  dD =
    _Left . dD

instance DE d => DE (Either d x) where
  dE =
    _Left . dE

instance DF d => DF (Either d x) where
  dF =
    _Left . dF
