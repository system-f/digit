{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit2(
  BinaryNoZero
, Binary
, DecimalNoZero
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
, Digit0(..)
, Digit1(..)
, Digit2(..)
, Digit3(..)
, Digit4(..)
, Digit5(..)
, Digit6(..)
, Digit7(..)
, Digit8(..)
, Digit9(..)
, Digita(..)
, Digitb(..)
, Digitc(..)
, Digitd(..)
, Digite(..)
, Digitf(..)
, DigitA(..)
, DigitB(..)
, DigitC(..)
, DigitD(..)
, DigitE(..)
, DigitF(..)
) where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

-- $setup
-- >>> import Text.Parsec

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

newtype Digit2 a =
  Digit2 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D2 a => D2 (Digit2 a) where
  d2 =
    _Wrapped . d2

instance Functor Digit2 where
  fmap f (Digit2 a) =
    Digit2 (f a)
    
instance Apply Digit2 where
  Digit2 f <.> Digit2 a =
    Digit2 (f a)

instance Applicative Digit2 where
  pure =
    Digit2
  (<*>) =
    (<.>)

instance Bind Digit2 where
  Digit2 a >>- f =
    f a

instance Monad Digit2 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit2 where
  foldMap f (Digit2 a) = 
    f a

instance Foldable1 Digit2 where
  foldMap1 f (Digit2 a) = 
    f a

instance Traversable Digit2 where
  traverse f (Digit2 a) = 
    Digit2 <$> f a

instance Traversable1 Digit2 where
  traverse1 f (Digit2 a) = 
    Digit2 <$> f a

instance Semigroup a => Semigroup (Digit2 a) where
  Digit2 x <> Digit2 y =
    Digit2 (x <> y)

instance Monoid a => Monoid (Digit2 a) where
  Digit2 x `mappend` Digit2 y =
    Digit2 (x `mappend` y)
  mempty =
    Digit2 mempty

instance Field1 (Digit2 a) (Digit2 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit2 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit2 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit2 where
  itraverse f =
    traverse (f ())

instance Each (Digit2 a) (Digit2 b) a b where
  each =
    traverse

type instance Index (Digit2 a) = 
  ()
type instance IxValue (Digit2 a) =
  a
instance Ixed (Digit2 a) where
  ix () f (Digit2 a) =
    Digit2 <$> f a

makeWrapped ''Digit2

newtype Digit3 a =
  Digit3 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D3 a => D3 (Digit3 a) where
  d3 =
    _Wrapped . d3

instance Functor Digit3 where
  fmap f (Digit3 a) =
    Digit3 (f a)
    
instance Apply Digit3 where
  Digit3 f <.> Digit3 a =
    Digit3 (f a)

instance Applicative Digit3 where
  pure =
    Digit3
  (<*>) =
    (<.>)

instance Bind Digit3 where
  Digit3 a >>- f =
    f a

instance Monad Digit3 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit3 where
  foldMap f (Digit3 a) = 
    f a

instance Foldable1 Digit3 where
  foldMap1 f (Digit3 a) = 
    f a

instance Traversable Digit3 where
  traverse f (Digit3 a) = 
    Digit3 <$> f a

instance Traversable1 Digit3 where
  traverse1 f (Digit3 a) = 
    Digit3 <$> f a

instance Semigroup a => Semigroup (Digit3 a) where
  Digit3 x <> Digit3 y =
    Digit3 (x <> y)

instance Monoid a => Monoid (Digit3 a) where
  Digit3 x `mappend` Digit3 y =
    Digit3 (x `mappend` y)
  mempty =
    Digit3 mempty

instance Field1 (Digit3 a) (Digit3 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit3 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit3 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit3 where
  itraverse f =
    traverse (f ())

instance Each (Digit3 a) (Digit3 b) a b where
  each =
    traverse

type instance Index (Digit3 a) = 
  ()
type instance IxValue (Digit3 a) =
  a
instance Ixed (Digit3 a) where
  ix () f (Digit3 a) =
    Digit3 <$> f a

makeWrapped ''Digit3

newtype Digit4 a =
  Digit4 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D4 a => D4 (Digit4 a) where
  d4 =
    _Wrapped . d4

instance Functor Digit4 where
  fmap f (Digit4 a) =
    Digit4 (f a)
    
instance Apply Digit4 where
  Digit4 f <.> Digit4 a =
    Digit4 (f a)

instance Applicative Digit4 where
  pure =
    Digit4
  (<*>) =
    (<.>)

instance Bind Digit4 where
  Digit4 a >>- f =
    f a

instance Monad Digit4 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit4 where
  foldMap f (Digit4 a) = 
    f a

instance Foldable1 Digit4 where
  foldMap1 f (Digit4 a) = 
    f a

instance Traversable Digit4 where
  traverse f (Digit4 a) = 
    Digit4 <$> f a

instance Traversable1 Digit4 where
  traverse1 f (Digit4 a) = 
    Digit4 <$> f a

instance Semigroup a => Semigroup (Digit4 a) where
  Digit4 x <> Digit4 y =
    Digit4 (x <> y)

instance Monoid a => Monoid (Digit4 a) where
  Digit4 x `mappend` Digit4 y =
    Digit4 (x `mappend` y)
  mempty =
    Digit4 mempty

instance Field1 (Digit4 a) (Digit4 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit4 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit4 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit4 where
  itraverse f =
    traverse (f ())

instance Each (Digit4 a) (Digit4 b) a b where
  each =
    traverse

type instance Index (Digit4 a) = 
  ()
type instance IxValue (Digit4 a) =
  a
instance Ixed (Digit4 a) where
  ix () f (Digit4 a) =
    Digit4 <$> f a

makeWrapped ''Digit4

newtype Digit5 a =
  Digit5 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D5 a => D5 (Digit5 a) where
  d5 =
    _Wrapped . d5

instance Functor Digit5 where
  fmap f (Digit5 a) =
    Digit5 (f a)
    
instance Apply Digit5 where
  Digit5 f <.> Digit5 a =
    Digit5 (f a)

instance Applicative Digit5 where
  pure =
    Digit5
  (<*>) =
    (<.>)

instance Bind Digit5 where
  Digit5 a >>- f =
    f a

instance Monad Digit5 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit5 where
  foldMap f (Digit5 a) = 
    f a

instance Foldable1 Digit5 where
  foldMap1 f (Digit5 a) = 
    f a

instance Traversable Digit5 where
  traverse f (Digit5 a) = 
    Digit5 <$> f a

instance Traversable1 Digit5 where
  traverse1 f (Digit5 a) = 
    Digit5 <$> f a

instance Semigroup a => Semigroup (Digit5 a) where
  Digit5 x <> Digit5 y =
    Digit5 (x <> y)

instance Monoid a => Monoid (Digit5 a) where
  Digit5 x `mappend` Digit5 y =
    Digit5 (x `mappend` y)
  mempty =
    Digit5 mempty

instance Field1 (Digit5 a) (Digit5 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit5 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit5 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit5 where
  itraverse f =
    traverse (f ())

instance Each (Digit5 a) (Digit5 b) a b where
  each =
    traverse

type instance Index (Digit5 a) = 
  ()
type instance IxValue (Digit5 a) =
  a
instance Ixed (Digit5 a) where
  ix () f (Digit5 a) =
    Digit5 <$> f a

makeWrapped ''Digit5

newtype Digit6 a =
  Digit6 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D6 a => D6 (Digit6 a) where
  d6  =
    _Wrapped . d6

instance Functor Digit6 where
  fmap f (Digit6 a) =
    Digit6 (f a)
    
instance Apply Digit6 where
  Digit6 f <.> Digit6 a =
    Digit6 (f a)

instance Applicative Digit6 where
  pure =
    Digit6
  (<*>) =
    (<.>)

instance Bind Digit6 where
  Digit6 a >>- f =
    f a

instance Monad Digit6 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit6 where
  foldMap f (Digit6 a) = 
    f a

instance Foldable1 Digit6 where
  foldMap1 f (Digit6 a) = 
    f a

instance Traversable Digit6 where
  traverse f (Digit6 a) = 
    Digit6 <$> f a

instance Traversable1 Digit6 where
  traverse1 f (Digit6 a) = 
    Digit6 <$> f a

instance Semigroup a => Semigroup (Digit6 a) where
  Digit6 x <> Digit6 y =
    Digit6 (x <> y)

instance Monoid a => Monoid (Digit6 a) where
  Digit6 x `mappend` Digit6 y =
    Digit6 (x `mappend` y)
  mempty =
    Digit6 mempty

instance Field1 (Digit6 a) (Digit6 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit6 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit6 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit6 where
  itraverse f =
    traverse (f ())

instance Each (Digit6 a) (Digit6 b) a b where
  each =
    traverse

type instance Index (Digit6 a) = 
  ()
type instance IxValue (Digit6 a) =
  a
instance Ixed (Digit6 a) where
  ix () f (Digit6 a) =
    Digit6 <$> f a

makeWrapped ''Digit6

newtype Digit7 a =
  Digit7 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D7 a => D7 (Digit7 a) where
  d7  =
    _Wrapped . d7

instance Functor Digit7 where
  fmap f (Digit7 a) =
    Digit7 (f a)
    
instance Apply Digit7 where
  Digit7 f <.> Digit7 a =
    Digit7 (f a)

instance Applicative Digit7 where
  pure =
    Digit7
  (<*>) =
    (<.>)

instance Bind Digit7 where
  Digit7 a >>- f =
    f a

instance Monad Digit7 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit7 where
  foldMap f (Digit7 a) = 
    f a

instance Foldable1 Digit7 where
  foldMap1 f (Digit7 a) = 
    f a

instance Traversable Digit7 where
  traverse f (Digit7 a) = 
    Digit7 <$> f a

instance Traversable1 Digit7 where
  traverse1 f (Digit7 a) = 
    Digit7 <$> f a

instance Semigroup a => Semigroup (Digit7 a) where
  Digit7 x <> Digit7 y =
    Digit7 (x <> y)

instance Monoid a => Monoid (Digit7 a) where
  Digit7 x `mappend` Digit7 y =
    Digit7 (x `mappend` y)
  mempty =
    Digit7 mempty

instance Field1 (Digit7 a) (Digit7 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit7 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit7 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit7 where
  itraverse f =
    traverse (f ())

instance Each (Digit7 a) (Digit7 b) a b where
  each =
    traverse

type instance Index (Digit7 a) = 
  ()
type instance IxValue (Digit7 a) =
  a
instance Ixed (Digit7 a) where
  ix () f (Digit7 a) =
    Digit7 <$> f a

makeWrapped ''Digit7

newtype Digit8 a =
  Digit8 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D8 a => D8 (Digit8 a) where
  d8  =
    _Wrapped . d8

instance Functor Digit8 where
  fmap f (Digit8 a) =
    Digit8 (f a)
    
instance Apply Digit8 where
  Digit8 f <.> Digit8 a =
    Digit8 (f a)

instance Applicative Digit8 where
  pure =
    Digit8
  (<*>) =
    (<.>)

instance Bind Digit8 where
  Digit8 a >>- f =
    f a

instance Monad Digit8 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit8 where
  foldMap f (Digit8 a) = 
    f a

instance Foldable1 Digit8 where
  foldMap1 f (Digit8 a) = 
    f a

instance Traversable Digit8 where
  traverse f (Digit8 a) = 
    Digit8 <$> f a

instance Traversable1 Digit8 where
  traverse1 f (Digit8 a) = 
    Digit8 <$> f a

instance Semigroup a => Semigroup (Digit8 a) where
  Digit8 x <> Digit8 y =
    Digit8 (x <> y)

instance Monoid a => Monoid (Digit8 a) where
  Digit8 x `mappend` Digit8 y =
    Digit8 (x `mappend` y)
  mempty =
    Digit8 mempty

instance Field1 (Digit8 a) (Digit8 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit8 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit8 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit8 where
  itraverse f =
    traverse (f ())

instance Each (Digit8 a) (Digit8 b) a b where
  each =
    traverse

type instance Index (Digit8 a) = 
  ()
type instance IxValue (Digit8 a) =
  a
instance Ixed (Digit8 a) where
  ix () f (Digit8 a) =
    Digit8 <$> f a

makeWrapped ''Digit8

newtype Digit9 a =
  Digit9 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D9 a => D9 (Digit9 a) where
  d9  =
    _Wrapped . d9

instance Functor Digit9 where
  fmap f (Digit9 a) =
    Digit9 (f a)
    
instance Apply Digit9 where
  Digit9 f <.> Digit9 a =
    Digit9 (f a)

instance Applicative Digit9 where
  pure =
    Digit9
  (<*>) =
    (<.>)

instance Bind Digit9 where
  Digit9 a >>- f =
    f a

instance Monad Digit9 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit9 where
  foldMap f (Digit9 a) = 
    f a

instance Foldable1 Digit9 where
  foldMap1 f (Digit9 a) = 
    f a

instance Traversable Digit9 where
  traverse f (Digit9 a) = 
    Digit9 <$> f a

instance Traversable1 Digit9 where
  traverse1 f (Digit9 a) = 
    Digit9 <$> f a

instance Semigroup a => Semigroup (Digit9 a) where
  Digit9 x <> Digit9 y =
    Digit9 (x <> y)

instance Monoid a => Monoid (Digit9 a) where
  Digit9 x `mappend` Digit9 y =
    Digit9 (x `mappend` y)
  mempty =
    Digit9 mempty

instance Field1 (Digit9 a) (Digit9 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit9 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit9 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit9 where
  itraverse f =
    traverse (f ())

instance Each (Digit9 a) (Digit9 b) a b where
  each =
    traverse

type instance Index (Digit9 a) = 
  ()
type instance IxValue (Digit9 a) =
  a
instance Ixed (Digit9 a) where
  ix () f (Digit9 a) =
    Digit9 <$> f a

makeWrapped ''Digit9

newtype Digita a =
  Digita a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Da a => Da (Digita a) where
  da  =
    _Wrapped . da

instance Functor Digita where
  fmap f (Digita a) =
    Digita (f a)
    
instance Apply Digita where
  Digita f <.> Digita a =
    Digita (f a)

instance Applicative Digita where
  pure =
    Digita
  (<*>) =
    (<.>)

instance Bind Digita where
  Digita a >>- f =
    f a

instance Monad Digita where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digita where
  foldMap f (Digita a) = 
    f a

instance Foldable1 Digita where
  foldMap1 f (Digita a) = 
    f a

instance Traversable Digita where
  traverse f (Digita a) = 
    Digita <$> f a

instance Traversable1 Digita where
  traverse1 f (Digita a) = 
    Digita <$> f a

instance Semigroup a => Semigroup (Digita a) where
  Digita x <> Digita y =
    Digita (x <> y)

instance Monoid a => Monoid (Digita a) where
  Digita x `mappend` Digita y =
    Digita (x `mappend` y)
  mempty =
    Digita mempty

instance Field1 (Digita a) (Digita b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digita where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digita where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digita where
  itraverse f =
    traverse (f ())

instance Each (Digita a) (Digita b) a b where
  each =
    traverse

type instance Index (Digita a) = 
  ()
type instance IxValue (Digita a) =
  a
instance Ixed (Digita a) where
  ix () f (Digita a) =
    Digita <$> f a

makeWrapped ''Digita

newtype Digitb a =
  Digitb a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Db a => Db (Digitb a) where
  db  =
    _Wrapped . db

instance Functor Digitb where
  fmap f (Digitb a) =
    Digitb (f a)
    
instance Apply Digitb where
  Digitb f <.> Digitb a =
    Digitb (f a)

instance Applicative Digitb where
  pure =
    Digitb
  (<*>) =
    (<.>)

instance Bind Digitb where
  Digitb a >>- f =
    f a

instance Monad Digitb where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitb where
  foldMap f (Digitb a) = 
    f a

instance Foldable1 Digitb where
  foldMap1 f (Digitb a) = 
    f a

instance Traversable Digitb where
  traverse f (Digitb a) = 
    Digitb <$> f a

instance Traversable1 Digitb where
  traverse1 f (Digitb a) = 
    Digitb <$> f a

instance Semigroup a => Semigroup (Digitb a) where
  Digitb x <> Digitb y =
    Digitb (x <> y)

instance Monoid a => Monoid (Digitb a) where
  Digitb x `mappend` Digitb y =
    Digitb (x `mappend` y)
  mempty =
    Digitb mempty

instance Field1 (Digitb a) (Digitb b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitb where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitb where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitb where
  itraverse f =
    traverse (f ())

instance Each (Digitb a) (Digitb b) a b where
  each =
    traverse

type instance Index (Digitb a) = 
  ()
type instance IxValue (Digitb a) =
  a
instance Ixed (Digitb a) where
  ix () f (Digitb a) =
    Digitb <$> f a

makeWrapped ''Digitb

newtype Digitc a =
  Digitc a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Dc a => Dc (Digitc a) where
  dc  =
    _Wrapped . dc

instance Functor Digitc where
  fmap f (Digitc a) =
    Digitc (f a)
    
instance Apply Digitc where
  Digitc f <.> Digitc a =
    Digitc (f a)

instance Applicative Digitc where
  pure =
    Digitc
  (<*>) =
    (<.>)

instance Bind Digitc where
  Digitc a >>- f =
    f a

instance Monad Digitc where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitc where
  foldMap f (Digitc a) = 
    f a

instance Foldable1 Digitc where
  foldMap1 f (Digitc a) = 
    f a

instance Traversable Digitc where
  traverse f (Digitc a) = 
    Digitc <$> f a

instance Traversable1 Digitc where
  traverse1 f (Digitc a) = 
    Digitc <$> f a

instance Semigroup a => Semigroup (Digitc a) where
  Digitc x <> Digitc y =
    Digitc (x <> y)

instance Monoid a => Monoid (Digitc a) where
  Digitc x `mappend` Digitc y =
    Digitc (x `mappend` y)
  mempty =
    Digitc mempty

instance Field1 (Digitc a) (Digitc b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitc where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitc where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitc where
  itraverse f =
    traverse (f ())

instance Each (Digitc a) (Digitc b) a b where
  each =
    traverse

type instance Index (Digitc a) = 
  ()
type instance IxValue (Digitc a) =
  a
instance Ixed (Digitc a) where
  ix () f (Digitc a) =
    Digitc <$> f a

makeWrapped ''Digitc

newtype Digitd a =
  Digitd a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Dd a => Dd (Digitd a) where
  dd  =
    _Wrapped . dd

instance Functor Digitd where
  fmap f (Digitd a) =
    Digitd (f a)
    
instance Apply Digitd where
  Digitd f <.> Digitd a =
    Digitd (f a)

instance Applicative Digitd where
  pure =
    Digitd
  (<*>) =
    (<.>)

instance Bind Digitd where
  Digitd a >>- f =
    f a

instance Monad Digitd where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitd where
  foldMap f (Digitd a) = 
    f a

instance Foldable1 Digitd where
  foldMap1 f (Digitd a) = 
    f a

instance Traversable Digitd where
  traverse f (Digitd a) = 
    Digitd <$> f a

instance Traversable1 Digitd where
  traverse1 f (Digitd a) = 
    Digitd <$> f a

instance Semigroup a => Semigroup (Digitd a) where
  Digitd x <> Digitd y =
    Digitd (x <> y)

instance Monoid a => Monoid (Digitd a) where
  Digitd x `mappend` Digitd y =
    Digitd (x `mappend` y)
  mempty =
    Digitd mempty

instance Field1 (Digitd a) (Digitd b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitd where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitd where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitd where
  itraverse f =
    traverse (f ())

instance Each (Digitd a) (Digitd b) a b where
  each =
    traverse

type instance Index (Digitd a) = 
  ()
type instance IxValue (Digitd a) =
  a
instance Ixed (Digitd a) where
  ix () f (Digitd a) =
    Digitd <$> f a

makeWrapped ''Digitd

newtype Digite a =
  Digite a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance De a => De (Digite a) where
  de  =
    _Wrapped . de

instance Functor Digite where
  fmap f (Digite a) =
    Digite (f a)
    
instance Apply Digite where
  Digite f <.> Digite a =
    Digite (f a)

instance Applicative Digite where
  pure =
    Digite
  (<*>) =
    (<.>)

instance Bind Digite where
  Digite a >>- f =
    f a

instance Monad Digite where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digite where
  foldMap f (Digite a) = 
    f a

instance Foldable1 Digite where
  foldMap1 f (Digite a) = 
    f a

instance Traversable Digite where
  traverse f (Digite a) = 
    Digite <$> f a

instance Traversable1 Digite where
  traverse1 f (Digite a) = 
    Digite <$> f a

instance Semigroup a => Semigroup (Digite a) where
  Digite x <> Digite y =
    Digite (x <> y)

instance Monoid a => Monoid (Digite a) where
  Digite x `mappend` Digite y =
    Digite (x `mappend` y)
  mempty =
    Digite mempty

instance Field1 (Digite a) (Digite b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digite where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digite where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digite where
  itraverse f =
    traverse (f ())

instance Each (Digite a) (Digite b) a b where
  each =
    traverse

type instance Index (Digite a) = 
  ()
type instance IxValue (Digite a) =
  a
instance Ixed (Digite a) where
  ix () f (Digite a) =
    Digite <$> f a

makeWrapped ''Digite

newtype Digitf a =
  Digitf a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Df a => Df (Digitf a) where
  df  =
    _Wrapped . df

instance Functor Digitf where
  fmap f (Digitf a) =
    Digitf (f a)
    
instance Apply Digitf where
  Digitf f <.> Digitf a =
    Digitf (f a)

instance Applicative Digitf where
  pure =
    Digitf
  (<*>) =
    (<.>)

instance Bind Digitf where
  Digitf a >>- f =
    f a

instance Monad Digitf where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitf where
  foldMap f (Digitf a) = 
    f a

instance Foldable1 Digitf where
  foldMap1 f (Digitf a) = 
    f a

instance Traversable Digitf where
  traverse f (Digitf a) = 
    Digitf <$> f a

instance Traversable1 Digitf where
  traverse1 f (Digitf a) = 
    Digitf <$> f a

instance Semigroup a => Semigroup (Digitf a) where
  Digitf x <> Digitf y =
    Digitf (x <> y)

instance Monoid a => Monoid (Digitf a) where
  Digitf x `mappend` Digitf y =
    Digitf (x `mappend` y)
  mempty =
    Digitf mempty

instance Field1 (Digitf a) (Digitf b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitf where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitf where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitf where
  itraverse f =
    traverse (f ())

instance Each (Digitf a) (Digitf b) a b where
  each =
    traverse

type instance Index (Digitf a) = 
  ()
type instance IxValue (Digitf a) =
  a
instance Ixed (Digitf a) where
  ix () f (Digitf a) =
    Digitf <$> f a

makeWrapped ''Digitf

newtype DigitA a =
  DigitA a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DA a => DA (DigitA a) where
  dA  =
    _Wrapped . dA

instance Functor DigitA where
  fmap f (DigitA a) =
    DigitA (f a)
    
instance Apply DigitA where
  DigitA f <.> DigitA a =
    DigitA (f a)

instance Applicative DigitA where
  pure =
    DigitA
  (<*>) =
    (<.>)

instance Bind DigitA where
  DigitA a >>- f =
    f a

instance Monad DigitA where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitA where
  foldMap f (DigitA a) = 
    f a

instance Foldable1 DigitA where
  foldMap1 f (DigitA a) = 
    f a

instance Traversable DigitA where
  traverse f (DigitA a) = 
    DigitA <$> f a

instance Traversable1 DigitA where
  traverse1 f (DigitA a) = 
    DigitA <$> f a

instance Semigroup a => Semigroup (DigitA a) where
  DigitA x <> DigitA y =
    DigitA (x <> y)

instance Monoid a => Monoid (DigitA a) where
  DigitA x `mappend` DigitA y =
    DigitA (x `mappend` y)
  mempty =
    DigitA mempty

instance Field1 (DigitA a) (DigitA b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitA where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitA where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitA where
  itraverse f =
    traverse (f ())

instance Each (DigitA a) (DigitA b) a b where
  each =
    traverse

type instance Index (DigitA a) = 
  ()
type instance IxValue (DigitA a) =
  a
instance Ixed (DigitA a) where
  ix () f (DigitA a) =
    DigitA <$> f a

makeWrapped ''DigitA

newtype DigitB a =
  DigitB a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DB a => DB (DigitB a) where
  dB  =
    _Wrapped . dB

instance Functor DigitB where
  fmap f (DigitB a) =
    DigitB (f a)
    
instance Apply DigitB where
  DigitB f <.> DigitB a =
    DigitB (f a)

instance Applicative DigitB where
  pure =
    DigitB
  (<*>) =
    (<.>)

instance Bind DigitB where
  DigitB a >>- f =
    f a

instance Monad DigitB where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitB where
  foldMap f (DigitB a) = 
    f a

instance Foldable1 DigitB where
  foldMap1 f (DigitB a) = 
    f a

instance Traversable DigitB where
  traverse f (DigitB a) = 
    DigitB <$> f a

instance Traversable1 DigitB where
  traverse1 f (DigitB a) = 
    DigitB <$> f a

instance Semigroup a => Semigroup (DigitB a) where
  DigitB x <> DigitB y =
    DigitB (x <> y)

instance Monoid a => Monoid (DigitB a) where
  DigitB x `mappend` DigitB y =
    DigitB (x `mappend` y)
  mempty =
    DigitB mempty

instance Field1 (DigitB a) (DigitB b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitB where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitB where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitB where
  itraverse f =
    traverse (f ())

instance Each (DigitB a) (DigitB b) a b where
  each =
    traverse

type instance Index (DigitB a) = 
  ()
type instance IxValue (DigitB a) =
  a
instance Ixed (DigitB a) where
  ix () f (DigitB a) =
    DigitB <$> f a

makeWrapped ''DigitB

newtype DigitC a =
  DigitC a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DC a => DC (DigitC a) where
  dC  =
    _Wrapped . dC

instance Functor DigitC where
  fmap f (DigitC a) =
    DigitC (f a)
    
instance Apply DigitC where
  DigitC f <.> DigitC a =
    DigitC (f a)

instance Applicative DigitC where
  pure =
    DigitC
  (<*>) =
    (<.>)

instance Bind DigitC where
  DigitC a >>- f =
    f a

instance Monad DigitC where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitC where
  foldMap f (DigitC a) = 
    f a

instance Foldable1 DigitC where
  foldMap1 f (DigitC a) = 
    f a

instance Traversable DigitC where
  traverse f (DigitC a) = 
    DigitC <$> f a

instance Traversable1 DigitC where
  traverse1 f (DigitC a) = 
    DigitC <$> f a

instance Semigroup a => Semigroup (DigitC a) where
  DigitC x <> DigitC y =
    DigitC (x <> y)

instance Monoid a => Monoid (DigitC a) where
  DigitC x `mappend` DigitC y =
    DigitC (x `mappend` y)
  mempty =
    DigitC mempty

instance Field1 (DigitC a) (DigitC b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitC where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitC where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitC where
  itraverse f =
    traverse (f ())

instance Each (DigitC a) (DigitC b) a b where
  each =
    traverse

type instance Index (DigitC a) = 
  ()
type instance IxValue (DigitC a) =
  a
instance Ixed (DigitC a) where
  ix () f (DigitC a) =
    DigitC <$> f a

makeWrapped ''DigitC

newtype DigitD a =
  DigitD a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DD a => DD (DigitD a) where
  dD  =
    _Wrapped . dD

instance Functor DigitD where
  fmap f (DigitD a) =
    DigitD (f a)
    
instance Apply DigitD where
  DigitD f <.> DigitD a =
    DigitD (f a)

instance Applicative DigitD where
  pure =
    DigitD
  (<*>) =
    (<.>)

instance Bind DigitD where
  DigitD a >>- f =
    f a

instance Monad DigitD where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitD where
  foldMap f (DigitD a) = 
    f a

instance Foldable1 DigitD where
  foldMap1 f (DigitD a) = 
    f a

instance Traversable DigitD where
  traverse f (DigitD a) = 
    DigitD <$> f a

instance Traversable1 DigitD where
  traverse1 f (DigitD a) = 
    DigitD <$> f a

instance Semigroup a => Semigroup (DigitD a) where
  DigitD x <> DigitD y =
    DigitD (x <> y)

instance Monoid a => Monoid (DigitD a) where
  DigitD x `mappend` DigitD y =
    DigitD (x `mappend` y)
  mempty =
    DigitD mempty

instance Field1 (DigitD a) (DigitD b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitD where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitD where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitD where
  itraverse f =
    traverse (f ())

instance Each (DigitD a) (DigitD b) a b where
  each =
    traverse

type instance Index (DigitD a) = 
  ()
type instance IxValue (DigitD a) =
  a
instance Ixed (DigitD a) where
  ix () f (DigitD a) =
    DigitD <$> f a

makeWrapped ''DigitD

newtype DigitE a =
  DigitE a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DE a => DE (DigitE a) where
  dE  =
    _Wrapped . dE

instance Functor DigitE where
  fmap f (DigitE a) =
    DigitE (f a)
    
instance Apply DigitE where
  DigitE f <.> DigitE a =
    DigitE (f a)

instance Applicative DigitE where
  pure =
    DigitE
  (<*>) =
    (<.>)

instance Bind DigitE where
  DigitE a >>- f =
    f a

instance Monad DigitE where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitE where
  foldMap f (DigitE a) = 
    f a

instance Foldable1 DigitE where
  foldMap1 f (DigitE a) = 
    f a

instance Traversable DigitE where
  traverse f (DigitE a) = 
    DigitE <$> f a

instance Traversable1 DigitE where
  traverse1 f (DigitE a) = 
    DigitE <$> f a

instance Semigroup a => Semigroup (DigitE a) where
  DigitE x <> DigitE y =
    DigitE (x <> y)

instance Monoid a => Monoid (DigitE a) where
  DigitE x `mappend` DigitE y =
    DigitE (x `mappend` y)
  mempty =
    DigitE mempty

instance Field1 (DigitE a) (DigitE b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitE where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitE where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitE where
  itraverse f =
    traverse (f ())

instance Each (DigitE a) (DigitE b) a b where
  each =
    traverse

type instance Index (DigitE a) = 
  ()
type instance IxValue (DigitE a) =
  a
instance Ixed (DigitE a) where
  ix () f (DigitE a) =
    DigitE <$> f a

makeWrapped ''DigitE

newtype DigitF a =
  DigitF a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DF a => DF (DigitF a) where
  dF  =
    _Wrapped . dF

instance Functor DigitF where
  fmap f (DigitF a) =
    DigitF (f a)
    
instance Apply DigitF where
  DigitF f <.> DigitF a =
    DigitF (f a)

instance Applicative DigitF where
  pure =
    DigitF
  (<*>) =
    (<.>)

instance Bind DigitF where
  DigitF a >>- f =
    f a

instance Monad DigitF where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitF where
  foldMap f (DigitF a) = 
    f a

instance Foldable1 DigitF where
  foldMap1 f (DigitF a) = 
    f a

instance Traversable DigitF where
  traverse f (DigitF a) = 
    DigitF <$> f a

instance Traversable1 DigitF where
  traverse1 f (DigitF a) = 
    DigitF <$> f a

instance Semigroup a => Semigroup (DigitF a) where
  DigitF x <> DigitF y =
    DigitF (x <> y)

instance Monoid a => Monoid (DigitF a) where
  DigitF x `mappend` DigitF y =
    DigitF (x `mappend` y)
  mempty =
    DigitF mempty

instance Field1 (DigitF a) (DigitF b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitF where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitF where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitF where
  itraverse f =
    traverse (f ())

instance Each (DigitF a) (DigitF b) a b where
  each =
    traverse

type instance Index (DigitF a) = 
  ()
type instance IxValue (DigitF a) =
  a
instance Ixed (DigitF a) where
  ix () f (DigitF a) =
    DigitF <$> f a

makeWrapped ''DigitF

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
