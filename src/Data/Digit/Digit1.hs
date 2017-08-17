{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit1 where

import Papa
import Data.Digit.D1
import Prelude(Bounded, RealFrac)

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
