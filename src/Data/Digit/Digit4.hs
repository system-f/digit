{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit4 where

import Papa
import Data.Digit.D4
import Prelude(Bounded, RealFrac)

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
