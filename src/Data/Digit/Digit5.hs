{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit5 where

import Papa
import Data.Digit.D5(D5(d5))
import Prelude(Bounded, RealFrac)

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

