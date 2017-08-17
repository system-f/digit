{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit0(
  Digit0(..)
) where

import Papa
import Data.Digit.D0(D0(d0))
import Prelude(Bounded, RealFrac)

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
