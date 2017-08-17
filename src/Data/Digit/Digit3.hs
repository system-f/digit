{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit3(
  Digit3(..)
) where

import Papa
import Data.Digit.D3(D3(d3))
import Prelude(Bounded, RealFrac)

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
