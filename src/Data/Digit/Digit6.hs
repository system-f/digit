{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit6 where

import Papa
import Data.Digit.D6(D6(d6))
import Prelude(Bounded, RealFrac)

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
