{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit9 where

import Papa
import Data.Digit.D9(D9(d9))
import Prelude(Bounded, RealFrac)

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
