{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit7 where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Digit.D7

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
