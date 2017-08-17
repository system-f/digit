{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit8 where

import Control.Lens hiding ((<.>))
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Digit.D8

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
