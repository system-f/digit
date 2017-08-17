{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digitf where

import Papa
import Data.Digit.Df(Df(df))
import Prelude(Bounded, RealFrac)

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
