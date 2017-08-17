{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DigitB(
  DigitB(..)
) where

import Papa
import Data.Digit.DB(DB(dB))
import Prelude(Bounded, RealFrac)

newtype DigitB a =
  DigitB a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DB a => DB (DigitB a) where
  dB  =
    _Wrapped . dB

instance Functor DigitB where
  fmap f (DigitB a) =
    DigitB (f a)
    
instance Apply DigitB where
  DigitB f <.> DigitB a =
    DigitB (f a)

instance Applicative DigitB where
  pure =
    DigitB
  (<*>) =
    (<.>)

instance Bind DigitB where
  DigitB a >>- f =
    f a

instance Monad DigitB where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitB where
  foldMap f (DigitB a) = 
    f a

instance Foldable1 DigitB where
  foldMap1 f (DigitB a) = 
    f a

instance Traversable DigitB where
  traverse f (DigitB a) = 
    DigitB <$> f a

instance Traversable1 DigitB where
  traverse1 f (DigitB a) = 
    DigitB <$> f a

instance Semigroup a => Semigroup (DigitB a) where
  DigitB x <> DigitB y =
    DigitB (x <> y)

instance Monoid a => Monoid (DigitB a) where
  DigitB x `mappend` DigitB y =
    DigitB (x `mappend` y)
  mempty =
    DigitB mempty

instance Field1 (DigitB a) (DigitB b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitB where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitB where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitB where
  itraverse f =
    traverse (f ())

instance Each (DigitB a) (DigitB b) a b where
  each =
    traverse

type instance Index (DigitB a) = 
  ()
type instance IxValue (DigitB a) =
  a
instance Ixed (DigitB a) where
  ix () f (DigitB a) =
    DigitB <$> f a

makeWrapped ''DigitB
