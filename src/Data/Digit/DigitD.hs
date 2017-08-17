{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DigitD where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Digit.DD

newtype DigitD a =
  DigitD a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DD a => DD (DigitD a) where
  dD  =
    _Wrapped . dD

instance Functor DigitD where
  fmap f (DigitD a) =
    DigitD (f a)
    
instance Apply DigitD where
  DigitD f <.> DigitD a =
    DigitD (f a)

instance Applicative DigitD where
  pure =
    DigitD
  (<*>) =
    (<.>)

instance Bind DigitD where
  DigitD a >>- f =
    f a

instance Monad DigitD where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitD where
  foldMap f (DigitD a) = 
    f a

instance Foldable1 DigitD where
  foldMap1 f (DigitD a) = 
    f a

instance Traversable DigitD where
  traverse f (DigitD a) = 
    DigitD <$> f a

instance Traversable1 DigitD where
  traverse1 f (DigitD a) = 
    DigitD <$> f a

instance Semigroup a => Semigroup (DigitD a) where
  DigitD x <> DigitD y =
    DigitD (x <> y)

instance Monoid a => Monoid (DigitD a) where
  DigitD x `mappend` DigitD y =
    DigitD (x `mappend` y)
  mempty =
    DigitD mempty

instance Field1 (DigitD a) (DigitD b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitD where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitD where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitD where
  itraverse f =
    traverse (f ())

instance Each (DigitD a) (DigitD b) a b where
  each =
    traverse

type instance Index (DigitD a) = 
  ()
type instance IxValue (DigitD a) =
  a
instance Ixed (DigitD a) where
  ix () f (DigitD a) =
    DigitD <$> f a

makeWrapped ''DigitD
