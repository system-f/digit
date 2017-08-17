{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DigitC where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Digit.DC

newtype DigitC a =
  DigitC a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DC a => DC (DigitC a) where
  dC  =
    _Wrapped . dC

instance Functor DigitC where
  fmap f (DigitC a) =
    DigitC (f a)
    
instance Apply DigitC where
  DigitC f <.> DigitC a =
    DigitC (f a)

instance Applicative DigitC where
  pure =
    DigitC
  (<*>) =
    (<.>)

instance Bind DigitC where
  DigitC a >>- f =
    f a

instance Monad DigitC where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitC where
  foldMap f (DigitC a) = 
    f a

instance Foldable1 DigitC where
  foldMap1 f (DigitC a) = 
    f a

instance Traversable DigitC where
  traverse f (DigitC a) = 
    DigitC <$> f a

instance Traversable1 DigitC where
  traverse1 f (DigitC a) = 
    DigitC <$> f a

instance Semigroup a => Semigroup (DigitC a) where
  DigitC x <> DigitC y =
    DigitC (x <> y)

instance Monoid a => Monoid (DigitC a) where
  DigitC x `mappend` DigitC y =
    DigitC (x `mappend` y)
  mempty =
    DigitC mempty

instance Field1 (DigitC a) (DigitC b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitC where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitC where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitC where
  itraverse f =
    traverse (f ())

instance Each (DigitC a) (DigitC b) a b where
  each =
    traverse

type instance Index (DigitC a) = 
  ()
type instance IxValue (DigitC a) =
  a
instance Ixed (DigitC a) where
  ix () f (DigitC a) =
    DigitC <$> f a

makeWrapped ''DigitC
