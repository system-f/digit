{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DigitE where

import Papa
import Data.Digit.DE
import Prelude(Bounded, RealFrac)

newtype DigitE a =
  DigitE a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DE a => DE (DigitE a) where
  dE  =
    _Wrapped . dE

instance Functor DigitE where
  fmap f (DigitE a) =
    DigitE (f a)
    
instance Apply DigitE where
  DigitE f <.> DigitE a =
    DigitE (f a)

instance Applicative DigitE where
  pure =
    DigitE
  (<*>) =
    (<.>)

instance Bind DigitE where
  DigitE a >>- f =
    f a

instance Monad DigitE where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitE where
  foldMap f (DigitE a) = 
    f a

instance Foldable1 DigitE where
  foldMap1 f (DigitE a) = 
    f a

instance Traversable DigitE where
  traverse f (DigitE a) = 
    DigitE <$> f a

instance Traversable1 DigitE where
  traverse1 f (DigitE a) = 
    DigitE <$> f a

instance Semigroup a => Semigroup (DigitE a) where
  DigitE x <> DigitE y =
    DigitE (x <> y)

instance Monoid a => Monoid (DigitE a) where
  DigitE x `mappend` DigitE y =
    DigitE (x `mappend` y)
  mempty =
    DigitE mempty

instance Field1 (DigitE a) (DigitE b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitE where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitE where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitE where
  itraverse f =
    traverse (f ())

instance Each (DigitE a) (DigitE b) a b where
  each =
    traverse

type instance Index (DigitE a) = 
  ()
type instance IxValue (DigitE a) =
  a
instance Ixed (DigitE a) where
  ix () f (DigitE a) =
    DigitE <$> f a

makeWrapped ''DigitE
