{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digite(
  Digite(..)
) where

import Papa
import Data.Digit.De(De(de))
import Prelude(Bounded, RealFrac)

newtype Digite a =
  Digite a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance De a => De (Digite a) where
  de  =
    _Wrapped . de

instance Functor Digite where
  fmap f (Digite a) =
    Digite (f a)
    
instance Apply Digite where
  Digite f <.> Digite a =
    Digite (f a)

instance Applicative Digite where
  pure =
    Digite
  (<*>) =
    (<.>)

instance Bind Digite where
  Digite a >>- f =
    f a

instance Monad Digite where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digite where
  foldMap f (Digite a) = 
    f a

instance Foldable1 Digite where
  foldMap1 f (Digite a) = 
    f a

instance Traversable Digite where
  traverse f (Digite a) = 
    Digite <$> f a

instance Traversable1 Digite where
  traverse1 f (Digite a) = 
    Digite <$> f a

instance Semigroup a => Semigroup (Digite a) where
  Digite x <> Digite y =
    Digite (x <> y)

instance Monoid a => Monoid (Digite a) where
  Digite x `mappend` Digite y =
    Digite (x `mappend` y)
  mempty =
    Digite mempty

instance Field1 (Digite a) (Digite b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digite where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digite where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digite where
  itraverse f =
    traverse (f ())

instance Each (Digite a) (Digite b) a b where
  each =
    traverse

type instance Index (Digite a) = 
  ()
type instance IxValue (Digite a) =
  a
instance Ixed (Digite a) where
  ix () f (Digite a) =
    Digite <$> f a

makeWrapped ''Digite
