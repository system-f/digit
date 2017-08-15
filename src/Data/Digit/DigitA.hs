{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DigitA(
  DigitA(..)
) where

import Papa
import Data.Digit.DA(DA(dA))
import Prelude(Bounded, RealFrac)

newtype DigitA a =
  DigitA a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DA a => DA (DigitA a) where
  dA  =
    _Wrapped . dA

instance Functor DigitA where
  fmap f (DigitA a) =
    DigitA (f a)
    
instance Apply DigitA where
  DigitA f <.> DigitA a =
    DigitA (f a)

instance Applicative DigitA where
  pure =
    DigitA
  (<*>) =
    (<.>)

instance Bind DigitA where
  DigitA a >>- f =
    f a

instance Monad DigitA where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitA where
  foldMap f (DigitA a) = 
    f a

instance Foldable1 DigitA where
  foldMap1 f (DigitA a) = 
    f a

instance Traversable DigitA where
  traverse f (DigitA a) = 
    DigitA <$> f a

instance Traversable1 DigitA where
  traverse1 f (DigitA a) = 
    DigitA <$> f a

instance Semigroup a => Semigroup (DigitA a) where
  DigitA x <> DigitA y =
    DigitA (x <> y)

instance Monoid a => Monoid (DigitA a) where
  DigitA x `mappend` DigitA y =
    DigitA (x `mappend` y)
  mempty =
    DigitA mempty

instance Field1 (DigitA a) (DigitA b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitA where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitA where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitA where
  itraverse f =
    traverse (f ())

instance Each (DigitA a) (DigitA b) a b where
  each =
    traverse

type instance Index (DigitA a) = 
  ()
type instance IxValue (DigitA a) =
  a
instance Ixed (DigitA a) where
  ix () f (DigitA a) =
    DigitA <$> f a

makeWrapped ''DigitA
