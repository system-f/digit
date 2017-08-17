{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digitd where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.Dd

newtype Digitd a =
  Digitd a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Dd a => Dd (Digitd a) where
  dd  =
    _Wrapped . dd

instance Functor Digitd where
  fmap f (Digitd a) =
    Digitd (f a)
    
instance Apply Digitd where
  Digitd f <.> Digitd a =
    Digitd (f a)

instance Applicative Digitd where
  pure =
    Digitd
  (<*>) =
    (<.>)

instance Bind Digitd where
  Digitd a >>- f =
    f a

instance Monad Digitd where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitd where
  foldMap f (Digitd a) = 
    f a

instance Foldable1 Digitd where
  foldMap1 f (Digitd a) = 
    f a

instance Traversable Digitd where
  traverse f (Digitd a) = 
    Digitd <$> f a

instance Traversable1 Digitd where
  traverse1 f (Digitd a) = 
    Digitd <$> f a

instance Semigroup a => Semigroup (Digitd a) where
  Digitd x <> Digitd y =
    Digitd (x <> y)

instance Monoid a => Monoid (Digitd a) where
  Digitd x `mappend` Digitd y =
    Digitd (x `mappend` y)
  mempty =
    Digitd mempty

instance Field1 (Digitd a) (Digitd b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitd where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitd where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitd where
  itraverse f =
    traverse (f ())

instance Each (Digitd a) (Digitd b) a b where
  each =
    traverse

type instance Index (Digitd a) = 
  ()
type instance IxValue (Digitd a) =
  a
instance Ixed (Digitd a) where
  ix () f (Digitd a) =
    Digitd <$> f a

makeWrapped ''Digitd
