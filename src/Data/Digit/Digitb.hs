{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digitb where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.Db

newtype Digitb a =
  Digitb a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Db a => Db (Digitb a) where
  db  =
    _Wrapped . db

instance Functor Digitb where
  fmap f (Digitb a) =
    Digitb (f a)
    
instance Apply Digitb where
  Digitb f <.> Digitb a =
    Digitb (f a)

instance Applicative Digitb where
  pure =
    Digitb
  (<*>) =
    (<.>)

instance Bind Digitb where
  Digitb a >>- f =
    f a

instance Monad Digitb where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitb where
  foldMap f (Digitb a) = 
    f a

instance Foldable1 Digitb where
  foldMap1 f (Digitb a) = 
    f a

instance Traversable Digitb where
  traverse f (Digitb a) = 
    Digitb <$> f a

instance Traversable1 Digitb where
  traverse1 f (Digitb a) = 
    Digitb <$> f a

instance Semigroup a => Semigroup (Digitb a) where
  Digitb x <> Digitb y =
    Digitb (x <> y)

instance Monoid a => Monoid (Digitb a) where
  Digitb x `mappend` Digitb y =
    Digitb (x `mappend` y)
  mempty =
    Digitb mempty

instance Field1 (Digitb a) (Digitb b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitb where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitb where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitb where
  itraverse f =
    traverse (f ())

instance Each (Digitb a) (Digitb b) a b where
  each =
    traverse

type instance Index (Digitb a) = 
  ()
type instance IxValue (Digitb a) =
  a
instance Ixed (Digitb a) where
  ix () f (Digitb a) =
    Digitb <$> f a

makeWrapped ''Digitb
