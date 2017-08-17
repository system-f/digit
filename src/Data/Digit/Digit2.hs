{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digit2 where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D2

newtype Digit2 a =
  Digit2 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D2 a => D2 (Digit2 a) where
  d2 =
    _Wrapped . d2

instance Functor Digit2 where
  fmap f (Digit2 a) =
    Digit2 (f a)
    
instance Apply Digit2 where
  Digit2 f <.> Digit2 a =
    Digit2 (f a)

instance Applicative Digit2 where
  pure =
    Digit2
  (<*>) =
    (<.>)

instance Bind Digit2 where
  Digit2 a >>- f =
    f a

instance Monad Digit2 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit2 where
  foldMap f (Digit2 a) = 
    f a

instance Foldable1 Digit2 where
  foldMap1 f (Digit2 a) = 
    f a

instance Traversable Digit2 where
  traverse f (Digit2 a) = 
    Digit2 <$> f a

instance Traversable1 Digit2 where
  traverse1 f (Digit2 a) = 
    Digit2 <$> f a

instance Semigroup a => Semigroup (Digit2 a) where
  Digit2 x <> Digit2 y =
    Digit2 (x <> y)

instance Monoid a => Monoid (Digit2 a) where
  Digit2 x `mappend` Digit2 y =
    Digit2 (x `mappend` y)
  mempty =
    Digit2 mempty

instance Field1 (Digit2 a) (Digit2 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit2 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit2 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit2 where
  itraverse f =
    traverse (f ())

instance Each (Digit2 a) (Digit2 b) a b where
  each =
    traverse

type instance Index (Digit2 a) = 
  ()
type instance IxValue (Digit2 a) =
  a
instance Ixed (Digit2 a) where
  ix () f (Digit2 a) =
    Digit2 <$> f a

makeWrapped ''Digit2
