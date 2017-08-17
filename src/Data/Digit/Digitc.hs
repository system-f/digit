{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digitc where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.Dc

newtype Digitc a =
  Digitc a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Dc a => Dc (Digitc a) where
  dc  =
    _Wrapped . dc

instance Functor Digitc where
  fmap f (Digitc a) =
    Digitc (f a)
    
instance Apply Digitc where
  Digitc f <.> Digitc a =
    Digitc (f a)

instance Applicative Digitc where
  pure =
    Digitc
  (<*>) =
    (<.>)

instance Bind Digitc where
  Digitc a >>- f =
    f a

instance Monad Digitc where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitc where
  foldMap f (Digitc a) = 
    f a

instance Foldable1 Digitc where
  foldMap1 f (Digitc a) = 
    f a

instance Traversable Digitc where
  traverse f (Digitc a) = 
    Digitc <$> f a

instance Traversable1 Digitc where
  traverse1 f (Digitc a) = 
    Digitc <$> f a

instance Semigroup a => Semigroup (Digitc a) where
  Digitc x <> Digitc y =
    Digitc (x <> y)

instance Monoid a => Monoid (Digitc a) where
  Digitc x `mappend` Digitc y =
    Digitc (x `mappend` y)
  mempty =
    Digitc mempty

instance Field1 (Digitc a) (Digitc b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitc where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitc where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitc where
  itraverse f =
    traverse (f ())

instance Each (Digitc a) (Digitc b) a b where
  each =
    traverse

type instance Index (Digitc a) = 
  ()
type instance IxValue (Digitc a) =
  a
instance Ixed (Digitc a) where
  ix () f (Digitc a) =
    Digitc <$> f a

makeWrapped ''Digitc
