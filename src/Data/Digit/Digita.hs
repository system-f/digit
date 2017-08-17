{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.Digita where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.Da

newtype Digita a =
  Digita a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Da a => Da (Digita a) where
  da  =
    _Wrapped . da

instance Functor Digita where
  fmap f (Digita a) =
    Digita (f a)
    
instance Apply Digita where
  Digita f <.> Digita a =
    Digita (f a)

instance Applicative Digita where
  pure =
    Digita
  (<*>) =
    (<.>)

instance Bind Digita where
  Digita a >>- f =
    f a

instance Monad Digita where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digita where
  foldMap f (Digita a) = 
    f a

instance Foldable1 Digita where
  foldMap1 f (Digita a) = 
    f a

instance Traversable Digita where
  traverse f (Digita a) = 
    Digita <$> f a

instance Traversable1 Digita where
  traverse1 f (Digita a) = 
    Digita <$> f a

instance Semigroup a => Semigroup (Digita a) where
  Digita x <> Digita y =
    Digita (x <> y)

instance Monoid a => Monoid (Digita a) where
  Digita x `mappend` Digita y =
    Digita (x `mappend` y)
  mempty =
    Digita mempty

instance Field1 (Digita a) (Digita b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digita where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digita where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digita where
  itraverse f =
    traverse (f ())

instance Each (Digita a) (Digita b) a b where
  each =
    traverse

type instance Index (Digita a) = 
  ()
type instance IxValue (Digita a) =
  a
instance Ixed (Digita a) where
  ix () f (Digita a) =
    Digita <$> f a

makeWrapped ''Digita
