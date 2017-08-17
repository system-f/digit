{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DigitF where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DF

newtype DigitF a =
  DigitF a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DF a => DF (DigitF a) where
  dF  =
    _Wrapped . dF

instance Functor DigitF where
  fmap f (DigitF a) =
    DigitF (f a)
    
instance Apply DigitF where
  DigitF f <.> DigitF a =
    DigitF (f a)

instance Applicative DigitF where
  pure =
    DigitF
  (<*>) =
    (<.>)

instance Bind DigitF where
  DigitF a >>- f =
    f a

instance Monad DigitF where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitF where
  foldMap f (DigitF a) = 
    f a

instance Foldable1 DigitF where
  foldMap1 f (DigitF a) = 
    f a

instance Traversable DigitF where
  traverse f (DigitF a) = 
    DigitF <$> f a

instance Traversable1 DigitF where
  traverse1 f (DigitF a) = 
    DigitF <$> f a

instance Semigroup a => Semigroup (DigitF a) where
  DigitF x <> DigitF y =
    DigitF (x <> y)

instance Monoid a => Monoid (DigitF a) where
  DigitF x `mappend` DigitF y =
    DigitF (x `mappend` y)
  mempty =
    DigitF mempty

instance Field1 (DigitF a) (DigitF b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitF where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitF where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitF where
  itraverse f =
    traverse (f ())

instance Each (DigitF a) (DigitF b) a b where
  each =
    traverse

type instance Index (DigitF a) = 
  ()
type instance IxValue (DigitF a) =
  a
instance Ixed (DigitF a) where
  ix () f (DigitF a) =
    DigitF <$> f a

makeWrapped ''DigitF
