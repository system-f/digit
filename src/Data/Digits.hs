{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digits(
  Digits(..)
, AsDigits(..)
, HasDigits(..)
, ManyDigits(..)
, parsedigits
) where

import Control.Applicative(many)
import Data.Data (Data)
import Data.Digit(Digit, AsDigit(..), parsedigit)
import Data.Digits1(Digits1(..), AsDigits1(..))
import Data.Monoid(mappend)
import Data.Semigroup((<>))
import Data.Typeable (Typeable)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>))
import Papa

newtype Digits =
  Digits
    [Digit]
  deriving (Eq, Ord, Show, Data, Typeable)

makeWrapped ''Digits

class AsDigits a where
  _Digits ::
    Prism'
      a
      Digits

instance AsDigits Digits where
  _Digits =
    id
    
instance AsDigit a => AsDigits [a] where
  _Digits =
    prism'
      (\(Digits d) -> (digit #) <$> d)
      (\s -> Digits <$> traverse (^? digit) s)

class HasDigits a where
  digitsL :: 
    Lens'
      a
      Digits

instance HasDigits Digits where
  digitsL =
    id

class ManyDigits a where
  digitsT ::
    Traversal' 
      a
      Digits

instance ManyDigits Digits where
  digitsT =
    id

instance Cons Digits Digits Digit Digit where
  _Cons =
    prism'
      (\(h, Digits t) -> Digits (h:t))
      (\(Digits d) -> (_2 %~ Digits) <$> d ^? _Cons)

instance Snoc Digits Digits Digit Digit where
  _Snoc =
    prism'
      (\(Digits t, z) -> Digits (t ++ [z]))
      (\(Digits d) -> (_1 %~ Digits) <$> d ^? _Snoc)

instance AsEmpty Digits where
  _Empty =
    _Wrapped . _Empty

instance Each Digits Digits Digit Digit where
  each f (Digits d) =
    Digits <$> each f d

type instance IxValue Digits = Digit
type instance Index Digits = Int
instance Ixed Digits where
  ix i f (Digits d) =
    Digits <$> ix i f d

instance Plated Digits where
  plate f (Digits d) =
    Digits <$> plate (\x -> (\(Digits e) -> e) <$> f (Digits x)) d

instance Reversing Digits where
  reversing (Digits d) =
    Digits (reversing d)

instance Semigroup Digits where
  Digits d <> Digits e =
    Digits (d <> e)

instance Monoid Digits where
  mempty =
    Digits mempty
  mappend =
    (<>)

parsedigits ::
  CharParsing p =>
  p Digits
parsedigits = 
  Digits <$> many parsedigit <?> "Digits"

instance AsDigits1 Digits where
  _Digits1 =
    prism'
      (\(Digits1 x) -> Digits (toList x))
      (\(Digits x) -> case x of
                        [] ->
                          Nothing
                        h:t -> 
                          Just (Digits1 (h :| t)))
