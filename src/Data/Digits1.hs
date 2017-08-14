{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digits1(
  Digits1(..)
, AsDigits1(..)
, HasDigits1(..)
, ManyDigits1(..)
, parsedigitlist1
) where

import Data.Data(Data)
import Data.Digit(Digit, AsDigit(..), parsedigit)
import Data.List.NonEmpty(some1)
import qualified Data.List.NonEmpty as NonEmpty(cons)
import Data.Semigroup((<>))
import Data.Typeable (Typeable)
import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>))
import Papa

newtype Digits1 =
  Digits1
    (NonEmpty Digit)
  deriving (Eq, Ord, Show, Data, Typeable)

makeWrapped ''Digits1

class AsDigits1 a where
  _Digits1 ::
    Prism'
      a
      Digits1

instance AsDigits1 Digits1 where
  _Digits1 =
    id


instance AsDigit a => AsDigits1 (NonEmpty a) where
  _Digits1 =
    prism'
      (\(Digits1 d) -> (digit #) <$> d)
      (\s -> Digits1 <$> traverse (^? digit) s)

instance AsDigit a => AsDigits1 [a] where
  _Digits1 =
    -- where does this live?
    let nonemptyP ::
          Prism [a] [a] (NonEmpty a) (NonEmpty a)
        nonemptyP =
            prism'
              toList
              (\x ->  case x of
                        [] ->
                          Nothing
                        h:t ->
                          Just (h :| t))
    in  nonemptyP . _Digits1

class HasDigits1 a where
  digits1L :: 
    Lens'
      a
      Digits1

instance HasDigits1 Digits1 where
  digits1L =
    id

class ManyDigits1 a where
  digits1T ::
    Traversal' 
      a
      Digits1

instance ManyDigits1 Digits1 where
  digits1T =
    id

instance Semigroup Digits1 where
  Digits1 x <> Digits1 y =
    Digits1 (x <> y)

instance Plated Digits1 where
  plate f (Digits1 d) =
    let platedNonEmpty ::
          Traversal' (NonEmpty a) (NonEmpty a)
        platedNonEmpty k l@(_ :| []) = 
          k l
        platedNonEmpty k (h :| i : t) = 
          (h `NonEmpty.cons`) <$> k (i :| t)
    in  Digits1 <$> platedNonEmpty (\n -> (\(Digits1 x) -> x) <$> f (Digits1 n)) d

instance Reversing Digits1 where
  reversing (Digits1 d) =
    Digits1 (reversing d)

instance Each Digits1 Digits1 Digit Digit where
  each f (Digits1 d) =
    Digits1 <$> each f d

instance Snoc Digits1 Digits1 Digit Digit where
  _Snoc =
    prism'
      (\(Digits1 ds, d) -> Digits1 (ds <> (d :| [])))
      (\(Digits1 (h :| t)) -> (_1 %~ (\ds -> Digits1 (h :| ds))) <$> t ^? _Snoc)

instance Cons Digits1 Digits1 Digit Digit where
  _Cons =
    prism'
      (\(d, Digits1 ds) -> Digits1 (d `NonEmpty.cons` ds))
      (\(Digits1 (h :| t)) -> (_2 %~ (\ds -> Digits1 (h :| ds))) <$> t ^? _Cons)

type instance IxValue Digits1 = Digit
type instance Index Digits1 = Int
instance Ixed Digits1 where
  ix i f (Digits1 d) =
    Digits1 <$> ix i f d

parsedigitlist1 ::
  CharParsing p =>
  p Digits1
parsedigitlist1 =
  Digits1 <$> some1 parsedigit <?> "Digits1"
