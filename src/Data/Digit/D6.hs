{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D6 where

import Prelude(Eq)

class Eq a => D6 a where
  d6 ::
    a
