{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D2 where

import Prelude(Eq)

class Eq a => D2 a where
  d2 ::
    a
