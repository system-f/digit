{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D1 where

import Prelude(Eq)

class Eq a => D1 a where
  d1 ::
    a
