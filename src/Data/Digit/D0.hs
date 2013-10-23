{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D0 where

import Prelude(Eq)

class Eq a => D0 a where
  d0 ::
    a
