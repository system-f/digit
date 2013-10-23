{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D8 where

import Prelude(Eq)

class Eq a => D8 a where
  d8 ::
    a
