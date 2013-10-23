{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D4 where

import Prelude(Eq)

class Eq a => D4 a where
  d4 ::
    a
