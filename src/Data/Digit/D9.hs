{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D9 where

import Prelude(Eq)

class Eq a => D9 a where
  d9 ::
    a
