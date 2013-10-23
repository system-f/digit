{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D5 where

import Prelude(Eq)

class Eq a => D5 a where
  d5 ::
    a
