{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D7 where

import Prelude(Eq)

class Eq a => D7 a where
  d7 ::
    a
