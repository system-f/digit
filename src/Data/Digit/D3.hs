{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.D3 where

import Prelude(Eq)

class Eq a => D3 a where
  d3 ::
    a
