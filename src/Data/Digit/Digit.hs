{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Digit(
  Digit(..)
) where

import Papa

data Digit =
  Digit0
  | Digit1
  | Digit2
  | Digit3
  | Digit4
  | Digit5
  | Digit6
  | Digit7
  | Digit8
  | Digit9
  | Digita
  | Digitb
  | Digitc
  | Digitd
  | Digite
  | Digitf
  | DigitA
  | DigitB
  | DigitC
  | DigitD
  | DigitE
  | DigitF
  deriving (Eq, Ord)

instance Show Digit where
  show Digit0 =
    "0"
  show Digit1 =
    "1"
  show Digit2 =
    "2"
  show Digit3 =
    "3"
  show Digit4 =
    "4"
  show Digit5 =
    "5"
  show Digit6 =
    "6"
  show Digit7 =
    "7"
  show Digit8 =
    "8"
  show Digit9 =
    "9"
  show Digita =
    "a"
  show Digitb =
    "b"
  show Digitc =
    "c"
  show Digitd =
    "d"
  show Digite =
    "e"
  show Digitf =
    "f"
  show DigitA =
    "A"
  show DigitB =
    "B"
  show DigitC =
    "C"
  show DigitD =
    "D"
  show DigitE =
    "E"
  show DigitF =
    "F"
