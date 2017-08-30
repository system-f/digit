{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Digit(
  Digit
) where

import Data.Digit.D0(D0(d0))
import Data.Digit.D1(D1(d1))
import Data.Digit.D2(D2(d2))
import Data.Digit.D3(D3(d3))
import Data.Digit.D4(D4(d4))
import Data.Digit.D5(D5(d5))
import Data.Digit.D6(D6(d6))
import Data.Digit.D7(D7(d7))
import Data.Digit.D8(D8(d8))
import Data.Digit.D9(D9(d9))
import Data.Digit.Da(Da(da))
import Data.Digit.Db(Db(db))
import Data.Digit.Dc(Dc(dc))
import Data.Digit.Dd(Dd(dd))
import Data.Digit.De(De(de))
import Data.Digit.Df(Df(df))
import Data.Digit.DA(DA(dA))
import Data.Digit.DB(DB(dB))
import Data.Digit.DC(DC(dC))
import Data.Digit.DD(DD(dD))
import Data.Digit.DE(DE(dE))
import Data.Digit.DF(DF(dF))
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

instance D0 Digit where
  d0 =
    prism'
      (\() -> Digit0)
      (\d ->  case d of
                Digit0 ->
                  Just ()
                _ ->
                  Nothing)

instance D1 Digit where
  d1 =
    prism'
      (\() -> Digit1)
      (\d ->  case d of
                Digit1 ->
                  Just ()
                _ ->
                  Nothing)

instance D2 Digit where
  d2 =
    prism'
      (\() -> Digit2)
      (\d ->  case d of
                Digit2 ->
                  Just ()
                _ ->
                  Nothing)

instance D3 Digit where
  d3 =
    prism'
      (\() -> Digit3)
      (\d ->  case d of
                Digit3 ->
                  Just ()
                _ ->
                  Nothing)

instance D4 Digit where
  d4 =
    prism'
      (\() -> Digit4)
      (\d ->  case d of
                Digit4 ->
                  Just ()
                _ ->
                  Nothing)

instance D5 Digit where
  d5 =
    prism'
      (\() -> Digit5)
      (\d ->  case d of
                Digit5 ->
                  Just ()
                _ ->
                  Nothing)

instance D6 Digit where
  d6 =
    prism'
      (\() -> Digit6)
      (\d ->  case d of
                Digit6 ->
                  Just ()
                _ ->
                  Nothing)

instance D7 Digit where
  d7 =
    prism'
      (\() -> Digit7)
      (\d ->  case d of
                Digit7 ->
                  Just ()
                _ ->
                  Nothing)

instance D8 Digit where
  d8 =
    prism'
      (\() -> Digit8)
      (\d ->  case d of
                Digit8 ->
                  Just ()
                _ ->
                  Nothing)

instance D9 Digit where
  d9 =
    prism'
      (\() -> Digit9)
      (\d ->  case d of
                Digit9 ->
                  Just ()
                _ ->
                  Nothing)

instance Da Digit where
  da =
    prism'
      (\() -> Digita)
      (\d ->  case d of
                Digita ->
                  Just ()
                _ ->
                  Nothing)

instance Db Digit where
  db =
    prism'
      (\() -> Digitb)
      (\d ->  case d of
                Digitb ->
                  Just ()
                _ ->
                  Nothing)

instance Dc Digit where
  dc =
    prism'
      (\() -> Digitc)
      (\d ->  case d of
                Digitc ->
                  Just ()
                _ ->
                  Nothing)

instance Dd Digit where
  dd =
    prism'
      (\() -> Digitd)
      (\d ->  case d of
                Digitd ->
                  Just ()
                _ ->
                  Nothing)

instance De Digit where
  de =
    prism'
      (\() -> Digite)
      (\d ->  case d of
                Digite ->
                  Just ()
                _ ->
                  Nothing)

instance Df Digit where
  df =
    prism'
      (\() -> Digitf)
      (\d ->  case d of
                Digitf ->
                  Just ()
                _ ->
                  Nothing)

instance DA Digit where
  dA =
    prism'
      (\() -> DigitA)
      (\d ->  case d of
                DigitA ->
                  Just ()
                _ ->
                  Nothing)

instance DB Digit where
  dB =
    prism'
      (\() -> DigitB)
      (\d ->  case d of
                DigitB ->
                  Just ()
                _ ->
                  Nothing)

instance DC Digit where
  dC =
    prism'
      (\() -> DigitC)
      (\d ->  case d of
                DigitC ->
                  Just ()
                _ ->
                  Nothing)

instance DD Digit where
  dD =
    prism'
      (\() -> DigitD)
      (\d ->  case d of
                DigitD ->
                  Just ()
                _ ->
                  Nothing)

instance DE Digit where
  dE =
    prism'
      (\() -> DigitE)
      (\d ->  case d of
                DigitE ->
                  Just ()
                _ ->
                  Nothing)

instance DF Digit where
  dF =
    prism'
      (\() -> DigitF)
      (\d ->  case d of
                DigitF ->
                  Just ()
                _ ->
                  Nothing)
