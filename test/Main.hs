{-# LANGUAGE RankNTypes #-}

module Main(
  main
) where

import Data.Digit
import Papa
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import qualified Hedgehog.Gen as Gen

charPrism ::
  String
  -> Prism' Char Digit
  -> [(Char, Digit)]
  -> [TestTree]
charPrism n p x =
  testProperty
    (n ++ " prism invalid values")
    (
      property $
        do  c <- forAll . Gen.filter (`notElem` (map fst x)) . Gen.choice $ [Gen.hexit, Gen.unicode]
            (c ^? p :: Maybe Digit) === Nothing
    ) :
    (
      x >>= \(c, d) ->
        [
          testCase
            (n ++ " prism ->")
            ((c ^? p :: Maybe Digit) @?= Just d)
        , testCase
            (n ++ " prism <-")
            (p # d @?= c)
        ]
    )

main ::
  IO ()
main =
  defaultMain $
    testGroup "digit tests" $
      concat [
        charPrism "charBinaryNoZero" charBinaryNoZero [('1', Digit1)]
      , charPrism "charBinary" charBinary [('0', Digit0), ('1', Digit1)]
      , charPrism "charOctalNoZero" charOctalNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7)]
      , charPrism "charOctal" charOctal [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7)]
      , charPrism "charDecimalNoZero" charDecimalNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9)]
      , charPrism "charDecimal" charDecimal [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9)]
      , charPrism "charHexadecimalNoZero" charHexadecimalNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('a', Digita), ('b', Digitb), ('c', Digitc), ('d', Digitd), ('e', Digite), ('f', Digitf)]
      , charPrism "charHEXADECIMALNoZero" charHEXADECIMALNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('A', DigitA), ('B', DigitB), ('C', DigitC), ('D', DigitD), ('E', DigitE), ('F', DigitF)]
      , charPrism "charHeXaDeCiMaLNoZero" charHeXaDeCiMaLNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('a', Digita), ('b', Digitb), ('c', Digitc), ('d', Digitd), ('e', Digite), ('f', Digitf), ('A', DigitA), ('B', DigitB), ('C', DigitC), ('D', DigitD), ('E', DigitE), ('F', DigitF)]
      ]
