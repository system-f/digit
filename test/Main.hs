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
import qualified Hedgehog.Range as Range

testPrism ::
  (Show x, Eq x) =>
  Gen x
  -> String
  -> Prism' x Digit
  -> [(x, Digit)]
  -> [TestTree]
testPrism g n p x =
  testProperty
    (n ++ " prism invalid values")
    (
      property $
        do  c <- forAll  . Gen.filter (`notElem` (map fst x)) $ g
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

integralPrism ::
  (Show n, Integral n) =>
  String
  -> Prism' n Digit
  -> [(n, Digit)]
  -> [TestTree]
integralPrism =
  testPrism (Gen.choice [Gen.integral (Range.linear 0 9), Gen.integral (Range.linear 10 99999), Gen.integral (Range.linear (-1) (-99999))])
  
charPrism ::
  String
  -> Prism' Char Digit
  -> [(Char, Digit)]
  -> [TestTree]
charPrism =
  testPrism (Gen.choice [Gen.hexit, Gen.unicode])

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
      , integralPrism "integralBinaryNoZero" integralBinaryNoZero [(1 :: Integer, Digit1)]
      , integralPrism "integralBinary" integralBinary [(0 :: Integer, Digit0), (1, Digit1)]
      , integralPrism "integralOctalNoZero" integralOctalNoZero [(1 :: Integer, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7)]
      , integralPrism "integralOctal" integralOctal [(0 :: Integer, Digit0), (1, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7)]
      , integralPrism "integralDecimalNoZero" integralDecimalNoZero [(1 :: Integer, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7), (8, Digit8), (9, Digit9)]
      , integralPrism "integralDecimal" integralDecimal [(0 :: Integer, Digit0), (1, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7), (8, Digit8), (9, Digit9)]
      , integralPrism "integralHexadecimalNoZero" integralHexadecimalNoZero [(1 :: Integer, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7), (8, Digit8), (9, Digit9), (10, Digita), (11, Digitb), (12, Digitc), (13, Digitd), (14, Digite), (15, Digitf)]
      , integralPrism "integralHexadecimal" integralHexadecimal [(0 :: Integer, Digit0), (1, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7), (8, Digit8), (9, Digit9), (10, Digita), (11, Digitb), (12, Digitc), (13, Digitd), (14, Digite), (15, Digitf)]
      , integralPrism "integralHEXADECIMALNoZero" integralHEXADECIMALNoZero [(1 :: Integer, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7), (8, Digit8), (9, Digit9), (10, DigitA), (11, DigitB), (12, DigitC), (13, DigitD), (14, DigitE), (15, DigitF)]
      , integralPrism "integralHEXADECIMAL" integralHEXADECIMAL [(0 :: Integer, Digit0), (1, Digit1), (2, Digit2), (3, Digit3), (4, Digit4), (5, Digit5), (6, Digit6), (7, Digit7), (8, Digit8), (9, Digit9), (10, DigitA), (11, DigitB), (12, DigitC), (13, DigitD), (14, DigitE), (15, DigitF)]
      ]
