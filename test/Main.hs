{-# LANGUAGE RankNTypes #-}

module Main(
  main
) where

import Data.Digit
import Papa
import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit(testCase, (@?=))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Parsec(parse, ParseError, Parsec, eof)

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
        do  c <- forAll . Gen.filter (`notElem` (map fst x)) $ g
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

testParser ::
  String
  -> Parsec String () Digit
  -> [(Char, Digit)]
  -> [TestTree]
testParser n p x =
    (
      let n' = n ++ " parser invalid values"
      in  testProperty
            n'
            (
              property $
                do  c <- forAll . Gen.filter (`notElem` (map fst x)) . Gen.choice $ [Gen.hexit, Gen.unicode]
                    assert (isLeft (parse p (n' ++ " test") [c] :: Either ParseError Digit))
            )
    ) :
    (
      x >>= \(c, d) ->
        [
          let n' = n ++ " parses exactly one character"
          in  testCase
                n'
                ((parse (p <* eof) (n' ++ " test") [c] :: Either ParseError Digit) @?= Right d)
        , let n' = n ++ " parses the correct digit"
          in  testCase
            n'
            ((parse p (n' ++ "test") (c:"xyz") :: Either ParseError Digit) @?= Right d)
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
      , testParser "parseBinaryNoZero" parseBinaryNoZero [('1', Digit1)]
      , testParser "parseBinary" parseBinary [('0', Digit0), ('1', Digit1)]
      , testParser "parseOctalNoZero" parseOctalNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7)]
      , testParser "parseOctal" parseOctal [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7)]
      , testParser "parseDecimalNoZero" parseDecimalNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9)]
      , testParser "parseDecimal" parseDecimal [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9)]
      , testParser "parseHexadecimalNoZero" parseHexadecimalNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('a', Digita), ('b', Digitb), ('c', Digitc), ('d', Digitd), ('e', Digite), ('f', Digitf)]
      , testParser "parseHexadecimalDecimal" parseHexadecimal [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('a', Digita), ('b', Digitb), ('c', Digitc), ('d', Digitd), ('e', Digite), ('f', Digitf)]
      , testParser "parseHEXADECIMALNoZero" parseHEXADECIMALNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('A', DigitA), ('B', DigitB), ('C', DigitC), ('D', DigitD), ('E', DigitE), ('F', DigitF)]
      , testParser "parseHEXADECIMALDecimal" parseHEXADECIMAL [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('A', DigitA), ('B', DigitB), ('C', DigitC), ('D', DigitD), ('E', DigitE), ('F', DigitF)]
      , testParser "parseHeXaDeCiMaLNoZero" parseHeXaDeCiMaLNoZero [('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('a', Digita), ('b', Digitb), ('c', Digitc), ('d', Digitd), ('e', Digite), ('f', Digitf), ('A', DigitA), ('B', DigitB), ('C', DigitC), ('D', DigitD), ('E', DigitE), ('F', DigitF)]
      , testParser "parseHeXaDeCiMaLDecimal" parseHeXaDeCiMaL [('0', Digit0), ('1', Digit1), ('2', Digit2), ('3', Digit3), ('4', Digit4), ('5', Digit5), ('6', Digit6), ('7', Digit7), ('8', Digit8), ('9', Digit9), ('a', Digita), ('b', Digitb), ('c', Digitc), ('d', Digitd), ('e', Digite), ('f', Digitf), ('A', DigitA), ('B', DigitB), ('C', DigitC), ('D', DigitD), ('E', DigitE), ('F', DigitF)]
      , testParser "parse0" parse0 [('0', Digit0)]
      , testParser "parse1" parse1 [('1', Digit1)]
      , testParser "parse2" parse2 [('2', Digit2)]
      , testParser "parse3" parse3 [('3', Digit3)]
      , testParser "parse4" parse4 [('4', Digit4)]
      , testParser "parse5" parse5 [('5', Digit5)]
      , testParser "parse6" parse6 [('6', Digit6)]
      , testParser "parse7" parse7 [('7', Digit7)]
      , testParser "parse8" parse8 [('8', Digit8)]
      , testParser "parse9" parse9 [('9', Digit9)]
      , testParser "parsea" parsea [('a', Digita)]
      , testParser "parseb" parseb [('b', Digitb)]
      , testParser "parsec" parsec [('c', Digitc)]
      , testParser "parsed" parsed [('d', Digitd)]
      , testParser "parsee" parsee [('e', Digite)]
      , testParser "parsef" parsef [('f', Digitf)]
      , testParser "parseA" parseA [('A', DigitA)]
      , testParser "parseB" parseB [('B', DigitB)]
      , testParser "parseC" parseC [('C', DigitC)]
      , testParser "parseD" parseD [('D', DigitD)]
      , testParser "parseE" parseE [('E', DigitE)]
      , testParser "parseF" parseF [('F', DigitF)]
      , testParser "parseAa" parseAa [('A', DigitA), ('a', Digita)]
      , testParser "parseBb" parseBb [('B', DigitB), ('b', Digitb)]
      , testParser "parseCc" parseCc [('C', DigitC), ('c', Digitc)]
      , testParser "parseDd" parseDd [('D', DigitD), ('d', Digitd)]
      , testParser "parseEe" parseEe [('E', DigitE), ('e', Digite)]
      , testParser "parseFf" parseFf [('F', DigitF), ('f', Digitf)]
      ]
