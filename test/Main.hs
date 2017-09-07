{-# LANGUAGE RankNTypes #-}

module Main(
  main
) where

import Data.Digit
import Papa hiding (re)
import Hedgehog(Gen, forAll, property, assert, (===))
import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog(testProperty)
import Test.Tasty.HUnit(testCase, (@?=))
import qualified Hedgehog.Gen as Gen(choice, integral, unicode, hexit, filter)
import qualified Hedgehog.Range as Range(linear)
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
      let q0  = ('0', Digit0)
          q1  = ('1', Digit1)
          q2  = ('2', Digit2)
          q3  = ('3', Digit3)
          q4  = ('4', Digit4)
          q5  = ('5', Digit5)
          q6  = ('6', Digit6)
          q7  = ('7', Digit7)
          q8  = ('8', Digit8)
          q9  = ('9', Digit9)
          qa  = ('a', Digita)
          qb  = ('b', Digitb)
          qc  = ('c', Digitc)
          qd  = ('d', Digitd)
          qe  = ('e', Digite)
          qf  = ('f', Digitf)
          qA  = ('A', DigitA)
          qB  = ('B', DigitB)
          qC  = ('C', DigitC)
          qD  = ('D', DigitD)
          qE  = ('E', DigitE)
          qF  = ('F', DigitF)
          r0 :: (Integer, Digit)
          r0  = (0  , Digit0)
          r1 :: (Integer, Digit)
          r1  = (1  , Digit1)
          r2 :: (Integer, Digit)
          r2  = (2  , Digit2)
          r3 :: (Integer, Digit)
          r3  = (3  , Digit3)
          r4 :: (Integer, Digit)
          r4  = (4  , Digit4)
          r5 :: (Integer, Digit)
          r5  = (5  , Digit5)
          r6 :: (Integer, Digit)
          r6  = (6  , Digit6)
          r7 :: (Integer, Digit)
          r7  = (7  , Digit7)
          r8 :: (Integer, Digit)
          r8  = (8  , Digit8)
          r9 :: (Integer, Digit)
          r9  = (9  , Digit9)
          ra :: (Integer, Digit)
          ra =  (10 , Digita)
          rb :: (Integer, Digit)
          rb =  (11 , Digitb)
          rc :: (Integer, Digit)
          rc =  (12 , Digitc)
          rd :: (Integer, Digit)
          rd =  (13 , Digitd)
          re :: (Integer, Digit)
          re =  (14 , Digite)
          rf :: (Integer, Digit)
          rf =  (15 , Digitf)
          rA :: (Integer, Digit)
          rA =  (10 , DigitA)
          rB :: (Integer, Digit)
          rB =  (11 , DigitB)
          rC :: (Integer, Digit)
          rC =  (12 , DigitC)
          rD :: (Integer, Digit)
          rD =  (13 , DigitD)
          rE :: (Integer, Digit)
          rE =  (14 , DigitE)
          rF :: (Integer, Digit)
          rF =  (15 , DigitF)
      in  concat [
            charPrism "charBinaryNoZero" charBinaryNoZero [q1]
          , charPrism "charBinary" charBinary [q0, q1]
          , charPrism "charOctalNoZero" charOctalNoZero [q1, q2, q3, q4, q5, q6, q7]
          , charPrism "charOctal" charOctal [q0, q1, q2, q3, q4, q5, q6, q7]
          , charPrism "charDecimalNoZero" charDecimalNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9]
          , charPrism "charDecimal" charDecimal [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9]
          , charPrism "charHexadecimalNoZero" charHexadecimalNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf]
          , charPrism "charHexadecimal" charHexadecimal [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf]
          , charPrism "charHEXADECIMALNoZero" charHEXADECIMALNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9, qA, qB, qC, qD, qE, qF]
          , charPrism "charHEXADECIMAL" charHEXADECIMAL [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qA, qB, qC, qD, qE, qF]
          , charPrism "charHeXaDeCiMaLNoZero" charHeXaDeCiMaLNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf, qA, qB, qC, qD, qE, qF]
          , charPrism "charHeXaDeCiMaL" charHeXaDeCiMaL [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf, qA, qB, qC, qD, qE, qF]
          , integralPrism "integralBinaryNoZero" integralBinaryNoZero [r1]
          , integralPrism "integralBinary" integralBinary [r0, r1]
          , integralPrism "integralOctalNoZero" integralOctalNoZero [r1, r2, r3, r4, r5, r6, r7]
          , integralPrism "integralOctal" integralOctal [r0, r1, r2, r3, r4, r5, r6, r7]
          , integralPrism "integralDecimalNoZero" integralDecimalNoZero [r1, r2, r3, r4, r5, r6, r7, r8, r9]
          , integralPrism "integralDecimal" integralDecimal [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9]
          , integralPrism "integralHexadecimalNoZero" integralHexadecimalNoZero [r1, r2, r3, r4, r5, r6, r7, r8, r9, ra, rb, rc, rd, re, rf]
          , integralPrism "integralHexadecimal" integralHexadecimal [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, ra, rb, rc, rd, re, rf]
          , integralPrism "integralHEXADECIMALNoZero" integralHEXADECIMALNoZero [r1, r2, r3, r4, r5, r6, r7, r8, r9, rA, rB, rC, rD, rE, rF]
          , integralPrism "integralHEXADECIMAL" integralHEXADECIMAL [r0, r1, r2, r3, r4, r5, r6, r7, r8, r9, rA, rB, rC, rD, rE, rF] , testParser "parseBinaryNoZero" parseBinaryNoZero [q1]
          , testParser "parseBinary" parseBinary [q0, q1]
          , testParser "parseOctalNoZero" parseOctalNoZero [q1, q2, q3, q4, q5, q6, q7]
          , testParser "parseOctal" parseOctal [q0, q1, q2, q3, q4, q5, q6, q7]
          , testParser "parseDecimalNoZero" parseDecimalNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9]
          , testParser "parseDecimal" parseDecimal [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9]
          , testParser "parseHexadecimalNoZero" parseHexadecimalNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf]
          , testParser "parseHexadecimal" parseHexadecimal [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf]
          , testParser "parseHEXADECIMALNoZero" parseHEXADECIMALNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9, qA, qB, qC, qD, qE, qF]
          , testParser "parseHEXADECIMALDecimal" parseHEXADECIMAL [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qA, qB, qC, qD, qE, qF]
          , testParser "parseHeXaDeCiMaLNoZero" parseHeXaDeCiMaLNoZero [q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf, qA, qB, qC, qD, qE, qF]
          , testParser "parseHeXaDeCiMaLDecimal" parseHeXaDeCiMaL [q0, q1, q2, q3, q4, q5, q6, q7, q8, q9, qa, qb, qc, qd, qe, qf, qA, qB, qC, qD, qE, qF]
          , testParser "parse0" parse0 [q0]
          , testParser "parse1" parse1 [q1]
          , testParser "parse2" parse2 [q2]
          , testParser "parse3" parse3 [q3]
          , testParser "parse4" parse4 [q4]
          , testParser "parse5" parse5 [q5]
          , testParser "parse6" parse6 [q6]
          , testParser "parse7" parse7 [q7]
          , testParser "parse8" parse8 [q8]
          , testParser "parse9" parse9 [q9]
          , testParser "parsea" parsea [qa]
          , testParser "parseb" parseb [qb]
          , testParser "parsec" parsec [qc]
          , testParser "parsed" parsed [qd]
          , testParser "parsee" parsee [qe]
          , testParser "parsef" parsef [qf]
          , testParser "parseA" parseA [qA]
          , testParser "parseB" parseB [qB]
          , testParser "parseC" parseC [qC]
          , testParser "parseD" parseD [qD]
          , testParser "parseE" parseE [qE]
          , testParser "parseF" parseF [qF]
          , testParser "parseAa" parseAa [qA, qa]
          , testParser "parseBb" parseBb [qB, qb]
          , testParser "parseCc" parseCc [qC, qc]
          , testParser "parseDd" parseDd [qD, qd]
          , testParser "parseEe" parseEe [qE, qe]
          , testParser "parseFf" parseFf [qF, qf]
          ]
