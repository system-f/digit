{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main(
  main
) where

import Data.Digit
import Numeric.Natural (Natural)
import Prelude (Char, Integral, Int, String, Eq, Show, IO, Integer, minBound, maxBound, fromIntegral,fst,notElem)

import Control.Category ((.))
import Control.Monad ((>>=))
import Control.Applicative ((<*))
import Control.Lens (Prism', (^?), (#))

import Data.Semigroup ((<>))
import Data.Monoid (mconcat)
import Data.Functor ((<$>),fmap)
import Data.Function (($))
import Data.Maybe (Maybe (..))
import Data.Either (Either (..),isLeft)
import Data.List.NonEmpty (NonEmpty)

import Hedgehog(Property, Gen, forAll, property, assert, (===))

import Test.Tasty(TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog(testProperty)
import Test.Tasty.HUnit(testCase, (@?=))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Text.Parsec(parse, ParseError, Parsec, eof)

testPrism ::
  (Show x, Eq x) =>
  Gen x
  -> String
  -> Prism' x HeXDigit
  -> [(x, HeXDigit)]
  -> [TestTree]
testPrism g n p x =
  testProperty
    (n <> " prism invalid values")
    (
      property $
        do  c <- forAll . Gen.filter (`notElem` (fmap fst x)) $ g
            (c ^? p :: Maybe HeXDigit) === Nothing
    ) :
    (
      x >>= \(c, d) ->
        [
          testCase
            (n <> " prism ->")
            ((c ^? p :: Maybe HeXDigit) @?= Just d)
        , testCase
            (n <> " prism <-")
            (p # d @?= c)
        ]
    )

testParser ::
  String
  -> Parsec String () HeXDigit
  -> [(Char, HeXDigit)]
  -> [TestTree]
testParser n p x =
    (
      let n' = n <> " parser invalid values"
      in  testProperty
            n'
            (
              property $
                do  c <- forAll . Gen.filter (`notElem` (fmap fst x)) . Gen.choice $ [Gen.hexit, Gen.unicode]
                    assert (isLeft (parse p (n' <> " test") [c] :: Either ParseError HeXDigit))
            )
    ) :
    (
      x >>= \(c, d) ->
        [
          let n' = n <> " parses exactly one character"
          in  testCase
                n'
                ((parse (p <* eof) (n' <> " test") [c] :: Either ParseError HeXDigit) @?= Right d)
        , let n' = n <> " parses the correct digit"
          in  testCase
                n'
                ((parse p (n' <> "test") (c:"xyz") :: Either ParseError HeXDigit) @?= Right d)
            ]
    )

integralPrism ::
  (Show n, Integral n) =>
  String
  -> Prism' n HeXDigit
  -> [(n, HeXDigit)]
  -> [TestTree]
integralPrism =
  testPrism (Gen.choice [Gen.integral (Range.linear 0 9), Gen.integral (Range.linear 10 99999), Gen.integral (Range.linear (-1) (-99999))])

charPrism ::
  String
  -> Prism' Char HeXDigit
  -> [(Char, HeXDigit)]
  -> [TestTree]
charPrism =
  testPrism (Gen.choice [Gen.hexit, Gen.unicode])

genNatural :: Gen Natural
genNatural = fromIntegral <$> Gen.integral (Range.linear 0 (maxBound :: Int))

prop_natural_digits_roundtrip :: Property
prop_natural_digits_roundtrip = property $ do
  n <- forAll genNatural
  digitsToNatural (naturalToDigits n) === Just n

digitNaturalTests :: TestTree
digitNaturalTests = testGroup "digit Natural tests"
  [ testProperty "Natural <-> NonEmpty Digit" prop_natural_digits_roundtrip
  ]

prop_integer_X :: (Integer -> a) -> (a -> Integer) -> Property
prop_integer_X iToX xToI =
  property $ do
    n <- forAll .
      Gen.integral $
        Range.constant
          (fromIntegral (minBound :: Int))
          (fromIntegral (maxBound :: Int))
    xToI (iToX n) === n

prop_X_integer
  :: (Eq a, Show a)
  => Gen a
  -> (Integer -> Either (NonEmpty a) (NonEmpty a))
  -> (Either (NonEmpty a) (NonEmpty a) -> Integer)
  -> Property
prop_X_integer genX iToX xToI =
  property $ do
    let digits = Gen.nonEmpty (Range.constant 1 50) genX
    ds <- forAll $ Gen.choice [Left <$> digits, Right <$> digits]
    xToI (iToX (xToI ds)) === xToI ds

toFromIntegralTests :: TestTree
toFromIntegralTests =
  testGroup "To and from integers"
  [ testProperty "Integer <-> BinDigit" $
      prop_integer_X integralBinDigits binDigitsIntegral
  , testProperty "BinDigit <-> Integer" $
      prop_X_integer (Gen.element enumBinary) integralBinDigits binDigitsIntegral
  , testProperty "Integer <-> OctDigit" $
      prop_integer_X integralOctDigits octDigitsIntegral
  , testProperty "OctDigit <-> Integer" $
      prop_X_integer (Gen.element enumOctal) integralOctDigits octDigitsIntegral
  , testProperty "Integer <-> DecDigit" $
      prop_integer_X integralDecDigits decDigitsIntegral
  , testProperty "DecDigit <-> Integer" $
      prop_X_integer (Gen.element enumDecimal) integralDecDigits decDigitsIntegral
  , testProperty "Integer <-> HexDigit" $
      prop_integer_X integralHexDigits hexDigitsIntegral
  , testProperty "HexDigit <-> Integer" $
      prop_X_integer (Gen.element enumHexadecimal) integralHexDigits hexDigitsIntegral
  , testProperty "Integer <-> HEXDigit" $
      prop_integer_X integralHEXDigits _HEXDigitsIntegral
  , testProperty "HEXDigit <-> Integer" $
      prop_X_integer (Gen.element enumHEXADECIMAL) integralHEXDigits _HEXDigitsIntegral
  ]

main ::
  IO ()
main = defaultMain $
  testGroup "All Digit Tests"
    [ digitBaseTests
    , digitNaturalTests
    , toFromIntegralTests
    ]

digitBaseTests :: TestTree
digitBaseTests =
  testGroup "digit parser/prism tests" $
    let q0  = ('0', HeXDigit0)
        q1  = ('1', HeXDigit1)
        q2  = ('2', HeXDigit2)
        q3  = ('3', HeXDigit3)
        q4  = ('4', HeXDigit4)
        q5  = ('5', HeXDigit5)
        q6  = ('6', HeXDigit6)
        q7  = ('7', HeXDigit7)
        q8  = ('8', HeXDigit8)
        q9  = ('9', HeXDigit9)
        qa  = ('a', HeXDigita)
        qb  = ('b', HeXDigitb)
        qc  = ('c', HeXDigitc)
        qd  = ('d', HeXDigitd)
        qe  = ('e', HeXDigite)
        qf  = ('f', HeXDigitf)
        qA  = ('A', HeXDigitA)
        qB  = ('B', HeXDigitB)
        qC  = ('C', HeXDigitC)
        qD  = ('D', HeXDigitD)
        qE  = ('E', HeXDigitE)
        qF  = ('F', HeXDigitF)
        r0 :: (Integer, HeXDigit)
        r0  = (0  , HeXDigit0)
        r1 :: (Integer, HeXDigit)
        r1  = (1  , HeXDigit1)
        r2 :: (Integer, HeXDigit)
        r2  = (2  , HeXDigit2)
        r3 :: (Integer, HeXDigit)
        r3  = (3  , HeXDigit3)
        r4 :: (Integer, HeXDigit)
        r4  = (4  , HeXDigit4)
        r5 :: (Integer, HeXDigit)
        r5  = (5  , HeXDigit5)
        r6 :: (Integer, HeXDigit)
        r6  = (6  , HeXDigit6)
        r7 :: (Integer, HeXDigit)
        r7  = (7  , HeXDigit7)
        r8 :: (Integer, HeXDigit)
        r8  = (8  , HeXDigit8)
        r9 :: (Integer, HeXDigit)
        r9  = (9  , HeXDigit9)
        ra :: (Integer, HeXDigit)
        ra =  (10 , HeXDigita)
        rb :: (Integer, HeXDigit)
        rb =  (11 , HeXDigitb)
        rc :: (Integer, HeXDigit)
        rc =  (12 , HeXDigitc)
        rd :: (Integer, HeXDigit)
        rd =  (13 , HeXDigitd)
        re :: (Integer, HeXDigit)
        re =  (14 , HeXDigite)
        rf :: (Integer, HeXDigit)
        rf =  (15 , HeXDigitf)
        rA :: (Integer, HeXDigit)
        rA =  (10 , HeXDigitA)
        rB :: (Integer, HeXDigit)
        rB =  (11 , HeXDigitB)
        rC :: (Integer, HeXDigit)
        rC =  (12 , HeXDigitC)
        rD :: (Integer, HeXDigit)
        rD =  (13 , HeXDigitD)
        rE :: (Integer, HeXDigit)
        rE =  (14 , HeXDigitE)
        rF :: (Integer, HeXDigit)
        rF =  (15 , HeXDigitF)
    in  mconcat [
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
