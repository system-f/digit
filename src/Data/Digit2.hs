{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit2 {- (
  BinaryNoZero
, Binary
, parseBinaryNoZero
, parseBinary
, BinaryNoZeroDigit
, BinaryDigit
, BinaryDigit'
----
, OctalNoZero
, Octal
, parseOctalNoZero
, parseOctal
, OctalNoZeroDigit
, OctalNoZeroDigit'
, OctalDigit
, OctalDigit'
----
, DecimalNoZero
, Decimal
, parseDecimalNoZero
, parseDecimal
, DecimalNoZeroDigit
, DecimalNoZeroDigit'
, DecimalDigit
, DecimalDigit'
----
, HexadecimalNoZero
, Hexadecimal
, parseHexadecimalNoZero
, parseHexadecimal
, HexadecimalNoZeroDigit
, HexadecimalNoZeroDigit'
, HexadecimalDigit
, HexadecimalDigit'
----
, HeXaDeCiMaLNoZero
, HeXaDeCiMaL
, parseHeXaDeCiMaLNoZero
, parseHeXaDeCiMaL
, HeXaDeCiMaLNoZeroDigit
, HeXaDeCiMaLNoZeroDigit'
, HeXaDeCiMaLDigit
, HeXaDeCiMaLDigit'
----
, HEXADECIMALNoZero
, HEXADECIMAL
, parseHEXADECIMALNoZero
, parseHEXADECIMAL
, HEXADECIMALNoZeroDigit
, HEXADECIMALNoZeroDigit'
, HEXADECIMALDigit
, HEXADECIMALDigit'
----
, D0(..)
, parse0
, D1(..)
, parse1
, D2(..)
, parse2
, D3(..)
, parse3
, D4(..)
, parse4
, D5(..)
, parse5
, D6(..)
, parse6
, D7(..)
, parse7
, D8(..)
, parse8
, D9(..)
, parse9
, DA(..)
, parseA
, DB(..)
, parseB
, DC(..)
, parseC
, DD(..)
, parseD
, DE(..)
, parseE
, DF(..)
, parseF
, Da(..)
, parsea
, Db(..)
, parseb
, Dc(..)
, parsec
, Dd(..)
, parsed
, De(..)
, parsee
, Df(..)
, parsef
, DAa
, parseAa
, DBb
, parseBb
, DCc
, parseCc
, DDd
, parseDd
, DEe
, parseEe
, DFf
, parseFf
, Digit0(..)
, Digit1(..)
, Digit2(..)
, Digit3(..)
, Digit4(..)
, Digit5(..)
, Digit6(..)
, Digit7(..)
, Digit8(..)
, Digit9(..)
, Digita(..)
, Digitb(..)
, Digitc(..)
, Digitd(..)
, Digite(..)
, Digitf(..)
, DigitA(..)
, DigitB(..)
, DigitC(..)
, DigitD(..)
, DigitE(..)
, DigitF(..)
) -} where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

----

----
----

----

----

----

----
----
----

----

----


