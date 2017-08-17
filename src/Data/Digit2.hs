{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit2(
  BinaryNoZero
, Binary
, OctalNoZero
, Octal
, DecimalNoZero
, Decimal
, HexadecimalNoZero
, Hexadecimal
, HeXaDeCiMaLNoZero
, HeXaDeCiMaL
, HEXADECIMALNoZero
, HEXADECIMAL
, D0(..)
, D1(..)
, D2(..)
, D3(..)
, D4(..)
, D5(..)
, D6(..)
, D7(..)
, D8(..)
, D9(..)
, DA(..)
, DB(..)
, DC(..)
, DD(..)
, DE(..)
, DF(..)
, Da(..)
, Db(..)
, Dc(..)
, Dd(..)
, De(..)
, Df(..)
, parse0
, parse1
, parse2
, parse3
, parse4
, parse5
, parse6
, parse7
, parse8
, parse9
, parseA
, parseB
, parseC
, parseD
, parseE
, parseF
, parsea
, parseb
, parsec
, parsed
, parsee
, parsef
, parseAa
, parseBb
, parseCc
, parseDd
, parseEe
, parseFf
, parseBinaryNoZero
, parseBinary
, parseOctalNoZero
, parseOctal
, parseDecimalNoZero
, parseDecimal
, parseHEXADECIMALNoZero
, parseHEXADECIMAL
, parseHexadecimalNoZero
, parseHexadecimal
, parseHeXaDeCiMaLNoZero
, parseHeXaDeCiMaL
, BinaryNoZeroDigit
, BinaryDigit
, BinaryDigit'
, OctalNoZeroDigit
, OctalNoZeroDigit'
, OctalDigit
, OctalDigit'
, DecimalNoZeroDigit
, DecimalNoZeroDigit'
, DecimalDigit
, DecimalDigit'
, HexadecimalNoZeroDigit
, HexadecimalNoZeroDigit'
, HexadecimalDigit
, HexadecimalDigit'
, HeXaDeCiMaLNoZeroDigit
, HeXaDeCiMaLNoZeroDigit'
, HeXaDeCiMaLDigit
, HeXaDeCiMaLDigit'
, HEXADECIMALNoZeroDigit
, HEXADECIMALNoZeroDigit'
, HEXADECIMALDigit
, HEXADECIMALDigit'
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
) where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

-- $setup
-- >>> import Text.Parsec
-- >>> import Data.Void

type BinaryNoZero d =
  D1 d

type Binary d =
  (D0 d, BinaryNoZero d)

type OctalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d)

type Octal d =
  (D0 d, OctalNoZero d)

type DecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d)

type Decimal d =
  (D0 d, DecimalNoZero d)

type HeXaDeCiMaLNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

type HeXaDeCiMaL d =
  (D0 d, HeXaDeCiMaLNoZero d)

type HEXADECIMALNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d)

type HEXADECIMAL d =
  (D0 d, HEXADECIMALNoZero d)

type HexadecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

type Hexadecimal d =
  (D0 d, HexadecimalNoZero d)

class D0 d where
  d0 ::
    Prism'
      d
      ()
  x0 ::
    d
  x0 =
    d0 # ()

instance D0 () where
  d0 =
    id

-- |
--
-- >>> parse (parse0 <* eof) "test" "0" :: Either ParseError (Digit0 ())
-- Right (Digit0 ())
--
-- >>> parse parse0 "test" "0xyz" :: Either ParseError (Digit0 ())
-- Right (Digit0 ())
--
-- >>> isn't _Right (parse parse0 "test" "xyz" :: Either ParseError (Digit0 ()))
-- True
--
-- prop> \c -> c /= '0' ==> isn't _Right (parse parse0 "test" [c] :: Either ParseError (Digit0 ()))
parse0 ::
  (D0 d, CharParsing p) =>
  p d
parse0 =
  x0 <$ char '0' <?> "0"

class D1 d where
  d1 ::
    Prism'
      d
      ()
  x1 ::
    d
  x1 =
    d1 # ()

instance D1 () where
  d1 =
    id

-- |
--
-- >>> parse (parse1 <* eof) "test" "1" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> parse parse1 "test" "1xyz" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> isn't _Right (parse parse1 "test" "xyz" :: Either ParseError (Digit1 ()))
-- True
--
-- prop> \c -> c /= '1' ==> isn't _Right (parse parse1 "test" [c] :: Either ParseError (Digit1 ()))
parse1 ::
  (D1 d, CharParsing p) =>
  p d
parse1 =
  x1 <$ char '1' <?> "1"

class D2 d where
  d2 ::
    Prism'
      d
      ()
  x2 ::
    d
  x2 =
    d2 # ()

instance D2 () where
  d2 =
    id
    
-- |
--
-- >>> parse (parse2 <* eof) "test" "2" :: Either ParseError (Digit2 ())
-- Right (Digit2 ())
--
-- >>> parse parse2 "test" "2xyz" :: Either ParseError (Digit2 ())
-- Right (Digit2 ())
--
-- >>> isn't _Right (parse parse2 "test" "xyz" :: Either ParseError (Digit2 ()))
-- True
--
-- prop> \c -> c /= '2' ==> isn't _Right (parse parse2 "test" [c] :: Either ParseError (Digit2 ()))
parse2 ::
  (D2 d, CharParsing p) =>
  p d
parse2 =
  x2 <$ char '2' <?> "2"

class D3 d where
  d3 ::
    Prism'
      d
      ()
  x3 ::
    d
  x3 =
    d3 # ()

instance D3 () where
  d3 =
    id

-- |
--
-- >>> parse (parse3 <* eof) "test" "3" :: Either ParseError (Digit3 ())
-- Right (Digit3 ())
--
-- >>> parse parse3 "test" "3xyz" :: Either ParseError (Digit3 ())
-- Right (Digit3 ())
--
-- >>> isn't _Right (parse parse3 "test" "xyz" :: Either ParseError (Digit3 ()))
-- True
--
-- prop> \c -> c /= '3' ==> isn't _Right (parse parse3 "test" [c] :: Either ParseError (Digit3 ()))
parse3 ::
  (D3 d, CharParsing p) =>
  p d
parse3 =
  x3 <$ char '3' <?> "3"

class D4 d where
  d4 ::
    Prism'
      d
      ()
  x4 ::
    d
  x4 =
    d4 # ()

instance D4 () where
  d4 =
    id
   
-- |
--
-- >>> parse (parse4 <* eof) "test" "4" :: Either ParseError (Digit4 ())
-- Right (Digit4 ())
--
-- >>> parse parse4 "test" "4xyz" :: Either ParseError (Digit4 ())
-- Right (Digit4 ())
--
-- >>> isn't _Right (parse parse4 "test" "xyz" :: Either ParseError (Digit4 ()))
-- True 
--
-- prop> \c -> c /= '4' ==> isn't _Right (parse parse4 "test" [c] :: Either ParseError (Digit4 ()))
parse4 ::
  (D4 d, CharParsing p) =>
  p d
parse4 =
  x4 <$ char '4' <?> "4"

class D5 d where
  d5 ::
    Prism'
      d
      ()
  x5 ::
    d
  x5 =
    d5 # ()

instance D5 () where
  d5 =
    id
    
-- |
--
-- >>> parse (parse5 <* eof) "test" "5" :: Either ParseError (Digit5 ())
-- Right (Digit5 ())
--
-- >>> parse parse5 "test" "5xyz" :: Either ParseError (Digit5 ())
-- Right (Digit5 ())
--
-- >>> isn't _Right (parse parse5 "test" "xyz" :: Either ParseError (Digit5 ()))
-- True
--
-- prop> \c -> c /= '5' ==> isn't _Right (parse parse5 "test" [c] :: Either ParseError (Digit5 ()))
parse5 ::
  (D5 d, CharParsing p) =>
  p d
parse5 =
  x5 <$ char '5' <?> "5"

class D6 d where
  d6 ::
    Prism'
      d
      ()
  x6 ::
    d
  x6 =
    d6 # ()

instance D6 () where
  d6 =
    id
    
-- |
--
-- >>> parse (parse6 <* eof) "test" "6" :: Either ParseError (Digit6 ())
-- Right (Digit6 ())
--
-- >>> parse parse6 "test" "6xyz" :: Either ParseError (Digit6 ())
-- Right (Digit6 ())
--
-- >>> isn't _Right (parse parse6 "test" "xyz" :: Either ParseError (Digit6 ()))
-- True
--
-- prop> \c -> c /= '6' ==> isn't _Right (parse parse6 "test" [c] :: Either ParseError (Digit6 ()))
parse6 ::
  (D6 d, CharParsing p) =>
  p d
parse6 =
  x6 <$ char '6' <?> "6"

class D7 d where
  d7 ::
    Prism'
      d
      ()
  x7 ::
    d
  x7 =
    d7 # ()

instance D7 () where
  d7 =
    id
    
-- |
--
-- >>> parse (parse7 <* eof) "test" "7" :: Either ParseError (Digit7 ())
-- Right (Digit7 ())
--
-- >>> parse parse7 "test" "7xyz" :: Either ParseError (Digit7 ())
-- Right (Digit7 ())
--
-- >>> isn't _Right (parse parse7 "test" "xyz" :: Either ParseError (Digit7 ()))
-- True
--
-- prop> \c -> c /= '7' ==> isn't _Right (parse parse7 "test" [c] :: Either ParseError (Digit7 ()))
parse7 ::
  (D7 d, CharParsing p) =>
  p d
parse7 =
  x7 <$ char '7' <?> "7"

class D8 d where
  d8 ::
    Prism'
      d
      ()
  x8 ::
    d
  x8 =
    d8 # ()

instance D8 () where
  d8 =
    id
    
-- |
--
-- >>> parse (parse8 <* eof) "test" "8" :: Either ParseError (Digit8 ())
-- Right (Digit8 ())
--
-- >>> parse parse8 "test" "8xyz" :: Either ParseError (Digit8 ())
-- Right (Digit8 ())
--
-- >>> isn't _Right (parse parse8 "test" "xyz" :: Either ParseError (Digit8 ()))
-- True
--
-- prop> \c -> c /= '8' ==> isn't _Right (parse parse8 "test" [c] :: Either ParseError (Digit8 ()))
parse8 ::
  (D8 d, CharParsing p) =>
  p d
parse8 =
  x8 <$ char '8' <?> "8"

class D9 d where
  d9 ::
    Prism'
      d
      ()
  x9 ::
    d
  x9 =
    d9 # ()

instance D9 () where
  d9 =
    id
    
-- |
--
-- >>> parse (parse9 <* eof) "test" "9" :: Either ParseError (Digit9 ())
-- Right (Digit9 ())
--
-- >>> parse parse9 "test" "9xyz" :: Either ParseError (Digit9 ())
-- Right (Digit9 ())
--
-- >>> isn't _Right (parse parse9 "test" "xyz" :: Either ParseError (Digit9 ()))
-- True
--
-- prop> \c -> c /= '9' ==> isn't _Right (parse parse9 "test" [c] :: Either ParseError (Digit9 ()))
parse9 ::
  (D9 d, CharParsing p) =>
  p d
parse9 =
  x9 <$ char '9' <?> "9"

class DA d where
  dA ::
    Prism'
      d
      ()
  xA ::
    d
  xA =
    dA # ()

instance DA () where
  dA =
    id

-- |
--
-- >>> parse (parseA <* eof) "test" "A" :: Either ParseError (DigitA ())
-- Right (DigitA ())
--
-- >>> parse parseA "test" "Axyz" :: Either ParseError (DigitA ())
-- Right (DigitA ())
--
-- >>> isn't _Right (parse parseA "test" "xyz" :: Either ParseError (DigitA ()))
-- True
--
-- prop> \c -> c /= 'A' ==> isn't _Right (parse parseA "test" [c] :: Either ParseError (DigitA ()))
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"

class DB d where
  dB ::
    Prism'
      d
      ()
  xB ::
    d
  xB =
    dB # ()

instance DB () where
  dB =
    id
    
-- |
--
-- >>> parse (parseB <* eof) "test" "B" :: Either ParseError (DigitB ())
-- Right (DigitB ())
--
-- >>> parse parseB "test" "Bxyz" :: Either ParseError (DigitB ())
-- Right (DigitB ())
--
-- >>> isn't _Right (parse parseB "test" "xyz" :: Either ParseError (DigitB ()))
-- True
--
-- prop> \c -> c /= 'B' ==> isn't _Right (parse parseB "test" [c] :: Either ParseError (DigitB ()))
parseB ::
  (DB d, CharParsing p) =>
  p d
parseB =
  xB <$ char 'B' <?> "B"

class DC d where
  dC ::
    Prism'
      d
      ()
  xC ::
    d
  xC =
    dC # ()

instance DC () where
  dC =
    id
    
-- |
--
-- >>> parse (parseC <* eof) "test" "C" :: Either ParseError (DigitC ())
-- Right (DigitC ())
--
-- >>> parse parseC "test" "Cxyz" :: Either ParseError (DigitC ())
-- Right (DigitC ())
--
-- >>> isn't _Right (parse parseC "test" "xyz" :: Either ParseError (DigitC ()))
-- True
--
-- prop> \c -> c /= 'C' ==> isn't _Right (parse parseC "test" [c] :: Either ParseError (DigitC ()))
parseC ::
  (DC d, CharParsing p) =>
  p d
parseC =
  xC <$ char 'C' <?> "C"

class DD d where
  dD ::
    Prism'
      d
      ()
  xD ::
    d
  xD =
    dD # ()

instance DD () where
  dD =
    id
    
-- |
--
-- >>> parse (parseD <* eof) "test" "D" :: Either ParseError (DigitD ())
-- Right (DigitD ())
--
-- >>> parse parseD "test" "Dxyz" :: Either ParseError (DigitD ())
-- Right (DigitD ())
--
-- >>> isn't _Right (parse parseD "test" "xyz" :: Either ParseError (DigitD ()))
-- True
--
-- prop> \c -> c /= 'D' ==> isn't _Right (parse parseD "test" [c] :: Either ParseError (DigitD ()))
parseD ::
  (DD d, CharParsing p) =>
  p d
parseD =
  xD <$ char 'D' <?> "D"

class DE d where
  dE ::
    Prism'
      d
      ()
  xE ::
    d
  xE =
    dE # ()

instance DE () where
  dE =
    id
    
-- |
--
-- >>> parse (parseE <* eof) "test" "E" :: Either ParseError (DigitE ())
-- Right (DigitE ())
--
-- >>> parse parseE "test" "Exyz" :: Either ParseError (DigitE ())
-- Right (DigitE ())
--
-- >>> isn't _Right (parse parseE "test" "xyz" :: Either ParseError (DigitE ()))
-- True
--
-- prop> \c -> c /= 'E' ==> isn't _Right (parse parseE "test" [c] :: Either ParseError (DigitE ()))
parseE ::
  (DE d, CharParsing p) =>
  p d
parseE =
  xE <$ char 'E' <?> "E"

class DF d where
  dF ::
    Prism'
      d
      ()
  xF ::
    d
  xF =
    dF # ()

instance DF () where
  dF =
    id
    
-- |
--
-- >>> parse (parseF <* eof) "test" "F" :: Either ParseError (DigitF ())
-- Right (DigitF ())
--
-- >>> parse parseF "test" "Fxyz" :: Either ParseError (DigitF ())
-- Right (DigitF ())
--
-- >>> isn't _Right (parse parseF "test" "xyz" :: Either ParseError (DigitF ()))
-- True
--
-- prop> \c -> c /= 'F' ==> isn't _Right (parse parseF "test" [c] :: Either ParseError (DigitF ()))
parseF ::
  (DF d, CharParsing p) =>
  p d
parseF =
  xF <$ char 'F' <?> "F"

class Da d where
  da ::
    Prism'
      d
      ()
  xa ::
    d
  xa =
    da # ()

instance Da () where
  da =
    id
    
-- |
--
-- >>> parse (parsea <* eof) "test" "a" :: Either ParseError (Digita ())
-- Right (Digita ())
--
-- >>> parse parsea "test" "axyz" :: Either ParseError (Digita ())
-- Right (Digita ())
--
-- >>> isn't _Right (parse parsea "test" "xyz" :: Either ParseError (Digita ()))
-- True
--
-- prop> \c -> c /= 'a' ==> isn't _Right (parse parsea "test" [c] :: Either ParseError (Digita ()))
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"

class Db d where
  db ::
    Prism'
      d
      ()
  xb ::
    d
  xb =
    db # ()

instance Db () where
  db =
    id
    
-- |
--
-- >>> parse (parseb <* eof) "test" "b" :: Either ParseError (Digitb ())
-- Right (Digitb ())
--
-- >>> parse parseb "test" "bxyz" :: Either ParseError (Digitb ())
-- Right (Digitb ())
--
-- >>> isn't _Right (parse parseb "test" "xyz" :: Either ParseError (Digitb ()))
-- True
--
-- prop> \c -> c /= 'b' ==> isn't _Right (parse parseb "test" [c] :: Either ParseError (Digitb ()))
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"

class Dc d where
  dc ::
    Prism'
      d
      ()
  xc ::
    d
  xc =
    dc # ()

instance Dc () where
  dc =
    id
   
-- |
--
-- >>> parse (parsec <* eof) "test" "c" :: Either ParseError (Digitc ())
-- Right (Digitc ())
--
-- >>> parse parsec "test" "cxyz" :: Either ParseError (Digitc ())
-- Right (Digitc ())
--
-- >>> isn't _Right (parse parsec "test" "xyz" :: Either ParseError (Digitc ()))
-- True
--
-- prop> \c -> c /= 'c' ==> isn't _Right (parse parsec "test" [c] :: Either ParseError (Digitc ()))
parsec ::
  (Dc d, CharParsing p) =>
  p d
parsec =
  xc <$ char 'c' <?> "c"

class Dd d where
  dd ::
    Prism'
      d
      ()
  xd ::
    d
  xd =
    dd # ()

instance Dd () where
  dd =
    id
    
-- |
--
-- >>> parse (parsed <* eof) "test" "d" :: Either ParseError (Digitd ())
-- Right (Digitd ())
--
-- >>> parse parsed "test" "dxyz" :: Either ParseError (Digitd ())
-- Right (Digitd ())
--
-- >>> isn't _Right (parse parsed "test" "xyz" :: Either ParseError (Digitd ()))
-- True
--
-- prop> \c -> c /= 'd' ==> isn't _Right (parse parsed "test" [c] :: Either ParseError (Digitd ()))
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"

class De d where
  de ::
    Prism'
      d
      ()
  xe ::
    d
  xe =
    de # ()

instance De () where
  de =
    id
    
-- |
--
-- >>> parse (parsee <* eof) "test" "e" :: Either ParseError (Digite ())
-- Right (Digite ())
--
-- >>> parse parsee "test" "exyz" :: Either ParseError (Digite ())
-- Right (Digite ())
--
-- >>> isn't _Right (parse parsee "test" "xyz" :: Either ParseError (Digite ()))
-- True
--
-- prop> \c -> c /= 'e' ==> isn't _Right (parse parsee "test" [c] :: Either ParseError (Digite ()))
parsee ::
  (De d, CharParsing p) =>
  p d
parsee =
  xe <$ char 'e' <?> "e"

class Df d where
  df ::
    Prism'
      d
      ()
  xf ::
    d
  xf =
    df # ()

instance Df () where
  df =
    id
    
-- |
--
-- >>> parse (parsef <* eof) "test" "f" :: Either ParseError (Digitf ())
-- Right (Digitf ())
--
-- >>> parse parsef "test" "fxyz" :: Either ParseError (Digitf ())
-- Right (Digitf ())
--
-- >>> isn't _Right (parse parsef "test" "xyz" :: Either ParseError (Digitf ()))
-- True
--
-- prop> \c -> c /= 'f' ==> isn't _Right (parse parsef "test" [c] :: Either ParseError (Digitf ()))
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"

-- |
--
-- >>> parse (parseAa <* eof) "test" "A" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseAa <* eof) "test" "a" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseAa "test" "Axyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseAa "test" "axyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseAa "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Aa") ==> isn't _Right (parse parseAa "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseAa ::
  (DA d, Da d, CharParsing p) =>
  p d
parseAa =
  choice [parseA, parsea]

-- |
--
-- >>> parse (parseBb <* eof) "test" "B" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseBb <* eof) "test" "b" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseBb "test" "Bxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseBb "test" "bxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseBb "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Bb") ==> isn't _Right (parse parseBb "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseBb ::
  (DB d, Db d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb]

-- |
--
-- >>> parse (parseCc <* eof) "test" "C" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseCc <* eof) "test" "c" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseCc "test" "Cxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseCc "test" "cxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseCc "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Cc") ==> isn't _Right (parse parseCc "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseCc ::
  (DC d, Dc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec]

-- |
--
-- >>> parse (parseDd <* eof) "test" "D" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDd <* eof) "test" "d" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseDd "test" "Dxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseDd "test" "dxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseDd "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Dd") ==> isn't _Right (parse parseDd "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseDd ::
  (DD d, Dd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed]

-- |
--
-- >>> parse (parseEe <* eof) "test" "E" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseEe <* eof) "test" "e" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseEe "test" "Exyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseEe "test" "exyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseEe "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Ee") ==> isn't _Right (parse parseEe "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseEe ::
  (DE d, De d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee]

-- |
--
-- >>> parse (parseFf <* eof) "test" "F" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseFf <* eof) "test" "f" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseFf "test" "Fxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseFf "test" "fxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseFf "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "Ff") ==> isn't _Right (parse parseFf "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseFf ::
  (DF d, Df d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef]

-- |
--
-- >>> parse (parseBinaryNoZero <* eof) "test" "1" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> parse parseBinaryNoZero "test" "1xyz" :: Either ParseError (Digit1 ())
-- Right (Digit1 ())
--
-- >>> isn't _Right (parse parseBinaryNoZero "test" "xyz" :: Either ParseError (Digit1 ()))
-- True
--
-- prop> \c -> (c `notElem` "1") ==> isn't _Right (parse parseBinaryNoZero "test" [c] :: Either ParseError (Digit1 ()))
parseBinaryNoZero ::
  (BinaryNoZero d, CharParsing p) =>
  p d
parseBinaryNoZero =
  parse1 <?> "BinaryNoZero"

-- |
--
-- >>> parse (parseBinary <* eof) "test" "0" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> parse parseBinary "test" "0xyz" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> parse (parseBinary <* eof) "test" "1" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> parse parseBinary "test" "1xyz" :: Either ParseError (BinaryDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseBinary "test" "xyz" :: Either ParseError (BinaryDigit' ()))
-- True
--
-- prop> \c -> (c `notElem` "01") ==> isn't _Right (parse parseBinary "test" [c] :: Either ParseError (BinaryDigit' ()))
parseBinary ::
  (Binary d, CharParsing p) =>
  p d
parseBinary =
  choice
    [
      parse0
    , parseBinaryNoZero
    ] <?> "Binary"

-- |
--
-- >>> parse (parseOctalNoZero <* eof) "test" "1" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "1xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "2" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "2xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "3" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "3xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "4" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "4xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "5" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "5xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "6" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "6xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctalNoZero <* eof) "test" "7" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctalNoZero "test" "7xyz" :: Either ParseError (OctalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseOctalNoZero "test" "xyz" :: Either ParseError (OctalNoZeroDigit' ()))
-- True
parseOctalNoZero ::
  (OctalNoZero d, CharParsing p) =>
  p d
parseOctalNoZero =
  choice
    [
      parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    ] <?> "OctalNoZero"

-- |
--
-- >>> parse (parseOctal <* eof) "test" "0" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "1xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "0" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "1xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "2" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "2xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "3" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "3xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "4" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "4xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "5" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "5xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "6" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "6xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseOctal <* eof) "test" "7" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> parse parseOctal "test" "7xyz" :: Either ParseError (OctalDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseOctal "test" "xyz" :: Either ParseError (OctalDigit' ()))
-- True
parseOctal ::
  (Octal d, CharParsing p) =>
  p d
parseOctal =
  choice
    [
      parse0
    , parseOctalNoZero
    ] <?> "Octal"

-- |
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "1" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "1xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "2" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "2xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "3" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "3xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "4" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "4xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "5" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "5xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "6" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "6xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "7" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "7xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "8" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "8xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimalNoZero <* eof) "test" "9" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimalNoZero "test" "9xyz" :: Either ParseError (DecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseDecimalNoZero "test" "xyz" :: Either ParseError (DecimalNoZeroDigit' ()))
-- True
parseDecimalNoZero ::
  (DecimalNoZero d, CharParsing p) =>
  p d
parseDecimalNoZero =
  choice
    [
      parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    , parse8
    , parse9
    ] <?> "DecimalNoZero"


-- |
--
-- >>> parse (parseDecimal <* eof) "test" "0" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "0xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "1" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "1xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "2" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "2xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "3" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "3xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "4" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "4xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "5" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "5xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "6" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "6xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "7" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "7xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "8" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "8xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseDecimal <* eof) "test" "9" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseDecimal "test" "9xyz" :: Either ParseError (DecimalDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseDecimal "test" "xyz" :: Either ParseError (DecimalDigit' ()))
-- True
parseDecimal ::
  (Decimal d, CharParsing p) =>
  p d
parseDecimal =
  choice
    [
      parse0
    , parseDecimalNoZero
    ] <?> "Decimal"

-- |
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "1" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "1xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "2" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "2xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "3" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "3xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "4" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "4xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "5" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "5xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "6" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "6xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "7" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "7xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "8" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "8xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "9" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "9xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "A" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Axyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "B" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Bxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "C" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Cxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "D" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Dxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "E" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Exyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMALNoZero <* eof) "test" "F" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMALNoZero "test" "Fxyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHEXADECIMALNoZero "test" "xyz" :: Either ParseError (HEXADECIMALNoZeroDigit' ()))
-- True
parseHEXADECIMALNoZero ::
  (HEXADECIMALNoZero d, CharParsing p) =>
  p d
parseHEXADECIMALNoZero =
  choice
    [
      parseDecimalNoZero
    , parseA
    , parseB
    , parseC
    , parseD
    , parseE
    , parseF
    ] <?> "HEXADECIMALNoZero"


-- |
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "0" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "0xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "1" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "1xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "2" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "2xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "3" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "3xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "4" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "4xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "5" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "5xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "6" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "6xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "7" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "7xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "8" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "8xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "9" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "9xyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "A" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Axyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "B" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Bxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "C" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Cxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "D" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Dxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "E" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Exyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHEXADECIMAL <* eof) "test" "F" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> parse parseHEXADECIMAL "test" "Fxyz" :: Either ParseError (HEXADECIMALDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHEXADECIMAL "test" "xyz" :: Either ParseError (HEXADECIMALDigit' ()))
-- True
parseHEXADECIMAL ::
  (HEXADECIMAL d, CharParsing p) =>
  p d
parseHEXADECIMAL =
  choice
    [
      parse0
    , parseHEXADECIMALNoZero
    ] <?> "HEXADECIMAL"

-- |
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "1" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "1xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "2" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "2xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "3" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "3xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "4" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "4xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "5" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "5xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "6" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "6xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "7" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "7xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "8" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "8xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "9" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "9xyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "a" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "axyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "b" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "bxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "c" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "cxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "d" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "dxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "e" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "exyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimalNoZero <* eof) "test" "f" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimalNoZero "test" "fxyz" :: Either ParseError (HexadecimalNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHexadecimalNoZero "test" "xyz" :: Either ParseError (HexadecimalNoZeroDigit' ()))
-- True
parseHexadecimalNoZero ::
  (HexadecimalNoZero d, CharParsing p) =>
  p d
parseHexadecimalNoZero =
  choice
    [
      parseDecimalNoZero
    , parsea
    , parseb
    , parsec
    , parsed
    , parsee
    , parsef
    ] <?> "HexadecimalNoZero"

-- |
--
-- >>> parse (parseHexadecimal <* eof) "test" "0" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "0xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "0" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "0xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "2" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "2xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "3" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "3xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "4" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "4xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "5" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "5xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "6" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "6xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "7" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "7xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "8" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "8xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "9" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "9xyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "a" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "axyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "b" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "bxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "c" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "cxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "d" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "dxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "e" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "exyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHexadecimal <* eof) "test" "f" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> parse parseHexadecimal "test" "fxyz" :: Either ParseError (HexadecimalDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHexadecimal "test" "xyz" :: Either ParseError (HexadecimalDigit' ()))
-- True
parseHexadecimal ::
  (Hexadecimal d, CharParsing p) =>
  p d
parseHexadecimal =
  choice
    [
      parse0
    , parseHexadecimalNoZero
    ] <?> "Hexadecimal"

-- |
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "1" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "1xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "2" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "2xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "3" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "3xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "4" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "4xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "5" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "5xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "6" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "6xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "7" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "7xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "8" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "8xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "9" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "9xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "a" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "axyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "b" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "bxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "c" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "cxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "d" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "dxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "e" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "exyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "f" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "fxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "A" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Axyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "B" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Bxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "C" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Cxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "D" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Dxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "E" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Exyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "F" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Fxyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHeXaDeCiMaLNoZero "test" "xyz" :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ()))
-- True
parseHeXaDeCiMaLNoZero ::
  (HeXaDeCiMaLNoZero d, CharParsing p) =>
  p d
parseHeXaDeCiMaLNoZero =
  choice
    [
      parseDecimalNoZero
    , parseAa
    , parseBb
    , parseCc
    , parseDd
    , parseEe
    , parseFf
    ] <?> "HeXaDeCiMaLNoZero"

-- |
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "1" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "1xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "2" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "2xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "3" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "3xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "4" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "4xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "5" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "5xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "6" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "6xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "7" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "7xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "8" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "8xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "9" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "9xyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "a" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "axyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "b" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "bxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "c" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "cxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "d" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "dxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "e" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "exyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "f" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "fxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "A" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "Axyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "B" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "Bxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "C" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "Cxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "D" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "Dxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "E" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "Exyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "F" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> parse parseHeXaDeCiMaL "test" "Fxyz" :: Either ParseError (HeXaDeCiMaLDigit' ())
-- Right (Left ())
--
-- >>> isn't _Right (parse parseHeXaDeCiMaL "test" "xyz" :: Either ParseError (HeXaDeCiMaLDigit' ()))
-- True
parseHeXaDeCiMaL ::
  (HeXaDeCiMaL d, CharParsing p) =>
  p d
parseHeXaDeCiMaL =
  choice
    [
      parse0
    , parseHeXaDeCiMaLNoZero
    ] <?> "HeXaDeCiMaL"

newtype Digit0 a =
  Digit0 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D0 a => D0 (Digit0 a) where
  d0 =
    _Wrapped . d0

instance Functor Digit0 where
  fmap f (Digit0 a) =
    Digit0 (f a)
    
instance Apply Digit0 where
  Digit0 f <.> Digit0 a =
    Digit0 (f a)

instance Applicative Digit0 where
  pure =
    Digit0
  (<*>) =
    (<.>)

instance Bind Digit0 where
  Digit0 a >>- f =
    f a

instance Monad Digit0 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit0 where
  foldMap f (Digit0 a) = 
    f a

instance Foldable1 Digit0 where
  foldMap1 f (Digit0 a) = 
    f a

instance Traversable Digit0 where
  traverse f (Digit0 a) = 
    Digit0 <$> f a

instance Traversable1 Digit0 where
  traverse1 f (Digit0 a) = 
    Digit0 <$> f a

instance Semigroup a => Semigroup (Digit0 a) where
  Digit0 x <> Digit0 y =
    Digit0 (x <> y)

instance Monoid a => Monoid (Digit0 a) where
  Digit0 x `mappend` Digit0 y =
    Digit0 (x `mappend` y)
  mempty =
    Digit0 mempty

instance Field1 (Digit0 a) (Digit0 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit0 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit0 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit0 where
  itraverse f =
    traverse (f ())

instance Each (Digit0 a) (Digit0 b) a b where
  each =
    traverse

type instance Index (Digit0 a) = 
  ()
type instance IxValue (Digit0 a) =
  a
instance Ixed (Digit0 a) where
  ix () f (Digit0 a) =
    Digit0 <$> f a

makeWrapped ''Digit0

newtype Digit1 a =
  Digit1 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D1 a => D1 (Digit1 a) where
  d1 =
    _Wrapped . d1

instance Functor Digit1 where
  fmap f (Digit1 a) =
    Digit1 (f a)
    
instance Apply Digit1 where
  Digit1 f <.> Digit1 a =
    Digit1 (f a)

instance Applicative Digit1 where
  pure =
    Digit1
  (<*>) =
    (<.>)

instance Bind Digit1 where
  Digit1 a >>- f =
    f a

instance Monad Digit1 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit1 where
  foldMap f (Digit1 a) = 
    f a

instance Foldable1 Digit1 where
  foldMap1 f (Digit1 a) = 
    f a

instance Traversable Digit1 where
  traverse f (Digit1 a) = 
    Digit1 <$> f a

instance Traversable1 Digit1 where
  traverse1 f (Digit1 a) = 
    Digit1 <$> f a

instance Semigroup a => Semigroup (Digit1 a) where
  Digit1 x <> Digit1 y =
    Digit1 (x <> y)

instance Monoid a => Monoid (Digit1 a) where
  Digit1 x `mappend` Digit1 y =
    Digit1 (x `mappend` y)
  mempty =
    Digit1 mempty

instance Field1 (Digit1 a) (Digit1 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit1 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit1 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit1 where
  itraverse f =
    traverse (f ())

instance Each (Digit1 a) (Digit1 b) a b where
  each =
    traverse

type instance Index (Digit1 a) = 
  ()
type instance IxValue (Digit1 a) =
  a
instance Ixed (Digit1 a) where
  ix () f (Digit1 a) =
    Digit1 <$> f a

makeWrapped ''Digit1

newtype Digit2 a =
  Digit2 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D2 a => D2 (Digit2 a) where
  d2 =
    _Wrapped . d2

instance Functor Digit2 where
  fmap f (Digit2 a) =
    Digit2 (f a)
    
instance Apply Digit2 where
  Digit2 f <.> Digit2 a =
    Digit2 (f a)

instance Applicative Digit2 where
  pure =
    Digit2
  (<*>) =
    (<.>)

instance Bind Digit2 where
  Digit2 a >>- f =
    f a

instance Monad Digit2 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit2 where
  foldMap f (Digit2 a) = 
    f a

instance Foldable1 Digit2 where
  foldMap1 f (Digit2 a) = 
    f a

instance Traversable Digit2 where
  traverse f (Digit2 a) = 
    Digit2 <$> f a

instance Traversable1 Digit2 where
  traverse1 f (Digit2 a) = 
    Digit2 <$> f a

instance Semigroup a => Semigroup (Digit2 a) where
  Digit2 x <> Digit2 y =
    Digit2 (x <> y)

instance Monoid a => Monoid (Digit2 a) where
  Digit2 x `mappend` Digit2 y =
    Digit2 (x `mappend` y)
  mempty =
    Digit2 mempty

instance Field1 (Digit2 a) (Digit2 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit2 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit2 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit2 where
  itraverse f =
    traverse (f ())

instance Each (Digit2 a) (Digit2 b) a b where
  each =
    traverse

type instance Index (Digit2 a) = 
  ()
type instance IxValue (Digit2 a) =
  a
instance Ixed (Digit2 a) where
  ix () f (Digit2 a) =
    Digit2 <$> f a

makeWrapped ''Digit2

newtype Digit3 a =
  Digit3 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D3 a => D3 (Digit3 a) where
  d3 =
    _Wrapped . d3

instance Functor Digit3 where
  fmap f (Digit3 a) =
    Digit3 (f a)
    
instance Apply Digit3 where
  Digit3 f <.> Digit3 a =
    Digit3 (f a)

instance Applicative Digit3 where
  pure =
    Digit3
  (<*>) =
    (<.>)

instance Bind Digit3 where
  Digit3 a >>- f =
    f a

instance Monad Digit3 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit3 where
  foldMap f (Digit3 a) = 
    f a

instance Foldable1 Digit3 where
  foldMap1 f (Digit3 a) = 
    f a

instance Traversable Digit3 where
  traverse f (Digit3 a) = 
    Digit3 <$> f a

instance Traversable1 Digit3 where
  traverse1 f (Digit3 a) = 
    Digit3 <$> f a

instance Semigroup a => Semigroup (Digit3 a) where
  Digit3 x <> Digit3 y =
    Digit3 (x <> y)

instance Monoid a => Monoid (Digit3 a) where
  Digit3 x `mappend` Digit3 y =
    Digit3 (x `mappend` y)
  mempty =
    Digit3 mempty

instance Field1 (Digit3 a) (Digit3 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit3 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit3 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit3 where
  itraverse f =
    traverse (f ())

instance Each (Digit3 a) (Digit3 b) a b where
  each =
    traverse

type instance Index (Digit3 a) = 
  ()
type instance IxValue (Digit3 a) =
  a
instance Ixed (Digit3 a) where
  ix () f (Digit3 a) =
    Digit3 <$> f a

makeWrapped ''Digit3

newtype Digit4 a =
  Digit4 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D4 a => D4 (Digit4 a) where
  d4 =
    _Wrapped . d4

instance Functor Digit4 where
  fmap f (Digit4 a) =
    Digit4 (f a)
    
instance Apply Digit4 where
  Digit4 f <.> Digit4 a =
    Digit4 (f a)

instance Applicative Digit4 where
  pure =
    Digit4
  (<*>) =
    (<.>)

instance Bind Digit4 where
  Digit4 a >>- f =
    f a

instance Monad Digit4 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit4 where
  foldMap f (Digit4 a) = 
    f a

instance Foldable1 Digit4 where
  foldMap1 f (Digit4 a) = 
    f a

instance Traversable Digit4 where
  traverse f (Digit4 a) = 
    Digit4 <$> f a

instance Traversable1 Digit4 where
  traverse1 f (Digit4 a) = 
    Digit4 <$> f a

instance Semigroup a => Semigroup (Digit4 a) where
  Digit4 x <> Digit4 y =
    Digit4 (x <> y)

instance Monoid a => Monoid (Digit4 a) where
  Digit4 x `mappend` Digit4 y =
    Digit4 (x `mappend` y)
  mempty =
    Digit4 mempty

instance Field1 (Digit4 a) (Digit4 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit4 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit4 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit4 where
  itraverse f =
    traverse (f ())

instance Each (Digit4 a) (Digit4 b) a b where
  each =
    traverse

type instance Index (Digit4 a) = 
  ()
type instance IxValue (Digit4 a) =
  a
instance Ixed (Digit4 a) where
  ix () f (Digit4 a) =
    Digit4 <$> f a

makeWrapped ''Digit4

newtype Digit5 a =
  Digit5 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D5 a => D5 (Digit5 a) where
  d5 =
    _Wrapped . d5

instance Functor Digit5 where
  fmap f (Digit5 a) =
    Digit5 (f a)
    
instance Apply Digit5 where
  Digit5 f <.> Digit5 a =
    Digit5 (f a)

instance Applicative Digit5 where
  pure =
    Digit5
  (<*>) =
    (<.>)

instance Bind Digit5 where
  Digit5 a >>- f =
    f a

instance Monad Digit5 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit5 where
  foldMap f (Digit5 a) = 
    f a

instance Foldable1 Digit5 where
  foldMap1 f (Digit5 a) = 
    f a

instance Traversable Digit5 where
  traverse f (Digit5 a) = 
    Digit5 <$> f a

instance Traversable1 Digit5 where
  traverse1 f (Digit5 a) = 
    Digit5 <$> f a

instance Semigroup a => Semigroup (Digit5 a) where
  Digit5 x <> Digit5 y =
    Digit5 (x <> y)

instance Monoid a => Monoid (Digit5 a) where
  Digit5 x `mappend` Digit5 y =
    Digit5 (x `mappend` y)
  mempty =
    Digit5 mempty

instance Field1 (Digit5 a) (Digit5 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit5 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit5 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit5 where
  itraverse f =
    traverse (f ())

instance Each (Digit5 a) (Digit5 b) a b where
  each =
    traverse

type instance Index (Digit5 a) = 
  ()
type instance IxValue (Digit5 a) =
  a
instance Ixed (Digit5 a) where
  ix () f (Digit5 a) =
    Digit5 <$> f a

makeWrapped ''Digit5

newtype Digit6 a =
  Digit6 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D6 a => D6 (Digit6 a) where
  d6  =
    _Wrapped . d6

instance Functor Digit6 where
  fmap f (Digit6 a) =
    Digit6 (f a)
    
instance Apply Digit6 where
  Digit6 f <.> Digit6 a =
    Digit6 (f a)

instance Applicative Digit6 where
  pure =
    Digit6
  (<*>) =
    (<.>)

instance Bind Digit6 where
  Digit6 a >>- f =
    f a

instance Monad Digit6 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit6 where
  foldMap f (Digit6 a) = 
    f a

instance Foldable1 Digit6 where
  foldMap1 f (Digit6 a) = 
    f a

instance Traversable Digit6 where
  traverse f (Digit6 a) = 
    Digit6 <$> f a

instance Traversable1 Digit6 where
  traverse1 f (Digit6 a) = 
    Digit6 <$> f a

instance Semigroup a => Semigroup (Digit6 a) where
  Digit6 x <> Digit6 y =
    Digit6 (x <> y)

instance Monoid a => Monoid (Digit6 a) where
  Digit6 x `mappend` Digit6 y =
    Digit6 (x `mappend` y)
  mempty =
    Digit6 mempty

instance Field1 (Digit6 a) (Digit6 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit6 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit6 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit6 where
  itraverse f =
    traverse (f ())

instance Each (Digit6 a) (Digit6 b) a b where
  each =
    traverse

type instance Index (Digit6 a) = 
  ()
type instance IxValue (Digit6 a) =
  a
instance Ixed (Digit6 a) where
  ix () f (Digit6 a) =
    Digit6 <$> f a

makeWrapped ''Digit6

newtype Digit7 a =
  Digit7 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D7 a => D7 (Digit7 a) where
  d7  =
    _Wrapped . d7

instance Functor Digit7 where
  fmap f (Digit7 a) =
    Digit7 (f a)
    
instance Apply Digit7 where
  Digit7 f <.> Digit7 a =
    Digit7 (f a)

instance Applicative Digit7 where
  pure =
    Digit7
  (<*>) =
    (<.>)

instance Bind Digit7 where
  Digit7 a >>- f =
    f a

instance Monad Digit7 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit7 where
  foldMap f (Digit7 a) = 
    f a

instance Foldable1 Digit7 where
  foldMap1 f (Digit7 a) = 
    f a

instance Traversable Digit7 where
  traverse f (Digit7 a) = 
    Digit7 <$> f a

instance Traversable1 Digit7 where
  traverse1 f (Digit7 a) = 
    Digit7 <$> f a

instance Semigroup a => Semigroup (Digit7 a) where
  Digit7 x <> Digit7 y =
    Digit7 (x <> y)

instance Monoid a => Monoid (Digit7 a) where
  Digit7 x `mappend` Digit7 y =
    Digit7 (x `mappend` y)
  mempty =
    Digit7 mempty

instance Field1 (Digit7 a) (Digit7 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit7 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit7 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit7 where
  itraverse f =
    traverse (f ())

instance Each (Digit7 a) (Digit7 b) a b where
  each =
    traverse

type instance Index (Digit7 a) = 
  ()
type instance IxValue (Digit7 a) =
  a
instance Ixed (Digit7 a) where
  ix () f (Digit7 a) =
    Digit7 <$> f a

makeWrapped ''Digit7

newtype Digit8 a =
  Digit8 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D8 a => D8 (Digit8 a) where
  d8  =
    _Wrapped . d8

instance Functor Digit8 where
  fmap f (Digit8 a) =
    Digit8 (f a)
    
instance Apply Digit8 where
  Digit8 f <.> Digit8 a =
    Digit8 (f a)

instance Applicative Digit8 where
  pure =
    Digit8
  (<*>) =
    (<.>)

instance Bind Digit8 where
  Digit8 a >>- f =
    f a

instance Monad Digit8 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit8 where
  foldMap f (Digit8 a) = 
    f a

instance Foldable1 Digit8 where
  foldMap1 f (Digit8 a) = 
    f a

instance Traversable Digit8 where
  traverse f (Digit8 a) = 
    Digit8 <$> f a

instance Traversable1 Digit8 where
  traverse1 f (Digit8 a) = 
    Digit8 <$> f a

instance Semigroup a => Semigroup (Digit8 a) where
  Digit8 x <> Digit8 y =
    Digit8 (x <> y)

instance Monoid a => Monoid (Digit8 a) where
  Digit8 x `mappend` Digit8 y =
    Digit8 (x `mappend` y)
  mempty =
    Digit8 mempty

instance Field1 (Digit8 a) (Digit8 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit8 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit8 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit8 where
  itraverse f =
    traverse (f ())

instance Each (Digit8 a) (Digit8 b) a b where
  each =
    traverse

type instance Index (Digit8 a) = 
  ()
type instance IxValue (Digit8 a) =
  a
instance Ixed (Digit8 a) where
  ix () f (Digit8 a) =
    Digit8 <$> f a

makeWrapped ''Digit8

newtype Digit9 a =
  Digit9 a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance D9 a => D9 (Digit9 a) where
  d9  =
    _Wrapped . d9

instance Functor Digit9 where
  fmap f (Digit9 a) =
    Digit9 (f a)
    
instance Apply Digit9 where
  Digit9 f <.> Digit9 a =
    Digit9 (f a)

instance Applicative Digit9 where
  pure =
    Digit9
  (<*>) =
    (<.>)

instance Bind Digit9 where
  Digit9 a >>- f =
    f a

instance Monad Digit9 where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digit9 where
  foldMap f (Digit9 a) = 
    f a

instance Foldable1 Digit9 where
  foldMap1 f (Digit9 a) = 
    f a

instance Traversable Digit9 where
  traverse f (Digit9 a) = 
    Digit9 <$> f a

instance Traversable1 Digit9 where
  traverse1 f (Digit9 a) = 
    Digit9 <$> f a

instance Semigroup a => Semigroup (Digit9 a) where
  Digit9 x <> Digit9 y =
    Digit9 (x <> y)

instance Monoid a => Monoid (Digit9 a) where
  Digit9 x `mappend` Digit9 y =
    Digit9 (x `mappend` y)
  mempty =
    Digit9 mempty

instance Field1 (Digit9 a) (Digit9 b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digit9 where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digit9 where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digit9 where
  itraverse f =
    traverse (f ())

instance Each (Digit9 a) (Digit9 b) a b where
  each =
    traverse

type instance Index (Digit9 a) = 
  ()
type instance IxValue (Digit9 a) =
  a
instance Ixed (Digit9 a) where
  ix () f (Digit9 a) =
    Digit9 <$> f a

makeWrapped ''Digit9

newtype Digita a =
  Digita a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Da a => Da (Digita a) where
  da  =
    _Wrapped . da

instance Functor Digita where
  fmap f (Digita a) =
    Digita (f a)
    
instance Apply Digita where
  Digita f <.> Digita a =
    Digita (f a)

instance Applicative Digita where
  pure =
    Digita
  (<*>) =
    (<.>)

instance Bind Digita where
  Digita a >>- f =
    f a

instance Monad Digita where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digita where
  foldMap f (Digita a) = 
    f a

instance Foldable1 Digita where
  foldMap1 f (Digita a) = 
    f a

instance Traversable Digita where
  traverse f (Digita a) = 
    Digita <$> f a

instance Traversable1 Digita where
  traverse1 f (Digita a) = 
    Digita <$> f a

instance Semigroup a => Semigroup (Digita a) where
  Digita x <> Digita y =
    Digita (x <> y)

instance Monoid a => Monoid (Digita a) where
  Digita x `mappend` Digita y =
    Digita (x `mappend` y)
  mempty =
    Digita mempty

instance Field1 (Digita a) (Digita b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digita where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digita where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digita where
  itraverse f =
    traverse (f ())

instance Each (Digita a) (Digita b) a b where
  each =
    traverse

type instance Index (Digita a) = 
  ()
type instance IxValue (Digita a) =
  a
instance Ixed (Digita a) where
  ix () f (Digita a) =
    Digita <$> f a

makeWrapped ''Digita

newtype Digitb a =
  Digitb a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Db a => Db (Digitb a) where
  db  =
    _Wrapped . db

instance Functor Digitb where
  fmap f (Digitb a) =
    Digitb (f a)
    
instance Apply Digitb where
  Digitb f <.> Digitb a =
    Digitb (f a)

instance Applicative Digitb where
  pure =
    Digitb
  (<*>) =
    (<.>)

instance Bind Digitb where
  Digitb a >>- f =
    f a

instance Monad Digitb where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitb where
  foldMap f (Digitb a) = 
    f a

instance Foldable1 Digitb where
  foldMap1 f (Digitb a) = 
    f a

instance Traversable Digitb where
  traverse f (Digitb a) = 
    Digitb <$> f a

instance Traversable1 Digitb where
  traverse1 f (Digitb a) = 
    Digitb <$> f a

instance Semigroup a => Semigroup (Digitb a) where
  Digitb x <> Digitb y =
    Digitb (x <> y)

instance Monoid a => Monoid (Digitb a) where
  Digitb x `mappend` Digitb y =
    Digitb (x `mappend` y)
  mempty =
    Digitb mempty

instance Field1 (Digitb a) (Digitb b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitb where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitb where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitb where
  itraverse f =
    traverse (f ())

instance Each (Digitb a) (Digitb b) a b where
  each =
    traverse

type instance Index (Digitb a) = 
  ()
type instance IxValue (Digitb a) =
  a
instance Ixed (Digitb a) where
  ix () f (Digitb a) =
    Digitb <$> f a

makeWrapped ''Digitb

newtype Digitc a =
  Digitc a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Dc a => Dc (Digitc a) where
  dc  =
    _Wrapped . dc

instance Functor Digitc where
  fmap f (Digitc a) =
    Digitc (f a)
    
instance Apply Digitc where
  Digitc f <.> Digitc a =
    Digitc (f a)

instance Applicative Digitc where
  pure =
    Digitc
  (<*>) =
    (<.>)

instance Bind Digitc where
  Digitc a >>- f =
    f a

instance Monad Digitc where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitc where
  foldMap f (Digitc a) = 
    f a

instance Foldable1 Digitc where
  foldMap1 f (Digitc a) = 
    f a

instance Traversable Digitc where
  traverse f (Digitc a) = 
    Digitc <$> f a

instance Traversable1 Digitc where
  traverse1 f (Digitc a) = 
    Digitc <$> f a

instance Semigroup a => Semigroup (Digitc a) where
  Digitc x <> Digitc y =
    Digitc (x <> y)

instance Monoid a => Monoid (Digitc a) where
  Digitc x `mappend` Digitc y =
    Digitc (x `mappend` y)
  mempty =
    Digitc mempty

instance Field1 (Digitc a) (Digitc b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitc where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitc where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitc where
  itraverse f =
    traverse (f ())

instance Each (Digitc a) (Digitc b) a b where
  each =
    traverse

type instance Index (Digitc a) = 
  ()
type instance IxValue (Digitc a) =
  a
instance Ixed (Digitc a) where
  ix () f (Digitc a) =
    Digitc <$> f a

makeWrapped ''Digitc

newtype Digitd a =
  Digitd a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Dd a => Dd (Digitd a) where
  dd  =
    _Wrapped . dd

instance Functor Digitd where
  fmap f (Digitd a) =
    Digitd (f a)
    
instance Apply Digitd where
  Digitd f <.> Digitd a =
    Digitd (f a)

instance Applicative Digitd where
  pure =
    Digitd
  (<*>) =
    (<.>)

instance Bind Digitd where
  Digitd a >>- f =
    f a

instance Monad Digitd where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitd where
  foldMap f (Digitd a) = 
    f a

instance Foldable1 Digitd where
  foldMap1 f (Digitd a) = 
    f a

instance Traversable Digitd where
  traverse f (Digitd a) = 
    Digitd <$> f a

instance Traversable1 Digitd where
  traverse1 f (Digitd a) = 
    Digitd <$> f a

instance Semigroup a => Semigroup (Digitd a) where
  Digitd x <> Digitd y =
    Digitd (x <> y)

instance Monoid a => Monoid (Digitd a) where
  Digitd x `mappend` Digitd y =
    Digitd (x `mappend` y)
  mempty =
    Digitd mempty

instance Field1 (Digitd a) (Digitd b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitd where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitd where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitd where
  itraverse f =
    traverse (f ())

instance Each (Digitd a) (Digitd b) a b where
  each =
    traverse

type instance Index (Digitd a) = 
  ()
type instance IxValue (Digitd a) =
  a
instance Ixed (Digitd a) where
  ix () f (Digitd a) =
    Digitd <$> f a

makeWrapped ''Digitd

newtype Digite a =
  Digite a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance De a => De (Digite a) where
  de  =
    _Wrapped . de

instance Functor Digite where
  fmap f (Digite a) =
    Digite (f a)
    
instance Apply Digite where
  Digite f <.> Digite a =
    Digite (f a)

instance Applicative Digite where
  pure =
    Digite
  (<*>) =
    (<.>)

instance Bind Digite where
  Digite a >>- f =
    f a

instance Monad Digite where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digite where
  foldMap f (Digite a) = 
    f a

instance Foldable1 Digite where
  foldMap1 f (Digite a) = 
    f a

instance Traversable Digite where
  traverse f (Digite a) = 
    Digite <$> f a

instance Traversable1 Digite where
  traverse1 f (Digite a) = 
    Digite <$> f a

instance Semigroup a => Semigroup (Digite a) where
  Digite x <> Digite y =
    Digite (x <> y)

instance Monoid a => Monoid (Digite a) where
  Digite x `mappend` Digite y =
    Digite (x `mappend` y)
  mempty =
    Digite mempty

instance Field1 (Digite a) (Digite b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digite where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digite where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digite where
  itraverse f =
    traverse (f ())

instance Each (Digite a) (Digite b) a b where
  each =
    traverse

type instance Index (Digite a) = 
  ()
type instance IxValue (Digite a) =
  a
instance Ixed (Digite a) where
  ix () f (Digite a) =
    Digite <$> f a

makeWrapped ''Digite

newtype Digitf a =
  Digitf a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance Df a => Df (Digitf a) where
  df  =
    _Wrapped . df

instance Functor Digitf where
  fmap f (Digitf a) =
    Digitf (f a)
    
instance Apply Digitf where
  Digitf f <.> Digitf a =
    Digitf (f a)

instance Applicative Digitf where
  pure =
    Digitf
  (<*>) =
    (<.>)

instance Bind Digitf where
  Digitf a >>- f =
    f a

instance Monad Digitf where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable Digitf where
  foldMap f (Digitf a) = 
    f a

instance Foldable1 Digitf where
  foldMap1 f (Digitf a) = 
    f a

instance Traversable Digitf where
  traverse f (Digitf a) = 
    Digitf <$> f a

instance Traversable1 Digitf where
  traverse1 f (Digitf a) = 
    Digitf <$> f a

instance Semigroup a => Semigroup (Digitf a) where
  Digitf x <> Digitf y =
    Digitf (x <> y)

instance Monoid a => Monoid (Digitf a) where
  Digitf x `mappend` Digitf y =
    Digitf (x `mappend` y)
  mempty =
    Digitf mempty

instance Field1 (Digitf a) (Digitf b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () Digitf where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () Digitf where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () Digitf where
  itraverse f =
    traverse (f ())

instance Each (Digitf a) (Digitf b) a b where
  each =
    traverse

type instance Index (Digitf a) = 
  ()
type instance IxValue (Digitf a) =
  a
instance Ixed (Digitf a) where
  ix () f (Digitf a) =
    Digitf <$> f a

makeWrapped ''Digitf

newtype DigitA a =
  DigitA a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DA a => DA (DigitA a) where
  dA  =
    _Wrapped . dA

instance Functor DigitA where
  fmap f (DigitA a) =
    DigitA (f a)
    
instance Apply DigitA where
  DigitA f <.> DigitA a =
    DigitA (f a)

instance Applicative DigitA where
  pure =
    DigitA
  (<*>) =
    (<.>)

instance Bind DigitA where
  DigitA a >>- f =
    f a

instance Monad DigitA where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitA where
  foldMap f (DigitA a) = 
    f a

instance Foldable1 DigitA where
  foldMap1 f (DigitA a) = 
    f a

instance Traversable DigitA where
  traverse f (DigitA a) = 
    DigitA <$> f a

instance Traversable1 DigitA where
  traverse1 f (DigitA a) = 
    DigitA <$> f a

instance Semigroup a => Semigroup (DigitA a) where
  DigitA x <> DigitA y =
    DigitA (x <> y)

instance Monoid a => Monoid (DigitA a) where
  DigitA x `mappend` DigitA y =
    DigitA (x `mappend` y)
  mempty =
    DigitA mempty

instance Field1 (DigitA a) (DigitA b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitA where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitA where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitA where
  itraverse f =
    traverse (f ())

instance Each (DigitA a) (DigitA b) a b where
  each =
    traverse

type instance Index (DigitA a) = 
  ()
type instance IxValue (DigitA a) =
  a
instance Ixed (DigitA a) where
  ix () f (DigitA a) =
    DigitA <$> f a

makeWrapped ''DigitA

newtype DigitB a =
  DigitB a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DB a => DB (DigitB a) where
  dB  =
    _Wrapped . dB

instance Functor DigitB where
  fmap f (DigitB a) =
    DigitB (f a)
    
instance Apply DigitB where
  DigitB f <.> DigitB a =
    DigitB (f a)

instance Applicative DigitB where
  pure =
    DigitB
  (<*>) =
    (<.>)

instance Bind DigitB where
  DigitB a >>- f =
    f a

instance Monad DigitB where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitB where
  foldMap f (DigitB a) = 
    f a

instance Foldable1 DigitB where
  foldMap1 f (DigitB a) = 
    f a

instance Traversable DigitB where
  traverse f (DigitB a) = 
    DigitB <$> f a

instance Traversable1 DigitB where
  traverse1 f (DigitB a) = 
    DigitB <$> f a

instance Semigroup a => Semigroup (DigitB a) where
  DigitB x <> DigitB y =
    DigitB (x <> y)

instance Monoid a => Monoid (DigitB a) where
  DigitB x `mappend` DigitB y =
    DigitB (x `mappend` y)
  mempty =
    DigitB mempty

instance Field1 (DigitB a) (DigitB b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitB where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitB where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitB where
  itraverse f =
    traverse (f ())

instance Each (DigitB a) (DigitB b) a b where
  each =
    traverse

type instance Index (DigitB a) = 
  ()
type instance IxValue (DigitB a) =
  a
instance Ixed (DigitB a) where
  ix () f (DigitB a) =
    DigitB <$> f a

makeWrapped ''DigitB

newtype DigitC a =
  DigitC a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DC a => DC (DigitC a) where
  dC  =
    _Wrapped . dC

instance Functor DigitC where
  fmap f (DigitC a) =
    DigitC (f a)
    
instance Apply DigitC where
  DigitC f <.> DigitC a =
    DigitC (f a)

instance Applicative DigitC where
  pure =
    DigitC
  (<*>) =
    (<.>)

instance Bind DigitC where
  DigitC a >>- f =
    f a

instance Monad DigitC where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitC where
  foldMap f (DigitC a) = 
    f a

instance Foldable1 DigitC where
  foldMap1 f (DigitC a) = 
    f a

instance Traversable DigitC where
  traverse f (DigitC a) = 
    DigitC <$> f a

instance Traversable1 DigitC where
  traverse1 f (DigitC a) = 
    DigitC <$> f a

instance Semigroup a => Semigroup (DigitC a) where
  DigitC x <> DigitC y =
    DigitC (x <> y)

instance Monoid a => Monoid (DigitC a) where
  DigitC x `mappend` DigitC y =
    DigitC (x `mappend` y)
  mempty =
    DigitC mempty

instance Field1 (DigitC a) (DigitC b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitC where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitC where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitC where
  itraverse f =
    traverse (f ())

instance Each (DigitC a) (DigitC b) a b where
  each =
    traverse

type instance Index (DigitC a) = 
  ()
type instance IxValue (DigitC a) =
  a
instance Ixed (DigitC a) where
  ix () f (DigitC a) =
    DigitC <$> f a

makeWrapped ''DigitC

newtype DigitD a =
  DigitD a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DD a => DD (DigitD a) where
  dD  =
    _Wrapped . dD

instance Functor DigitD where
  fmap f (DigitD a) =
    DigitD (f a)
    
instance Apply DigitD where
  DigitD f <.> DigitD a =
    DigitD (f a)

instance Applicative DigitD where
  pure =
    DigitD
  (<*>) =
    (<.>)

instance Bind DigitD where
  DigitD a >>- f =
    f a

instance Monad DigitD where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitD where
  foldMap f (DigitD a) = 
    f a

instance Foldable1 DigitD where
  foldMap1 f (DigitD a) = 
    f a

instance Traversable DigitD where
  traverse f (DigitD a) = 
    DigitD <$> f a

instance Traversable1 DigitD where
  traverse1 f (DigitD a) = 
    DigitD <$> f a

instance Semigroup a => Semigroup (DigitD a) where
  DigitD x <> DigitD y =
    DigitD (x <> y)

instance Monoid a => Monoid (DigitD a) where
  DigitD x `mappend` DigitD y =
    DigitD (x `mappend` y)
  mempty =
    DigitD mempty

instance Field1 (DigitD a) (DigitD b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitD where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitD where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitD where
  itraverse f =
    traverse (f ())

instance Each (DigitD a) (DigitD b) a b where
  each =
    traverse

type instance Index (DigitD a) = 
  ()
type instance IxValue (DigitD a) =
  a
instance Ixed (DigitD a) where
  ix () f (DigitD a) =
    DigitD <$> f a

makeWrapped ''DigitD

newtype DigitE a =
  DigitE a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DE a => DE (DigitE a) where
  dE  =
    _Wrapped . dE

instance Functor DigitE where
  fmap f (DigitE a) =
    DigitE (f a)
    
instance Apply DigitE where
  DigitE f <.> DigitE a =
    DigitE (f a)

instance Applicative DigitE where
  pure =
    DigitE
  (<*>) =
    (<.>)

instance Bind DigitE where
  DigitE a >>- f =
    f a

instance Monad DigitE where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitE where
  foldMap f (DigitE a) = 
    f a

instance Foldable1 DigitE where
  foldMap1 f (DigitE a) = 
    f a

instance Traversable DigitE where
  traverse f (DigitE a) = 
    DigitE <$> f a

instance Traversable1 DigitE where
  traverse1 f (DigitE a) = 
    DigitE <$> f a

instance Semigroup a => Semigroup (DigitE a) where
  DigitE x <> DigitE y =
    DigitE (x <> y)

instance Monoid a => Monoid (DigitE a) where
  DigitE x `mappend` DigitE y =
    DigitE (x `mappend` y)
  mempty =
    DigitE mempty

instance Field1 (DigitE a) (DigitE b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitE where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitE where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitE where
  itraverse f =
    traverse (f ())

instance Each (DigitE a) (DigitE b) a b where
  each =
    traverse

type instance Index (DigitE a) = 
  ()
type instance IxValue (DigitE a) =
  a
instance Ixed (DigitE a) where
  ix () f (DigitE a) =
    DigitE <$> f a

makeWrapped ''DigitE

newtype DigitF a =
  DigitF a
  deriving (Eq, Ord, Bounded, Show, Enum, Floating, Fractional, Num, Integral, Real, RealFloat, RealFrac)

instance DF a => DF (DigitF a) where
  dF  =
    _Wrapped . dF

instance Functor DigitF where
  fmap f (DigitF a) =
    DigitF (f a)
    
instance Apply DigitF where
  DigitF f <.> DigitF a =
    DigitF (f a)

instance Applicative DigitF where
  pure =
    DigitF
  (<*>) =
    (<.>)

instance Bind DigitF where
  DigitF a >>- f =
    f a

instance Monad DigitF where
  return = 
    pure
  (>>=) =
    (>>-)

instance Foldable DigitF where
  foldMap f (DigitF a) = 
    f a

instance Foldable1 DigitF where
  foldMap1 f (DigitF a) = 
    f a

instance Traversable DigitF where
  traverse f (DigitF a) = 
    DigitF <$> f a

instance Traversable1 DigitF where
  traverse1 f (DigitF a) = 
    DigitF <$> f a

instance Semigroup a => Semigroup (DigitF a) where
  DigitF x <> DigitF y =
    DigitF (x <> y)

instance Monoid a => Monoid (DigitF a) where
  DigitF x `mappend` DigitF y =
    DigitF (x `mappend` y)
  mempty =
    DigitF mempty

instance Field1 (DigitF a) (DigitF b) a b where
  _1 =
    _Wrapped

instance FunctorWithIndex () DigitF where
  imap f =
    fmap (f ())
    
instance FoldableWithIndex () DigitF where
  ifoldMap f =
    foldMap (f ())
    
instance TraversableWithIndex () DigitF where
  itraverse f =
    traverse (f ())

instance Each (DigitF a) (DigitF b) a b where
  each =
    traverse

type instance Index (DigitF a) = 
  ()
type instance IxValue (DigitF a) =
  a
instance Ixed (DigitF a) where
  ix () f (DigitF a) =
    DigitF <$> f a

makeWrapped ''DigitF

type BinaryNoZeroDigit d1 =
  Either d1 Void

type BinaryDigit d0 d1 =
  Either d0 (BinaryNoZeroDigit d1)

type BinaryDigit' d =
  BinaryDigit d d

type OctalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 Void)))))))

type OctalNoZeroDigit' d =
  OctalNoZeroDigit d d d d d d d 

type OctalDigit d0 d1 d2 d3 d4 d5 d6 d7 =
  Either d0 (OctalNoZeroDigit d1 d2 d3 d4 d5 d6 d7)

type OctalDigit' d =
  OctalDigit d d d d d d d d

type DecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 Void)))))))))

type DecimalNoZeroDigit' d =
  DecimalNoZeroDigit d d d d d d d d d

type DecimalDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 =
  Either d0 (DecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9)

type DecimalDigit' d =
  DecimalDigit d d d d d d d d d d
  
type HexadecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 (Either da (Either db (Either dc (Either dd (Either de (Either df Void)))))))))))))))

type HexadecimalNoZeroDigit' d =
  HexadecimalNoZeroDigit d d d d d d d d d d d d d d d
  
type HexadecimalDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df =
  Either d0 (HexadecimalNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df)

type HexadecimalDigit' d =
  HexadecimalDigit d d d d d d d d d d d d d d d d

type HEXADECIMALNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 dA dB dC dD dE dF =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 (Either dA (Either dB (Either dC (Either dD (Either dE (Either dF Void)))))))))))))))

type HEXADECIMALNoZeroDigit' d =
  HEXADECIMALNoZeroDigit d d d d d d d d d d d d d d d
  
type HEXADECIMALDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 dA dB dC dD dE dF =
  Either d0 (HEXADECIMALNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 dA dB dC dD dE dF)

type HEXADECIMALDigit' d =
  HEXADECIMALDigit d d d d d d d d d d d d d d d d

type HeXaDeCiMaLNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dA dB dC dD dE dF =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 (Either da (Either db (Either dc (Either dd (Either de (Either df (Either dA (Either dB (Either dC (Either dD (Either dE (Either dF Void)))))))))))))))))))))

type HeXaDeCiMaLNoZeroDigit' d =
  HeXaDeCiMaLNoZeroDigit d d d d d d d d d d d d d d d d d d d d d
  
type HeXaDeCiMaLDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dA dB dC dD dE dF =
  Either d0 (HeXaDeCiMaLNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dA dB dC dD dE dF)

type HeXaDeCiMaLDigit' d =
  HeXaDeCiMaLDigit d d d d d d d d d d d d d d d d d d d d d d

instance D0 d => D0 (Either d x) where
  d0 =
    _Left . d0
    
instance D1 d => D1 (Either d x) where
  d1 =
    _Left . d1

instance D2 d => D2 (Either d x) where
  d2 =
    _Left . d2

instance D3 d => D3 (Either d x) where
  d3 =
    _Left . d3

instance D4 d => D4 (Either d x) where
  d4 =
    _Left . d4

instance D5 d => D5 (Either d x) where
  d5 =
    _Left . d5

instance D6 d => D6 (Either d x) where
  d6 =
    _Left . d6

instance D7 d => D7 (Either d x) where
  d7 =
    _Left . d7

instance D8 d => D8 (Either d x) where
  d8 =
    _Left . d8

instance D9 d => D9 (Either d x) where
  d9 =
    _Left . d9

instance Da d => Da (Either d x) where
  da =
    _Left . da

instance Db d => Db (Either d x) where
  db =
    _Left . db

instance Dc d => Dc (Either d x) where
  dc =
    _Left . dc

instance Dd d => Dd (Either d x) where
  dd =
    _Left . dd

instance De d => De (Either d x) where
  de =
    _Left . de

instance Df d => Df (Either d x) where
  df =
    _Left . df

instance DA d => DA (Either d x) where
  dA =
    _Left . dA

instance DB d => DB (Either d x) where
  dB =
    _Left . dB

instance DC d => DC (Either d x) where
  dC =
    _Left . dC

instance DD d => DD (Either d x) where
  dD =
    _Left . dD

instance DE d => DE (Either d x) where
  dE =
    _Left . dE

instance DF d => DF (Either d x) where
  dF =
    _Left . dF
