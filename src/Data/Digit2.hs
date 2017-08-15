{-# LANGUAGE ConstraintKinds #-}

module Data.Digit2(
  DecimalNoZero
, Decimal
, OctalNoZero
, Octal
, HEXadecimalNoZero
, HEXadecimal
, HEXADECIMALNoZero
, HEXADECIMAL
, HexadecimalNoZero
, Hexadecimal
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
) where

import Control.Lens
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)

type DecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d)

type Decimal d =
  (D0 d, DecimalNoZero d)

type OctalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d)

type Octal d =
  (D0 d, OctalNoZero d)

type HEXadecimalNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

type HEXadecimal d =
  (D0 d, HEXadecimalNoZero d)

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
    
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"

parseAa ::
  (DA d, Da d, CharParsing p) =>
  p d
parseAa =
  choice [parseA, parsea]

parseBb ::
  (DB d, Db d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb]

parseCc ::
  (DC d, Dc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec]

parseDd ::
  (DD d, Dd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed]

parseEe ::
  (DE d, De d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee]

parseFf ::
  (DF d, Df d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef]


----

parseDecimal ::
  (Decimal d, CharParsing p) =>
  p d
parseDecimal =
  choice
    [
      parse0
    , parse1
    , parse2
    , parse3
    , parse4
    , parse5
    , parse6
    , parse7
    , parse8
    , parse9
    ] <?> "decimal"

parseHEXADECIMAL ::
  (HEXADECIMAL d, CharParsing p) =>
  p d
parseHEXADECIMAL =
  choice
    [
      parseDecimal
    , parseA
    , parseB
    , parseC
    , parseD
    , parseE
    , parseF
    ] <?> "parseHEXADECIMAL"

parseHexadecimal ::
  (Hexadecimal d, CharParsing p) =>
  p d
parseHexadecimal =
  choice
    [
      parseDecimal
    , parsea
    , parseb
    , parsec
    , parsed
    , parsee
    , parsef
    ] <?> "parseHexadecimal"

parseHEXadecimal ::
  (HEXadecimal d, CharParsing p) =>
  p d
parseHEXadecimal =
  choice
    [
      parseDecimal
    , parseAa
    , parseBb
    , parseCc
    , parseDd
    , parseEe
    , parseFf
    ] <?> "HEXadecimal"
