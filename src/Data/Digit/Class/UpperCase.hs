{-# LANGUAGE NoImplicitPrelude #-}

module Data.Digit.Class.UpperCase
  (
    -- * Classes
    DA(..)
  , DB(..)
  , DC(..)
  , DD(..)
  , DE(..)
  , DF(..)

    -- * Parsers
  , parseA
  , parseB
  , parseC
  , parseD
  , parseE
  , parseF
  ) where

import           Control.Category        (id)
import           Control.Lens            (Prism', ( # ))

import           Data.Functor            ((<$))

import           Text.Parser.Char        (CharParsing, char)
import           Text.Parser.Combinators ((<?>))

-- $setup
-- >>> import Data.Digit
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)

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
-- >>> parse (parseA <* eof) "test" "A" :: Either ParseError HEXDigit
-- Right HEXDigitA
--
-- >>> parse parseA "test" "Axyz" :: Either ParseError HEXDigit
-- Right HEXDigitA
--
-- >>> isn't _Right (parse parseA "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseA ::
  (DA d, CharParsing p) =>
  p d
parseA =
  xA <$ char 'A' <?> "A"

-- |
--
-- >>> parse (parseB <* eof) "test" "B" :: Either ParseError HEXDigit
-- Right HEXDigitB
--
-- >>> parse parseB "test" "Bxyz" :: Either ParseError HEXDigit
-- Right HEXDigitB
--
-- >>> isn't _Right (parse parseB "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseB ::
  (DB d, CharParsing p) =>
  p d
parseB =
  xB <$ char 'B' <?> "B"

-- |
--
-- >>> parse (parseC <* eof) "test" "C" :: Either ParseError HEXDigit
-- Right HEXDigitC
--
-- >>> parse parseC "test" "Cxyz" :: Either ParseError HEXDigit
-- Right HEXDigitC
--
-- >>> isn't _Right (parse parseC "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseC ::
  (DC d, CharParsing p) =>
  p d
parseC =
  xC <$ char 'C' <?> "C"

-- |
--
-- >>> parse (parseD <* eof) "test" "D" :: Either ParseError HEXDigit
-- Right HEXDigitD
--
-- >>> parse parseD "test" "Dxyz" :: Either ParseError HEXDigit
-- Right HEXDigitD
--
-- >>> isn't _Right (parse parseD "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseD ::
  (DD d, CharParsing p) =>
  p d
parseD =
  xD <$ char 'D' <?> "D"

-- |
--
-- >>> parse (parseE <* eof) "test" "E" :: Either ParseError HEXDigit
-- Right HEXDigitE
--
-- >>> parse parseE "test" "Exyz" :: Either ParseError HEXDigit
-- Right HEXDigitE
--
-- >>> isn't _Right (parse parseE "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseE ::
  (DE d, CharParsing p) =>
  p d
parseE =
  xE <$ char 'E' <?> "E"

-- |
--
-- >>> parse (parseF <* eof) "test" "F" :: Either ParseError HEXDigit
-- Right HEXDigitF
--
-- >>> parse parseF "test" "Fxyz" :: Either ParseError HEXDigit
-- Right HEXDigitF
--
-- >>> isn't _Right (parse parseF "test" "xyz" :: Either ParseError HEXDigit)
-- True
parseF ::
  (DF d, CharParsing p) =>
  p d
parseF =
  xF <$ char 'F' <?> "F"
