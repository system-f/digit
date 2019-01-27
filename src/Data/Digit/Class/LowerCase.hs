{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module      : Data.Digit.Class.LowerCase
Copyright   : (C) 2010-2016 NICTA Limited
              (C) 2017-2018 CSIRO
License     : BSD3
Maintainer  : Queensland Functional Programming Lab <oᴉ˙ldɟb@llǝʞsɐɥ>
Stability   : experimental
Portability : non-portable
-}

module Data.Digit.Class.LowerCase (

  -- * Classes
    Da(..)
  , Db(..)
  , Dc(..)
  , Dd(..)
  , De(..)
  , Df(..)

  -- * Parsers
 , parsea
 , parseb
 , parsec
 , parsed
 , parsee
 , parsef

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
-- >>> parse (parsea <* eof) "test" "a" :: Either ParseError HexDigit
-- Right HexDigita
--
-- >>> parse parsea "test" "axyz" :: Either ParseError HexDigit
-- Right HexDigita
--
-- >>> isn't _Right (parse parsea "test" "xyz" :: Either ParseError HexDigit)
-- True
parsea ::
  (Da d, CharParsing p) =>
  p d
parsea =
  xa <$ char 'a' <?> "a"

-- |
--
-- >>> parse (parseb <* eof) "test" "b" :: Either ParseError HexDigit
-- Right HexDigitb
--
-- >>> parse parseb "test" "bxyz" :: Either ParseError HexDigit
-- Right HexDigitb
--
-- >>> isn't _Right (parse parseb "test" "xyz" :: Either ParseError HexDigit)
-- True
parseb ::
  (Db d, CharParsing p) =>
  p d
parseb =
  xb <$ char 'b' <?> "b"

-- |
--
-- >>> parse (parsec <* eof) "test" "c" :: Either ParseError HexDigit
-- Right HexDigitc
--
-- >>> parse parsec "test" "cxyz" :: Either ParseError HexDigit
-- Right HexDigitc
--
-- >>> isn't _Right (parse parsec "test" "xyz" :: Either ParseError HexDigit)
-- True
parsec ::
  (Dc d, CharParsing p) =>
  p d
parsec =
  xc <$ char 'c' <?> "c"

-- |
--
-- >>> parse (parsed <* eof) "test" "d" :: Either ParseError HexDigit
-- Right HexDigitd
--
-- >>> parse parsed "test" "dxyz" :: Either ParseError HexDigit
-- Right HexDigitd
--
-- >>> isn't _Right (parse parsed "test" "xyz" :: Either ParseError HexDigit)
-- True
parsed ::
  (Dd d, CharParsing p) =>
  p d
parsed =
  xd <$ char 'd' <?> "d"

-- |
--
-- >>> parse (parsee <* eof) "test" "e" :: Either ParseError HexDigit
-- Right HexDigite
--
-- >>> parse parsee "test" "exyz" :: Either ParseError HexDigit
-- Right HexDigite
--
-- >>> isn't _Right (parse parsee "test" "xyz" :: Either ParseError HexDigit)
-- True
parsee ::
  (De d, CharParsing p) =>
  p d
parsee =
  xe <$ char 'e' <?> "e"

-- |
--
-- >>> parse (parsef <* eof) "test" "f" :: Either ParseError HexDigit
-- Right HexDigitf
--
-- >>> parse parsef "test" "fxyz" :: Either ParseError HexDigit
-- Right HexDigitf
--
-- >>> isn't _Right (parse parsef "test" "xyz" :: Either ParseError HexDigit)
-- True
parsef ::
  (Df d, CharParsing p) =>
  p d
parsef =
  xf <$ char 'f' <?> "f"
