{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.HeXaDeCiMaL where

import Control.Lens hiding ((<.>))
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Void
import Text.Parser.Char
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0
import Data.Digit.D1
import Data.Digit.D2
import Data.Digit.D3
import Data.Digit.D4
import Data.Digit.D5
import Data.Digit.D6
import Data.Digit.D7
import Data.Digit.D8
import Data.Digit.D9
import Data.Digit.DA
import Data.Digit.DB
import Data.Digit.DC
import Data.Digit.DD
import Data.Digit.DE
import Data.Digit.DF
import Data.Digit.Da
import Data.Digit.Db
import Data.Digit.Dc
import Data.Digit.Dd
import Data.Digit.De
import Data.Digit.Df
import Data.Digit.Decimal
import Data.Digit.DAa
import Data.Digit.DBb
import Data.Digit.DCc
import Data.Digit.DDd
import Data.Digit.DEe
import Data.Digit.DFf

type HeXaDeCiMaLNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

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
--
-- prop> \c -> (c `notElem` "123456789abcdefABCDEF") ==> isn't _Right (parse parseHeXaDeCiMaLNoZero "test" [c] :: Either ParseError (HeXaDeCiMaLNoZeroDigit' ()))
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

type HeXaDeCiMaLNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dA dB dC dD dE dF =
  Either d1 (Either d2 (Either d3 (Either d4 (Either d5 (Either d5 (Either d6 (Either d7 (Either d8 (Either d9 (Either da (Either db (Either dc (Either dd (Either de (Either df (Either dA (Either dB (Either dC (Either dD (Either dE (Either dF Void)))))))))))))))))))))

type HeXaDeCiMaLNoZeroDigit' d =
  HeXaDeCiMaLNoZeroDigit d d d d d d d d d d d d d d d d d d d d d
  
type HeXaDeCiMaL d =
  (D0 d, HeXaDeCiMaLNoZero d)

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
--
-- prop> \c -> (c `notElem` "0123456789abcdefABCDEF") ==> isn't _Right (parse parseHeXaDeCiMaL "test" [c] :: Either ParseError (HeXaDeCiMaLDigit' ()))
parseHeXaDeCiMaL ::
  (HeXaDeCiMaL d, CharParsing p) =>
  p d
parseHeXaDeCiMaL =
  choice
    [
      parse0
    , parseHeXaDeCiMaLNoZero
    ] <?> "HeXaDeCiMaL"

type HeXaDeCiMaLDigit d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dA dB dC dD dE dF =
  Either d0 (HeXaDeCiMaLNoZeroDigit d1 d2 d3 d4 d5 d6 d7 d8 d9 da db dc dd de df dA dB dC dD dE dF)

type HeXaDeCiMaLDigit' d =
  HeXaDeCiMaLDigit d d d d d d d d d d d d d d d d d d d d d d

