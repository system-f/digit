{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}

module Data.Digit.HeXaDeCiMaL(
  HeXaDeCiMaLNoZero
, HeXaDeCiMaL
, parseHeXaDeCiMaLNoZero
, parseHeXaDeCiMaL
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.D0(D0, parse0)
import Data.Digit.D1(D1)
import Data.Digit.D2(D2)
import Data.Digit.D3(D3)
import Data.Digit.D4(D4)
import Data.Digit.D5(D5)
import Data.Digit.D6(D6)
import Data.Digit.D7(D7)
import Data.Digit.D8(D8)
import Data.Digit.D9(D9)
import Data.Digit.DA(DA)
import Data.Digit.DB(DB)
import Data.Digit.DC(DC)
import Data.Digit.DD(DD)
import Data.Digit.DE(DE)
import Data.Digit.DF(DF)
import Data.Digit.Da(Da)
import Data.Digit.Db(Db)
import Data.Digit.Dc(Dc)
import Data.Digit.Dd(Dd)
import Data.Digit.De(De)
import Data.Digit.Df(Df)
import Data.Digit.Decimal(parseDecimalNoZero)
import Data.Digit.DAa(parseAa)
import Data.Digit.DBb(parseBb)
import Data.Digit.DCc(parseCc)
import Data.Digit.DDd(parseDd)
import Data.Digit.DEe(parseEe)
import Data.Digit.DFf(parseFf)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type HeXaDeCiMaLNoZero d =
  (D1 d, D2 d, D3 d, D4 d, D5 d, D6 d, D7 d, D8 d, D9 d, DA d, DB d, DC d, DD d, DE d, DF d, Da d, Db d, Dc d, Dd d, De d, De d, Df d)

-- |
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseHeXaDeCiMaLNoZero <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseHeXaDeCiMaLNoZero "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseHeXaDeCiMaLNoZero "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "123456789abcdefABCDEF") ==> isn't _Right (parse parseHeXaDeCiMaLNoZero "test" [c] :: Either ParseError Digit)
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

type HeXaDeCiMaL d =
  (D0 d, HeXaDeCiMaLNoZero d)

-- |
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "0" :: Either ParseError Digit
-- Right 0
--
-- >>> parse parseHeXaDeCiMaL "test" "0xyz" :: Either ParseError Digit
-- Right 0
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "1" :: Either ParseError Digit
-- Right 1
--
-- >>> parse parseHeXaDeCiMaL "test" "1xyz" :: Either ParseError Digit
-- Right 1
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "2" :: Either ParseError Digit
-- Right 2
--
-- >>> parse parseHeXaDeCiMaL "test" "2xyz" :: Either ParseError Digit
-- Right 2
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "3" :: Either ParseError Digit
-- Right 3
--
-- >>> parse parseHeXaDeCiMaL "test" "3xyz" :: Either ParseError Digit
-- Right 3
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "4" :: Either ParseError Digit
-- Right 4
--
-- >>> parse parseHeXaDeCiMaL "test" "4xyz" :: Either ParseError Digit
-- Right 4
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "5" :: Either ParseError Digit
-- Right 5
--
-- >>> parse parseHeXaDeCiMaL "test" "5xyz" :: Either ParseError Digit
-- Right 5
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "6" :: Either ParseError Digit
-- Right 6
--
-- >>> parse parseHeXaDeCiMaL "test" "6xyz" :: Either ParseError Digit
-- Right 6
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "7" :: Either ParseError Digit
-- Right 7
--
-- >>> parse parseHeXaDeCiMaL "test" "7xyz" :: Either ParseError Digit
-- Right 7
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "8" :: Either ParseError Digit
-- Right 8
--
-- >>> parse parseHeXaDeCiMaL "test" "8xyz" :: Either ParseError Digit
-- Right 8
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "9" :: Either ParseError Digit
-- Right 9
--
-- >>> parse parseHeXaDeCiMaL "test" "9xyz" :: Either ParseError Digit
-- Right 9
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseHeXaDeCiMaL "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "b" :: Either ParseError Digit
-- Right b
--
-- >>> parse parseHeXaDeCiMaL "test" "bxyz" :: Either ParseError Digit
-- Right b
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "c" :: Either ParseError Digit
-- Right c
--
-- >>> parse parseHeXaDeCiMaL "test" "cxyz" :: Either ParseError Digit
-- Right c
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "d" :: Either ParseError Digit
-- Right d
--
-- >>> parse parseHeXaDeCiMaL "test" "dxyz" :: Either ParseError Digit
-- Right d
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "e" :: Either ParseError Digit
-- Right e
--
-- >>> parse parseHeXaDeCiMaL "test" "exyz" :: Either ParseError Digit
-- Right e
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "f" :: Either ParseError Digit
-- Right f
--
-- >>> parse parseHeXaDeCiMaL "test" "fxyz" :: Either ParseError Digit
-- Right f
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseHeXaDeCiMaL "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "B" :: Either ParseError Digit
-- Right B
--
-- >>> parse parseHeXaDeCiMaL "test" "Bxyz" :: Either ParseError Digit
-- Right B
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "C" :: Either ParseError Digit
-- Right C
--
-- >>> parse parseHeXaDeCiMaL "test" "Cxyz" :: Either ParseError Digit
-- Right C
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "D" :: Either ParseError Digit
-- Right D
--
-- >>> parse parseHeXaDeCiMaL "test" "Dxyz" :: Either ParseError Digit
-- Right D
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "E" :: Either ParseError Digit
-- Right E
--
-- >>> parse parseHeXaDeCiMaL "test" "Exyz" :: Either ParseError Digit
-- Right E
--
-- >>> parse (parseHeXaDeCiMaL <* eof) "test" "F" :: Either ParseError Digit
-- Right F
--
-- >>> parse parseHeXaDeCiMaL "test" "Fxyz" :: Either ParseError Digit
-- Right F
--
-- >>> isn't _Right (parse parseHeXaDeCiMaL "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "0123456789abcdefABCDEF") ==> isn't _Right (parse parseHeXaDeCiMaL "test" [c] :: Either ParseError Digit)
parseHeXaDeCiMaL ::
  (HeXaDeCiMaL d, CharParsing p) =>
  p d
parseHeXaDeCiMaL =
  choice
    [
      parse0
    , parseHeXaDeCiMaLNoZero
    ] <?> "HeXaDeCiMaL"
