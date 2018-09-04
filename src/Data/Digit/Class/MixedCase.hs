{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Digit.Class.MixedCase
  (
    -- * Types
    DAa
  , DBb
  , DCc
  , DDd
  , DEe
  , DFf

    -- * Parsers
  , parseAa
  , parseBb
  , parseCc
  , parseDd
  , parseEe
  , parseFf

    -- * Re-exports
  , module Data.Digit.Class.UpperCase
  , module Data.Digit.Class.LowerCase

  ) where

import           Text.Parser.Char        (CharParsing)
import           Text.Parser.Combinators (choice, (<?>))

import           Data.Digit.Class.LowerCase
import           Data.Digit.Class.UpperCase

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit
-- >>> import Papa

type DAa a =
  (DA a, Da a)

type DBb a =
  (DB a, Db a)

type DCc a =
  (DC a, Dc a)

type DDd a =
  (DD a, Dd a)

type DEe a =
  (DE a, De a)

type DFf a =
  (DF a, Df a)

-- |
--
-- >>> parse (parseAa <* eof) "test" "A" :: Either ParseError HeXDigit
-- Right HeXDigitA
--
-- >>> parse parseAa "test" "Axyz" :: Either ParseError HeXDigit
-- Right HeXDigitA
--
-- >>> parse (parseAa <* eof) "test" "a" :: Either ParseError HeXDigit
-- Right HeXDigita
--
-- >>> parse parseAa "test" "axyz" :: Either ParseError HeXDigit
-- Right HeXDigita
--
-- >>> isn't _Right (parse parseAa "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseAa ::
  (DAa d, CharParsing p) =>
  p d
parseAa =
  choice [parseA, parsea] <?> "Aa"

-- |
--
-- >>> parse (parseBb <* eof) "test" "B" :: Either ParseError HeXDigit
-- Right HeXDigitB
--
-- >>> parse parseBb "test" "Bxyz" :: Either ParseError HeXDigit
-- Right HeXDigitB
--
-- >>> parse (parseBb <* eof) "test" "b" :: Either ParseError HeXDigit
-- Right HeXDigitb
--
-- >>> parse parseBb "test" "bxyz" :: Either ParseError HeXDigit
-- Right HeXDigitb
--
-- >>> isn't _Right (parse parseBb "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseBb ::
  (DBb d, CharParsing p) =>
  p d
parseBb =
  choice [parseB, parseb] <?> "Bb"

-- |
--
-- >>> parse (parseCc <* eof) "test" "C" :: Either ParseError HeXDigit
-- Right HeXDigitC
--
-- >>> parse parseCc "test" "Cxyz" :: Either ParseError HeXDigit
-- Right HeXDigitC
--
-- >>> parse (parseCc <* eof) "test" "c" :: Either ParseError HeXDigit
-- Right HeXDigitc
--
-- >>> parse parseCc "test" "cxyz" :: Either ParseError HeXDigit
-- Right HeXDigitc
--
-- >>> isn't _Right (parse parseCc "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseCc ::
  (DCc d, CharParsing p) =>
  p d
parseCc =
  choice [parseC, parsec] <?> "Cc"


-- |
--
-- >>> parse (parseDd <* eof) "test" "D" :: Either ParseError HeXDigit
-- Right HeXDigitD
--
-- >>> parse parseDd "test" "Dxyz" :: Either ParseError HeXDigit
-- Right HeXDigitD
--
-- >>> parse (parseDd <* eof) "test" "d" :: Either ParseError HeXDigit
-- Right HeXDigitd
--
-- >>> parse parseDd "test" "dxyz" :: Either ParseError HeXDigit
-- Right HeXDigitd
--
-- >>> isn't _Right (parse parseDd "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseDd ::
  (DDd d, CharParsing p) =>
  p d
parseDd =
  choice [parseD, parsed] <?> "Dd"

-- |
--
-- >>> parse (parseEe <* eof) "test" "E" :: Either ParseError HeXDigit
-- Right HeXDigitE
--
-- >>> parse parseEe "test" "Exyz" :: Either ParseError HeXDigit
-- Right HeXDigitE
--
-- >>> parse (parseEe <* eof) "test" "e" :: Either ParseError HeXDigit
-- Right HeXDigite
--
-- >>> parse parseEe "test" "exyz" :: Either ParseError HeXDigit
-- Right HeXDigite
--
-- >>> isn't _Right (parse parseEe "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseEe ::
  (DEe d, CharParsing p) =>
  p d
parseEe =
  choice [parseE, parsee] <?> "Ee"

-- |
--
-- >>> parse (parseFf <* eof) "test" "F" :: Either ParseError HeXDigit
-- Right HeXDigitF
--
-- >>> parse parseFf "test" "Fxyz" :: Either ParseError HeXDigit
-- Right HeXDigitF
--
-- >>> parse (parseFf <* eof) "test" "f" :: Either ParseError HeXDigit
-- Right HeXDigitf
--
-- >>> parse parseFf "test" "fxyz" :: Either ParseError HeXDigit
-- Right HeXDigitf
--
-- >>> isn't _Right (parse parseFf "test" "xyz" :: Either ParseError HeXDigit)
-- True
parseFf ::
  (DFf d, CharParsing p) =>
  p d
parseFf =
  choice [parseF, parsef] <?> "Ff"
