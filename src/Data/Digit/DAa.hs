{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Digit.DAa(
  module Data.Digit.DA
, module Data.Digit.Da
, DAa
, parseAa
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DA
import Data.Digit.Da

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit
-- >>> import Papa

type DAa a =
  (DA a, Da a)
  
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
