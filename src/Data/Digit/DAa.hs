{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Digit.DAa(
  DAa
, parseAa
) where

import Text.Parser.Char(CharParsing)
import Text.Parser.Combinators((<?>), choice)
import Data.Digit.DA(DA, parseA)
import Data.Digit.Da(Da, parsea)

-- $setup
-- >>> import Text.Parsec(parse, ParseError, eof)
-- >>> import Data.Void(Void)
-- >>> import Data.Digit.Digit
-- >>> import Papa

type DAa a =
  (DA a, Da a)
  
-- |
--
-- >>> parse (parseAa <* eof) "test" "A" :: Either ParseError Digit
-- Right A
--
-- >>> parse parseAa "test" "Axyz" :: Either ParseError Digit
-- Right A
--
-- >>> parse (parseAa <* eof) "test" "a" :: Either ParseError Digit
-- Right a
--
-- >>> parse parseAa "test" "axyz" :: Either ParseError Digit
-- Right a
--
-- >>> isn't _Right (parse parseAa "test" "xyz" :: Either ParseError Digit)
-- True
--
-- prop> \c -> (c `notElem` "Aa") ==> isn't _Right (parse parseAa "test" [c] :: Either ParseError Digit)
parseAa ::
  (DAa d, CharParsing p) =>
  p d
parseAa =
  choice [parseA, parsea] <?> "Aa"
