module Data.Digit.Enum
  ( -- * Binary
    enumBinaryNoZero
  , enumBinary
    -- * Octal
  , enumOctalNoZero
  , enumOctal
    -- * Decimal
  , enumDecimalNoZero
  , enumDecimal
    -- * Hexadecimal
  , enumHexadecimalNoZero
  , enumHexadecimal
    -- * HEXADECIMAL
  , enumHEXADECIMALNoZero
  , enumHEXADECIMAL
  )
where

import Control.Lens.Review ((#))
import Data.Digit.Binary
import Data.Digit.Decimal
import Data.Digit.Hexadecimal
import Data.Digit.HEXADECIMAL
import Data.Digit.Octal

-- | @[1]@
enumBinaryNoZero :: BinaryNoZero d => [d]
enumBinaryNoZero = [d1 # ()]

-- | @[0, 1]@
enumBinary :: Binary d => [d]
enumBinary = [d0 # (), d1 # ()]

-- | @[1..7]@
enumOctalNoZero :: OctalNoZero d => [d]
enumOctalNoZero =
  [ d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  ]

-- | @[0..7]@
enumOctal :: Octal d => [d]
enumOctal =
  [ d0 # ()
  , d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  ]

-- | @[1..9]@
enumDecimalNoZero :: DecimalNoZero d => [d]
enumDecimalNoZero =
  [ d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  , d8 # ()
  , d9 # ()
  ]

-- | @[0..9]@
enumDecimal :: Decimal d => [d]
enumDecimal =
  [ d0 # ()
  , d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  , d8 # ()
  , d9 # ()
  ]

-- | @[1..9] ++ [a..f]@
enumHexadecimalNoZero :: HexadecimalNoZero d => [d]
enumHexadecimalNoZero =
  [ d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  , d8 # ()
  , d9 # ()
  , da # ()
  , db # ()
  , dc # ()
  , dd # ()
  , de # ()
  , df # ()
  ]

-- | @[0..9] ++ [a..f]@
enumHexadecimal :: Hexadecimal d => [d]
enumHexadecimal =
  [ d0 # ()
  , d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  , d8 # ()
  , d9 # ()
  , da # ()
  , db # ()
  , dc # ()
  , dd # ()
  , de # ()
  , df # ()
  ]

-- | @[1..9] ++ [A..F]@
enumHEXADECIMALNoZero :: HEXADECIMALNoZero d => [d]
enumHEXADECIMALNoZero =
  [ d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  , d8 # ()
  , d9 # ()
  , dA # ()
  , dB # ()
  , dC # ()
  , dD # ()
  , dE # ()
  , dF # ()
  ]

-- | @[0..9] ++ [A..F]@
enumHEXADECIMAL :: HEXADECIMAL d => [d]
enumHEXADECIMAL =
  [ d0 # ()
  , d1 # ()
  , d2 # ()
  , d3 # ()
  , d4 # ()
  , d5 # ()
  , d6 # ()
  , d7 # ()
  , d8 # ()
  , d9 # ()
  , dA # ()
  , dB # ()
  , dC # ()
  , dD # ()
  , dE # ()
  , dF # ()
  ]
