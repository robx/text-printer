{-# LANGUAGE UnicodeSyntax #-}

-- | Print fractions.
module Text.Printer.Fractional
  ( fraction'
  , fraction
  ) where

import Data.Ratio (numerator, denominator)
import Data.Monoid (mempty)
import Text.Printer
import Text.Printer.Integral

-- | Print a fraction in the decimal numeral system.
fraction' ∷ (Real α, Printer p)
          ⇒ p -- ^ Prefix for negative values
          → p -- ^ Prefix for the zero
          → p -- ^ Prefix for positive values
          → p -- ^ Dividing symbol
          → α → p
fraction' neg z pos ds a =
    if d == 1
    then decimal' neg z pos n
    else decimal' neg z pos n <> ds <> nnDecimal d
  where r = toRational a
        n = numerator r
        d = denominator r

-- | Print a fraction in the decimal numeral system. Numerator and
--   denominotor are separated with a slash, negative values are prefixed
--   with a minus sign.
fraction ∷ (Real α, Printer p) ⇒ α → p
fraction = fraction' (char7 '-') mempty mempty (char7 '/')
{-# INLINE fraction #-}

