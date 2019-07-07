{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    number'
  ) where

import Data.String (IsString(..))
import Data.Monoid (Monoid(..))

class PositionalSystem s

data Decimal = Decimal deriving ( Eq, Ord, Show, Read )

instance PositionalSystem Decimal

number' ∷ (PositionalSystem s, Ord α, Integral α, Printer p)
        ⇒ s → p → p → p → α → p
number' = undefined
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ Decimal → p → p → p → α → p #-}

class (IsString p, Semigroup p, Monoid p) ⇒ Printer p
