{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    number'
  ) where

class PositionalSystem s

data Decimal

instance PositionalSystem Decimal

number' ∷ (PositionalSystem s, Ord α, Integral α)
        ⇒ s → p → p → p → α → p
number' = undefined
{-# SPECIALIZE number' ∷ (Ord α, Integral α) ⇒ Decimal → p → p → p → α → p #-}
