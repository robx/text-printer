{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    number'
  ) where

number' ∷ (Ord α, Integral α)
        ⇒ p → p → p → α → p
number' = undefined
{-# SPECIALIZE number' ∷ (Ord α, Integral α) ⇒ p → p → p → α → p #-}
