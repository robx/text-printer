{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    number'
  ) where

number' ∷ (Ord α, Integral α)
        ⇒ α → p
number' = undefined
{-# SPECIALIZE number' ∷ (Ord α, Integral α) ⇒ α → p #-}
