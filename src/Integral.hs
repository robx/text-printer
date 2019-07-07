{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    number'
  ) where

number' :: (Ord a, Integral a)
        => a -> p
number' = undefined
{-# SPECIALIZE number' ∷ (Ord a, Integral a) ⇒ a → p #-}
