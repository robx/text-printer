{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    number'
  ) where

number' :: (Ord a, Integral a)
        => a
number' = undefined
{-# SPECIALIZE number' ∷ (Ord a, Integral a) => a #-}
