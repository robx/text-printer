{-# LANGUAGE UnicodeSyntax #-}

module M
  ( f
  ) where

f :: (Ord a, Integral a) => a
f = undefined
{-# SPECIALIZE f ∷ (Ord a, Integral a) => a #-}
