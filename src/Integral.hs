{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    f
  ) where

class X x

data A
instance X A

data B
instance X B

data C
instance X C

data D
instance X D

f ∷ (X s, Ord a, Integral a)
        => s -> a 
f = undefined
{-# SPECIALIZE f ∷ (Ord a, Integral a) => A -> a #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => B -> a #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => C -> a #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => D -> a #-}
