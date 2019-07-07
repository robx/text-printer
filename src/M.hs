{-# LANGUAGE UnicodeSyntax #-}

module M
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
        => s
        -> p -- ^ Prefix for negative values
        -> p -- ^ Zero printer
        -> p -- ^ Prefix for positive values
        -> a -> p
f = undefined
{-# SPECIALIZE f ∷ (Ord a, Integral a) => A -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => B -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => C -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => D -> p -> p -> p -> a -> p #-}
