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

f ∷ (X s, Eq a, Ord a)
        => s
        -> p -- ^ Prefix for negative values
        -> p -- ^ Zero printer
        -> p -- ^ Prefix for positive values
        -> a -> p
f = undefined
{-# SPECIALIZE f ∷ (Eq a, Ord a) => A -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Eq a, Ord a) => B -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Eq a, Ord a) => C -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Eq a, Ord a) => D -> p -> p -> p -> a -> p #-}
