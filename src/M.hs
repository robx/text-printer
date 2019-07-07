{-# LANGUAGE UnicodeSyntax #-}

module M
  ( f
  ) where

f :: (Eq a, Ord a) => a
f = undefined
{-# SPECIALIZE f ∷ (Eq a, Ord a) => a #-}
