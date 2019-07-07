{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    f
  ) where

data Decimal = Decimal deriving ( Eq, Ord, Show, Read )

data Hexadecimal = Hexadecimal deriving ( Eq, Ord, Show, Read )

data LowHex = LowHex deriving ( Eq, Ord, Show, Read )

data UpHex = UpHex deriving ( Eq, Ord, Show, Read )

f ∷ (Ord a, Integral a)
        => p -> a
f = undefined
{-# SPECIALIZE f ∷ (Ord a, Integral a) => Decimal -> a #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => Hexadecimal -> a #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => LowHex -> a #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => UpHex -> a #-}
