{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    f
  ) where

class PositionalSystem s

data Decimal = Decimal

instance PositionalSystem Decimal

data Hexadecimal = Hexadecimal

instance PositionalSystem Hexadecimal

data LowHex = LowHex

instance PositionalSystem LowHex

data UpHex = UpHex

instance PositionalSystem UpHex 

f ∷ (PositionalSystem s, Ord a, Integral a)
        => s
        -> p -- ^ Prefix for negative values
        -> p -- ^ Zero printer
        -> p -- ^ Prefix for positive values
        -> a -> p
f = undefined
{-# SPECIALIZE f ∷ (Ord a, Integral a) => Decimal -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => Hexadecimal -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => LowHex -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f ∷ (Ord a, Integral a) => UpHex -> p -> p -> p -> a -> p #-}
