{-# LANGUAGE UnicodeSyntax #-}

module Integral
  (
    f
  ) where

import Data.Char (chr, ord)
import Data.Int
import Data.Word
import Data.String (IsString(..))
import Data.Monoid (Monoid(..))

class PositionalSystem s where
  radixIn :: Num a => s -> a
  intToDigitIn :: s -> Int -> Char
  printDigitIn :: Printer p => s -> Char -> p
  printDigitIn _ = char7
  {-# INLINE printDigitIn #-}

data Decimal = Decimal deriving ( Eq, Ord, Show, Read )

instance PositionalSystem Decimal where
  radixIn _ = 10
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

data Hexadecimal = Hexadecimal deriving ( Eq, Ord, Show, Read )

instance PositionalSystem Hexadecimal where
  radixIn _ = 16
  {-# INLINE radixIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}

data LowHex = LowHex deriving ( Eq, Ord, Show, Read )

instance PositionalSystem LowHex where
  radixIn _ = 16
  {-# INLINE radixIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'a' + (i - 10) 
  {-# INLINE intToDigitIn #-}

data UpHex = UpHex deriving ( Eq, Ord, Show, Read )

instance PositionalSystem UpHex where
  radixIn _ = 16
  {-# INLINE radixIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}

f :: (PositionalSystem s, Ord a, Integral a, Printer p)
        => s
        -> p -- ^ Prefix for negative values
        -> p -- ^ Zero printer
        -> p -- ^ Prefix for positive values
        -> a -> p
f = undefined
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Int -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Int8 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Int16 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Int32 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Int64 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Word -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Word8 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Word16 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Word32 -> p #-}
{-# SPECIALIZE f :: Printer p => Decimal -> p -> p -> p -> Word64 -> p #-}
{-# SPECIALIZE f :: (Ord a, Integral a, Printer p) => Decimal -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f :: (Ord a, Integral a, Printer p) => Hexadecimal -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f :: (Ord a, Integral a, Printer p) => LowHex -> p -> p -> p -> a -> p #-}
{-# SPECIALIZE f :: (Ord a, Integral a, Printer p) => UpHex -> p -> p -> p -> a -> p #-}

class (IsString p, Semigroup p, Monoid p) => Printer p where
  char :: Char -> p
  char c = string [c]
  {-# INLINE char #-}
  char7 :: Char -> p
  char7 = char
  {-# INLINE char7 #-}
  string :: String -> p
  string = fromString
  {-# INLINE string #-}
