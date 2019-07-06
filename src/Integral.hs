{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Integral
  (
    number'
  ) where

import Data.Char (chr, ord)
import Data.Int
import Data.Word
import Data.String (IsString(..))
import Data.Monoid (Monoid(..))

class PositionalSystem s where
  radixIn ∷ Num α ⇒ s → α
  intToDigitIn ∷ s → Int → Char
  printDigitIn ∷ Printer p ⇒ s → Char → p
  printDigitIn _ = char7
  {-# INLINE printDigitIn #-}

data Binary = Binary deriving ( Eq, Ord, Show, Read )

instance PositionalSystem Binary where
  radixIn _ = 2
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

data Octal = Octal deriving ( Eq, Ord, Show, Read )

instance PositionalSystem Octal where
  radixIn _ = 8
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

data Decimal = Decimal deriving ( Eq, Ord, Show, Read )

instance PositionalSystem Decimal where
  radixIn _ = 10
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

number' ∷ (PositionalSystem s, Ord α, Integral α, Printer p)
        ⇒ s
        → p -- ^ Prefix for negative values
        → p -- ^ Zero printer
        → p -- ^ Prefix for positive values
        → α → p
number' s neg z pos n = case compare n 0 of
    LT → go neg q <> printDigitIn s d
      where (q, r) = quotRem n (negate radix)
            !d     = intToDigitIn s $ negate $ fromIntegral r
    EQ → z
    GT → go pos q <> printDigitIn s d
      where (q, r) = quotRem n radix
            !d     = intToDigitIn s $ fromIntegral r
  where go p 0 = p
        go p m = go p q <> printDigitIn s d
          where (q, r) = quotRem m radix
                !d     = intToDigitIn s $ fromIntegral r
        radix = radixIn s
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int8 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int16 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int32 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Int64 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word8 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word16 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word32 → p #-}
{-# SPECIALIZE number' ∷ Printer p ⇒ Decimal → p → p → p → Word64 → p #-}
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ Binary → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ Decimal → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ Octal → p → p → p → α → p #-}

class (IsString p, Semigroup p, Monoid p) ⇒ Printer p where
  char ∷ Char → p
  char c = string [c]
  {-# INLINE char #-}
  char7 ∷ Char → p
  char7 = char
  {-# INLINE char7 #-}
  string ∷ String → p
  string = fromString
  {-# INLINE string #-}
