{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

-- | Print integral numbers in common positional numeral systems. 
module Text.Printer.Integral
  (
  -- * Positional systems
    PositionalSystem(..)
  , number'
  ) where

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Char (chr, ord)
import Data.Int
import Data.Word
import Data.Bits (Bits(..))
import Data.Monoid (mempty)
import qualified Text.Ascii as A
import Text.Printer

-- | Positional numeral system.
class PositionalSystem s where
  -- | The name of the system (e.g. \"binary\", \"decimal\").
  systemName ∷ s → String
  -- | The radix of the system.
  radixIn ∷ Num α ⇒ s → α
  -- | Test if a character is a digit.
  isDigitIn ∷ s → Char → Bool
  -- | Test if a character is a non-zero digit.
  isNzDigitIn ∷ s → Char → Bool
  -- | Map digits to the corresponding numbers. Return 'Nothing' on
  --   other inputs.
  fromDigitIn ∷ Num α ⇒ s → Char → Maybe α
  -- | Map non-zero digits to the corresponding numbers. Return 'Nothing' on
  --   other inputs.
  fromNzDigitIn ∷ Num α ⇒ s → Char → Maybe α
  -- | Map digits to the corresponding numbers. No checks are performed.
  unsafeFromDigitIn ∷ Num α ⇒ s → Char → α
  -- | Map 'Int' values to the corresponding digits. Inputs /must/ be
  --   non-negative and less than the radix.
  intToDigitIn ∷ s → Int → Char
  -- | Print a digit.
  printDigitIn ∷ Printer p ⇒ s → Char → p
  printDigitIn _ = char7
  {-# INLINE printDigitIn #-}
  printZeroIn ∷ Printer p ⇒ s → p
  printZeroIn s = printDigitIn s $! intToDigitIn s 0
  {-# INLINE printZeroIn #-}

-- | The binary numeral system.
data Binary = Binary deriving ( Typeable
                              , Generic
                              , Eq, Ord, Show, Read )

instance PositionalSystem Binary where
  systemName _ = "binary"
  {-# INLINE systemName #-}
  radixIn _ = 2
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isBinDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzBinDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromBinDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzBinDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromBinDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}
  printZeroIn _ = char7 '0'
  {-# INLINE printZeroIn #-}

-- | Print a number in the specified positional numeral system.
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
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ Binary → p → p → p → α → p #-}
