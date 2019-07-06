{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


-- | Print integral numbers in common positional numeral systems. 
module Integral
  (
    number'
  ) where

import Data.Typeable (Typeable)
import Data.Char (chr, ord)
import Data.Int
import Data.Word
import qualified Text.Ascii as A
import Prelude hiding (foldr, foldr1, print, lines)
import Data.String (IsString(..))
import Data.Monoid (Monoid(..))

-- | Positional numeral system.
class PositionalSystem s where
  -- | The radix of the system.
  radixIn ∷ Num α ⇒ s → α
  -- | Map digits to the corresponding numbers. No checks are performed.
  intToDigitIn ∷ s → Int → Char
  -- | Print a digit.
  printDigitIn ∷ Printer p ⇒ s → Char → p
  printDigitIn _ = char7
  {-# INLINE printDigitIn #-}

-- | The binary numeral system.
data Binary = Binary deriving ( Typeable
                              , Eq, Ord, Show, Read )

instance PositionalSystem Binary where
  radixIn _ = 2
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

-- | The octal numeral system.
data Octal = Octal deriving ( Typeable
                            , Eq, Ord, Show, Read )

instance PositionalSystem Octal where
  radixIn _ = 8
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

-- | The decimal numeral system.
data Decimal = Decimal deriving ( Typeable
                                , Eq, Ord, Show, Read )

instance PositionalSystem Decimal where
  radixIn _ = 10
  {-# INLINE radixIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

-- | The hexadecimal numeral system.
data Hexadecimal = Hexadecimal deriving ( Typeable
                                        , Eq, Ord, Show, Read )

instance PositionalSystem Hexadecimal where
  radixIn _ = 16
  {-# INLINE radixIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}

-- | The hexadecimal numeral system, using lower case digits.
data LowHex = LowHex deriving ( Typeable
                              , Eq, Ord, Show, Read )

instance PositionalSystem LowHex where
  radixIn _ = 16
  {-# INLINE radixIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'a' + (i - 10) 
  {-# INLINE intToDigitIn #-}

-- | The hexadecimal numeral system, using upper case digits.
data UpHex = UpHex deriving ( Typeable
                            , Eq, Ord, Show, Read )

instance PositionalSystem UpHex where
  radixIn _ = 16
  {-# INLINE radixIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}

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
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ Hexadecimal → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ LowHex → p → p → p → α → p #-}
{-# SPECIALIZE number' ∷ (Ord α, Integral α, Printer p) ⇒ UpHex → p → p → p → α → p #-}


-- | Text monoid. 'string' must be equivalent to 'fromString' and be a monoid
--   homomorphism, i.e. @'string' 'mempty' = 'mempty'@ and
--   @'mappend' ('string' /x/) ('string' /y/) = 'string' ('mappend' /x/ /y/)@.
--   Other operations must be monoid homomorphisms that are eqiuvalent (but
--   possibly faster) to the composition of 'string' and the corresponding
--   embedding, e.g. @'text' = 'string' . 'TS.unpack'@.
class (IsString p, Semigroup p, Monoid p) ⇒ Printer p where
  -- | Print a character. @'char' /c/@ must be equivalent to
  --   @'string' [/c/]@, but hopefully is faster.
  char ∷ Char → p
  char c = string [c]
  {-# INLINE char #-}
  -- | Print an ASCII character, can be faster than 'char'.
  char7 ∷ Char → p
  char7 = char
  {-# INLINE char7 #-}
  -- | Print a string.
  string ∷ String → p
  string = fromString
  {-# INLINE string #-}
