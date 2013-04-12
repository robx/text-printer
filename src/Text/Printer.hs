{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Monoids with a homomorphism from 'String' to themselves.
module Text.Printer
  (
  -- * The class
     Printer(..)
  -- * Builders
  , StringBuilder(..)
  , buildString
  , buildText
  , buildLazyText
  , AsciiBuilder(..)
  , buildAscii
  , buildLazyAscii
  , Utf8Builder(..)
  , buildUtf8
  , buildLazyUtf8
  -- * Combinators
  , (<>)
  , hcat
  , fcat
  , separate
  , (<+>)
  , hsep
  , fsep
  , list
  , parens
  , brackets
  , braces
  , angles
  , squotes
  , dquotes
  , punctuateL
  , punctuateR
  -- * Number printers
  , PositionalSystem(..)
  , BitSystem(..)
  , Binary(..)
  , Octal(..)
  , Decimal(..)
  , Hexadecimal(..)
  , LowHex(..)
  , UpHex(..)
  , nonNegative
  , nnBinary
  , nnOctal
  , nnDecimal
  , nnLowHex
  , nnUpHex
  , nnBits
  , nnBinaryBits
  , nnOctalBits
  , nnLowHexBits
  , nnUpHexBits
  , nonPositive
  , npBinary
  , npOctal
  , npDecimal
  , npLowHex
  , npUpHex
  , npBits
  , npBinaryBits
  , npOctalBits
  , npLowHexBits
  , npUpHexBits
  , number'
  , number
  , binary'
  , binary
  , octal'
  , octal
  , decimal'
  , decimal
  , lowHex'
  , lowHex
  , upHex'
  , upHex
  , bits'
  , bits
  , binaryBits'
  , binaryBits
  , octalBits'
  , octalBits
  , lowHexBits'
  , lowHexBits
  , upHexBits'
  , upHexBits
  -- * Multiline printers
  , MultilinePrinter(..)
  , lines
  , newLine
  , crlf
  , LinePrinter(..)
  , lfPrinter
  , crlfPrinter
  ) where

import Prelude hiding (foldr, foldr1, print, lines)
import Data.Typeable (Typeable)
import Data.Int
import Data.Word
import Data.Bits (Bits(..))
import Data.Char (chr, ord)
import Data.String (IsString(..))
import Data.Monoid (Monoid(..), (<>))
import Data.Foldable (Foldable(..), toList)
import Data.Traversable (Traversable, mapAccumL, mapAccumR)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Lazy.Builder as BB
import qualified Text.PrettyPrint as PP
import qualified Text.Ascii as A

-- | Text monoid. 'string' must be equivalent to 'fromString' and be a monoid
--   homomorphism, i.e. @'string' 'mempty' = 'mempty'@ and
--   @'mappend' ('string' /x/) ('string' /y/) = 'string' ('mappend' /x/ /y/)@.
--   Other operations must be monoid homomorphisms that are eqiuvalent (but
--   possibly faster) to the composition of 'string' and the corresponding
--   embedding, e.g. @'text' = 'string' . 'TS.unpack'@.
class (IsString p, Monoid p) ⇒ Printer p where
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
  -- | Print an ASCII string, can be faster than 'string'.
  string7 ∷ String → p
  string7 = string
  {-# INLINE string7 #-}
  -- | Print a 'TS.Text'.
  text ∷ TS.Text → p
  text = string . TS.unpack
  {-# INLINE text #-}
  -- | Print a lazy 'TL.Text'.
  lazyText ∷ TL.Text → p
  lazyText = string . TL.unpack
  {-# INLINE lazyText #-}
  -- | Print an ASCII 'BS.ByteString'.
  ascii ∷ BS.ByteString → p
  ascii = string . BS8.unpack
  {-# INLINE ascii #-}
  -- | Print a lazy ASCII 'BL.ByteString'.
  lazyAscii ∷ BL.ByteString → p
  lazyAscii = string . BL8.unpack
  {-# INLINE lazyAscii #-}
  -- | Print a UTF-8 'BS.ByteString'.
  utf8 ∷ BS.ByteString → p
  utf8 = text . TS.decodeUtf8
  {-# INLINE utf8 #-}
  -- | Print a lazy UTF-8 'BL.ByteString'
  lazyUtf8 ∷ BL.ByteString → p
  lazyUtf8 = lazyText . TL.decodeUtf8
  {-# INLINE lazyUtf8 #-}

instance Printer String where

-- | A simple string builder as used by 'Show'.
newtype StringBuilder = StringBuilder { stringBuilder ∷ String → String }
                        deriving (Typeable, Monoid)

instance IsString StringBuilder where
  fromString s = StringBuilder (s ++)
  {-# INLINE fromString #-}

instance Printer StringBuilder where
  char c = StringBuilder (c :)
  {-# INLINE char #-}

buildString ∷ StringBuilder → String
buildString b = stringBuilder b ""
{-# INLINE buildString #-}

instance Printer TB.Builder where
  char = TB.singleton
  {-# INLINE char #-}
  text = TB.fromText
  {-# INLINE text #-}
  lazyText = TB.fromLazyText
  {-# INLINE lazyText #-}

buildText ∷ TB.Builder → TS.Text
buildText = fold . TL.toChunks . buildLazyText
{-# INLINE buildText #-}

buildLazyText ∷ TB.Builder → TL.Text
buildLazyText = TB.toLazyText
{-# INLINE buildLazyText #-}

-- | Use this builder when you are sure that only ASCII characters
--   will get printed to it.
newtype AsciiBuilder = AsciiBuilder { asciiBuilder ∷ BB.Builder }
                       deriving (Typeable, Monoid)

instance IsString AsciiBuilder where
  fromString = AsciiBuilder . BB.string7
  {-# INLINE fromString #-}

instance Printer AsciiBuilder where
  char = AsciiBuilder . BB.char7
  {-# INLINE char #-}
  ascii = AsciiBuilder . BB.byteString
  {-# INLINE ascii #-}
  lazyAscii = AsciiBuilder . BB.lazyByteString
  {-# INLINE lazyAscii #-}
  utf8 = AsciiBuilder . BB.byteString
  {-# INLINE utf8 #-}
  lazyUtf8 = AsciiBuilder . BB.lazyByteString
  {-# INLINE lazyUtf8 #-}

buildAscii ∷ AsciiBuilder → BS.ByteString
buildAscii = fold . BL.toChunks . buildLazyAscii
{-# INLINE buildAscii #-}

buildLazyAscii ∷ AsciiBuilder → BL.ByteString
buildLazyAscii = BB.toLazyByteString . asciiBuilder
{-# INLINE buildLazyAscii #-}

-- | UTF-8 lazy 'BL.ByteString' builder.
newtype Utf8Builder = Utf8Builder { utf8Builder ∷ BB.Builder }
                      deriving (Typeable, Monoid)

instance IsString Utf8Builder where
  fromString = Utf8Builder . BB.stringUtf8
  {-# INLINE fromString #-}

instance Printer Utf8Builder where
  char = Utf8Builder . BB.charUtf8
  {-# INLINE char #-}
  char7 = Utf8Builder . BB.char7
  {-# INLINE char7 #-}
  string7 = Utf8Builder . BB.string7
  {-# INLINE string7 #-}
  text = Utf8Builder . BB.byteString . TS.encodeUtf8
  {-# INLINE text #-}
  lazyText = Utf8Builder . BB.lazyByteString . TL.encodeUtf8
  {-# INLINE lazyText #-}
  ascii = Utf8Builder . BB.byteString
  {-# INLINE ascii #-}
  lazyAscii = Utf8Builder . BB.lazyByteString
  {-# INLINE lazyAscii #-}
  utf8 = Utf8Builder . BB.byteString
  {-# INLINE utf8 #-}
  lazyUtf8 = Utf8Builder . BB.lazyByteString
  {-# INLINE lazyUtf8 #-}

buildUtf8 ∷ Utf8Builder → BS.ByteString
buildUtf8 = fold . BL.toChunks . buildLazyUtf8
{-# INLINE buildUtf8 #-}

buildLazyUtf8 ∷ Utf8Builder → BL.ByteString
buildLazyUtf8 = BB.toLazyByteString . utf8Builder
{-# INLINE buildLazyUtf8 #-}

instance Printer PP.Doc where
  char = PP.char
  {-# INLINE char #-}

#if !MIN_VERSION_base(4,5,0)
-- | An infix synonym for 'mappend'.
(<>) ∷ Monoid m ⇒ m → m → m
(<>) = mappend
{-# INLINE (<>) #-}
#endif

-- | 'mconcat' for 'Foldable' data structures.
hcat ∷ (Printer p, Foldable f) ⇒ f p → p
hcat = fold
{-# INLINE hcat #-}

-- | Combine the items of a 'Foldable' data structure using the provided
--   function. If the data structure is empty, 'mempty' is returned.
fcat ∷ (Foldable f, Printer p) ⇒ (p → p → p) → f p → p
fcat c f = case toList f of
  [] → mempty
  ps → foldr1 c ps
{-# INLINABLE fcat #-}

-- | Concatenate two 'Printer's with a separator between them.
separate ∷ Printer p
         ⇒ p -- ^ The separator
         → p → p → p
separate s x y = x <> s <> y
{-# INLINE separate #-}

infixr 6 <+>

-- | Concatenate two 'Printer's with a space between them.
(<+>) ∷ Printer p ⇒ p → p → p
(<+>) = separate (char7 ' ')
{-# INLINE (<+>) #-}

-- | Concatenate the items of a 'Foldable' data structure
--   with spaces between them.
hsep ∷ (Printer p, Foldable f) ⇒ f p → p
hsep = fcat (<+>)
{-# INLINE hsep #-}

-- | A shorthand for @'fcat' . 'separate'@.
fsep ∷ (Foldable f, Printer p) ⇒ p → f p → p
fsep = fcat . separate
{-# INLINE fsep #-}

-- | Concatenate the items of a 'Foldable' data structure with commas
--   between them.
--
-- @
--   'list' = 'fsep' ('char7' ',')
-- @
list ∷ (Foldable f, Printer p) ⇒ f p → p
list = fsep (char7 ',')
{-# INLINE list #-}

-- | Enclose a 'Printer' with parentheses.
parens ∷ Printer p ⇒ p → p
parens p = char7 '(' <> p <> char7 ')'
{-# INLINE parens #-}

-- | Enclose a 'Printer' with square brackets.
brackets ∷ Printer p ⇒ p → p
brackets p = char7 '[' <> p <> char7 ']'
{-# INLINE brackets #-}

-- | Enclose a 'Printer' with curly braces.
braces ∷ Printer p ⇒ p → p
braces p = char7 '{' <> p <> char7 '}'
{-# INLINE braces #-}

-- | Enclose a 'Printer' with angle brackets.
angles ∷ Printer p ⇒ p → p
angles p = char7 '<' <> p <> char7 '>'
{-# INLINE angles #-}

-- | Enclose a 'Printer' with single quotes.
squotes ∷ Printer p ⇒ p → p
squotes p = char7 '\'' <> p <> char7 '\''
{-# INLINE squotes #-}

-- | Enclose a 'Printer' with double quotes.
dquotes ∷ Printer p ⇒ p → p
dquotes p  = char7 '\"' <> p <> char7 '\"'
{-# INLINE dquotes #-}

-- | Prepend all but the first element of a 'Traversable' with the
--   provided value, e.g.
--   @'punctuateL' /p/ [/x1/, /x2/, ..., /xN/] =
--      [/x1/, /p/ '<>' /x2/, ..., /p/ '<>' /xN/]@
punctuateL ∷ (Traversable t, Printer p) ⇒ p → t p → t p
punctuateL p =
  snd . mapAccumL (\f a → if f then (False, a) else (False, p <> a)) True
{-# INLINE punctuateL #-}

-- | Append the provided value to all but the last element of a 'Traversable',
--   e.g. @'punctuateR' /p/ [/x1/, ..., /xN-1/, /xN/] =
--           [/x1/ '<>' /p/, ..., /xN-1/ '<>' /p/, /xN/]@
punctuateR ∷ (Traversable t, Printer p) ⇒ p → t p → t p
punctuateR p =
  snd . mapAccumR (\l a → if l then (False, a) else (False, a <> p)) True
{-# INLINE punctuateR #-}

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

-- | Positonal numeral system with a power of two radix.
class PositionalSystem s ⇒ BitSystem s where
  -- | Numer of bits occupied by a digit.
  digitBitsIn ∷ s → Int
  -- | The number that has 'digitBitsIn' least significant bits set to ones
  --   and all the other bits set to zeroes.
  digitMaskIn ∷ Num α ⇒ s → α
  -- | Map the last digit of a number to the corresponding 'Int' value.
  lastDigitIn ∷ Bits α ⇒ s → α → Int

-- | The binary numeral system.
data Binary = Binary deriving (Typeable, Eq, Ord, Show, Read)

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

instance BitSystem Binary where
  digitBitsIn _ = 1
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 1
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = if testBit n 0 then 1 else 0
  {-# INLINE lastDigitIn #-}

-- | The octal numeral system.
data Octal = Octal deriving (Typeable, Eq, Ord, Show, Read)

instance PositionalSystem Octal where
  systemName _ = "octal"
  {-# INLINE systemName #-}
  radixIn _ = 8
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isOctDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzOctDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromOctDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzOctDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromOctDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

instance BitSystem Octal where
  digitBitsIn _ = 3
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 7
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
  {-# INLINE lastDigitIn #-}

-- | The decimal numeral system.
data Decimal = Decimal deriving (Typeable, Eq, Ord, Show, Read)

instance PositionalSystem Decimal where
  systemName _ = "decimal"
  {-# INLINE systemName #-}
  radixIn _ = 10
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isDecDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzDecDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromDecDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzDecDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromDecDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i = chr $! ord '0' + i
  {-# INLINE intToDigitIn #-}

-- | The hexadecimal numeral system.
data Hexadecimal = Hexadecimal

instance PositionalSystem Hexadecimal where
  systemName _ = "hexadecimal"
  {-# INLINE systemName #-}
  radixIn _ = 16
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isHexDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzHexDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromHexDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzHexDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromHexDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}

instance BitSystem Hexadecimal where
  digitBitsIn _ = 4
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 15
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
                  + (if testBit n 3 then 8 else 0)
  {-# INLINABLE lastDigitIn #-}

-- | The hexadecimal numeral system, using lower case digits.
data LowHex = LowHex

instance PositionalSystem LowHex where
  systemName _ = "lower case hexadecimal"
  {-# INLINE systemName #-}
  radixIn _ = 16
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isUpHexDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzUpHexDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromUpHexDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzUpHexDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromUpHexDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'a' + (i - 10) 
  {-# INLINE intToDigitIn #-}

instance BitSystem LowHex where
  digitBitsIn _ = 4
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 15
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
                  + (if testBit n 3 then 8 else 0)
  {-# INLINABLE lastDigitIn #-}

-- | The hexadecimal numeral system, using upper case digits.
data UpHex = UpHex

instance PositionalSystem UpHex where
  systemName _ = "upper case hexadecimal"
  {-# INLINE systemName #-}
  radixIn _ = 16
  {-# INLINE radixIn #-}
  isDigitIn _ = A.isUpHexDigit
  {-# INLINE isDigitIn #-}
  isNzDigitIn _ = A.isNzUpHexDigit
  {-# INLINE isNzDigitIn #-}
  fromDigitIn _ = A.fromUpHexDigit
  {-# INLINE fromDigitIn #-}
  fromNzDigitIn _ = A.fromNzUpHexDigit
  {-# INLINE fromNzDigitIn #-}
  unsafeFromDigitIn _ = A.unsafeFromUpHexDigit
  {-# INLINE unsafeFromDigitIn #-}
  intToDigitIn _ i | i < 10    = chr $! ord '0' + i
                   | otherwise = chr $! ord 'A' + (i - 10) 
  {-# INLINE intToDigitIn #-}

instance BitSystem UpHex where
  digitBitsIn _ = 4
  {-# INLINE digitBitsIn #-}
  digitMaskIn _ = 15
  {-# INLINE digitMaskIn #-}
  lastDigitIn _ n = (if testBit n 0 then 1 else 0)
                  + (if testBit n 1 then 2 else 0)
                  + (if testBit n 2 then 4 else 0)
                  + (if testBit n 3 then 8 else 0)
  {-# INLINABLE lastDigitIn #-}

-- | Print a non-negative number in the specified positional numeral system.
nonNegative ∷ (PositionalSystem s, Integral α, Printer p) ⇒ s → α → p
nonNegative s = go (printDigitIn s $! intToDigitIn s 0)
  where go p 0 = p
        go _ m = go mempty q <> printDigitIn s d
          where (q, r) = quotRem m radix
                !d     = intToDigitIn s $ fromIntegral r
        radix = radixIn s
{-# INLINABLE nonNegative #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int8 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int16 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int32 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Int64 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word8 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word16 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word32 → p #-}
{-# SPECIALIZE nonNegative ∷ Printer p ⇒ Decimal → Word64 → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ Decimal → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE nonNegative ∷ (Integral α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-negative number in the binary numeral system.
nnBinary ∷ (Integral α, Printer p) ⇒ α → p
nnBinary = nonNegative Binary
{-# INLINE nnBinary #-}

-- | Print a non-negative number in the octal numeral system.
nnOctal ∷ (Integral α, Printer p) ⇒ α → p
nnOctal = nonNegative Octal
{-# INLINE nnOctal #-}

-- | Print a non-negative number in the decimal numeral system.
nnDecimal ∷ (Integral α, Printer p) ⇒ α → p
nnDecimal = nonNegative Decimal
{-# INLINE nnDecimal #-}

-- | Print a non-negative number in the hexadecimal numeral system
--   using lower case digits.
nnLowHex ∷ (Integral α, Printer p) ⇒ α → p
nnLowHex = nonNegative LowHex
{-# INLINE nnLowHex #-}

-- | Print a non-negative number in the hexadecimal numeral system
--   using upper case digits.
nnUpHex ∷ (Integral α, Printer p) ⇒ α → p
nnUpHex = nonNegative UpHex
{-# INLINE nnUpHex #-}

-- | Print a non-negative binary number in the specified positional numeral
--   system.
nnBits ∷ (BitSystem s, Num α, Bits α, Printer p) ⇒ s → α → p
nnBits s = go (printDigitIn s $! intToDigitIn s 0)
  where go p 0 = p
        go _ m = go mempty (shiftR m digitBits) <> printDigitIn s d
          where !d = intToDigitIn s $ lastDigitIn s m
        digitBits = digitBitsIn s
{-# INLINABLE nnBits #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Binary → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Octal → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ Hexadecimal → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ LowHex → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Int64 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word8 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word16 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word32 → p #-}
{-# SPECIALIZE nnBits ∷ Printer p ⇒ UpHex → Word64 → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ Hexadecimal → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE nnBits ∷ (Num α, Bits α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-negative binary number in the binary numeral system.
nnBinaryBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnBinaryBits = nnBits Binary
{-# INLINE nnBinaryBits #-}

-- | Print a non-negative binary number in the octal numeral system.
nnOctalBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnOctalBits = nnBits Octal
{-# INLINE nnOctalBits #-}

-- | Print a non-negative binary number in the hexadecimal numeral system
--   using lower case digits.
nnLowHexBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnLowHexBits = nnBits LowHex
{-# INLINE nnLowHexBits #-}

-- | Print a non-negative binary number in the hexadecimal numeral system
--   using upper case digits.
nnUpHexBits ∷ (Num α, Bits α, Printer p) ⇒ α → p
nnUpHexBits = nnBits UpHex
{-# INLINE nnUpHexBits #-}

-- | Print a non-positive number in the specified positional numeral system.
--   For example, @'nonPositive' 'Decimal' (-/123/)@ would print \"123\".
nonPositive ∷ (PositionalSystem s, Integral α, Printer p) ⇒ s → α → p
nonPositive s = go (printDigitIn s $! intToDigitIn s 0)
  where go p 0 = p
        go _ m = go mempty q <> printDigitIn s d
          where (q, r) = quotRem m radix
                !d     = intToDigitIn s $ negate $ fromIntegral r
        radix = radixIn s
{-# INLINABLE nonPositive #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int8 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int16 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int32 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Int64 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word8 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word16 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word32 → p #-}
{-# SPECIALIZE nonPositive ∷ Printer p ⇒ Decimal → Word64 → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ Decimal → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE nonPositive ∷ (Integral α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-positive number in the binary numeral system.
npBinary ∷ (Integral α, Printer p) ⇒ α → p
npBinary = nonPositive Binary
{-# INLINE npBinary #-}

-- | Print a non-positive number in the octal numeral system.
npOctal ∷ (Integral α, Printer p) ⇒ α → p
npOctal = nonPositive Octal
{-# INLINE npOctal #-}

-- | Print a non-positive number in the decimal numeral system.
npDecimal ∷ (Integral α, Printer p) ⇒ α → p
npDecimal = nonPositive Decimal
{-# INLINE npDecimal #-}

-- | Print a non-positive number in the hexadecimal numeral system
--   using lower case digits.
npLowHex ∷ (Integral α, Printer p) ⇒ α → p
npLowHex = nonPositive LowHex
{-# INLINE npLowHex #-}

-- | Print a non-positive number in the hexadecimal numeral system
--   using upper case digits.
npUpHex ∷ (Integral α, Printer p) ⇒ α → p
npUpHex = nonPositive UpHex
{-# INLINE npUpHex #-}

-- | Print a non-positive two-compliment binary number in the specified
--   positional numeral system. For example, @'npBits' 'UpHex' (-/0xABC/)@
--   would print \"ABC\".
npBits ∷ (BitSystem s, Ord α, Num α, Bits α, Printer p) ⇒ s → α → p
npBits s n = case testBit n 0 of
    True → go mempty (shiftR (negate n) digitBits) <> printDigitIn s d
      where !d = intToDigitIn s $ radix - lastDigitIn s n
    False → case n > negate (bit digitBits) of
        True → printDigitIn s d'
        False → go mempty m <> printDigitIn s d'
          where m | d == 0    = negate $ shiftR n digitBits
                  | otherwise = complement $ shiftR n digitBits
      where !d  = lastDigitIn s n
            !d' = intToDigitIn s $ (radix - d) .&. digitMask
  where go p 0 = p
        go p m = go p (shiftR m digitBits) <> printDigitIn s d
          where !d = intToDigitIn s $ lastDigitIn s m
        radix     = radixIn s
        digitMask = digitMaskIn s
        digitBits = digitBitsIn s
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Binary → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Octal → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ Hexadecimal → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ LowHex → Word64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Int64 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word8 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word16 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word32 → p #-}
{-# SPECIALIZE npBits ∷ Printer p ⇒ UpHex → Word64 → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Binary → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Octal → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Hexadecimal → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ LowHex → α → p #-}
{-# SPECIALIZE npBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ UpHex → α → p #-}

-- | Print a non-positive binary number in the binary numeral system.
npBinaryBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npBinaryBits = npBits Binary
{-# INLINE npBinaryBits #-}

-- | Print a non-positive binary number in the octal numeral system.
npOctalBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npOctalBits = npBits Octal
{-# INLINE npOctalBits #-}

-- | Print a non-positive binary number in the hexadecimal numeral system
--   using lower case digits.
npLowHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npLowHexBits = npBits LowHex
{-# INLINE npLowHexBits #-}

-- | Print a non-positive binary number in the hexadecimal numeral system
--   using upper case digits.
npUpHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
npUpHexBits = npBits UpHex
{-# INLINE npUpHexBits #-}

-- | Print a number in the specified positional numeral system.
number' ∷ (PositionalSystem s, Ord α, Integral α, Printer p)
        ⇒ s
        → p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for positive values
        → α → p
number' s neg z pos n = case compare n 0 of
    LT → go neg q <> printDigitIn s d
      where (q, r) = quotRem n (negate radix)
            !d     = intToDigitIn s $ negate $ fromIntegral r
    EQ → z <> (printDigitIn s $! intToDigitIn s 0)
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

-- | Print a number in the specified positional numeral system. Negative
--   values are prefixed with a minus sign.
number ∷ (PositionalSystem s, Ord α, Integral α, Printer p) ⇒ s → α → p
number s = number' s (char7 '-') mempty mempty
{-# INLINE number #-}

-- | Print a number in the binary numeral system.
binary' ∷ (Ord α, Integral α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for positive values
        → α → p
binary' = number' Binary
{-# INLINE binary' #-}

-- | Print a number in the binary numeral system. Negative values
--   are prefixed with a minus sign.
binary ∷ (Ord α, Integral α, Printer p) ⇒ α → p
binary = number Binary
{-# INLINE binary #-}

-- | Print a number in the octal numeral system.
octal' ∷ (Ord α, Integral α, Printer p)
       ⇒ p -- ^ Prefix for negative values
       → p -- ^ Prefix for the zero
       → p -- ^ Prefix for positive values
       → α → p
octal' = number' Octal
{-# INLINE octal' #-}

-- | Print a number in the octal numeral system. Negative values
--   are prefixed with a minus sign.
octal ∷ (Ord α, Integral α, Printer p) ⇒ α → p
octal = number Octal
{-# INLINE octal #-}

-- | Print a number in the decimal numeral system.
decimal' ∷ (Ord α, Integral α, Printer p)
         ⇒ p -- ^ Prefix for negative values
         → p -- ^ Prefix for the zero
         → p -- ^ Prefix for positive values
         → α → p
decimal' = number' Decimal
{-# INLINE decimal' #-}

-- | Print a number in the decimal numeral system. Negative values
--   are prefixed with a minus sign.
decimal ∷ (Ord α, Integral α, Printer p) ⇒ α → p
decimal = number Decimal
{-# INLINE decimal #-}

-- | Print a number in the hexadecimal numeral system using lower case
--   digits.
lowHex' ∷ (Ord α, Integral α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for positive values
        → α → p
lowHex' = number' LowHex
{-# INLINE lowHex' #-}

-- | Print a number in the hexadecimal numeral system using lower case
--   digits. Negative values are prefixed with a minus sign.
lowHex ∷ (Ord α, Integral α, Printer p) ⇒ α → p
lowHex = number LowHex
{-# INLINE lowHex #-}

-- | Print a number in the hexadecimal numeral system using upper case
--   digits.
upHex' ∷ (Ord α, Integral α, Printer p)
       ⇒ p -- ^ Prefix for negative values
       → p -- ^ Prefix for the zero
       → p -- ^ Prefix for positive values
       → α → p
upHex' = number' UpHex
{-# INLINE upHex' #-}

-- | Print a number in the hexadecimal numeral system using upper case
--   digits. Negative values are prefixed with a minus sign.
upHex ∷ (Ord α, Integral α, Printer p) ⇒ α → p
upHex = number UpHex
{-# INLINE upHex #-}

-- | Print a binary number in the specified positional numeral system.
bits' ∷ (BitSystem s, Ord α, Num α, Bits α, Printer p)
      ⇒ s
      → p -- ^ Prefix for negative values
      → p -- ^ Prefix for the zero
      → p -- ^ Prefix for positive values
      → α → p
bits' s neg z pos n = case compare n 0 of
    LT → case testBit n 0 of
           True → go neg (shiftR (negate n) digitBits) <> printDigitIn s d
             where !d = intToDigitIn s $ radix - lastDigitIn s n
           False → case n > negate (bit digitBits) of
               True → neg <> printDigitIn s d'
               False → go neg m <> printDigitIn s d'
                 where m | d == 0    = negate $ shiftR n digitBits
                         | otherwise = complement $ shiftR n digitBits
             where !d  = lastDigitIn s n
                   !d' = intToDigitIn s $ (radix - d) .&. digitMask
    EQ → z <> (printDigitIn s $! intToDigitIn s 0)
    GT → go pos (shiftR n digitBits) <> printDigitIn s d
      where !d = intToDigitIn s $ lastDigitIn s n
  where go p 0 = p
        go p m = go p (shiftR m digitBits) <> printDigitIn s d
          where !d = intToDigitIn s $ lastDigitIn s m
        radix     = radixIn s
        digitMask = digitMaskIn s
        digitBits = digitBitsIn s
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Binary → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Octal → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ Hexadecimal → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ LowHex → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Int64 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word8 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word16 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word32 → p #-}
{-# SPECIALIZE bits' ∷ Printer p ⇒ UpHex → p → p → p → Word64 → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Binary → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Octal → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Hexadecimal → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ LowHex → p → p → p → α → p #-}
{-# SPECIALIZE bits' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ UpHex → p → p → p → α → p #-}

-- | Print a binary number in the specified positional numeral system.
--   Negative values are prefixed with a minus sign.
bits ∷ (BitSystem s, Ord α, Num α, Bits α, Printer p) ⇒ s → α → p
bits s = bits' s (char7 '-') mempty mempty
{-# INLINE bits #-}

-- | Print a binary number in the binary numeral system.
binaryBits' ∷ (Ord α, Num α, Bits α, Printer p)
            ⇒ p -- ^ Prefix for negative values
            → p -- ^ Prefix for the zero
            → p -- ^ Prefix for positive values
            → α → p
binaryBits' = bits' Binary
{-# INLINE binaryBits' #-}

-- | Print a binary number in the binary numeral system. Negative values
--   are prefixed with a minus sign.
binaryBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
binaryBits = bits Binary
{-# INLINE binaryBits #-}

-- | Print a binary number in the octal numeral system.
octalBits' ∷ (Ord α, Num α, Bits α, Printer p)
           ⇒ p -- ^ Prefix for negative values
           → p -- ^ Prefix for the zero
           → p -- ^ Prefix for positive values
           → α → p
octalBits' = bits' Octal
{-# INLINE octalBits' #-}

-- | Print a binary number in the octal numeral system. Negative values
--   are prefixed with a minus sign.
octalBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
octalBits = bits Octal
{-# INLINE octalBits #-}

-- | Print a binary number in the hexadecimal numeral system using lower
--   case digits.
lowHexBits' ∷ (Ord α, Num α, Bits α, Printer p)
            ⇒ p -- ^ Prefix for negative values
            → p -- ^ Prefix for the zero
            → p -- ^ Prefix for positive values
            → α → p
lowHexBits' = bits' LowHex
{-# INLINE lowHexBits' #-}

-- | Print a binary number in the hexadecimal numeral system using lower
--   case digits. Negative values are prefixed with a minus sign.
lowHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
lowHexBits = bits LowHex
{-# INLINE lowHexBits #-}

-- | Print a binary number in the hexadecimal numeral system using upper
--   case digits.
upHexBits' ∷ (Ord α, Num α, Bits α, Printer p)
           ⇒ p -- ^ Prefix for negative values
           → p -- ^ Prefix for the zero
           → p -- ^ Prefix for positive values
           → α → p
upHexBits' = bits' UpHex
{-# INLINE upHexBits' #-}

-- | Print a binary number in the hexadecimal numeral system using upper
--   case digits. Negative values are prefixed with a minus sign.
upHexBits ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
upHexBits = bits UpHex
{-# INLINE upHexBits #-}

infixr 5 <->

-- | Printers that can produce multiple lines of text.
class Printer p ⇒ MultilinePrinter p where
  -- | Combine two lines. Must be associative, i.e.
  --   /x/ '<->' (/y/ '<->' /z/) = (/x/ '<->' /y/) '<->' /z/.
  (<->) ∷ p → p → p

instance MultilinePrinter PP.Doc where
  (<->) = (PP.$+$)
  {-# INLINE (<->) #-}

-- | Combine the items of a 'Foldable' data structure with '<->'.
lines ∷ (MultilinePrinter p, Foldable f) ⇒ f p → p
lines = fcat (<->)
{-# INLINE lines #-}

-- | Print the LF character (/'\n'/).
newLine ∷ Printer p ⇒ p
newLine = char '\n'
{-# INLINE newLine #-}

-- | Print CR (/'\r'/) followed by LF (/'\n'/).
crlf ∷ Printer p ⇒ p
crlf = char '\r' <> char '\n'
{-# INLINE crlf #-}

-- | A multiline printer that combines lines with the provided function.
newtype LinePrinter p = LinePrinter { linePrinter ∷ (p → p → p) → p }
                        deriving Typeable

instance IsString p ⇒ IsString (LinePrinter p) where
  fromString = LinePrinter . const . fromString
  {-# INLINE fromString #-}

instance Monoid p ⇒ Monoid (LinePrinter p) where
  mempty = LinePrinter $ const mempty
  {-# INLINE mempty #-}
  mappend x y = LinePrinter $ \l →
                  mappend (linePrinter x l) (linePrinter y l)
  {-# INLINE mappend #-}
  mconcat xs = LinePrinter $ \l → mconcat (map (\x → linePrinter x l) xs)
  {-# INLINE mconcat #-}

instance Printer p ⇒ Printer (LinePrinter p) where
  char = LinePrinter . const . char
  {-# INLINE char #-}
  char7 = LinePrinter . const . char7
  {-# INLINE char7 #-}
  string = LinePrinter . const . string
  {-# INLINE string #-}
  string7 = LinePrinter . const . string7
  {-# INLINE string7 #-}
  text = LinePrinter . const . text
  {-# INLINE text #-}
  lazyText = LinePrinter . const . lazyText
  {-# INLINE lazyText #-}
  ascii = LinePrinter . const . ascii
  {-# INLINE ascii #-}
  lazyAscii = LinePrinter . const . lazyAscii
  {-# INLINE lazyAscii #-}
  utf8 = LinePrinter . const . utf8
  {-# INLINE utf8 #-}
  lazyUtf8 = LinePrinter . const . lazyUtf8
  {-# INLINE lazyUtf8 #-}
  
instance Printer p ⇒ MultilinePrinter (LinePrinter p) where
  x <-> y = LinePrinter $ \l → l (linePrinter x l) (linePrinter y l)
  {-# INLINE (<->) #-}

-- | Separate lines with 'newLine'.
lfPrinter ∷ Printer p ⇒ LinePrinter p → p
lfPrinter p = linePrinter p (separate newLine)
{-# INLINE lfPrinter #-}

-- | Separate lines with 'crlf'.
crlfPrinter ∷ Printer p ⇒ LinePrinter p → p
crlfPrinter p = linePrinter p (separate crlf)
{-# INLINE crlfPrinter #-}

