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
  , AsciiBuilder(..)
  , buildAscii
  , Utf8Builder(..)
  , buildUtf8
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
  , unsignedBinary
  , unsignedOctal
  , unsignedUpHex
  , unsignedLowHex
  , unsignedDecimal
  , binary'
  , binary
  , octal'
  , octal
  , lowHex'
  , lowHex
  , upHex'
  , upHex
  , decimal'
  , decimal
  -- * Multiline printers
  , MultilinePrinter(..)
  , lines
  , newLine
  , crlf
  , LinePrinter(..)
  , lfPrinter
  , crlfPrinter
  -- * Default printers
  , Printable(..)
  , toString
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

buildAscii ∷ AsciiBuilder → BL.ByteString
buildAscii = BB.toLazyByteString . asciiBuilder
{-# INLINE buildAscii #-}

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

buildUtf8 ∷ Utf8Builder → BL.ByteString
buildUtf8 = BB.toLazyByteString . utf8Builder
{-# INLINE buildUtf8 #-}

instance Printer PP.Doc where
  char = PP.char
  {-# INLINE char #-}

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

-- | An alias for @'fcat' . 'separate'@.
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

-- | Print an unsigned number in the binary numeral system.
unsignedBinary ∷ (Num α, Bits α, Printer p) ⇒ α → p
unsignedBinary = go (char7 '0')
  where go p 0 = p
        go _ m = go mempty (shiftR m 1) <> char7 c
          where !c = if testBit m 0 then '1' else '0'
{-# INLINABLE unsignedBinary #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Int → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Int8 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Int16 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Int32 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Int64 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Word → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Word8 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Word16 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Word32 → p #-}
{-# SPECIALIZE unsignedBinary ∷ Printer p ⇒ Word64 → p #-}

-- | Print an unsigned number in the octal numeral system.
unsignedOctal ∷ (Num α, Bits α, Printer p) ⇒ α → p
unsignedOctal = go (char7 '0')
  where go p 0 = p
        go _ m = go mempty (shiftR m 3) <> char7 c
          where !r = (if testBit m 0 then 1 else 0)
                   + (if testBit m 1 then 2 else 0)
                   + (if testBit m 2 then 4 else 0)
                !c = chr $ ord '0' + r
{-# INLINABLE unsignedOctal #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Int → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Int8 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Int16 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Int32 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Int64 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Word → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Word8 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Word16 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Word32 → p #-}
{-# SPECIALIZE unsignedOctal ∷ Printer p ⇒ Word64 → p #-}

-- | Print an unsigned number in the hexadecimal numeral system
--   using lower case digits.
unsignedLowHex ∷ (Num α, Bits α, Printer p) ⇒ α → p
unsignedLowHex = go (char7 '0')
  where go p 0 = p
        go _ m = go mempty (shiftR m 4) <> char7 c
          where !r = (if testBit m 0 then 1 else 0)
                   + (if testBit m 1 then 2 else 0)
                   + (if testBit m 2 then 4 else 0)
                   + (if testBit m 3 then 8 else 0)
                !c | r < 10    = chr $ ord '0' + r
                   | otherwise = chr $ ord 'a' + r - 10
{-# INLINABLE unsignedLowHex #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Int → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Int8 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Int16 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Int32 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Int64 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Word → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Word8 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Word16 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Word32 → p #-}
{-# SPECIALIZE unsignedLowHex ∷ Printer p ⇒ Word64 → p #-}

-- | Print an unsigned number in the hexadecimal numeral system
--   using upper case digits.
unsignedUpHex ∷ (Num α, Bits α, Printer p) ⇒ α → p
unsignedUpHex = go (char7 '0')
  where go p 0 = p
        go _ m = go mempty (shiftR m 4) <> char7 c
          where !r = (if testBit m 0 then 1 else 0)
                   + (if testBit m 1 then 2 else 0)
                   + (if testBit m 2 then 4 else 0)
                   + (if testBit m 3 then 8 else 0)
                !c | r < 10    = chr $ ord '0' + r
                   | otherwise = chr $ ord 'A' + r - 10
{-# INLINABLE unsignedUpHex #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Int → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Int8 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Int16 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Int32 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Int64 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Word → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Word8 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Word16 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Word32 → p #-}
{-# SPECIALIZE unsignedUpHex ∷ Printer p ⇒ Word64 → p #-}

-- | Print an unsigned number in the decimal numeral system.
unsignedDecimal ∷ (Integral α, Printer p) ⇒ α → p
unsignedDecimal = go (char7 '0')
  where go p 0 = p
        go _ m = go mempty q <> char7 c
          where (q, r) = quotRem m 10
                !c = chr $ ord '0' + fromIntegral r
{-# INLINABLE unsignedDecimal #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Int → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Int8 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Int16 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Int32 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Int64 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Word → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Word8 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Word16 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Word32 → p #-}
{-# SPECIALIZE unsignedDecimal ∷ Printer p ⇒ Word64 → p #-}

-- | Print a number in the binary numeral system.
binary' ∷ (Ord α, Num α, Bits α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for positive values
        → α → p
binary' neg z pos n = case compare n 0 of
    LT → case testBit n 0 of
           True → go neg (shiftR (negate n) 1) <> char7 '1'
           False → go neg (negate $ shiftR n 1) <> char7 '0'
    EQ → z <> char7 '0'
    GT → go pos (shiftR n 1) <> char7 c
      where !c = if testBit n 0 then '1' else '0'
  where go p 0 = p
        go p m = go p (shiftR m 1) <> char7 c
          where !c = if testBit m 0 then '1' else '0'
{-# INLINABLE binary' #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Int → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Int8 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Int16 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Int32 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Int64 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Word → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Word8 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Word16 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Word32 → p #-}
{-# SPECIALIZE binary' ∷ Printer p ⇒ p → p → p → Word64 → p #-}

-- | Print a number in the binary numeral system. Negative values
--   are prefixed with \"-0b\", postive values are prefixed with \"0b\".
binary ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
binary = binary' (string7 "-0b") mempty (string7 "0b")
{-# INLINE binary #-}

-- | Print a number in the octal numeral system.
octal' ∷ (Ord α, Num α, Bits α, Printer p)
       ⇒ p -- ^ Prefix for negative values
       → p -- ^ Prefix for the zero
       → p -- ^ Prefix for positive values
       → α → p
octal' neg z pos n = case compare n 0 of
    LT → case testBit n 0 of
           True → go neg (shiftR (negate n) 3) <> char7 c
             where !r = 7
                      - (if testBit n 1 then 2 else 0)
                      - (if testBit n 2 then 4 else 0)
                   !c = chr $ ord '0' + r
           False → case n > negate (bit 3) of
               True → neg <> char7 c 
               False → go neg m <> char7 c
                 where m | v == 0    = negate $ shiftR n 3
                         | otherwise = complement $ shiftR n 3
             where !v = (if testBit n 1 then 2 else 0)
                      + (if testBit n 2 then 4 else 0)
                   !r = (8 - v) .&. 7
                   !c = chr $ ord '0' + r
    EQ → z <> char7 '0'
    GT → go pos (shiftR n 3) <> char7 c
      where !r = (if testBit n 0 then 1 else 0)
               + (if testBit n 1 then 2 else 0)
               + (if testBit n 2 then 4 else 0)
            !c = chr $ ord '0' + r
  where go p 0 = p
        go p m = go p (shiftR m 3) <> char7 c
          where !r = (if testBit m 0 then 1 else 0)
                   + (if testBit m 1 then 2 else 0)
                   + (if testBit m 2 then 4 else 0)
                !c = chr $ ord '0' + r
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Int → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Int8 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Int16 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Int32 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Int64 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Word → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Word8 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Word16 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Word32 → p #-}
{-# SPECIALIZE octal' ∷ Printer p ⇒ p → p → p → Word64 → p #-}

-- | Print a number in the octal numeral system. Negative values
--   are prefixed with \"-0o\", postive values are prefixed with \"0o\".
octal ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
octal = octal' (string7 "-0o") mempty (string7 "0o")
{-# INLINE octal #-}

hex' ∷ (Ord α, Num α, Bits α, Printer p) ⇒ Int → p → p → p → α → p
hex' a neg z pos n = case compare n 0 of
    LT → case testBit n 0 of
      True → go neg (shiftR (negate n) 4) <> char7 c
        where !r = 15
                 - (if testBit n 1 then 2 else 0)
                 - (if testBit n 2 then 4 else 0)
                 - (if testBit n 3 then 8 else 0)
              !c | r < 10    = chr $ ord '0' + r
                 | otherwise = chr $ a + (r - 10) 
      False → case n > negate (bit 4) of
          True → neg <> char7 c 
          False → go neg m <> char7 c
            where m | v == 0    = negate $ shiftR n 4
                    | otherwise = complement $ shiftR n 4
        where !v = (if testBit n 1 then 2 else 0)
                 + (if testBit n 2 then 4 else 0)
                 + (if testBit n 3 then 8 else 0)
              !r = (16 - v) .&. 15
              !c | r < 10    = chr $ ord '0' + r
                 | otherwise = chr $ a + (r - 10)
    EQ → z <> char7 '0'
    GT → go pos (shiftR n 4) <> char7 c
      where !r = (if testBit n 0 then 1 else 0)
               + (if testBit n 1 then 2 else 0)
               + (if testBit n 2 then 4 else 0)
               + (if testBit n 3 then 8 else 0)
            !c | r < 10    = chr $ ord '0' + r
               | otherwise = chr $ a + (r - 10)
  where go p 0 = p
        go p m = go p (shiftR m 4) <> char7 c
          where !r = (if testBit m 0 then 1 else 0)
                   + (if testBit m 1 then 2 else 0)
                   + (if testBit m 2 then 4 else 0)
                   + (if testBit m 3 then 8 else 0)
                !c | r < 10    = chr $ ord '0' + r
                   | otherwise = chr $ a + (r - 10)
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Int → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Int8 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Int16 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Int32 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Int64 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Word → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Word8 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Word16 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Word32 → p #-}
{-# SPECIALIZE hex' ∷ Printer p ⇒ Int → p → p → p → Word64 → p #-}

-- | Print a number in the hexadecimal numeral system using lower case
--   digits.
lowHex' ∷ (Ord α, Num α, Bits α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for postive values
        → α → p
lowHex' = hex' (ord 'a')
{-# INLINE lowHex' #-}

-- | Print a number in the hexadecimal numeral system using lower case
--   digits. Negative values are prefixed with \"-0x\", positive values
--   are prefixed with \"0x\".
lowHex ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
lowHex = lowHex' (string7 "-0x") mempty (string7 "0x")
{-# INLINE lowHex #-}

-- | Print a number in the hexadecimal numeral system using upper case
--   digits.
upHex' ∷ (Ord α, Num α, Bits α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for postive values
        → α → p
upHex' = hex' (ord 'A')
{-# INLINE upHex' #-}

-- | Print a number in the hexadecimal numeral system using upper case
--   digits. Negative values are prefixed with \"-0x\", positive values
--   are prefixed with \"0x\".
upHex ∷ (Ord α, Num α, Bits α, Printer p) ⇒ α → p
upHex = upHex' (string7 "-0x") mempty (string7 "0x")
{-# INLINE upHex #-}

-- | Print a number in the decimal numeral system.
decimal' ∷ (Integral α, Printer p)
        ⇒ p -- ^ Prefix for negative values
        → p -- ^ Prefix for the zero
        → p -- ^ Prefix for postive values
        → α → p
decimal' neg z pos n = case compare n 0 of
    LT → go neg q <> char7 c
      where (q, r) = quotRem n (-10)
            !c     = chr $ ord '0' - fromIntegral r
    EQ → z <> char7 '0'
    GT → go pos q <> char7 c
      where (q, r) = quotRem n 10
            !c = chr $ ord '0' + fromIntegral r
  where go p 0 = p
        go p m = go p q <> char7 c
          where (q, r) = quotRem m 10
                !c = chr $ ord '0' + fromIntegral r
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Int → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Int8 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Int16 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Int32 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Int64 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Word → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Word8 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Word16 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Word32 → p #-}
{-# SPECIALIZE decimal' ∷ Printer p ⇒ p → p → p → Word64 → p #-}

-- | Print a number in the decimal numeral system. Negative values
--   are prefixed with \"-\".
decimal ∷ (Integral α, Printer p) ⇒ α → p
decimal = decimal' (char7 '-') mempty mempty
{-# INLINE decimal #-}

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

-- | The default printer for values of a type.
class Printable α where
  print ∷ Printer p ⇒ α → p
  printList ∷ Printer p ⇒ [α] → p
  printList = defaultPrintList
  {-# INLINE printList #-}

defaultPrintList ∷ (Printable α, Printer p) ⇒ [α] → p
defaultPrintList = brackets . list . map print
{-# INLINE defaultPrintList #-}

instance Printable () where
  print _ = string7 "()"
  {-# INLINE print #-}

instance Printable α ⇒ Printable [α] where
  print = printList
  {-# INLINE print #-}

instance Printable Char where
  print = char
  {-# INLINE print #-}
  printList = string
  {-# INLINE printList #-}

instance Printable TS.Text where
  print = text
  {-# INLINE print #-}

instance Printable TL.Text where
  print = lazyText
  {-# INLINE print #-}

instance Printable Integer where
  print = decimal
  {-# INLINE print #-}

instance Printable Int where
  print = decimal
  {-# INLINE print #-}

instance Printable Int8 where
  print = decimal
  {-# INLINE print #-}

instance Printable Int16 where
  print = decimal
  {-# INLINE print #-}

instance Printable Int32 where
  print = decimal
  {-# INLINE print #-}

instance Printable Int64 where
  print = decimal
  {-# INLINE print #-}

instance Printable Word where
  print = unsignedDecimal
  {-# INLINE print #-}

instance Printable Word8 where
  print = unsignedDecimal
  {-# INLINE print #-}

instance Printable Word16 where
  print = unsignedDecimal
  {-# INLINE print #-}

instance Printable Word32 where
  print = unsignedDecimal
  {-# INLINE print #-}

instance Printable Word64 where
  print = unsignedDecimal
  {-# INLINE print #-}

-- | Print a 'Printable' value via 'StringBuilder', i.e.
--   @'toString' = 'buildString' . 'print'@.
toString ∷ Printable α ⇒ α → String
toString = buildString . print
{-# INLINE toString #-}

