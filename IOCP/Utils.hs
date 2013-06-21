{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Utils (
    -- * Shifts with conversion
    (.<<.),
    (.>>.),

    -- * Storing words in network byte order
    Word16be(..),
    Word32be(..),

    -- * Showing and reading hex-encoded words
    showHex8,
    showHex16,
    showHex32,
    readHex8,
    readHex16,
    readHex32,

    -- * Miscellaneous
    delimit,
    withPtrLen,
    zeroMemory,
) where

import Control.Applicative ((<$>))
import Data.Bits
import Data.Char (chr, ord)
import Data.Data (Data)
import Data.Ix (Ix)
import Data.List (intersperse)
import Data.Word
import Data.Typeable (Typeable)
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Text.Printf (PrintfArg)

-- infixl 5 .|.
infixl 8 .<<.
infixl 8 .>>.

-- | Extend and shift left.  Used to construct big words from little ones.
(.<<.) :: (Integral a, Num b, Bits b) => a -> Int -> b
x .<<. i = fromIntegral x `shiftL` i

-- | Shift right and truncate.  Used to extract little words from big ones.
(.>>.) :: (Integral a, Bits a, Num b) => a -> Int -> b
x .>>. i = fromIntegral (x `shiftR` i)

-- | Read a 'Word16' stored in network byte order (big endian).
peek16be :: Ptr Word8 -> IO Word16
peek16be ptr = do
    a0 <- peekElemOff ptr 0
    a1 <- peekElemOff ptr 1
    return $! (a0 .<<. 8) .|. (a1 .<<. 0)

-- | Write a 'Word16' in network byte order (big endian).
poke16be :: Ptr Word8 -> Word16 -> IO ()
poke16be ptr a = do
    pokeElemOff ptr 0 (a .>>. 8)
    pokeElemOff ptr 1 (a .>>. 0)

-- | Read a 'Word32' stored in network byte order (big endian).
peek32be :: Ptr Word8 -> IO Word32
peek32be ptr = do
    a0 <- peekElemOff ptr 0
    a1 <- peekElemOff ptr 1
    a2 <- peekElemOff ptr 2
    a3 <- peekElemOff ptr 3
    return $! (a0 .<<. 24) .|. (a1 .<<. 16) .|. (a2 .<<. 8) .|. (a3 .<<. 0)

-- | Write a 'Word32' in network byte order (big endian).
poke32be :: Ptr Word8 -> Word32 -> IO ()
poke32be ptr a = do
    pokeElemOff ptr 0 (a .>>. 24)
    pokeElemOff ptr 1 (a .>>. 16)
    pokeElemOff ptr 2 (a .>>.  8)
    pokeElemOff ptr 3 (a .>>.  0)

-- | A 'Word16' whose 'Storable' instance uses network byte order (big endian).
newtype Word16be = Word16be { fromWord16be :: Word16 }
    deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord,
              Real, Ix, Typeable, Bits, PrintfArg)

instance Show Word16be where
    showsPrec p (Word16be n) = showsPrec p n

instance Read Word16be where
    readsPrec p s = [(Word16be n, s') | (n, s') <- readsPrec p s]

-- | Uses network byte order (big endian)
instance Storable Word16be where
    -- Note: unwrapping a newtype does not force the value.
    sizeOf (Word16be n) = sizeOf n
    alignment (Word16be n) = alignment n
    peek ptr = Word16be <$> peek16be (castPtr ptr)
    poke ptr (Word16be n) = poke16be (castPtr ptr) n

-- | A 'Word32' whose 'Storable' instance uses network byte order (big endian).
newtype Word32be = Word32be { fromWord32be :: Word32 }
    deriving (Bounded, Enum, Eq, Integral, Data, Num, Ord,
              Real, Ix, Typeable, Bits, PrintfArg)

instance Show Word32be where
    showsPrec p (Word32be n) = showsPrec p n

instance Read Word32be where
    readsPrec p s = [(Word32be n, s') | (n, s') <- readsPrec p s]

-- | Uses network byte order (big endian)
instance Storable Word32be where
    -- Note: unwrapping a newtype does not force the value.
    sizeOf (Word32be n) = sizeOf n
    alignment (Word32be n) = alignment n
    peek ptr = Word32be <$> peek32be (castPtr ptr)
    poke ptr (Word32be n) = poke32be (castPtr ptr) n

-- | Show a 2-digit hex number, using capital letters.
showHex8 :: Word8 -> ShowS
showHex8 w = f (w `shiftR` 4) . f (w .&. 15)
  where
    f n | n < 10 = showChar $ chr $ ord '0' + fromIntegral n
        | n < 16 = showChar $ chr $ ord 'A' + fromIntegral n - 10
        | otherwise = error "IOCP.Windows.showHex8"

-- | Read a 2-digit hex number, accepting either capital or lowercase letters.
readHex8 :: ReadS Word8
readHex8 (a:b:rest) =
    case (f a, f b) of
        (Just hi, Just lo) -> [((hi `shiftL` 4) .|. lo, rest)]
        _ -> []
  where
    f c | c >= '0' && c <= '9' = Just $ fromIntegral $ ord c - ord '0'
        | c >= 'A' && c <= 'F' = Just $ fromIntegral $ ord c - ord 'A' + 10
        | c >= 'a' && c <= 'f' = Just $ fromIntegral $ ord c - ord 'a' + 10
        | otherwise            = Nothing
readHex8 _ = []

-- | Show a 4-digit hex number, using capital letters.
showHex16 :: Word16 -> ShowS
showHex16 w = showHex8 (w .>>. 8) . showHex8 (w .>>. 0)

-- | Read a 4-digit hex number, accepting either capital or lowercase letters.
readHex16 :: ReadS Word16
readHex16 s0 = do
    (a, s1) <- readHex8 s0
    (b, s2) <- readHex8 s1
    let ab = (a .<<. 8) .|. (b .<<. 0)
    [(ab, s2)]

-- | Show an 8-digit hex number, using capital letters.
showHex32 :: Word32 -> ShowS
showHex32 w = showHex8 (w .>>. 24)
            . showHex8 (w .>>. 16)
            . showHex8 (w .>>. 8)
            . showHex8 (w .>>. 0)

-- | Read an 8-digit hex number, accepting either capital or lowercase letters.
readHex32 :: ReadS Word32
readHex32 s0 = do
    (a, s1) <- readHex8 s0
    (b, s2) <- readHex8 s1
    (c, s3) <- readHex8 s2
    (d, s4) <- readHex8 s3
    let abcd = (a .<<. 24) .|. (b .<<. 16) .|. (c .<<. 8) .|. (d .<<. 0)
    [(abcd, s4)]

-- | Separate a list of 'ShowS' strings with a delimiter character.
delimit :: Char -> [ShowS] -> ShowS
delimit delim = foldr (.) id . intersperse (showChar delim)

-- | Like 'with', but also provide the size of the value (in bytes).
withPtrLen :: Storable a => a -> (Ptr a -> Int -> IO b) -> IO b
withPtrLen val f =
  alloca $ \ptr -> do
    poke ptr val
    f ptr (sizeOf val)
{-# INLINE withPtrLen #-}

foreign import ccall unsafe "string.h memset"
    memset :: Ptr a -> CInt -> CSize -> IO ()

-- | Set the bytes in the given memory range to zeros (using @memset@).
zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 nbytes
