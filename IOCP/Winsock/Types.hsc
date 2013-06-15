{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Winsock.Types (
    HostAddress(..),
    HostAddress6(..),
    PortNumber(..),
) where

import Control.Applicative
import Data.Bits
import Data.Typeable (Typeable)
import Data.Word
import Foreign

#include <winsock2.h>
#include <ws2tcpip.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- |
-- NOTE: the network package uses network byte order for the 'Word32' value,
-- but we use host byte order.  For us, 127.0.0.1 is @HostAddress 0x7f000001@
-- everywhere, rather than @HostAddress 0x0100007f@ on little-endian
-- architectures (such as x86).
newtype HostAddress = HostAddress Word32
    deriving Eq

-- | @in_addr@ (stored in network byte order)
instance Storable HostAddress where
    sizeOf    _ = #{size      struct in_addr}
    alignment _ = #{alignment struct in_addr}
    peek ptr = HostAddress <$> peek32be (castPtr ptr)
    poke ptr (HostAddress w) = poke32be (castPtr ptr) w

data HostAddress6 = HostAddress6 !Word32 !Word32 !Word32 !Word32

-- | @in6_addr@ (stored in network byte order)
instance Storable HostAddress6 where
    sizeOf    _ = #{size      struct in6_addr}
    alignment _ = #{alignment struct in6_addr}

    peek ptr = do
        a <- peek32be (ptr `plusPtr`  0)
        b <- peek32be (ptr `plusPtr`  4)
        c <- peek32be (ptr `plusPtr`  8)
        d <- peek32be (ptr `plusPtr` 12)
        return $! HostAddress6 a b c d

    poke ptr (HostAddress6 a b c d) = do
        poke32be (ptr `plusPtr`  0) a
        poke32be (ptr `plusPtr`  4) b
        poke32be (ptr `plusPtr`  8) c
        poke32be (ptr `plusPtr` 12) d

-- |
-- NOTE: uses host byte order for the 'Word16', unlike the network package.
-- See 'HostAddress'.
newtype PortNumber = PortNumber Word16
    deriving (Eq, Ord, Enum, Num, Real, Integral, Typeable)

instance Show PortNumber where
    showsPrec p (PortNumber n) = showsPrec p n

instance Storable PortNumber where
    sizeOf    _ = #{size      unsigned short}
    alignment _ = #{alignment unsigned short}
    peek ptr = PortNumber <$> peek16be (castPtr ptr)
    poke ptr (PortNumber w) = poke16be (castPtr ptr) w

-- | Read a 'Word16' stored in network byte order (big endian).
peek16be :: Ptr Word8 -> IO Word16
peek16be ptr = do
    let x .<<. i = fromIntegral x `shiftL` i
    a0 <- peekElemOff ptr 0
    a1 <- peekElemOff ptr 1
    return $! (a0 .<<. 8) .|. (a1 .<<. 0)

-- | Write a 'Word16' in network byte order (big endian).
poke16be :: Ptr Word8 -> Word16 -> IO ()
poke16be ptr a = do
    let x .>>. i = fromIntegral (x `shiftR` i)
    pokeElemOff ptr 0 (a .>>. 8)
    pokeElemOff ptr 1 (a .>>. 0)

-- | Read a 'Word32' stored in network byte order (big endian).
peek32be :: Ptr Word8 -> IO Word32
peek32be ptr = do
    let x .<<. i = fromIntegral x `shiftL` i
    a0 <- peekElemOff ptr 0
    a1 <- peekElemOff ptr 1
    a2 <- peekElemOff ptr 2
    a3 <- peekElemOff ptr 3
    return $! (a0 .<<. 24) .|. (a1 .<<. 16) .|. (a2 .<<. 8) .|. (a3 .<<. 0)

-- | Write a 'Word32' in network byte order (big endian).
poke32be :: Ptr Word8 -> Word32 -> IO ()
poke32be ptr a = do
    let x .>>. i = fromIntegral (x `shiftR` i)
    pokeElemOff ptr 0 (a .>>. 24)
    pokeElemOff ptr 1 (a .>>. 16)
    pokeElemOff ptr 2 (a .>>.  8)
    pokeElemOff ptr 3 (a .>>.  0)
