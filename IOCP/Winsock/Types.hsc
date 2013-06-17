{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Winsock.Types (
    Socket(..),

    -- * Socket addresses
    SockAddr(..),
    peekSockAddr,
    pokeSockAddr,
    sizeOfSockAddr,
    sizeOfSockAddrByFamily,
    withSockAddr,
    withNewSockAddr,

    PortNumber(..),
    HostAddress(..),
    HostAddress6(..),

    -- * Enumerations
    -- ** Family
    Family(..),
    CFamily(..),
    aF_UNSPEC,
    packFamily,
    unpackFamily,

    -- ** SocketType
    SocketType(..),
    CSocketType(..),
    noSocketType,
    packSocketType,
    unpackSocketType,

    -- ** Protocol
    Protocol(..),
    CProtocol(..),
    noProtocol,
    packProtocol,
    unpackProtocol,
) where

import Control.Applicative
import Data.Bits
import Data.List
import Data.Typeable (Typeable)
import Data.Word
import Foreign
import Foreign.C.Types (CInt(..), CSize(..))
import Numeric (showInt, showHex)

#include <winsock2.h>
#include <ws2tcpip.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype Socket = Socket (#type SOCKET)
  deriving (Eq, Ord, Show, Typeable)

data SockAddr
  = SockAddrInet
    { sin_port :: PortNumber
    , sin_addr :: HostAddress
    }
  | SockAddrInet6
    { sin6_port     :: PortNumber
    , sin6_flowinfo :: Word32
    , sin6_addr     :: HostAddress6
    , sin6_scope_id :: Word32
    }
    -- ^ NOTE: The format of sin6_flowinfo and sin6_scope_id is not specified.
    --   Avoid relying on particular content.  This library does not perform byte
    --   swapping on these fields like it does for sin6_port and sin6_addr.
  deriving (Eq, Typeable)

instance Show SockAddr where
    showsPrec _p SockAddrInet{..} =
        shows sin_addr . showChar ':' . shows sin_port
    showsPrec _p SockAddrInet6{..} =
        -- TODO: should we show sin6_flowinfo?  If so, how?
        showChar '[' . shows sin6_addr .
        (if sin6_scope_id == 0
            then id
            else showChar '%' . shows sin6_scope_id) .
        showString "]:" . shows sin6_port

-- | Read a @struct sockaddr@.  Return 'Left' if it has an
-- unknown address family.
peekSockAddr :: Ptr SockAddr -> IO (Either CFamily SockAddr)
peekSockAddr p = do
    family <- (#peek struct sockaddr, sa_family) p
    case family :: Csa_family of
        (#const AF_INET) -> do
            sin_port <- (#peek struct sockaddr_in, sin_port) p
            sin_addr <- (#peek struct sockaddr_in, sin_addr) p
            return $ Right $ SockAddrInet{..}
        (#const AF_INET6) -> do
            sin6_port     <- (#peek struct sockaddr_in6, sin6_port)     p
            sin6_flowinfo <- (#peek struct sockaddr_in6, sin6_flowinfo) p
            sin6_addr     <- (#peek struct sockaddr_in6, sin6_addr)     p
            sin6_scope_id <- (#peek struct sockaddr_in6, sin6_scope_id) p
            return $ Right $ SockAddrInet6{..}
        _ -> return $! Left $! CFamily (fromIntegral family)

pokeSockAddr :: Ptr SockAddr -> SockAddr -> IO ()
pokeSockAddr p SockAddrInet{..} = do
    let sin_family = (#const AF_INET) :: Csa_family
    zeroMemory p (#const sizeof(struct sockaddr_in))
    (#poke struct sockaddr_in, sin_family) p sin_family
    (#poke struct sockaddr_in, sin_port)   p sin_port
    (#poke struct sockaddr_in, sin_addr)   p sin_addr
pokeSockAddr p SockAddrInet6{..} = do
    let sin6_family = (#const AF_INET6) :: Csa_family
    zeroMemory p (#const sizeof(struct sockaddr_in6))
    (#poke struct sockaddr_in6, sin6_family)   p sin6_family
    (#poke struct sockaddr_in6, sin6_port)     p sin6_port
    (#poke struct sockaddr_in6, sin6_flowinfo) p sin6_flowinfo
    (#poke struct sockaddr_in6, sin6_addr)     p sin6_addr
    (#poke struct sockaddr_in6, sin6_scope_id) p sin6_scope_id

-- | The type of sa_family, which may be different than the type for the
-- address family parameter to @socket@.
type Csa_family = #type u_short

-- | Computes the storage requirements (in bytes) of the given
-- 'SockAddr'.  This function differs from 'Foreign.Storable.sizeOf'
-- in that the value of the argument /is/ used.
sizeOfSockAddr :: SockAddr -> CInt
sizeOfSockAddr SockAddrInet{}  = #const sizeof(struct sockaddr_in)
sizeOfSockAddr SockAddrInet6{} = #const sizeof(struct sockaddr_in6)

-- | Computes the storage requirements (in bytes) required for a
-- 'SockAddr' with the given 'Family'.
sizeOfSockAddrByFamily :: Family -> CInt
sizeOfSockAddrByFamily AF_INET  = #const sizeof(struct sockaddr_in)
sizeOfSockAddrByFamily AF_INET6 = #const sizeof(struct sockaddr_in6)

-- | Use a 'SockAddr' with a function requiring a pointer to a
-- @struct sockaddr@ and its length.
withSockAddr :: SockAddr -> (Ptr SockAddr -> CInt -> IO a) -> IO a
withSockAddr addr f = do
    let sz = sizeOfSockAddr addr
    allocaBytes (fromIntegral sz) $ \p -> pokeSockAddr p addr >> f p sz

-- | Create a new 'SockAddr' for use with a function requiring a pointer to a
-- @struct sockaddr@ and its length.  The bytes are zeroed out.
withNewSockAddr :: Family -> (Ptr SockAddr -> CInt -> IO a) -> IO a
withNewSockAddr family f = do
    let sz = sizeOfSockAddrByFamily family
    allocaBytes (fromIntegral sz) $ \ptr ->
        zeroMemory ptr (fromIntegral sz) >> f ptr sz

-- |
-- NOTE: the network package uses network byte order for the 'Word32' value,
-- but we use host byte order.  For us, 127.0.0.1 is @HostAddress 0x7f000001@
-- everywhere, rather than @HostAddress 0x0100007f@ on little-endian
-- architectures (such as x86).
newtype HostAddress = HostAddress Word32
    deriving (Eq, Typeable)

-- | Dotted decimal form, like @127.0.0.1@
instance Show HostAddress where
    -- We could use inet_ntoa, but then we'd have to wrap it up for
    -- thread safety, since the address returned might be modified by
    -- another green thread.  This is clearer, anyway.
    showsPrec _p (HostAddress a) =
        delimit '.' $ map showInt parts
      where
        parts :: [Word8]
        parts = [a .>>. 24, a .>>. 16, a .>>. 8, a .>>. 0]

-- | @in_addr@ (stored in network byte order)
instance Storable HostAddress where
    sizeOf    _ = #{size      struct in_addr}
    alignment _ = #{alignment struct in_addr}
    peek ptr = HostAddress <$> peek32be (castPtr ptr)
    poke ptr (HostAddress w) = poke32be (castPtr ptr) w

data HostAddress6 = HostAddress6 !Word32 !Word32 !Word32 !Word32
    deriving (Eq, Typeable)

-- | Abbreviated hex form, like @2001:123:4567::89ab:cdef:0@
instance Show HostAddress6 where
    -- IPv4-mapped IPv6 address
    showsPrec _p (HostAddress6 0 0 0x0000FFFF ipv4) =
        showString "::ffff:" . shows (HostAddress ipv4)

    showsPrec _p (HostAddress6 a b c d) =
        case longestZeros parts of
            Nothing -> showParts parts
            Just (before, after) ->
                showParts before . showString "::" . showParts after
      where
        parts :: [Word16]
        parts = [ a .>>. 16, a .>>. 0, b .>>. 16, b .>>. 0
                , c .>>. 16, c .>>. 0, d .>>. 16, d .>>. 0
                ]

        showParts = delimit ':' . map showHex

        -- Find the longest consecutive run of two or more 0s.  If there are multiple
        -- winners, pick the leftmost one.  If such a run is found, return the items
        -- before and after it.
        longestZeros xs = do
            n <- maximumMaybe [length g | g@(0:0:_) <- group xs]
            let (before, _:after) = break (== replicate n 0) (group xs)
            Just (concat before, concat after)

        maximumMaybe [] = Nothing
        maximumMaybe xs = Just (maximum xs)

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

------------------------------------------------------------------------
-- Enumerations

data Family
  = AF_INET
  | AF_INET6
  deriving (Eq, Show, Typeable)

newtype CFamily = CFamily CInt
  deriving (Eq, Show, Typeable)

aF_UNSPEC :: CFamily
aF_UNSPEC = CFamily #const AF_UNSPEC

-- | 'Nothing' means @AF_UNSPEC@.
packFamily :: Family -> CFamily
packFamily f = case f of
    AF_INET   -> CFamily #const AF_INET
    AF_INET6  -> CFamily #const AF_INET6

unpackFamily :: CFamily -> Maybe Family
unpackFamily (CFamily n) = case n of
    (#const AF_INET)   -> Just AF_INET
    (#const AF_INET6)  -> Just AF_INET6
    _ -> Nothing

data SocketType
  = SOCK_STREAM
  | SOCK_DGRAM
  deriving (Eq, Show, Typeable)

newtype CSocketType = CSocketType CInt
  deriving (Eq, Show, Typeable)

noSocketType :: CSocketType
noSocketType = CSocketType 0

packSocketType :: SocketType -> CSocketType
packSocketType t = case t of
    SOCK_STREAM -> CSocketType #const SOCK_STREAM
    SOCK_DGRAM  -> CSocketType #const SOCK_DGRAM

unpackSocketType :: CSocketType -> Maybe SocketType
unpackSocketType (CSocketType n) = case n of
    (#const SOCK_STREAM) -> Just SOCK_STREAM
    (#const SOCK_DGRAM)  -> Just SOCK_DGRAM
    _ -> Nothing

data Protocol
  = IPPROTO_TCP
  | IPPROTO_UDP
  deriving (Eq, Show, Typeable)

newtype CProtocol = CProtocol CInt
  deriving (Eq, Show, Typeable)

noProtocol :: CProtocol
noProtocol = CProtocol 0

packProtocol :: Protocol -> CProtocol
packProtocol p = case p of
    IPPROTO_TCP -> CProtocol #const IPPROTO_TCP
    IPPROTO_UDP -> CProtocol #const IPPROTO_UDP

unpackProtocol :: CProtocol -> Maybe Protocol
unpackProtocol (CProtocol n) = case n of
    (#const IPPROTO_TCP) -> Just IPPROTO_TCP
    (#const IPPROTO_UDP) -> Just IPPROTO_UDP
    _ -> Nothing

------------------------------------------------------------------------
-- Utilities

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

-- | Extend and shift left.  Used to construct big words from little ones.
(.<<.) :: (Integral a, Num b, Bits b) => a -> Int -> b
x .<<. i = fromIntegral x `shiftL` i

-- | Shift right and truncate.  Used to extract little words from big ones.
(.>>.) :: (Integral a, Bits a, Num b) => a -> Int -> b
x .>>. i = fromIntegral (x `shiftR` i)

delimit :: Char -> [ShowS] -> ShowS
delimit delim = foldr (.) id . intersperse (showChar delim)

foreign import ccall unsafe "string.h memset"
    memset :: Ptr a -> CInt -> CSize -> IO ()

zeroMemory :: Ptr a -> CSize -> IO ()
zeroMemory dest nbytes = memset dest 0 nbytes
