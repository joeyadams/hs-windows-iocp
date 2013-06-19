{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Winsock (
    initWinsock,
    socket,
    associateSocket,
    close,
    bind,
    connect,
    send,

    -- * Types
    Winsock,
    Socket,
    Family(..),
    SocketType(..),
    Protocol(..),
    SockAddr(..),
    aNY_PORT,
    iNADDR_ANY,
    iN6ADDR_ANY,
) where

import IOCP.Bindings
import IOCP.Windows
import IOCP.Winsock.Types

import Foreign
import Foreign.C

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

#include <winsock2.h>

newtype Winsock = Winsock (Ptr ())
  deriving (Eq, Show, Storable)

foreign import ccall unsafe "iocp_winsock_init"
    c_iocp_winsock_init :: IO Winsock

initWinsock :: IO Winsock
initWinsock = failIf (== Winsock nullPtr) "initWinsock" c_iocp_winsock_init

foreign import WINDOWS_CCONV unsafe "winsock2.h socket"
    c_socket :: CFamily -> CSocketType -> CProtocol -> IO Socket

socket :: Family -> SocketType -> Maybe Protocol -> IO Socket
socket f t mp =
    failIf (== iNVALID_SOCKET) "socket" $
    c_socket (packFamily f)
             (packSocketType t)
             (maybe noProtocol packProtocol mp)

castSOCKETToHANDLE :: Socket -> HANDLE
castSOCKETToHANDLE (Socket n) = wordPtrToPtr (fromIntegral n)

associateSocket :: CompletionPort a -> Socket -> IO (Handle a)
associateSocket cp sock = associate cp (castSOCKETToHANDLE sock)

foreign import WINDOWS_CCONV "winsock2.h closesocket"
    c_close :: Socket -> IO CInt

close :: Socket -> IO ()
close sock =
    failIf_ (/= 0) "close" $
    c_close sock

foreign import WINDOWS_CCONV "winsock2.h bind"
    c_bind :: Socket -> Ptr SockAddr -> CInt -> IO CInt

bind :: Socket -> SockAddr -> IO ()
bind sock addr =
    failIf_ (/= 0) "bind" $
    withSockAddr addr $ \ptr len ->
    c_bind sock ptr len

foreign import ccall "iocp_winsock_connect"
    c_iocp_winsock_connect
        :: Winsock -> Handle a -> Ptr SockAddr -> CInt -> Overlapped a -> IO BOOL

-- | Uses @ConnectEx@, which requires the socket to be initially bound.
connect :: Winsock -> Handle a -> SockAddr -> Overlapped a -> IO IOStatus
connect ws h addr ol =
    checkPendingIf_ (== 0) "connect" $
    withSockAddr addr $ \ptr len ->
    c_iocp_winsock_connect ws h ptr len ol

foreign import ccall "iocp_winsock_send"
    c_iocp_winsock_send :: Handle a -> Ptr CChar -> CULong -> Overlapped a -> IO CInt

send :: Handle cv -> Ptr a -> Int -> Overlapped cv -> IO IOStatus
send h ptr len ol =
    checkPendingIf_ (/= 0) "send" $
    c_iocp_winsock_send h (castPtr ptr) (fromIntegral len) ol
