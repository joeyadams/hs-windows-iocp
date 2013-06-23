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
    SOCKET,
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
import IOCP.Winsock.Bindings
import qualified IOCP.Winsock.Load as Load
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

foreign import ccall unsafe "iocp_winsock_init_with_mswsock"
    c_iocp_winsock_init_with_mswsock :: IO Winsock

initWinsock :: IO Winsock
initWinsock = failIf (== Winsock nullPtr) "initWinsock" c_iocp_winsock_init_with_mswsock

socket :: Family -> SocketType -> Maybe Protocol -> IO SOCKET
socket f t mp =
    failIf (== iNVALID_SOCKET) "socket" $
    c_socket (packFamily f)
             (packSocketType t)
             (maybe noProtocol packProtocol mp)

associateSocket :: CompletionPort a -> SOCKET -> IO (Handle a)
associateSocket cp sock = associate cp (toHANDLE sock)

close :: SOCKET -> IO ()
close sock =
    failIf_ (/= 0) "close" $
    c_close sock

foreign import WINDOWS_CCONV "winsock2.h bind"
    c_bind :: SOCKET -> Ptr SockAddr -> CInt -> IO CInt

bind :: SOCKET -> SockAddr -> IO ()
bind sock addr =
    failIf_ (/= 0) "bind" $
    withSockAddr addr $ \ptr len ->
    c_bind sock ptr len

-- | Uses @ConnectEx@, which requires the socket to be initially bound.
connect :: Handle a -> SockAddr -> Overlapped a -> IO IOStatus
connect h addr ol = do
    ws <- Load.loadWinsock
    withSockAddr addr $ \ptr len ->
      checkPendingIf_ (== 0) "connect" $
      Load.mswConnectEx ws (toSOCKET h) ptr len nullPtr 0 nullPtr (toLPOVERLAPPED ol)

send :: Handle cv -> LPWSABUF -> Int -> Overlapped cv -> IO IOStatus
send h buf bufCount ol =
    checkPendingIf_ (/= 0) "send" $
    c_WSASend (toSOCKET h) buf (fromIntegral bufCount) nullPtr 0 (toLPOVERLAPPED ol) nullFunPtr
