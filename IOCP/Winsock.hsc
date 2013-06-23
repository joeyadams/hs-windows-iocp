{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Winsock (
    socket,
    associateSocket,
    close,
    bind,
    connect,
    send,

    -- * Types
    SOCKET,
    Family(..),
    SocketType(..),
    Protocol(..),
    SockAddr(..),
    aNY_PORT,
    iNADDR_ANY,
    iN6ADDR_ANY,
    IOStatus(..),
    LPWSABUF,
    WSABUF(..),
) where

import IOCP.CompletionPort
import IOCP.Windows
import IOCP.Winsock.Bindings
import IOCP.Winsock.Load
import IOCP.Winsock.Types

import Foreign

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

socket :: Family -> SocketType -> Maybe Protocol -> IO SOCKET
socket f t mp = do
    loadWinsock_
    failIf (== iNVALID_SOCKET) "socket" $
      c_socket (packFamily f)
               (packSocketType t)
               (maybe noProtocol packProtocol mp)

associateSocket :: CompletionPort a -> SOCKET -> IO (Handle a)
associateSocket cp sock = associate cp (toHANDLE sock)

close :: SOCKET -> IO ()
close sock = do
    loadWinsock_
    failIf_ (/= 0) "close" $
      c_close sock

bind :: SOCKET -> SockAddr -> IO ()
bind sock addr = do
    loadWinsock_
    failIf_ (/= 0) "bind" $
      withSockAddr addr $ \ptr len ->
      c_bind sock ptr len

-- | Uses @ConnectEx@, which requires the socket to be initially bound.
connect :: Handle a -> SockAddr -> Overlapped a -> IO IOStatus
connect h addr ol = do
    ws <- loadWinsock
    withSockAddr addr $ \ptr len ->
      checkPendingIf_ (== 0) "connect" $
      mswConnectEx ws (toSOCKET h) ptr len nullPtr 0 nullPtr (toLPOVERLAPPED ol)

send :: Handle cv -> LPWSABUF -> Int -> Overlapped cv -> IO IOStatus
send h buf bufCount ol = do
    loadWinsock_
    checkPendingIf_ (/= 0) "send" $
      c_WSASend (toSOCKET h)
                buf (fromIntegral bufCount)
                nullPtr
                0
                (toLPOVERLAPPED ol) nullFunPtr
