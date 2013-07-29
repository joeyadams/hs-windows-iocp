{-# LANGUAGE CPP #-}
module IOCP.Winsock.Bindings (
    c_iocp_winsock_init,
    c_socket,
    c_close,
    c_bind,
    c_WSAIoctl,
    c_WSARecv,
    c_WSASend,
) where

import IOCP.Windows
import IOCP.Winsock.Types

import Foreign
import Foreign.C

#include <windows.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

foreign import ccall "iocp_winsock_init"
    c_iocp_winsock_init :: IO Bool

foreign import WINDOWS_CCONV unsafe "winsock2.h socket"
    c_socket :: CFamily -> CSocketType -> CProtocol -> IO SOCKET

foreign import WINDOWS_CCONV "winsock2.h closesocket"
    c_close :: SOCKET -> IO CInt

foreign import WINDOWS_CCONV "winsock2.h bind"
    c_bind :: SOCKET -> Ptr SockAddr -> CInt -> IO CInt

foreign import WINDOWS_CCONV unsafe "winsock2.h WSAIoctl"
    c_WSAIoctl
      :: SOCKET       -- ^ s
      -> DWORD        -- ^ dwIoControlCode
      -> LPVOID       -- ^ lpvInBuffer
      -> DWORD        -- ^ cbInBuffer
      -> LPVOID       -- ^ lpvOutBuffer
      -> DWORD        -- ^ cbOutBuffer
      -> LPDWORD      -- ^ lpcbBytesReturned
      -> LPWSAOVERLAPPED
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
      -> IO CInt

foreign import WINDOWS_CCONV unsafe "winsock2.h WSARecv"
    c_WSARecv
      :: SOCKET       -- ^ s
      -> LPWSABUF     -- ^ lpBuffers
      -> DWORD        -- ^ dwBufferCount
      -> LPDWORD      -- ^ lpNumberOfBytesRecvd
      -> LPDWORD      -- ^ lpFlags
      -> LPOVERLAPPED -- ^ lpOverlapped
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
                      -- ^ lpCompletionRoutine.  Must not call back
                      --   into Haskell, since 'c_WSARecv' is an
                      --   @unsafe@ foreign import.
      -> IO CInt

foreign import WINDOWS_CCONV unsafe "winsock2.h WSASend"
    c_WSASend
      :: SOCKET       -- ^ s
      -> LPWSABUF     -- ^ lpBuffers
      -> DWORD        -- ^ dwBufferCount
      -> LPDWORD      -- ^ lpNumberOfBytesSent
      -> DWORD        -- ^ dwFlags
      -> LPOVERLAPPED -- ^ lpOverlapped
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
                      -- ^ lpCompletionRoutine.  Must not call back
                      --   into Haskell, since 'c_WSASend' is an
                      --   @unsafe@ foreign import.
      -> IO CInt
