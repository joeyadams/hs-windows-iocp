{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Winsock.Bindings (
    getExtensionFunc,

    -- * Types
    LPWSAOVERLAPPED,
    LPWSAOVERLAPPED_COMPLETION_ROUTINE,
    WSAOVERLAPPED_COMPLETION_ROUTINE,

    -- * Mswsock
    loadMswsock,
    Mswsock(..),
    AcceptEx,
    ConnectEx,
    DisconnectEx,
    GetAcceptExSockaddrs,
    TransmitFile,
    TransmitPackets,
    WSARecvMsg,
    WSASendMsg,
) where

import IOCP.Utils (withPtrLen)
import IOCP.Windows
import IOCP.Winsock.Types

import Control.Applicative ((<$>))
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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

getExtensionFunc :: SOCKET -> GUID -> IO (FunPtr a)
getExtensionFunc sock guid =
  withPtrLen guid       $ \guid_ptr guid_sz ->
  withPtrLen nullFunPtr $ \fptr_ptr fptr_sz ->
  with 0                $ \bytes_ptr -> do
    rc <- c_WSAIoctl sock #{const SIO_GET_EXTENSION_FUNCTION_POINTER}
                     (castPtr guid_ptr) (fromIntegral guid_sz)
                     (castPtr fptr_ptr) (fromIntegral fptr_sz)
                     bytes_ptr nullPtr nullFunPtr
    if rc == 0
      then peek fptr_ptr
      else throwGetLastError "getExtensionFunc"

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

type LPWSAOVERLAPPED = LPOVERLAPPED

type WSAOVERLAPPED_COMPLETION_ROUTINE
    = DWORD           -- ^ dwError
   -> DWORD           -- ^ cbTransferred
   -> LPWSAOVERLAPPED -- ^ lpOverlapped
   -> DWORD           -- ^ dwFlags
   -> IO ()

type LPWSAOVERLAPPED_COMPLETION_ROUTINE =
    FunPtr WSAOVERLAPPED_COMPLETION_ROUTINE

type LPTRANSMIT_FILE_BUFFERS    = Ptr ()
type LPTRANSMIT_PACKETS_ELEMENT = Ptr ()
type LPWSAMSG                   = Ptr ()

------------------------------------------------------------------------
-- Mswsock
--
-- The GUIDs and function signatures here were copied from Wine's mswsock.h and
-- modified extensively.

-- TODO: create the SOCKET ourselves
loadMswsock :: SOCKET -> IO Mswsock
loadMswsock sock = do
    let get :: (FunPtr a -> a) -> GUID -> IO a
        get dynamic guid = dynamic <$> getExtensionFunc sock guid
    mswAcceptEx             <- get mkAcceptEx             "{B5367DF1-CBAC-11CF-95CA-00805F48A192}"
    mswConnectEx            <- get mkConnectEx            "{25A207B9-DDF3-4660-8EE9-76E58C74063E}"
    mswDisconnectEx         <- get mkDisconnectEx         "{7FDA2E11-8630-436F-A031-F536A6EEC157}"
    mswGetAcceptExSockaddrs <- get mkGetAcceptExSockaddrs "{B5367DF2-CBAC-11CF-95CA-00805F48A192}"
    mswTransmitFile         <- get mkTransmitFile         "{B5367DF0-CBAC-11CF-95CA-00805F48A192}"
    mswTransmitPackets      <- get mkTransmitPackets      "{D9689DA0-1F90-11D3-9971-00C04F68C876}"
    mswWSARecvMsg           <- get mkWSARecvMsg           "{F689D7C8-6F1F-436B-8A53-E54FE351C322}"
    mswWSASendMsg           <- get mkWSASendMsg           "{A441E712-754F-43CA-84A7-0DEE44CF606D}"
    return $! Mswsock{..}

data Mswsock = Mswsock
    { mswAcceptEx             :: !AcceptEx
    , mswConnectEx            :: !ConnectEx
    , mswDisconnectEx         :: !DisconnectEx
    , mswGetAcceptExSockaddrs :: !GetAcceptExSockaddrs
    , mswTransmitFile         :: !TransmitFile
    , mswTransmitPackets      :: !TransmitPackets
    , mswWSARecvMsg           :: !WSARecvMsg
    , mswWSASendMsg           :: !WSASendMsg
    }

type AcceptEx = SOCKET -> SOCKET -> PVOID -> DWORD -> DWORD -> DWORD -> LPDWORD -> LPOVERLAPPED -> IO BOOL
type ConnectEx = SOCKET -> Ptr SockAddr -> CInt -> PVOID -> DWORD -> LPDWORD -> LPOVERLAPPED -> IO BOOL
type DisconnectEx = SOCKET -> LPOVERLAPPED -> DWORD -> DWORD -> IO BOOL
type GetAcceptExSockaddrs = PVOID -> DWORD -> DWORD -> DWORD -> Ptr (Ptr SockAddr) -> LPINT -> Ptr (Ptr SockAddr) -> LPINT -> IO ()
type TransmitFile = SOCKET -> HANDLE -> DWORD -> DWORD -> LPOVERLAPPED -> LPTRANSMIT_FILE_BUFFERS -> DWORD -> IO BOOL
type TransmitPackets = SOCKET -> LPTRANSMIT_PACKETS_ELEMENT -> DWORD -> DWORD -> LPOVERLAPPED -> DWORD -> IO BOOL
type WSARecvMsg = SOCKET -> LPWSAMSG -> LPDWORD -> LPWSAOVERLAPPED -> LPWSAOVERLAPPED_COMPLETION_ROUTINE -> IO CInt
type WSASendMsg = SOCKET -> LPWSAMSG -> DWORD -> LPDWORD -> LPWSAOVERLAPPED -> LPWSAOVERLAPPED_COMPLETION_ROUTINE -> IO CInt

foreign import WINDOWS_CCONV "dynamic"
    mkAcceptEx :: FunPtr AcceptEx -> AcceptEx
foreign import WINDOWS_CCONV "dynamic"
    mkConnectEx :: FunPtr ConnectEx -> ConnectEx
foreign import WINDOWS_CCONV "dynamic"
    mkDisconnectEx :: FunPtr DisconnectEx -> DisconnectEx
foreign import WINDOWS_CCONV "dynamic"
    mkGetAcceptExSockaddrs :: FunPtr GetAcceptExSockaddrs -> GetAcceptExSockaddrs
foreign import WINDOWS_CCONV "dynamic"
    mkTransmitFile :: FunPtr TransmitFile -> TransmitFile
foreign import WINDOWS_CCONV "dynamic"
    mkTransmitPackets :: FunPtr TransmitPackets -> TransmitPackets
foreign import WINDOWS_CCONV "dynamic"
    mkWSARecvMsg :: FunPtr WSARecvMsg -> WSARecvMsg
foreign import WINDOWS_CCONV "dynamic"
    mkWSASendMsg :: FunPtr WSASendMsg -> WSASendMsg
