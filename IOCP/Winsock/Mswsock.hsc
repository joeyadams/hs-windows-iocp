{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | Bindings for functions declared in \"Mswsock.h\".  Unfortunately, we can't
-- call them directly; we have to fetch them with @WSAIoctl@.
module IOCP.Winsock.Mswsock (
    getMswsock,
    Mswsock(..),

    -- * Mswsock function signatures
    AcceptEx,
    ConnectEx,
    DisconnectEx,
    GetAcceptExSockaddrs,
    TransmitFile,
    TransmitPackets,
    WSARecvMsg,
    WSASendMsg,
) where

import IOCP.Windows
import IOCP.Winsock.Types

import Control.Applicative ((<$>))
import Control.Exception
import Foreign
import Foreign.C
import Network.Socket
import qualified System.IO.Unsafe as U (unsafePerformIO)

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

data Mswsock = Mswsock
    { mswAcceptEx             :: !AcceptEx
    , mswConnectEx            :: !ConnectEx
    , mswDisconnectEx         :: !DisconnectEx
    , mswGetAcceptExSockaddrs :: !GetAcceptExSockaddrs
    , mswTransmitFile         :: !TransmitFile
    , mswTransmitPackets      :: !TransmitPackets
    , mswWSARecvMsg           :: !WSARecvMsg
    , mswWSASendMsg           :: !(Maybe WSASendMsg)
      -- ^ Windows Vista or later
    }

-- | Get the mswsock.dll extension methods.
getMswsock :: IO Mswsock
getMswsock = evaluate mswsockCAF

mswsockCAF :: Mswsock
mswsockCAF = U.unsafePerformIO $ mask_ loadMswsock
{-# NOINLINE mswsockCAF #-}

loadMswsock :: IO Mswsock
loadMswsock =
  bracket (socket AF_INET Stream defaultProtocol) close $ \(MkSocket fd _ _ _ _) -> do
    let sock = SOCKET (fromIntegral fd)
    let get :: (FunPtr a -> a) -> GUID -> IO a
        get dynamic guid = do
            m <- getExtensionFunc sock guid
            case m of
                Nothing   -> throwErrCode "loadMswsock" eERROR_PROC_NOT_FOUND
                Just fptr -> return $! dynamic fptr
    let getMaybe :: (FunPtr a -> a) -> GUID -> IO (Maybe a)
        getMaybe dynamic guid = do
            m <- getExtensionFunc sock guid
            case m of
                Nothing   -> return Nothing
                Just fptr -> return $! Just $! dynamic fptr
    mswAcceptEx             <- get mkAcceptEx             "{B5367DF1-CBAC-11CF-95CA-00805F48A192}"
    mswConnectEx            <- get mkConnectEx            "{25A207B9-DDF3-4660-8EE9-76E58C74063E}"
    mswDisconnectEx         <- get mkDisconnectEx         "{7FDA2E11-8630-436F-A031-F536A6EEC157}"
    mswGetAcceptExSockaddrs <- get mkGetAcceptExSockaddrs "{B5367DF2-CBAC-11CF-95CA-00805F48A192}"
    mswTransmitFile         <- get mkTransmitFile         "{B5367DF0-CBAC-11CF-95CA-00805F48A192}"
    mswTransmitPackets      <- get mkTransmitPackets      "{D9689DA0-1F90-11D3-9971-00C04F68C876}"
    mswWSARecvMsg           <- get mkWSARecvMsg           "{F689D7C8-6F1F-436B-8A53-E54FE351C322}"
    mswWSASendMsg           <- getMaybe mkWSASendMsg      "{A441E712-754F-43CA-84A7-0DEE44CF606D}"
    return $! Mswsock{..}

getExtensionFunc :: SOCKET -> GUID -> IO (Maybe (FunPtr a))
getExtensionFunc sock guid =
  withPtrLen guid       $ \guid_ptr guid_sz ->
  withPtrLen nullFunPtr $ \fptr_ptr fptr_sz ->
  with 0                $ \bytes_ptr -> do
    rc <- c_WSAIoctl sock (#const SIO_GET_EXTENSION_FUNCTION_POINTER)
                     (castPtr guid_ptr) (fromIntegral guid_sz)
                     (castPtr fptr_ptr) (fromIntegral fptr_sz)
                     bytes_ptr nullPtr nullFunPtr
    if rc == 0
      then Just <$> peek fptr_ptr
      else do
        err <- getLastError
        if err == eWSAEINVAL
          then return Nothing
          else throwErrCode "getExtensionFunc" err

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

-- | Like 'with', but also provide the size of the value (in bytes).
withPtrLen :: Storable a => a -> (Ptr a -> Int -> IO b) -> IO b
withPtrLen val f =
  alloca $ \ptr -> do
    poke ptr val
    f ptr (sizeOf val)

------------------------------------------------------------------------
-- mswsock.dll function signatures and wrappers

-- The GUIDs and type signatures of these functions were copied from Wine's
-- mswsock.h and adapted to Haskell.

type AcceptEx = SOCKET -> SOCKET -> PVOID -> DWORD -> DWORD -> DWORD -> LPDWORD -> LPOVERLAPPED -> IO BOOL
type ConnectEx = SOCKET -> Ptr SockAddr -> CInt -> PVOID -> DWORD -> LPDWORD -> LPOVERLAPPED -> IO BOOL
type DisconnectEx = SOCKET -> LPOVERLAPPED -> DWORD -> DWORD -> IO BOOL
type GetAcceptExSockaddrs = PVOID -> DWORD -> DWORD -> DWORD -> Ptr (Ptr SockAddr) -> LPINT -> Ptr (Ptr SockAddr) -> LPINT -> IO ()
type TransmitFile = SOCKET -> HANDLE -> DWORD -> DWORD -> LPOVERLAPPED -> LPTRANSMIT_FILE_BUFFERS -> DWORD -> IO BOOL
type TransmitPackets = SOCKET -> LPTRANSMIT_PACKETS_ELEMENT -> DWORD -> DWORD -> LPOVERLAPPED -> DWORD -> IO BOOL
type WSARecvMsg = SOCKET -> LPWSAMSG -> LPDWORD -> LPWSAOVERLAPPED -> LPWSAOVERLAPPED_COMPLETION_ROUTINE -> IO CInt
type WSASendMsg = SOCKET -> LPWSAMSG -> DWORD -> LPDWORD -> LPWSAOVERLAPPED -> LPWSAOVERLAPPED_COMPLETION_ROUTINE -> IO CInt

type LPTRANSMIT_FILE_BUFFERS    = Ptr ()
type LPTRANSMIT_PACKETS_ELEMENT = Ptr ()
type LPWSAMSG                   = Ptr ()

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
