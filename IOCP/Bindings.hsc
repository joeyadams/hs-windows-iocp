{-# LANGUAGE CPP #-}
module IOCP.Bindings (
    c_CreateIoCompletionPort,
    c_CloseHandle,
    c_GetQueuedCompletionStatus,
    c_PostQueuedCompletionStatus,
    c_CancelIo,
    CancelIoEx(..),
    loadCancelIoEx,
) where

import IOCP.Types
import IOCP.Windows

import Control.Applicative
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

foreign import WINDOWS_CCONV unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort
        :: HANDLE                 -- ^ FileHandle
        -> CompletionPort a       -- ^ ExistingCompletionPort
        -> ULONG_PTR              -- ^ CompletionKey
        -> DWORD                  -- ^ NumberOfConcurrentThreads
        -> IO (CompletionPort a)

foreign import WINDOWS_CCONV safe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

foreign import WINDOWS_CCONV safe "GetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus
        :: CompletionPort a
        -> Ptr DWORD          -- ^ lpNumberOfBytes
        -> Ptr ULONG_PTR      -- ^ lpCompletionKey
        -> Ptr (Overlapped a) -- ^ lpOverlapped
        -> DWORD              -- ^ dwMilliseconds
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h PostQueuedCompletionStatus"
    c_PostQueuedCompletionStatus
        :: CompletionPort a
        -> DWORD              -- ^ dwNumberOfBytesTransferred
        -> ULONG_PTR          -- ^ dwCompletionKey
        -> Overlapped a       -- ^ lpOverlapped
        -> IO BOOL

foreign import WINDOWS_CCONV unsafe "windows.h CancelIo"
    c_CancelIo :: HANDLE -> IO BOOL

newtype CancelIoEx = CancelIoEx (HANDLE -> LPOVERLAPPED -> IO BOOL)

-- | Retrieve the @CancelIoEx@ function, if it is available on this system.
-- @CancelIoEx@ was introduced in Windows Vista.
loadCancelIoEx :: IO (Maybe CancelIoEx)
loadCancelIoEx =
    fmap mkCancelIoEx <$> getProcAddress kernel32 "CancelIoEx"

foreign import WINDOWS_CCONV unsafe "dynamic"
    mkCancelIoEx :: FunPtr CancelIoEx -> CancelIoEx
