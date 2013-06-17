{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Bindings (
    newCompletionPort,
    closeCompletionPort,
    associate,
    closeHandle,
    newOverlapped,
    newOverlappedWithOffset,
    freeOverlapped,
    getQueuedCompletionStatus,
    iNFINITE,
    postQueuedCompletionStatus,
    postQueuedCompletionStatusNB,
    postValue,

    -- * Types
    CompletionPort(..),
    Handle(..),
    Overlapped(..),
    Completion(..),
) where

import IOCP.Windows

import Control.Exception hiding (handle)
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

#include <windows.h>

newCompletionPort :: IO (CompletionPort a)
newCompletionPort =
    failIf (== CompletionPort nullPtr) "newCompletionPort" $
        c_CreateIoCompletionPort iNVALID_HANDLE_VALUE (CompletionPort nullPtr) 0 1

closeCompletionPort :: CompletionPort a -> IO ()
closeCompletionPort (CompletionPort h) = closeHANDLE "closeCompletionPort" h

associate :: CompletionPort a -> HANDLE -> IO (Handle a)
associate cport handle = do
    failIf_ (/= cport) "associate" $
        c_CreateIoCompletionPort handle cport 0 0
    return (Handle handle)

closeHandle :: Handle a -> IO ()
closeHandle (Handle h) = closeHANDLE "closeHandle" h

-- | Allocate a new
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms684342%28v=vs.85%29.aspx OVERLAPPED>
-- structure, attaching the given piggyback value to it.  The resulting
-- pointer may be passed to a system call that takes an @LPOVERLAPPED@,
-- provided the @Handle@ was associated with a 'CompletionPort' with the
-- same type @a@.
newOverlapped :: a -> IO (Overlapped a)
newOverlapped = newOverlappedWithOffset 0

-- | Like 'newOverlapped', but specify a value for the Offset/OffsetHigh fields
-- of the @OVERLAPPED@ structure.
newOverlappedWithOffset :: Word64 -> a -> IO (Overlapped a)
newOverlappedWithOffset offset ctx =
    bracketOnError (newStablePtr ctx) freeStablePtr $ \ptr ->
        failIf (== Overlapped nullPtr) "newOverlapped" $
            c_iocp_new_overlapped offset ptr

-- | Free an 'Overlapped'.  This must only be used if the 'Overlapped' will
-- never appear in a completion port (e.g. because I\/O could not be started).
-- Note that 'getQueuedCompletionStatus' will free the 'Overlapped'
-- automatically.
freeOverlapped :: Overlapped a -> IO ()
freeOverlapped ol = c_iocp_finish_overlapped ol >>= freeStablePtr

finishOverlapped :: Overlapped a -> IO a
finishOverlapped ol = do
    sptr <- c_iocp_finish_overlapped ol
    a <- deRefStablePtr sptr
    freeStablePtr sptr
    return a

-- | Dequeue a completion packet.
--
-- Return 'Nothing' if the timeout expires, or 'Just' if a completion packet
-- was dequeued.  If it is a completion for a failed operation, the 'ErrCode'
-- will be non-zero.
getQueuedCompletionStatus
    :: CompletionPort a
    -> DWORD  -- ^ Timeout, in milliseconds.  Use 'iNFINITE' to never time out.
    -> IO (Maybe (Completion a))
getQueuedCompletionStatus cport timeout =
  with 0 $ \pNumBytes ->
  with 0 $ \pCompletionKey ->
  with (Overlapped nullPtr) $ \pOverlapped -> do
    b <- c_GetQueuedCompletionStatus cport pNumBytes pCompletionKey pOverlapped timeout
    err <- getLastError
    ol <- peek pOverlapped
    if b /= 0 then do
        value <- finishOverlapped ol
        numBytes <- peek pNumBytes
        return $! Just $! Completion{ cValue = value, cNumBytes = numBytes, cError = 0 }
    else if ol == Overlapped nullPtr then
        if err == #{const WAIT_TIMEOUT}
            then return Nothing
            else throwWinError "getQueuedCompletionStatus" err
    else do
        value <- finishOverlapped ol
        numBytes <- peek pNumBytes
        return $! Just $! Completion{ cValue = value, cNumBytes = numBytes, cError = err }

postQueuedCompletionStatus :: CompletionPort a -> Overlapped a -> IO ()
postQueuedCompletionStatus = postQueuedCompletionStatusNB 0

postQueuedCompletionStatusNB
    :: DWORD  -- ^ Number of bytes transferred
    -> CompletionPort a
    -> Overlapped a
    -> IO ()
postQueuedCompletionStatusNB numBytes cport ol =
    failIf_ (== 0) "postQueuedCompletionStatus" $
    c_PostQueuedCompletionStatus cport numBytes 0 ol

postValue :: CompletionPort a -> a -> IO ()
postValue cport a = newOverlapped a >>= postQueuedCompletionStatus cport

newtype CompletionPort a = CompletionPort HANDLE
    deriving (Eq, Show, Storable)

newtype Handle a = Handle HANDLE
    deriving (Eq, Show, Storable)

newtype Overlapped a = Overlapped (Ptr ())
    deriving (Eq, Show, Storable)

data Completion a = Completion
    { cValue    :: a        -- ^ Value passed to 'newOverlapped'
    , cNumBytes :: !DWORD   -- ^ Number of bytes transferred during I/O operation
    , cError    :: !ErrCode -- ^ Zero if the I/O was successful
    }
    deriving Show

------------------------------------------------------------------------

closeHANDLE :: String -> HANDLE -> IO ()
closeHANDLE loc h = do
    b <- c_CloseHandle h
    if b /= 0 then return () else throwGetLastError loc

------------------------------------------------------------------------

foreign import WINDOWS_CCONV unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort
        :: HANDLE                 -- ^ FileHandle
        -> CompletionPort a       -- ^ ExistingCompletionPort
        -> ULONG_PTR              -- ^ CompletionKey
        -> DWORD                  -- ^ NumberOfConcurrentThreads
        -> IO (CompletionPort a)

foreign import WINDOWS_CCONV safe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

foreign import ccall unsafe "iocp_new_overlapped"
    c_iocp_new_overlapped :: Word64 -> StablePtr a -> IO (Overlapped a)

foreign import ccall unsafe "iocp_finish_overlapped"
    c_iocp_finish_overlapped :: Overlapped a -> IO (StablePtr a)

-- TODO: even though this blocks, we can probably mark it unsafe because we
-- call it from a bound thread.
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
