{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
-- | Low-level completion port management and usage
module IOCP.CompletionPort (
    newCompletionPort,
    closeCompletionPort,
    associate,
    closeHandle,
    newOverlapped,
    freeOverlapped,
    allocaOverlapped,
    peekOverlapped,
    getQueuedCompletionStatus,
    iNFINITE,
    postQueuedCompletionStatus,
    postQueuedCompletionStatusNB,
    postValue,
    cancelIo,
    CancelIoEx,
    loadCancelIoEx,
    runCancelIoEx,

    -- * Types
    CompletionPort(..),
    Handle(..),
    Overlapped(..),
    toLPOVERLAPPED,
    OverlappedRec(..),
    Completion(..),
) where

import IOCP.Windows

import Control.Applicative ((<$>))
import Control.Exception (bracket, finally)
import Foreign

#include "iocp.h"

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

closeHANDLE :: String -> HANDLE -> IO ()
closeHANDLE loc h =
    failIf_ isFALSE loc $
    c_CloseHandle h

-- | Allocate a new
-- <http://msdn.microsoft.com/en-us/library/windows/desktop/ms684342%28v=vs.85%29.aspx OVERLAPPED>
-- structure, attaching the given piggyback value to it.  The resulting
-- pointer may be passed to a system call that takes an @LPOVERLAPPED@,
-- provided the @HANDLE@ was associated with a 'CompletionPort' with the
-- same type @a@.
newOverlapped :: a -> IO (Overlapped a)
newOverlapped ctx = do
    sptr  <- newStablePtr ctx
    olptr <- new (mkOverlappedRec sptr)
    return (Overlapped olptr)

-- | Free an 'Overlapped' allocated with 'newOverlapped'.
freeOverlapped :: Overlapped a -> IO ()
freeOverlapped ol@(Overlapped ptr) = do
    True <- getOverlappedAlive ol
    setOverlappedAlive ol False
    peekOverlappedStablePtr ol >>= freeStablePtr
    free ptr

-- | Allocate a new @OVERLAPPED@, and free it after the inner
-- computation finishes.
allocaOverlapped :: a -> (Overlapped a -> IO b) -> IO b
allocaOverlapped ctx body =
    bracket (newStablePtr ctx) freeStablePtr $ \sptr ->
    with (mkOverlappedRec sptr) $ \olptr -> do
        let ol = Overlapped olptr
        body ol `finally` do
            True <- getOverlappedAlive ol
            setOverlappedAlive ol False

-- | Retrieve the context value passed to 'newOverlapped'.
peekOverlapped :: Overlapped a -> IO a
peekOverlapped ol = do
    True <- getOverlappedAlive ol
    peekOverlappedStablePtr ol >>= deRefStablePtr

-- | Dequeue a completion packet.
--
-- Return 'Nothing' if the timeout expires, or 'Just' if a completion packet
-- was dequeued.  If it is a completion for a failed operation, the 'ErrCode'
-- will be non-zero.
getQueuedCompletionStatus
    :: CompletionPort a
    -> DWORD  -- ^ Timeout, in milliseconds.  Use 'iNFINITE' to never time out.
    -> IO (Maybe (Completion, Overlapped a))
getQueuedCompletionStatus cport timeout =
  with 0 $ \pNumBytes ->
  with 0 $ \pCompletionKey ->
  with (Overlapped nullPtr) $ \pOverlapped -> do
    b <- c_GetQueuedCompletionStatus cport pNumBytes pCompletionKey pOverlapped timeout
    err <- getLastError
    ol <- peek pOverlapped
    if isTRUE b then do
        -- I/O succeeded (GetQueuedCompletionStatus returned TRUE)
        numBytes <- peek pNumBytes
        let !c = Completion{ cNumBytes = numBytes, cError = 0 }
        return $ Just (c, ol)
    else if ol == Overlapped nullPtr then
        -- GetQueuedCompletionStatus failed
        if err == eWAIT_TIMEOUT
            then return Nothing
            else throwErrCode "getQueuedCompletionStatus" err
    else do
        -- I/O failed
        numBytes <- peek pNumBytes
        let !c = Completion{ cNumBytes = numBytes, cError = err }
        return $ Just (c, ol)

postQueuedCompletionStatus :: CompletionPort a -> Overlapped a -> IO ()
postQueuedCompletionStatus = postQueuedCompletionStatusNB 0

postQueuedCompletionStatusNB
    :: DWORD  -- ^ Number of bytes transferred
    -> CompletionPort a
    -> Overlapped a
    -> IO ()
postQueuedCompletionStatusNB numBytes cport ol =
    failIf_ isFALSE "postQueuedCompletionStatus" $
    c_PostQueuedCompletionStatus cport numBytes 0 ol

postValue :: CompletionPort a -> a -> IO ()
postValue cport a = newOverlapped a >>= postQueuedCompletionStatus cport

-- | Cancel all pending overlapped I\/O issued by the current OS thread on the
-- given handle.
--
-- Warning: prior to Windows Vista, when an OS thread exits, all of its pending
-- I\/O is canceled automatically.  On Vista and later, this cancellation does
-- not happen if the 'Handle' has been associated with a completion port.
--
-- For earlier systems, we can use this workaround to support targeted
-- cancellation:
--
--  * Initiate the I\/O from a bound thread that doesn't go away (usually a
--    thread processing completions).  We'll call this the \"proxy thread\".
--
--  * To cancel the I\/O, call 'cancelIo' from the same proxy thread that
--    initiated it.
--
--  * For concurrent operations on the same 'Handle', use a separate OS thread
--    for each operation.  Multiple 'Handle's may share a proxy thread,
--    but operations on the same handle may not.
cancelIo :: Handle a -> IO ()
cancelIo (Handle h) =
    failIf_ isFALSE "cancelIo" $
    c_CancelIo h

-- | Cancel pending I\/O on a given handle.  Return 'False' if nothing was
-- found to be canceled (e.g. because the I\/O already completed).
runCancelIoEx
    :: CancelIoEx
    -> Handle a
    -> Maybe (Overlapped a)
       -- ^ Identifies the operation to cancel.  If 'Nothing', cancel all
       --   pending I\/O on this handle, regardless of what threads started it.
    -> IO Bool
runCancelIoEx (CancelIoEx f) (Handle h) mol = do
    b <- f h (maybe nullPtr toLPOVERLAPPED mol)
    if isTRUE b
      then return True
      else do
        err <- getLastError
        if err == eERROR_NOT_FOUND
          then return False
          else throwErrCode "CancelIoEx" err

------------------------------------------------------------------------
-- Bindings

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

------------------------------------------------------------------------
-- Types

newtype CompletionPort a = CompletionPort HANDLE
    deriving (Eq, Show, Storable)

newtype Handle a = Handle HANDLE
    deriving (Eq, Show, Storable)

newtype Overlapped a = Overlapped (Ptr (OverlappedRec a))
    deriving (Eq, Show, Storable)

toLPOVERLAPPED :: Overlapped a -> LPOVERLAPPED
toLPOVERLAPPED (Overlapped ptr) = castPtr ptr

data OverlappedRec a = OverlappedRec
    { orOverlapped :: !OVERLAPPED
    , orContext    :: !(StablePtr a)
    , orAlive      :: !BOOL
      -- ^ This is set to @FALSE@ when the overlapped is freed, to check for
      --   completion delivery when we didn't expect it.
    }
    deriving Eq

mkOverlappedRec :: StablePtr a -> OverlappedRec a
mkOverlappedRec sptr =
    OverlappedRec
    { orOverlapped = OVERLAPPED 0 0 0 nullPtr
    , orContext    = sptr
    , orAlive      = toBOOL True
    }

instance Storable (OverlappedRec a) where
    sizeOf    _ = #{size      OverlappedRec}
    alignment _ = #{alignment OverlappedRec}
    peek p = do
        ol    <- (#peek OverlappedRec, ol)    p
        sptr  <- (#peek OverlappedRec, sptr)  p
        alive <- (#peek OverlappedRec, alive) p
        return $! OverlappedRec ol sptr alive
    poke p (OverlappedRec ol sptr alive) = do
        (#poke OverlappedRec, ol)    p ol
        (#poke OverlappedRec, sptr)  p sptr
        (#poke OverlappedRec, alive) p alive

instance Show (OverlappedRec a) where
    showsPrec p (OverlappedRec ol sptr alive) =
      showParen (p > 10) $
        showString "OverlappedRec " .
        showsPrec 11 ol . showChar ' ' .
        showsPrec 11 (castStablePtrToPtr sptr) . showChar ' ' .
        showsPrec 11 alive

peekOverlappedStablePtr :: Overlapped a -> IO (StablePtr a)
peekOverlappedStablePtr (Overlapped p) = (#peek OverlappedRec, sptr) p

getOverlappedAlive :: Overlapped a -> IO Bool
getOverlappedAlive (Overlapped p) =
    isTRUE <$> (#peek OverlappedRec, alive) p

setOverlappedAlive :: Overlapped a -> Bool -> IO ()
setOverlappedAlive (Overlapped p) b =
    (#poke OverlappedRec, alive) p (toBOOL b)

data Completion = Completion
    { cNumBytes :: !DWORD   -- ^ Number of bytes transferred during I/O operation
    , cError    :: !ErrCode -- ^ Zero if the I/O was successful
    }
    deriving Show
