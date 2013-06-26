{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Manager (
    associateHandle,
    closeHandle,
    runOverlapped,

    -- * Types
    Handle,
    Overlapped(..),
    Completion(..),
) where

import IOCP.Windows
import IOCP.Types (Completion(..))

import Control.Concurrent
import Control.Exception hiding (handle)
import Data.Typeable (Typeable)
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

associateHandle :: HANDLE -> IO Handle
associateHandle = undefined

closeHandle :: Handle -> IO ()
closeHandle Handle{..} =
    failIf_ (== 0) "closeHandle" $ c_CloseHandle hHANDLE

runOverlapped :: Handle -> (HANDLE -> IO Overlapped) -> IO Completion
runOverlapped handle@Handle{..} mkStart = do
    mv <- newEmptyMVar

    -- Only allow asynchronous exceptions to interrupt the takeMVar.
    mask_ $
      -- Allocate and initialize an Overlapped, and borrow a worker from this
      -- handle's worker pool.
      bracketAssumeMask (newStablePtr (putMVar mv)) freeStablePtr $ \spSignal ->
      bracketAssumeMask (mkStart' spSignal) c_iocp_free $ \ol ->
      withWorker handle $ \worker@Worker{..} -> do
        -- Tell the worker to start the overlapped I/O.
        postQueuedCompletionStatus workerCompletionPort ol

        -- Wait for completion notification from the worker whose completion
        -- port this HANDLE is associated with (which may be different than the
        -- worker we borrowed).
        takeMVar mv `onException` do
          -- Don't let another async exception interrupt us while we wait for
          -- cancellation.
          uninterruptibleMask_ $
            -- Tell our worker (same worker that started the I/O) to cancel I/O
            -- on the handle issued by the worker's OS thread.
            postCancelIo workerCompletionPort hHANDLE `finallyAssumeMask`

            -- This takeMVar absolutely must succeed.  Otherwise, brackets
            -- around runOverlapped (e.g. 'alloca') will release resources
            -- before the system is done using them.
            --
            -- This won't throw BlockedIndefinitelyOnMVar because our StablePtr
            -- references the MVar, meaning it won't be garbage collected.
            -- It might throw NonTermination if the I/O manager dies making us
            -- the only thread left, so let's hope that doesn't happen.
            takeMVar mv
  where
    -- Wrap mkStart to check for allocation error and initialize signalCallback.
    mkStart' spSignal = do
        ol <- failIf isNullPtr "runOverlapped" $ mkStart hHANDLE
        pokeSignalCallback ol spSignal
        return ol

-- | Tell the thread processing this 'CompletionPort' to call @CancelIo@ on the
-- given 'HANDLE'.
postCancelIo :: CompletionPort -> HANDLE -> IO ()
postCancelIo cport h = do
    ol <- failIf isNullPtr "iocp_alloc_cancel" $ c_iocp_alloc_cancel h
    postQueuedCompletionStatus cport ol

withWorker :: Handle -> (Worker -> IO a) -> IO a
withWorker = undefined

------------------------------------------------------------------------
-- Types

newtype CompletionPort = CompletionPort HANDLE
  deriving (Eq, Ord, Show, Typeable, Storable, IsPtr)

-- | A 'HANDLE' associated with the I\/O manager.
data Handle = Handle
    { hHANDLE :: !HANDLE
    }

data Worker = Worker
    { workerCompletionPort :: !CompletionPort
    }

-- | Points to a Windows @OVERLAPPED@ structure plus additional context
-- so the I\/O manager knows what to do with it.
newtype Overlapped = Overlapped (Ptr ())
  deriving (Eq, Ord, Show, Typeable, Storable, IsPtr)

data OverlappedState
  = OverlappedStart
    { startCallback  :: FunPtr (Overlapped -> IO BOOL)
    , signalCallback :: StablePtr (Completion -> IO ())
    }
  | OverlappedSignal
    { signalCallback :: StablePtr (Completion -> IO ())
    }
  | OverlappedCancel
    { cancelHandle :: HANDLE
    }

peekOverlappedState :: Overlapped -> IO OverlappedState
peekOverlappedState (Overlapped p) = do
    tag <- #{peek Overlapped, state} p :: IO #{type OverlappedState}
    case tag of
        (#const O_START) -> do
            startCallback  <- #{peek Overlapped, startCallback} p
            signalCallback <- #{peek Overlapped, signalCallback} p
            return OverlappedStart{..}
        (#const O_SIGNAL) -> do
            signalCallback <- #{peek Overlapped, signalCallback} p
            return OverlappedSignal{..}
        (#const O_CANCEL) -> do
            cancelHandle <- #{peek Overlapped, cancelHandle} p
            return OverlappedCancel{..}
        _ -> fail "corrupt Overlapped object detected"

pokeSignalCallback :: Overlapped -> StablePtr (Completion -> IO ()) -> IO ()
pokeSignalCallback (Overlapped p) = #{poke Overlapped, signalCallback} p

------------------------------------------------------------------------
-- Bindings

foreign import ccall unsafe "iocp.h iocp_start"
    c_iocp_start :: Overlapped -> IO BOOL

foreign import ccall unsafe "iocp.h iocp_alloc_cancel"
    c_iocp_alloc_cancel :: HANDLE -> IO Overlapped

foreign import ccall unsafe "iocp.h iocp_free"
    c_iocp_free :: Overlapped -> IO ()

foreign import WINDOWS_CCONV unsafe "windows.h CreateIoCompletionPort"
    c_CreateIoCompletionPort
        :: HANDLE                 -- ^ FileHandle
        -> CompletionPort         -- ^ ExistingCompletionPort
        -> ULONG_PTR              -- ^ CompletionKey
        -> DWORD                  -- ^ NumberOfConcurrentThreads
        -> IO CompletionPort

foreign import WINDOWS_CCONV safe "windows.h CloseHandle"
    c_CloseHandle :: HANDLE -> IO BOOL

foreign import WINDOWS_CCONV safe "GetQueuedCompletionStatus"
    c_GetQueuedCompletionStatus
        :: CompletionPort
        -> Ptr DWORD          -- ^ lpNumberOfBytes
        -> Ptr ULONG_PTR      -- ^ lpCompletionKey
        -> Ptr Overlapped     -- ^ lpOverlapped (type is OVERLAPPED**)
        -> DWORD              -- ^ dwMilliseconds
        -> IO BOOL

-- | Dequeue a completion packet.
--
-- Return 'Nothing' if the timeout expires, or 'Just' if a completion packet
-- was dequeued.  If it is a completion for a failed operation, the 'ErrCode'
-- will be non-zero.
getQueuedCompletionStatus
    :: CompletionPort
    -> DWORD  -- ^ Timeout, in milliseconds.  Use 'iNFINITE' to never time out.
    -> IO (Maybe (Completion, Overlapped))
getQueuedCompletionStatus cport timeout =
  with 0 $ \pNumBytes ->
  with 0 $ \pCompletionKey ->
  with (Overlapped nullPtr) $ \pOverlapped -> do
    b <- c_GetQueuedCompletionStatus cport pNumBytes pCompletionKey pOverlapped timeout
    err <- getLastError
    ol <- peek pOverlapped
    if b /= 0 then do
        -- I/O succeeded (GetQueuedCompletionStatus returned TRUE)
        numBytes <- peek pNumBytes
        let !c = Completion{ cNumBytes = numBytes, cError = 0 }
        return $ Just (c, ol)
    else if isNullPtr ol then
        -- GetQueuedCompletionStatus failed
        if err == e_WAIT_TIMEOUT
            then return Nothing
            else throwWinError "GetQueuedCompletionStatus" err
    else do
        -- I/O failed
        numBytes <- peek pNumBytes
        let !c = Completion{ cNumBytes = numBytes, cError = err }
        return $ Just (c, ol)

foreign import WINDOWS_CCONV unsafe "windows.h PostQueuedCompletionStatus"
    c_PostQueuedCompletionStatus
        :: CompletionPort
        -> DWORD              -- ^ dwNumberOfBytesTransferred
        -> ULONG_PTR          -- ^ dwCompletionKey
        -> Overlapped         -- ^ lpOverlapped
        -> IO BOOL

postQueuedCompletionStatus :: CompletionPort -> Overlapped -> IO ()
postQueuedCompletionStatus cport ol =
    failIf_ (== 0) "PostQueuedCompletionStatus" $
    c_PostQueuedCompletionStatus cport 0 0 ol

foreign import WINDOWS_CCONV unsafe "windows.h CancelIo"
    c_CancelIo :: HANDLE -> IO BOOL

------------------------------------------------------------------------
-- Utilities

class IsPtr a where
    fromPtr :: Ptr () -> a
    toPtr   :: a -> Ptr ()

instance IsPtr (Ptr a) where
    fromPtr = castPtr
    toPtr   = castPtr

isNullPtr :: IsPtr a => a -> Bool
isNullPtr a = toPtr a == nullPtr
{-# INLINE isNullPtr #-}

-- | Like 'bracket', but assume we are already in an exception mask
-- (meaning don't bother with the 'mask' part).
bracketAssumeMask :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketAssumeMask acquire release body = do
    a <- acquire
    r <- body a `onException` release a
    _ <- release a
    return r

-- | Like 'finally', but assume we are already in an exception mask.
finallyAssumeMask :: IO a -> IO b -> IO a
finallyAssumeMask body sequel = do
    r <- body `onException` sequel
    _ <- sequel
    return r
