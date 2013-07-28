{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE RankNTypes #-}
-- | Low-level completion port management and usage
module IOCP.CompletionPort (
    newCompletionPort,
    closeCompletionPort,
    associate,
    closeHandle,
    newOverlapped,
    newOverlappedWithOffset,
    peekOverlapped,
    freeOverlapped,
    getQueuedCompletionStatus,
    iNFINITE,
    postQueuedCompletionStatus,
    postQueuedCompletionStatusNB,
    postValue,
    cancelIo,
    CancelIoEx,
    loadCancelIoEx,
    runCancelIoEx,
    IOStatus(..),
    checkPendingIf_,

    -- * Types
    CompletionPort(..),
    Handle(..),
    Overlapped(..),
    OverlappedRec(..),
    Completion(..),
) where

import IOCP.Bindings
import IOCP.Types
import IOCP.Windows

import Foreign

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
    failIf_ (== 0) loc $
    c_CloseHandle h

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
newOverlappedWithOffset offset ctx = do
    let !ol = OVERLAPPED{ olInternal     = 0
                        , olInternalHigh = 0
                        , olOffset       = offset
                        , olEvent        = nullPtr
                        }
    ptr  <- malloc
    sptr <- newStablePtr ctx
    poke ptr $! OverlappedRec ol sptr
    return (Overlapped ptr)

-- | Retrieve the context value passed to 'newOverlapped'.
peekOverlapped :: Overlapped a -> IO a
peekOverlapped ol =
    peekOverlappedStablePtr ol >>= deRefStablePtr

-- | Free an 'Overlapped'.  This must be called exactly once for every
-- 'Overlapped' that is created, and must not be called if the I\/O operation
-- is still pending.
freeOverlapped :: Overlapped a -> IO ()
freeOverlapped ol@(Overlapped ptr) = do
    peekOverlappedStablePtr ol >>= freeStablePtr
    free ptr

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
    if b /= 0 then do
        -- I/O succeeded (GetQueuedCompletionStatus returned TRUE)
        numBytes <- peek pNumBytes
        let !c = Completion{ cNumBytes = numBytes, cError = 0 }
        return $ Just (c, ol)
    else if ol == Overlapped nullPtr then
        -- GetQueuedCompletionStatus failed
        if err == e_WAIT_TIMEOUT
            then return Nothing
            else throwWinError "getQueuedCompletionStatus" err
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
    failIf_ (== 0) "postQueuedCompletionStatus" $
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
    failIf_ (== 0) "cancelIo" $
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
    if b /= 0
      then return True
      else do
        err <- getLastError
        if err == e_ERROR_NOT_FOUND
          then return False
          else throwWinError "CancelIoEx" err

data IOStatus
  = Pending
  | Done
  deriving (Eq, Show)

-- | Wrap an action that either completes immediately ('Done') or will signal
-- completion at a later time ('Pending').
checkPendingIf_
    :: (a -> Bool) -- ^ Predicate returning 'True' on \"failure\"
    -> String
    -> IO a
    -> IO IOStatus
checkPendingIf_ p loc act = do
    a <- act
    if p a
      then do
        err <- getLastError
        if err == e_ERROR_IO_PENDING
            then return Pending
            else throwWinError loc err
      else return Done