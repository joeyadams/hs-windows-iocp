{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | I\/O manager access.  Use 'withOverlapped' to wrap system calls that
-- support overlapped I\/O (e.g. @ReadFile@).
module IOCP.Manager (
    associate,
    withOverlapped,

    -- * Types
    HANDLE,
    OVERLAPPED(..),
    LPOVERLAPPED,
    Completion(..),
) where

import IOCP.CompletionPort hiding (associate)
import qualified IOCP.CompletionPort as CP
import IOCP.Windows

import Control.Concurrent
import Control.Exception hiding (handle)
import Control.Monad
import GHC.IO.Exception
import System.IO.Unsafe (unsafePerformIO)

-- | Associate a 'HANDLE' with the I\/O manager.  This must be performed before
-- any overlapped I\/O, and may be called only once on a 'HANDLE'.  A 'HANDLE'
-- is automatically unassociated when it is closed.
associate :: HANDLE -> IO ()
associate h = do
    Manager{..} <- getManager "associate"
    CP.associate mgrPort h

-- | Wrap a system call that supports overlapped I\/O.
--
-- Allocate temporary buffers /outside/ of 'withOverlapped'.  The callback
-- returns when the I\/O request is put in, not when the I\/O is done.
--
-- 'withOverlapped' will wait until the OS sends a completion packet.
-- If 'withOverlapped' is interrupted with an asynchronous exception, it will
-- interrupt the I\/O with @CancelIo@ or @CancelIoEx@, but continue waiting for
-- a completion packet.  If the OS doesn't deliver one even after cancellation
-- (e.g. due to a bad driver), then 'withOverlapped' will hang uninterruptibly.
-- This is by design, so temporary resources (e.g. 'Foreign.Marshal.alloca')
-- will remain valid until the OS is done with them.
withOverlapped
    :: String
       -- ^ Name of calling function (used in error messages).
    -> HANDLE
       -- ^ Handle to perform I\/O on.  It must have been 'associate'd with the
       --   I\/O manager, and must be the same 'HANDLE' the start callback
       --   passes to its system call.
    -> (a -> Bool)
       -- ^ Predicate that returns 'True' when the start callback's system
       --   call reports an error (including @ERROR_IO_PENDING@).  This is
       --   usually 'isFALSE' for functions returning a 'BOOL', and @(/= 0)@
       --   for functions returning an @int@.
    -> (LPOVERLAPPED -> IO a)
       -- ^ Start callback.  This should wrap a Windows system call.
       --   The call is expected to queue a completion packet in one of the
       --   following cases:
       --
       --    * The call succeeds (the predicate returns 'False').
       --
       --    * The call \"fails\" with @ERROR_IO_PENDING@.
       --
       --   In any other case, the call must /not/ queue a completion packet,
       --   since the @OVERLAPPED@ will be freed immediately.
    -> IO Completion
withOverlapped loc handle isErr startCB = do
    Manager{..} <- getManager loc
    mv <- newEmptyMVar
    -- Use mask_ so exception won't pop up between start callback and
    -- our exception handler.  mask_ does not prevent takeMVar from being
    -- interrupted, since takeMVar is an interruptible operation.
    mask_ $ allocaOverlapped (putMVar mv) $ \ol -> do
        checkPendingIf_ isErr loc $ startCB (toLPOVERLAPPED ol)
            -- If start callback fails, the Overlapped will be freed and the
            -- exception will be propagated.  That's what we want.
        takeMVar mv `onException` do
            _ <- runCancelIoEx mgrCancelIoEx handle (Just ol)
            -- DO NOT let 'withOverlapped' finish without waiting for
            -- completion delivery.  Otherwise, our 'allocaOverlapped' will
            -- free the overlapped before it is done being used, and allocas
            -- above us may release resources being used by the pending I/O.
            uninterruptibleMask_ $ takeMVar mv

-- | Like 'failIf_', but if the action fails with @ERROR_IO_PENDING@,
-- return successfully.
checkPendingIf_
    :: (a -> Bool) -- ^ Predicate returning 'True' on \"failure\"
    -> String
    -> IO a
    -> IO ()
checkPendingIf_ p loc act = do
    a <- act
    if p a
      then do
        err <- getLastError
        if err == eERROR_IO_PENDING
            then return ()
            else throwErrCode loc err
      else return ()

data Manager = Manager
    { mgrPort       :: !(CompletionPort (Completion -> IO ()))
    , mgrCancelIoEx :: !CancelIoEx
    }

managerLoop :: Manager -> IO loop
managerLoop Manager{..} =
  forever $ do
    Just (c, ol) <- getQueuedCompletionStatus mgrPort iNFINITE
    cb <- peekOverlapped ol
    cb c

newManager :: IO (Either String Manager)
newManager
  | rtsSupportsBoundThreads =
      mask_ $ do
        m <- loadCancelIoEx
        case m of
          Nothing -> return $ Left "I/O manager requires Windows Vista or later"
          Just mgrCancelIoEx -> do
            mgrPort <- newCompletionPort
            let !mgr = Manager{..}
            _ <- forkIOWithUnmask $ \unmask ->
                 unmask (managerLoop mgr)
            return $ Right mgr
  | otherwise = return $ Left "I/O manager only available under threaded RTS"

managerCAF :: Either String Manager
managerCAF = unsafePerformIO newManager
{-# NOINLINE managerCAF #-}

getManager :: String -> IO Manager
getManager loc =
    case managerCAF of
        Left errMsg ->
            throwIO IOError
                    { ioe_handle      = Nothing
                    , ioe_type        = UnsupportedOperation
                    , ioe_location    = loc
                    , ioe_description = errMsg
                    , ioe_errno       = Nothing
                    , ioe_filename    = Nothing
                    }
        Right mgr -> return mgr
