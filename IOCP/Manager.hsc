{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
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
    _ <- CP.associate mgrPort h
    return ()

-- | Wrap a system call that supports overlapped I\/O.
withOverlapped
    :: String
       -- ^ Name of calling function (used in error messages).
    -> HANDLE
       -- ^ Handle to perform I\/O on.  It must have been 'associate'd with the
       --   I\/O manager, and must be the same 'HANDLE' the start callback
       --   passes to its system call.
    -> (LPOVERLAPPED -> IO ())
       -- ^ Start callback.  If this returns successfully, completion will be
       --   signaled through the completion port mechanism.  If this throws an
       --   exception, it means an error occurred and no completion will
       --   be queued.
    -> IO Completion
withOverlapped loc handle startCB = do
    Manager{..} <- getManager loc
    mv <- newEmptyMVar
    -- Use mask_ so exception won't pop up between start callback and
    -- our exception handler.  mask_ does not prevent takeMVar from being
    -- interrupted, since takeMVar is an interruptible operation.
    mask_ $ allocaOverlapped (putMVar mv) $ \ol -> do
        startCB (toLPOVERLAPPED ol)
            -- If start callback fails, the Overlapped will be freed and the
            -- exception will be propagated.  That's what we want.
        takeMVar mv `onException` do
            _ <- runCancelIoEx mgrCancelIoEx (Handle handle) (Just ol)
            -- DO NOT let 'withOverlapped' finish without waiting for
            -- completion delivery.  Otherwise, our 'allocaOverlapped' will
            -- free the overlapped before it is done being used, and allocas
            -- above us may release resources being used by the pending I/O.
            uninterruptibleMask_ $ takeMVar mv

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
