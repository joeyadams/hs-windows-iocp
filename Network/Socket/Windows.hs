{-# LANGUAGE RecordWildCards #-}
-- | Interruptible I\/O support for "Network.Socket".
module Network.Socket.Windows (
    socket,
    associate,
    connect,
) where

import IOCP.Manager (Completion(..))
import qualified IOCP.Manager as M
import IOCP.Mswsock
import IOCP.Windows
import IOCP.Winsock.Types (SOCKET(..))

import Control.Concurrent.MVar
import Foreign
import Foreign.C
import Network.Socket hiding (socket, connect)
import qualified Network.Socket as NS
import Network.Socket.Internal (withSockAddr)

-- | Create a 'Socket' that can be used with the I\/O functions in this
-- module.  This calls 'NS.socket' followed by 'associate'.
socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
socket f t p = do
    sock <- NS.socket f t p
    associate sock
    return sock

-- | Associate a 'Socket' with the I\/O manager.  This step must be performed
-- before any of the I\/O functions in this module may be used.  It may not be
-- performed twice ('socket' does it for you).
associate :: Socket -> IO ()
associate = M.associate . sockHANDLE

-- | Variant of 'NS.connect' that can be cancelled with an
-- asynchronous exception.
connect :: Socket -> SockAddr -> IO ()
connect sock addr =
    modifyMVar_ (sockStatus sock) $ \status ->
    case status of
        NotConnected -> go -- TODO: Implement the implicit bind step.
        Bound -> go
        _ -> fail $ "connect: can't peform connect on socket in status " ++ show status
  where
    go = do
        rawConnect (sockSOCKET sock) addr
        return Connected

-- | Call @ConnectEx@, which requires the socket to be initially bound.
rawConnect :: SOCKET -> SockAddr -> IO ()
rawConnect sock addr = do
    Mswsock{..} <- getMswsock
    withSockAddr addr $ \ptr len ->
      withOverlapped_ "connect" sock isFALSE $
        mswConnectEx sock ptr (fromIntegral len) nullPtr 0 nullPtr

withOverlapped :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO Completion
withOverlapped loc sock = M.withOverlapped loc (toHANDLE sock)

withOverlapped_ :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO ()
withOverlapped_ loc sock p s = do
    Completion{..} <- withOverlapped loc sock p s
    if cError /= 0
      then throwErrCode loc cError
      else return ()

------------------------------------------------------------------------
-- Socket record accessors

sockFd :: Socket -> CInt
sockFd (MkSocket fd _family _stype _protocol _status) = fd

sockSOCKET :: Socket -> SOCKET
sockSOCKET = SOCKET . fromIntegral . sockFd

sockHANDLE :: Socket -> HANDLE
sockHANDLE = wordPtrToPtr . fromIntegral . sockFd

sockStatus :: Socket -> MVar SocketStatus
sockStatus (MkSocket _fd _family _stype _protocol status) = status
