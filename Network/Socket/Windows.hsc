{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Interruptible I\/O support for "Network.Socket".
module Network.Socket.Windows (
    socket,
    associate,
    connect,
    accept,
) where

import IOCP.Manager (Completion(..))
import qualified IOCP.Manager as M
import IOCP.Mswsock
import IOCP.Windows
import IOCP.Winsock.Types (SOCKET(..))

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Foreign
import Foreign.C
import GHC.IO.Exception
import Network.Socket hiding (socket, connect, accept)
import qualified Network.Socket as NS
import Network.Socket.Internal
    ( withSockAddr
    , peekSockAddr
    , sizeOfSockAddrByFamily
    , throwSocketErrorIfMinus1_
    )

#include <winsock2.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif


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
    join $ modifyMVar (sockStatus sock) $ \status ->
    case status of
        NotConnected ->
            -- Relinquish the MVar before calling 'bind', or it will deadlock.
            return (status, bindAnyPort sock >> connect sock addr)
        Bound -> do
            rawConnect (sockSOCKET sock) addr
            return (Connected, return ())
        _ -> fail $ "connect: can't peform connect on socket in status " ++ show status

-- | @ConnectEx()@ requires the socket to be bound to a local address already,
-- while @connect()@ does this step automatically.  Try to emulate the implicit
-- bind done by @connect()@.
bindAnyPort :: Socket -> IO ()
bindAnyPort sock =
  case sockFamily sock of
      AF_INET  -> bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
      AF_INET6 -> bind sock $ SockAddrInet6 aNY_PORT 0 iN6ADDR_ANY 0
                  -- TODO: test AF_INET6
      family ->
          -- Don't know how to perform implicit bind for other
          -- address families.
          throwUnsupported "connect" $
              "address family " ++ show family ++ " not supported"

-- | Variant of 'NS.accept' that can be cancelled with an
-- asynchronous exception.
accept :: Socket -> IO (Socket, SockAddr)
accept sock = do
    status <- readMVar (sockStatus sock)
    -- Network.Socket.accept also allows the Connected state, but I don't know
    -- when that would work, so not supporting it for now.
    if status == Listening
      then do
          newSock <- socket (sockFamily sock) (sockType sock) (sockProtocol sock)
          let !sz = sizeOfSockAddrByFamily (sockFamily sock)
          (_localAddr, remoteAddr) <- rawAccept (sockSOCKET sock) (sockSOCKET newSock) sz sz
          _ <- swapMVar (sockStatus newSock) Connected
          return (newSock, remoteAddr)
      else fail $ "accept: can't perform accept on socket in status " ++ show status

-- | Call @ConnectEx@, which requires the socket to be initially bound.
rawConnect :: SOCKET -> SockAddr -> IO ()
rawConnect sock addr = do
    Mswsock{..} <- getMswsock
    withSockAddr addr $ \ptr len ->
      withOverlapped_ "connect" sock isFALSE $
        mswConnectEx sock ptr (fromIntegral len) nullPtr 0 nullPtr

    -- Only a limited set of operations are available on a socket newly
    -- connected with ConnectEx.  Set SO_UPDATE_CONNECT_CONTEXT so we can do
    -- things like shutdown() and getpeername().
    updateConnectContext sock

-- | Call @AcceptEx@, which returns both the local and remote addresses.
rawAccept :: SOCKET -- ^ Socket that 'listen' was called on
          -> SOCKET -- ^ Socket that will be used to talk to the server
          -> Int    -- ^ Local address maximum length
          -> Int    -- ^ Remote address maximum length
          -> IO (SockAddr, SockAddr)
rawAccept listenSock acceptSock localAddrLen remoteAddrLen = do
    Mswsock{..} <- getMswsock
    allocaBytes (localAddrLen + remoteAddrLen + 32) $ \buf ->
      with nullPtr $ \localAddrPtr ->
      with nullPtr $ \remoteAddrPtr ->
      with (fromIntegral localAddrLen) $ \localAddrLenPtr ->
      with (fromIntegral remoteAddrLen) $ \remoteAddrLenPtr -> do
          withOverlapped_ "accept" listenSock isFALSE $
              mswAcceptEx listenSock acceptSock buf
                          0 (fi $ localAddrLen + 16) (fi $ remoteAddrLen + 16)
                          nullPtr
          mswGetAcceptExSockaddrs buf
              0 (fi $ localAddrLen + 16) (fi $ remoteAddrLen + 16)
              localAddrPtr localAddrLenPtr
              remoteAddrPtr remoteAddrLenPtr
          localAddr <- peek localAddrPtr >>= peekSockAddr
          remoteAddr <- peek remoteAddrPtr >>= peekSockAddr

          -- Only a limited set of operations are available on a socket newly
          -- accepted with AcceptEx.  Set SO_UPDATE_ACCEPT_CONTEXT so we can do
          -- things like shutdown() and getpeername().
          updateAcceptContext acceptSock listenSock

          return (localAddr, remoteAddr)

------------------------------------------------------------------------
-- withOverlapped wrappers for SOCKET

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
sockFd (MkSocket fd _ _ _ _) = fd

sockSOCKET :: Socket -> SOCKET
sockSOCKET = SOCKET . fromIntegral . sockFd

sockHANDLE :: Socket -> HANDLE
sockHANDLE = wordPtrToPtr . fromIntegral . sockFd

sockFamily :: Socket -> Family
sockFamily (MkSocket _ family _ _ _) = family

sockType :: Socket -> SocketType
sockType (MkSocket _ _ stype _ _) = stype

sockProtocol :: Socket -> ProtocolNumber
sockProtocol (MkSocket _ _ _ protocol _) = protocol

sockStatus :: Socket -> MVar SocketStatus
sockStatus (MkSocket _ _ _ _ status) = status

------------------------------------------------------------------------
-- Utilities

throwUnsupported :: String -> String -> IO a
throwUnsupported loc descr =
    throwIO IOError
            { ioe_handle      = Nothing
            , ioe_type        = UnsupportedOperation
            , ioe_location    = loc
            , ioe_description = descr
            , ioe_errno       = Nothing
            , ioe_filename    = Nothing
            }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

------------------------------------------------------------------------
-- Bindings

foreign import WINDOWS_CCONV unsafe "winsock2.h setsockopt"
    c_setsockopt :: SOCKET
                 -> CInt        -- ^ level
                 -> CInt        -- ^ optname
                 -> Ptr CChar   -- ^ optval
                 -> CInt        -- ^ optlen
                 -> IO CInt     -- ^ 0 on success, SOCKET_ERROR otherwise

sO_UPDATE_ACCEPT_CONTEXT, sO_UPDATE_CONNECT_CONTEXT :: CInt
-- Numbers from mswsock.h in mingw-w64.
sO_UPDATE_ACCEPT_CONTEXT  = 0x700B
sO_UPDATE_CONNECT_CONTEXT = 0x7010

updateAcceptContext :: SOCKET -> SOCKET -> IO ()
updateAcceptContext acceptSock listenSock =
    with listenSock $ \listenSockPtr ->
    throwSocketErrorIfMinus1_ "setsockopt(SO_UPDATE_ACCEPT_CONTEXT)" $
    c_setsockopt acceptSock
                 (#const SOL_SOCKET) sO_UPDATE_ACCEPT_CONTEXT
                 (castPtr listenSockPtr) (fi $ sizeOf listenSock)

updateConnectContext :: SOCKET -> IO ()
updateConnectContext sock =
    throwSocketErrorIfMinus1_ "setsockopt(SO_UPDATE_CONNECT_CONTEXT)" $
    c_setsockopt sock
                 (#const SOL_SOCKET) sO_UPDATE_CONNECT_CONTEXT
                 nullPtr 0
