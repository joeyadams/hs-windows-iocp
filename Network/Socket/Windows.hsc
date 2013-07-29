{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | Interruptible network I\/O for Windows.  This provides replacements for
-- similar functions in "Network.Socket" and "Network.Socket.ByteString".
--
-- Requirements:
--
--  * Threaded RTS (i.e. pass the @-threaded@ option to GHC).
--
--  * Windows Vista or later.  Windows XP support will be added in the future,
--    but with somewhat degraded performance.
--
-- WARNING: If the 'Socket' was not created by the 'socket' function in this
-- module, you have to 'associate' it with the I\/O manager before using it
-- with other functions in this module.  Otherwise, they will hang
-- uninterruptibly.
module Network.Socket.Windows (
    socket,
    associate,
    connect,
    accept,
    recv,
    send,
    sendAll,
) where

import IOCP.Manager (Completion(..))
import qualified IOCP.Manager as M
import IOCP.Mswsock
import IOCP.Windows
import IOCP.Winsock.Types

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.ByteString.Internal (createAndTrim)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign
import Foreign.C
import GHC.IO.Exception
import Network.Socket hiding (
    -- We have special implementations for these.
      socket
    , connect
    , accept
    , recv
    , recvLen
    , recvBuf
    , send
    , sendBuf

    -- We plan to add support for these in the future.
    , sendTo
    , sendBufTo
    , recvFrom
    , recvBufFrom

    -- Can't support socketToHandle because, at the moment, Handle only
    -- pretends to support custom backends.
    , socketToHandle
    )
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

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- | Create a 'Socket' that can be used with the I\/O functions in this
-- module.  This calls 'NS.socket' followed by 'associate'.
socket :: Family -> SocketType -> ProtocolNumber -> IO Socket
socket f t p = do
    sock <- NS.socket f t p
    associate sock
    return sock

-- | Associate an existing 'Socket' with the I\/O manager.  This step must be
-- performed before any of the I\/O functions in this module may be used,
-- unless you used the 'socket' function in this module to create the 'Socket'.
-- Calling 'associate' on an already-associated handle will throw an 'IOError'.
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

-- | Variant of 'Network.Socket.ByteString.recv' that can be cancelled with
-- an asynchronous exception.
--
-- On EOF, this will return an empty string.  Be warned that the EOF handling
-- between "Network.Socket" and "Network.Socket.ByteString" is inconsistent:
--
--  ["Network.Socket"]
--    'Network.Socket.recv' throws an exception when the system call returns
--    zero bytes.
--
--  ["Network.Socket.ByteString"]
--    'Network.Socket.ByteString.recv' returns 0 when the system call returns
--    zero bytes.  This is how our recv behaves, too.
recv :: Socket -> Int -> IO ByteString
recv sock nbytes
  | nbytes < 0 = throwInvalidArgument "recv" "non-positive length"
  | otherwise  =
      createAndTrim nbytes $ \ptr ->
      rawRecv (sockSOCKET sock) [mkWSABUF ptr nbytes]

-- | Variant of 'Network.Socket.ByteString.send' that can be cancelled with an
-- asynchronous exception.
send :: Socket -> ByteString -> IO Int
send sock bs =
    unsafeUseAsCStringLen bs $ \(ptr, nbytes) ->
    rawSend (sockSOCKET sock) [mkWSABUF ptr nbytes]

-- | Variant of 'Network.Socket.ByteString.sendAll' that can be cancelled with
-- an asynchronous exception.
sendAll :: Socket -> ByteString -> IO ()
sendAll sock bs = do
    -- TODO: Test to make sure this doesn't spin forever if the socket is
    --       nonblocking or something.
    sent <- send sock bs
    when (sent < B.length bs) $ sendAll sock (B.drop sent bs)

rawRecv :: SOCKET -> [WSABUF] -> IO Int
rawRecv sock bufs =
    withArrayLen bufs $ \bufCount bufPtr ->
    with 0 $ \lpFlags ->
    withOverlappedNB "recv" sock (/= 0) $ \ol ->
    c_WSARecv sock bufPtr (fromIntegral bufCount) nullPtr lpFlags ol nullFunPtr

rawSend :: SOCKET -> [WSABUF] -> IO Int
rawSend sock bufs =
    withArrayLen bufs $ \bufCount bufPtr ->
    withOverlappedNB "send" sock (/= 0) $ \ol ->
    c_WSASend sock bufPtr (fromIntegral bufCount) nullPtr 0 ol nullFunPtr

------------------------------------------------------------------------
-- withOverlapped wrappers for SOCKET

withOverlapped :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO Completion
withOverlapped loc s = M.withOverlapped loc (toHANDLE s)

withOverlapped_ :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO ()
withOverlapped_ loc sock p s = do
    Completion{..} <- withOverlapped loc sock p s
    if cError /= 0
      then throwErrCode loc cError
      else return ()

withOverlappedNB :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO Int
withOverlappedNB loc sock p s = do
    Completion{..} <- withOverlapped loc sock p s
    if cError /= 0
      then throwErrCode loc cError
      else return $! fromIntegral cNumBytes

------------------------------------------------------------------------
-- Socket record accessors

sockFd :: Socket -> CInt
sockFd (MkSocket fd _ _ _ _) = fd

sockSOCKET :: Socket -> SOCKET
sockSOCKET = SOCKET . fromIntegral . sockFd

sockHANDLE :: Socket -> HANDLE
sockHANDLE = toHANDLE . sockSOCKET

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

throwInvalidArgument :: String -> String -> IO a
throwInvalidArgument loc descr =
    throwIO IOError
            { ioe_handle      = Nothing
            , ioe_type        = InvalidArgument
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

foreign import WINDOWS_CCONV unsafe "winsock2.h WSARecv"
    c_WSARecv
      :: SOCKET       -- ^ s
      -> LPWSABUF     -- ^ lpBuffers
      -> DWORD        -- ^ dwBufferCount
      -> LPDWORD      -- ^ lpNumberOfBytesRecvd
      -> LPDWORD      -- ^ lpFlags
      -> LPOVERLAPPED -- ^ lpOverlapped
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
                      -- ^ lpCompletionRoutine.  Must not call back
                      --   into Haskell, since 'c_WSARecv' is an
                      --   @unsafe@ foreign import.
      -> IO CInt

foreign import WINDOWS_CCONV unsafe "winsock2.h WSASend"
    c_WSASend
      :: SOCKET       -- ^ s
      -> LPWSABUF     -- ^ lpBuffers
      -> DWORD        -- ^ dwBufferCount
      -> LPDWORD      -- ^ lpNumberOfBytesSent
      -> DWORD        -- ^ dwFlags
      -> LPOVERLAPPED -- ^ lpOverlapped
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
                      -- ^ lpCompletionRoutine.  Must not call back
                      --   into Haskell, since 'c_WSASend' is an
                      --   @unsafe@ foreign import.
      -> IO CInt
