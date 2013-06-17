{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Windows (
    -- * Windows data types
    BOOL,
    DWORD,
    HANDLE,
    LPWSTR,
    ULONG_PTR,

    -- * Constants
    iNFINITE,
    iNVALID_HANDLE_VALUE,

    -- * System errors
    ErrCode(..),
    e_WAIT_TIMEOUT,
    e_ERROR_IO_PENDING,
    e_WSANOTINITIALISED,
    getLastError,
    throwGetLastError,
    WinError(..),
    throwWinError,
    mkWinError,

    -- ** Guards for system calls that might fail
    failIf,
    failIf_,
) where

import Control.Exception
import Data.Char (isSpace)
import Data.Typeable (Typeable)
import Foreign
import Foreign.C

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

type BOOL       = #type BOOL
type DWORD      = #type DWORD
type HANDLE     = Ptr ()
type LPWSTR     = Ptr CWchar
type ULONG_PTR  = #type ULONG_PTR

iNFINITE :: DWORD
iNFINITE = #const INFINITE

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = wordPtrToPtr (-1)

newtype ErrCode = ErrCode DWORD
    deriving (Eq, Num)

instance Show ErrCode where
    show (ErrCode n) = f n where
        -- Add error codes here as needed.
        f #{const ERROR_SUCCESS}            = "ERROR_SUCCESS"             -- 0
        f #{const WAIT_TIMEOUT}             = "WAIT_TIMEOUT"              -- 258
        f #{const ERROR_IO_PENDING}         = "ERROR_IO_PENDING"          -- 997
        f #{const ERROR_INVALID_NETNAME}    = "ERROR_INVALID_NETNAME"     -- 1214
        f #{const ERROR_CONNECTION_REFUSED} = "ERROR_CONNECTION_REFUSED"  -- 1225
        f #{const WSAEINTR}                 = "WSAEINTR"                  -- 10004
        f #{const WSAEFAULT}                = "WSAEFAULT"                 -- 10014
        f #{const WSAEAFNOSUPPORT}          = "WSAEAFNOSUPPORT"           -- 10047
        f #{const WSAEISCONN}               = "WSAEISCONN"                -- 10056
        f #{const WSAENOTCONN}              = "WSAENOTCONN"               -- 10057
        f #{const WSAETIMEDOUT}             = "WSAETIMEDOUT"              -- 10060
        f #{const WSANOTINITIALISED}        = "WSANOTINITIALISED"         -- 10093
        f _ = "error code " ++ show n

e_WAIT_TIMEOUT :: ErrCode
e_WAIT_TIMEOUT = ErrCode #const WAIT_TIMEOUT

-- | Same as @WSA_IO_PENDING@
e_ERROR_IO_PENDING :: ErrCode
e_ERROR_IO_PENDING = ErrCode #const ERROR_IO_PENDING

e_WSANOTINITIALISED :: ErrCode
e_WSANOTINITIALISED = ErrCode #const WSANOTINITIALISED

data WinError = WinError
    { weCode      :: !ErrCode
    , weMessage   :: String
    , weLocation  :: String
    }
    deriving Typeable

instance Show WinError where
    show WinError{..} =
        weLocation ++ ": " ++ weMessage ++ " (" ++ show weCode ++ ")"

instance Exception WinError

throwWinError :: String -> ErrCode -> IO a
throwWinError loc code = mkWinError loc code >>= throwIO

-- | Get the last system error, and throw it as a 'WinError' exception.
throwGetLastError :: String -> IO a
throwGetLastError loc =
    getLastError >>= throwWinError loc

mkWinError :: String -> ErrCode -> IO WinError
mkWinError loc code = do
    msg <- getErrorMessage code
    return $! WinError
        { weCode     = code
        , weMessage  = msg
        , weLocation = loc
        }

-- | Get a string describing a Windows error code.  This uses the
-- @FormatMessage@ system call.
getErrorMessage :: ErrCode -> IO String
getErrorMessage code =
    -- Mask exceptions to avoid leaking string allocated by FormatMessageW.
    mask_ $ do
        -- Original code from System.Win32.Types.failWith
        c_msg <- c_getErrorMessage code
        if c_msg == nullPtr
          then return "(could not retrieve error message)"
          else do msg <- peekCWString c_msg
                  -- We ignore failure of freeing c_msg, given we're already failing
                  _ <- localFree c_msg
                  let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
                  return msg'

foreign import ccall unsafe "iocp_getErrorMessage" -- in iocp.c
    c_getErrorMessage :: ErrCode -> IO LPWSTR

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    localFree :: Ptr a -> IO (Ptr a)

-- | Get the last system error produced in the current thread.
foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
    getLastError :: IO ErrCode

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
    v <- act
    if p v then throwGetLastError wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
    v <- act
    if p v then throwGetLastError wh else return ()
