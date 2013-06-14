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
    getLastError,
    throwGetLastError,
    WinError(..),
    throwWinError,
    mkWinError,

    -- ** Guards for system calls that might fail
    failIf,
    failIf_,
    failIfNull,
    failIfZero,
    failIfFalse_,
    failUnlessSuccess,
    failUnlessSuccessOr,
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
        f #{const ERROR_SUCCESS}      = "ERROR_SUCCESS"     -- 0
        f #{const WAIT_TIMEOUT}       = "WAIT_TIMEOUT"      -- 258
        f #{const WSAEINTR}           = "WSAEINTR"          -- 10004
        f _ = "error code " ++ show n

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

failIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
failIfNull = failIf (== nullPtr)

failIfZero :: (Eq a, Num a) => String -> IO a -> IO a
failIfZero = failIf (== 0)

failIfFalse_ :: String -> IO Bool -> IO ()
failIfFalse_ = failIf_ not

failUnlessSuccess :: String -> IO ErrCode -> IO ()
failUnlessSuccess loc act = do
    r <- act
    if r == 0 then return () else throwWinError loc r

failUnlessSuccessOr :: ErrCode -> String -> IO ErrCode -> IO Bool
failUnlessSuccessOr val loc act = do
    r <- act
    if r == 0 then return False
        else if r == val then return True
        else throwWinError loc r
