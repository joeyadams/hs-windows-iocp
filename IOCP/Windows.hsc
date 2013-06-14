{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
module IOCP.Windows (
    DWORD,
    LPWSTR,

    ErrCode(..),
    getLastError,

    WinError(..),
    throwWinError,
    mkWinError,
) where

import Control.Exception
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

type DWORD  = #type DWORD
type LPWSTR = Ptr CWchar

newtype ErrCode = ErrCode DWORD
    deriving Eq

instance Show ErrCode where
    show (ErrCode n) = f n where
        -- Add error codes here as needed.
        f #{const ERROR_SUCCESS}                = "ERROR_SUCCESS"
        f #{const WSAEINTR}                     = "WSAEINTR"
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
                  return msg

foreign import ccall unsafe "iocp_getErrorMessage" -- in iocp.c
    c_getErrorMessage :: ErrCode -> IO LPWSTR

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    localFree :: Ptr a -> IO (Ptr a)

-- | Get the last system error produced in the current thread.
foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
    getLastError :: IO ErrCode
