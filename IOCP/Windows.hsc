{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
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
    LPDWORD,
    LPVOID,
    LPSTR,
    LPWSTR,
    ULONG_PTR,
    GUID(..),
    OVERLAPPED(..),
    LPOVERLAPPED,

    -- * Constants
    iNFINITE,
    iNVALID_HANDLE_VALUE,

    -- * Loading functions dynamically
    HMODULE(..),
    getModuleHandle,
    getProcAddress,
    kernel32,

    -- * System errors
    ErrCode(..),
    e_ERROR_PROC_NOT_FOUND,
    e_WAIT_TIMEOUT,
    e_ERROR_IO_PENDING,
    e_ERROR_NOT_FOUND,
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

import IOCP.Utils

import Control.Exception
import Data.Bits
import Data.Char (isSpace)
import Data.String (IsString(..))
import Data.Typeable (Typeable)
import Foreign
import Foreign.C
import qualified System.IO.Unsafe as U

#include <windows.h>

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

type BOOL       = #type BOOL
type DWORD      = #type DWORD
type HANDLE     = Ptr ()
type LPDWORD    = Ptr DWORD
type LPVOID     = Ptr ()
type LPSTR      = CString
type LPWSTR     = CWString
type ULONG_PTR  = #type ULONG_PTR

data GUID = GUID !Word32 !Word32 !Word32 !Word32
  deriving (Eq, Ord, Typeable)

-- | @{XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX}@, where X is an uppercase hex digit
instance Show GUID where
    showsPrec _p (GUID a b c d)
      = showChar '{'
      . showHex32 a           . showChar '-'
      . showHex16 (b .>>. 16) . showChar '-'
      . showHex16 (b .>>. 0)  . showChar '-'
      . showHex16 (c .>>. 16) . showChar '-'
      . showHex16 (c .>>. 0) . showHex32 d
      . showChar '}'

-- | Same format as 'Show', but lowercase hex digits are allowed,
-- and surrounding braces are optional.
instance Read GUID where
    readsPrec _p ('{' : s0) = do
        (guid, '}' : s1) <- readGUID s0
        [(guid, s1)]
    readsPrec _p s0 = readGUID s0

readGUID :: ReadS GUID
readGUID s0 = do
    (a , '-':s1) <- readHex32 s0
    (b0, '-':s2) <- readHex16 s1
    (b1, '-':s3) <- readHex16 s2
    (c0, '-':s4) <- readHex16 s3
    (c1,     s5) <- readHex16 s4
    (d ,     s6) <- readHex32 s5
    let !b = (b0 .<<. 16) .|. (b1 .<<. 0)
        !c = (c0 .<<. 16) .|. (c1 .<<. 0)
        !guid = GUID a b c d
    [(guid, s6)]

instance IsString GUID where
    fromString s =
      case reads s of
          [(guid,"")] -> guid
          _           -> error $ "invalid GUID literal " ++ show s

instance Storable GUID where
    sizeOf    _ = #{size      GUID}
    alignment _ = #{alignment GUID}
    peek p = do
        a  <- (#peek GUID, Data1) p :: IO Word32
        b0 <- (#peek GUID, Data2) p :: IO Word16
        b1 <- (#peek GUID, Data3) p :: IO Word16
        c  <- peekByteOff p ((#offset GUID, Data4) + 0) :: IO Word32be
        d  <- peekByteOff p ((#offset GUID, Data4) + 4) :: IO Word32be
        return $! GUID a ((b0 .<<. 16) .|. (b1 .<<. 0))
                       (fromWord32be c) (fromWord32be d)
    poke p (GUID a b c d) = do
        (#poke GUID, Data1) p (a          :: Word32)
        (#poke GUID, Data2) p (b .>>. 16  :: Word16)
        (#poke GUID, Data3) p (b .>>. 0   :: Word16)
        pokeByteOff p ((#offset GUID, Data4) + 0) (Word32be c)
        pokeByteOff p ((#offset GUID, Data4) + 4) (Word32be d)

data OVERLAPPED = OVERLAPPED
    { olInternal      :: !ULONG_PTR
    , olInternalHigh  :: !ULONG_PTR
    , olOffset        :: !Word64
    , olEvent         :: !HANDLE
    }
  deriving (Eq, Show, Typeable)

instance Storable OVERLAPPED where
    sizeOf    _ = #{size      OVERLAPPED}
    alignment _ = #{alignment OVERLAPPED}
    peek p = do
        olInternal     <- (#peek OVERLAPPED, Internal)     p
        olInternalHigh <- (#peek OVERLAPPED, InternalHigh) p
        offsetLo       <- (#peek OVERLAPPED, Offset)       p :: IO DWORD
        offsetHi       <- (#peek OVERLAPPED, OffsetHigh)   p :: IO DWORD
        olEvent        <- (#peek OVERLAPPED, hEvent)       p
        let olOffset = (offsetHi .<<. 32) .|. (offsetLo .<<. 0)
        return $! OVERLAPPED{..}
    poke p OVERLAPPED{..} = do
        (#poke OVERLAPPED, Internal)     p olInternal
        (#poke OVERLAPPED, InternalHigh) p olInternalHigh
        (#poke OVERLAPPED, Offset)       p (olOffset .>>.  0 :: DWORD)
        (#poke OVERLAPPED, OffsetHigh)   p (olOffset .>>. 32 :: DWORD)
        (#poke OVERLAPPED, hEvent)       p olEvent

type LPOVERLAPPED = Ptr OVERLAPPED

newtype HMODULE = HMODULE HANDLE
  deriving (Eq, Show, Typeable)

foreign import WINDOWS_CCONV unsafe "windows.h GetModuleHandleW"
    c_GetModuleHandleW :: LPWSTR -> IO HMODULE

getModuleHandle :: String -> IO HMODULE
getModuleHandle name =
    failIf (== HMODULE nullPtr) "getModuleHandle" $
    withCWString name c_GetModuleHandleW

foreign import WINDOWS_CCONV unsafe "windows.h GetProcAddress"
    c_GetProcAddress :: HMODULE -> LPSTR -> IO (FunPtr a)

-- | Retrieve an exported definition from a DLL.  Return 'Nothing' if the
-- definition is not available.
--
-- For example, @'getProcAddress' 'kernel32' \"GetTickCount64\"@ returns a
-- 'Just' on Windows Vista and later, or 'Nothing' on earlier systems.
getProcAddress :: HMODULE -> String -> IO (Maybe (FunPtr a))
getProcAddress hModule procName = do
    ptr <- withCString procName $ c_GetProcAddress hModule
    if ptr == nullFunPtr
      then do
        err <- getLastError
        if err == e_ERROR_PROC_NOT_FOUND
          then return Nothing
          else throwWinError "getProcAddress" err
      else return $ Just ptr

-- | The module handle for @\"kernel32\.dll\"@
kernel32 :: HMODULE
kernel32 = U.unsafePerformIO $ getModuleHandle "kernel32.dll"
{-# NOINLINE kernel32 #-}

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
        f #{const ERROR_INVALID_PARAMETER}  = "ERROR_INVALID_PARAMETER"   -- 87
        f #{const ERROR_SEM_TIMEOUT}        = "ERROR_SEM_TIMEOUT"         -- 121
        f #{const ERROR_MOD_NOT_FOUND}      = "ERROR_MOD_NOT_FOUND"       -- 126
        f #{const ERROR_PROC_NOT_FOUND}     = "ERROR_PROC_NOT_FOUND"      -- 127
        f #{const WAIT_TIMEOUT}             = "WAIT_TIMEOUT"              -- 258
        f #{const ERROR_OPERATION_ABORTED}  = "ERROR_OPERATION_ABORTED"   -- 995
        f #{const ERROR_IO_PENDING}         = "ERROR_IO_PENDING"          -- 997
        f #{const ERROR_NOT_FOUND}          = "ERROR_NOT_FOUND"           -- 1168
        f #{const ERROR_INVALID_NETNAME}    = "ERROR_INVALID_NETNAME"     -- 1214
        f #{const ERROR_CONNECTION_REFUSED} = "ERROR_CONNECTION_REFUSED"  -- 1225
        f #{const WSAEINTR}                 = "WSAEINTR"                  -- 10004
        f #{const WSAEFAULT}                = "WSAEFAULT"                 -- 10014
        f #{const WSAEINVAL}                = "WSAEINVAL"                 -- 10022
        f #{const WSAENOTSOCK}              = "WSAENOTSOCK"               -- 10038
        f #{const WSAEAFNOSUPPORT}          = "WSAEAFNOSUPPORT"           -- 10047
        f #{const WSAEADDRINUSE}            = "WSAEADDRINUSE"             -- 10048
        f #{const WSAEADDRNOTAVAIL}         = "WSAEADDRNOTAVAIL"          -- 10049
        f #{const WSAECONNRESET}            = "WSAECONNRESET"             -- 10054
        f #{const WSAEISCONN}               = "WSAEISCONN"                -- 10056
        f #{const WSAENOTCONN}              = "WSAENOTCONN"               -- 10057
        f #{const WSAETIMEDOUT}             = "WSAETIMEDOUT"              -- 10060
        f #{const WSANOTINITIALISED}        = "WSANOTINITIALISED"         -- 10093
        f _ = "error code " ++ show n

e_ERROR_PROC_NOT_FOUND :: ErrCode
e_ERROR_PROC_NOT_FOUND = ErrCode #const ERROR_PROC_NOT_FOUND

e_WAIT_TIMEOUT :: ErrCode
e_WAIT_TIMEOUT = ErrCode #const WAIT_TIMEOUT

-- | Same as @WSA_IO_PENDING@
e_ERROR_IO_PENDING :: ErrCode
e_ERROR_IO_PENDING = ErrCode #const ERROR_IO_PENDING

e_ERROR_NOT_FOUND :: ErrCode
e_ERROR_NOT_FOUND = ErrCode #const ERROR_NOT_FOUND

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
