{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
-- | Provides a similar set of definitions as "System.Win32.Types" or
-- "GHC.Windows", with some improvements we need.  For example:
--
--  * 'throwErrCode' may be called from a different thread than that producing
--    the error, allowing I\/O operations to throw exceptions that were
--    delivered as completions to the I\/O manager thread.
module IOCP.Windows (
    -- * Windows data types
    BOOL(..),
    toBOOL,
    isTRUE,
    isFALSE,
    DWORD,
    HANDLE,
    LPDWORD,
    LPINT,
    LPVOID,
    PVOID,
    LPSTR,
    LPWSTR,
    ULONG_PTR,
    GUID(..),
    OVERLAPPED(..),
    LPOVERLAPPED,
    pokeOffset,

    -- * Constants
    iNFINITE,
    iNVALID_HANDLE_VALUE,

    -- * Conversion helpers
    IsHANDLE(..),
    IsLPOVERLAPPED(..),

    -- * Loading functions dynamically
    HMODULE(..),
    getModuleHandle,
    getProcAddress,
    kernel32,

    -- * System errors
    ErrCode(..),
    getLastError,
    throwGetLastError,
    throwErrCode,
    errCodeToIOError,
    getErrorMessage,

    -- ** Error code constants
    eERROR_PROC_NOT_FOUND,
    eWAIT_TIMEOUT,
    eERROR_IO_PENDING,
    eERROR_NOT_FOUND,
    eWSAEINVAL,
    eWSANOTINITIALISED,

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
import GHC.IO.Exception
import qualified System.IO.Unsafe as U

#include "errno-posix.h"
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

type DWORD      = #type DWORD
type HANDLE     = Ptr ()
type LPDWORD    = Ptr DWORD
type LPINT      = Ptr CInt
type LPVOID     = Ptr ()
type PVOID      = Ptr ()
type LPSTR      = CString
type LPWSTR     = CWString
type ULONG_PTR  = #type ULONG_PTR

-- | The Windows @BOOL@ type.  Non-zero means true, and zero means false.
--
-- Note: "GHC.Windows" and "System.Win32.Types" incorrectly use 'Bool' to
-- represent this.
newtype BOOL = BOOL #type BOOL
    deriving (Eq, Storable, Typeable)

instance Show BOOL where
    show b = if isTRUE b then "TRUE" else "FALSE"

-- | Convert a Haskell boolean to a Windows @BOOL@.
toBOOL :: Bool -> BOOL
toBOOL False = BOOL (#const FALSE)
toBOOL True  = BOOL (#const TRUE)

-- | Return 'True' if the Windows @BOOL@ is true (i.e. non-zero).
isTRUE :: BOOL -> Bool
isTRUE (BOOL n) = n /= 0

-- | Return 'True' if the Windows @BOOL@ is false (i.e. equals zero).
isFALSE :: BOOL -> Bool
isFALSE (BOOL n) = n == 0

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

-- | Set the Offset\/OffsetHigh members of an @OVERLAPPED@ structure.
pokeOffset :: LPOVERLAPPED -> Word64 -> IO ()
pokeOffset p offset = do
    (#poke OVERLAPPED, Offset)     p (offset .>>.  0 :: DWORD)
    (#poke OVERLAPPED, OffsetHigh) p (offset .>>. 32 :: DWORD)

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
        if err == eERROR_PROC_NOT_FOUND
          then return Nothing
          else throwErrCode "getProcAddress" err
      else return $ Just ptr

-- | The module handle for @\"kernel32\.dll\"@
kernel32 :: HMODULE
kernel32 = U.unsafePerformIO $ getModuleHandle "kernel32.dll"
{-# NOINLINE kernel32 #-}

iNFINITE :: DWORD
iNFINITE = #const INFINITE

iNVALID_HANDLE_VALUE :: HANDLE
iNVALID_HANDLE_VALUE = wordPtrToPtr (-1)

class IsHANDLE a where
    fromHANDLE :: HANDLE -> a
    toHANDLE   :: a -> HANDLE

class IsLPOVERLAPPED a where
    fromLPOVERLAPPED :: LPOVERLAPPED -> a
    toLPOVERLAPPED   :: a -> LPOVERLAPPED

-- | A Windows error code, as returned by @GetLastError@.  Windows error codes
-- are not the same as @errno@!  @errno@ is a compatibility shim provided by
-- the Windows C runtime library, and has its own enumeration.
newtype ErrCode = ErrCode DWORD
    deriving (Eq, Ord, Enum, Num, Real, Integral, Typeable)

-- | Either the name of the error code (e.g. @\"WSAEINTR\"@), or something like
-- @\"error code 12345\"@.
instance Show ErrCode where
    show = eiName . getErrCodeInfo

-- | Get the last system error, and throw it as a 'WinError' exception.
throwGetLastError :: String -> IO a
throwGetLastError loc =
    getLastError >>= throwErrCode loc

-- | Convert a Windows error code to an exception, then throw it.
--
-- Note: "System.Win32.Types" has 'System.Win32.Types.failWith', but it expects
-- the error code we are throwing to be the most recent error in the current
-- thread.  'throwErrCode' does not make this assumption.
throwErrCode :: String -> ErrCode -> IO a
throwErrCode loc code =
    errCodeToIOError loc code >>= throwIO

-- | Convert a Windows error code to an exception.
errCodeToIOError :: String -> ErrCode -> IO IOError
errCodeToIOError loc code = do
    let info = getErrCodeInfo code
        name = eiName info
        Errno errno = eiErrno info
    msg <- getErrorMessage code >>= \m ->
           case m of
               Nothing -> return name
               Just s  -> return $ s ++ " (" ++ name ++ ")"
    -- TODO: Include Windows error code in the IOError.  Currently, the IOError
    -- record doesn't have a slot for it, only for errno.  We can't simply
    -- put a Windows error code in its place because Windows error codes and
    -- errno codes are incompatible, and some existing applications extract
    -- ioe_errno and expect it to be an errno value.
    return IOError
        { ioe_handle      = Nothing
        , ioe_type        = eiErrorType info
        , ioe_location    = loc
        , ioe_description = msg
        , ioe_errno       = Just errno
        , ioe_filename    = Nothing
        }

-- | Get a string describing a Windows error code.  This uses the
-- @FormatMessage@ system call.
getErrorMessage :: ErrCode -> IO (Maybe String)
getErrorMessage code =
  -- Mask exceptions to avoid leaking string allocated by FormatMessageW.
  mask_ $ do
    -- Original code from System.Win32.Types.failWith
    c_msg <- c_getErrorMessage code
    if c_msg == nullPtr
      then return Nothing
      else do msg <- peekCWString c_msg
              -- We ignore failure of freeing c_msg, given we're already failing
              _ <- localFree c_msg
              let msg' = reverse $ dropWhile isSpace $ reverse msg -- drop trailing \n
              return $ Just msg'

-- | Get the last system error produced in the current thread.
foreign import WINDOWS_CCONV unsafe "windows.h GetLastError"
    getLastError :: IO ErrCode

foreign import ccall unsafe "iocp_getErrorMessage" -- in iocp.c
    c_getErrorMessage :: ErrCode -> IO LPWSTR

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    localFree :: Ptr a -> IO (Ptr a)

failIf :: (a -> Bool) -> String -> IO a -> IO a
failIf p wh act = do
    v <- act
    if p v then throwGetLastError wh else return v

failIf_ :: (a -> Bool) -> String -> IO a -> IO ()
failIf_ p wh act = do
    v <- act
    if p v then throwGetLastError wh else return ()

------------------------------------------------------------------------
-- Error code constants

eERROR_PROC_NOT_FOUND :: ErrCode
eERROR_PROC_NOT_FOUND = #const ERROR_PROC_NOT_FOUND

eWAIT_TIMEOUT :: ErrCode
eWAIT_TIMEOUT = #const WAIT_TIMEOUT

-- | Same as @WSA_IO_PENDING@
eERROR_IO_PENDING :: ErrCode
eERROR_IO_PENDING = #const ERROR_IO_PENDING

eERROR_NOT_FOUND :: ErrCode
eERROR_NOT_FOUND = #const ERROR_NOT_FOUND

eWSAEINVAL :: ErrCode
eWSAEINVAL = #const WSAEINVAL

eWSANOTINITIALISED :: ErrCode
eWSANOTINITIALISED = #const WSANOTINITIALISED

------------------------------------------------------------------------
-- Error code table

data ErrCodeInfo = ErrCodeInfo
    { eiName      :: !String
    , eiErrorType :: !IOErrorType
    , eiErrno     :: !Errno
    }

-- | Look up information about a Windows error code.
getErrCodeInfo :: ErrCode -> ErrCodeInfo
getErrCodeInfo code@(ErrCode n) =
    case getErrCodeInfoMaybe code of
        Just info -> info
        Nothing   -> ErrCodeInfo
                     { eiName      = "error code " ++ show n
                     , eiErrorType = OtherError
                     , eiErrno     = Errno (#const EOTHER)
                     }

getErrCodeInfoMaybe :: ErrCode -> Maybe ErrCodeInfo
getErrCodeInfoMaybe (ErrCode n) = case n of
    -- Error codes are added to this table as they are encountered.
    -- See http://msdn.microsoft.com/en-us/library/windows/desktop/ms681381(v=vs.85).aspx
    --
    -- The errno values were picked from "errno Constants"
    -- (http://msdn.microsoft.com/en-us/library/5814770t.aspx) and are somewhat
    -- arbitrary.  The IOErrorType values are based on the mapping from errno
    -- to IOErrorType in Foreign.C.Error.errnoToIOError.  We don't let
    -- errnoToIOError do the conversion because:
    --
    --  * MinGW doesn't define several of these codes (we define some in
    --    errno-posix.h ourselves).  This means, for example, that 'eTIMEDOUT'
    --    will be converted to OtherError instead of TimeExpired, since the
    --    base package detected that ETIMEDOUT was undefined during
    --    configuration.
    --
    --  * We can decide what IOErrorType we want for each Windows error code,
    --    rather than funneling every error code through the errno bottleneck
    --    (though errno values tend to be more precise than IOErrorType).
    --
    -- TODO: merge the error codes from base/cbits/Win32Utils.c into this table.
    (#const ERROR_SUCCESS)            {- 0 -}     -> f "ERROR_SUCCESS"            OtherError           0
    (#const ERROR_INVALID_PARAMETER)  {- 87 -}    -> f "ERROR_INVALID_PARAMETER"  InvalidArgument      (#const EINVAL)
    (#const ERROR_SEM_TIMEOUT)        {- 121 -}   -> f "ERROR_SEM_TIMEOUT"        TimeExpired          (#const ETIMEDOUT)
    (#const ERROR_MOD_NOT_FOUND)      {- 126 -}   -> f "ERROR_MOD_NOT_FOUND"      NoSuchThing          (#const ENOENT)
    (#const ERROR_PROC_NOT_FOUND)     {- 127 -}   -> f "ERROR_PROC_NOT_FOUND"     NoSuchThing          (#const ENOENT)
    (#const WAIT_TIMEOUT)             {- 258 -}   -> f "WAIT_TIMEOUT"             TimeExpired          (#const ETIMEDOUT)
    (#const ERROR_OPERATION_ABORTED)  {- 995 -}   -> f "ERROR_OPERATION_ABORTED"  Interrupted          (#const EINTR)
    (#const ERROR_IO_PENDING)         {- 997 -}   -> f "ERROR_IO_PENDING"         AlreadyExists        (#const EINPROGRESS)
    (#const ERROR_NOT_FOUND)          {- 1168 -}  -> f "ERROR_NOT_FOUND"          NoSuchThing          (#const ENOENT)
    (#const ERROR_INVALID_NETNAME)    {- 1214 -}  -> f "ERROR_INVALID_NETNAME"    InvalidArgument      (#const EINVAL)
    (#const ERROR_CONNECTION_REFUSED) {- 1225 -}  -> f "ERROR_CONNECTION_REFUSED" NoSuchThing          (#const ECONNREFUSED)
    (#const WSAEINTR)                 {- 10004 -} -> f "WSAEINTR"                 Interrupted          (#const EINTR)
    (#const WSAEFAULT)                {- 10014 -} -> f "WSAEFAULT"                OtherError           (#const EFAULT)
    (#const WSAEINVAL)                {- 10022 -} -> f "WSAEINVAL"                InvalidArgument      (#const EINVAL)
    (#const WSAENOTSOCK)              {- 10038 -} -> f "WSAENOTSOCK"              InvalidArgument      (#const ENOTSOCK)
    (#const WSAEAFNOSUPPORT)          {- 10047 -} -> f "WSAEAFNOSUPPORT"          UnsupportedOperation (#const EAFNOSUPPORT)
    (#const WSAEADDRINUSE)            {- 10048 -} -> f "WSAEADDRINUSE"            ResourceBusy         (#const EADDRINUSE)
    (#const WSAEADDRNOTAVAIL)         {- 10049 -} -> f "WSAEADDRNOTAVAIL"         UnsupportedOperation (#const EADDRNOTAVAIL)
    (#const WSAECONNRESET)            {- 10054 -} -> f "WSAECONNRESET"            ResourceVanished     (#const ECONNRESET)
    (#const WSAEISCONN)               {- 10056 -} -> f "WSAEISCONN"               AlreadyExists        (#const EISCONN)
    (#const WSAENOTCONN)              {- 10057 -} -> f "WSAENOTCONN"              InvalidArgument      (#const ENOTCONN)
    (#const WSAETIMEDOUT)             {- 10060 -} -> f "WSAETIMEDOUT"             TimeExpired          (#const ETIMEDOUT)
    (#const WSANOTINITIALISED)        {- 10093 -} -> f "WSANOTINITIALISED"        InvalidArgument      (#const EINVAL)

    _ -> Nothing
  where
    f name etype errno = Just $!
        ErrCodeInfo
        { eiName      = name
        , eiErrorType = etype
        , eiErrno     = Errno errno
        }
