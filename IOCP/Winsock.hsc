{-# LANGUAGE CPP #-}
module IOCP.Winsock (
    Socket,
    bind,
) where

import IOCP.Windows
import IOCP.Winsock.Types

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

#include <winsock2.h>

foreign import WINDOWS_CCONV "winsock2.h bind"
    c_bind :: Socket -> Ptr SockAddr -> CInt -> IO CInt

bind :: Socket -> SockAddr -> IO ()
bind sock addr =
    failIf_ (/= 0) "bind" $
    withSockAddr addr $ \ptr len ->
    c_bind sock ptr len
