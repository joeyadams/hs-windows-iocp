{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
module IOCP.Winsock.Types (
    SOCKET(..),
    iNVALID_SOCKET,

    -- * Miscellaneous
    LPWSABUF,
    LPWSAOVERLAPPED,
    LPWSAOVERLAPPED_COMPLETION_ROUTINE,
    WSABUF(..),
    WSAOVERLAPPED_COMPLETION_ROUTINE,
) where

import IOCP.Windows

import Data.Typeable (Typeable)
import Data.Word
import Foreign
import Foreign.C.Types

#include <winsock2.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype SOCKET = SOCKET (#type SOCKET)
    deriving (Eq, Ord, Show, Storable, Typeable)

iNVALID_SOCKET :: SOCKET
iNVALID_SOCKET = SOCKET #const INVALID_SOCKET

------------------------------------------------------------------------
-- Miscellaneous

type LPWSAOVERLAPPED = LPOVERLAPPED

type LPWSAOVERLAPPED_COMPLETION_ROUTINE =
    FunPtr WSAOVERLAPPED_COMPLETION_ROUTINE

type WSAOVERLAPPED_COMPLETION_ROUTINE
    = DWORD           -- ^ dwError
   -> DWORD           -- ^ cbTransferred
   -> LPWSAOVERLAPPED -- ^ lpOverlapped
   -> DWORD           -- ^ dwFlags
   -> IO ()

data WSABUF = WSABUF
    { wbLen :: !CULong
    , wbBuf :: !(Ptr CChar)
    }
  deriving (Eq, Show)

type LPWSABUF = Ptr WSABUF

instance Storable WSABUF where
    sizeOf    _ = (#size      WSABUF)
    alignment _ = (#alignment WSABUF)
    peek p = do
        wbLen <- (#peek WSABUF, len) p
        wbBuf <- (#peek WSABUF, buf) p
        return $! WSABUF{..}
    poke p WSABUF{..} = do
        (#poke WSABUF, len) p wbLen
        (#poke WSABUF, buf) p wbBuf
