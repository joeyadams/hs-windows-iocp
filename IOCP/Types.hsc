{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Types (
    CompletionPort(..),
    Handle(..),
    Overlapped(..),
    OverlappedRec(..),
    peekOverlappedStablePtr,
    Completion(..),
) where

import IOCP.Windows
import Foreign

#include "iocp.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

newtype CompletionPort a = CompletionPort HANDLE
    deriving (Eq, Show, Storable)

newtype Handle a = Handle HANDLE
    deriving (Eq, Show, Storable)

newtype Overlapped a = Overlapped (Ptr (OverlappedRec a))
    deriving (Eq, Show, Storable)

instance IsLPOVERLAPPED (Overlapped a) where
    fromLPOVERLAPPED ptr = Overlapped (castPtr ptr)
    toLPOVERLAPPED (Overlapped ptr) = castPtr ptr

data OverlappedRec a = OverlappedRec !OVERLAPPED !(StablePtr a)
    deriving Eq

instance Storable (OverlappedRec a) where
    sizeOf    _ = #{size      OverlappedRec}
    alignment _ = #{alignment OverlappedRec}
    peek p = do
        ol   <- (#peek OverlappedRec, ol)   p
        sptr <- (#peek OverlappedRec, sptr) p
        return $! OverlappedRec ol sptr
    poke p (OverlappedRec ol sptr) = do
        (#poke OverlappedRec, ol)   p ol
        (#poke OverlappedRec, sptr) p sptr

instance Show (OverlappedRec a) where
    showsPrec p (OverlappedRec ol sptr) =
      showParen (p > 10) $
        showString "OverlappedRec " .
        showsPrec 11 ol . showChar ' ' .
        showsPrec 11 (castStablePtrToPtr sptr)

peekOverlappedStablePtr :: Overlapped a -> IO (StablePtr a)
peekOverlappedStablePtr (Overlapped p) = (#peek OverlappedRec, sptr) p

data Completion = Completion
    { cNumBytes :: !DWORD   -- ^ Number of bytes transferred during I/O operation
    , cError    :: !ErrCode -- ^ Zero if the I/O was successful
    }
    deriving Show
