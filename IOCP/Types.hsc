{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IOCP.Types (
    CompletionPort(..),
    Handle(..),
    Overlapped(..),
    OverlappedRec(..),
    peekOverlappedStablePtr,
    getOverlappedAlive,
    setOverlappedAlive,
    Completion(..),
) where

import IOCP.Windows

import Control.Applicative
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

data OverlappedRec a = OverlappedRec
    { orOverlapped :: !OVERLAPPED
    , orContext    :: !(StablePtr a)
    , orAlive      :: !BOOL
      -- ^ This is set to @FALSE@ when the overlapped is freed, to check for
      --   completion delivery when we didn't expect it.
    }
    deriving Eq

instance Storable (OverlappedRec a) where
    sizeOf    _ = #{size      OverlappedRec}
    alignment _ = #{alignment OverlappedRec}
    peek p = do
        ol    <- (#peek OverlappedRec, ol)    p
        sptr  <- (#peek OverlappedRec, sptr)  p
        alive <- (#peek OverlappedRec, alive) p
        return $! OverlappedRec ol sptr alive
    poke p (OverlappedRec ol sptr alive) = do
        (#poke OverlappedRec, ol)    p ol
        (#poke OverlappedRec, sptr)  p sptr
        (#poke OverlappedRec, alive) p alive

instance Show (OverlappedRec a) where
    showsPrec p (OverlappedRec ol sptr alive) =
      showParen (p > 10) $
        showString "OverlappedRec " .
        showsPrec 11 ol . showChar ' ' .
        showsPrec 11 (castStablePtrToPtr sptr) . showChar ' ' .
        showsPrec 11 alive

peekOverlappedStablePtr :: Overlapped a -> IO (StablePtr a)
peekOverlappedStablePtr (Overlapped p) = (#peek OverlappedRec, sptr) p

getOverlappedAlive :: Overlapped a -> IO Bool
getOverlappedAlive (Overlapped p) =
    isTRUE <$> (#peek OverlappedRec, alive) p

setOverlappedAlive :: Overlapped a -> Bool -> IO ()
setOverlappedAlive (Overlapped p) b =
    (#poke OverlappedRec, alive) p (toBOOL b)

data Completion = Completion
    { cNumBytes :: !DWORD   -- ^ Number of bytes transferred during I/O operation
    , cError    :: !ErrCode -- ^ Zero if the I/O was successful
    }
    deriving Show
