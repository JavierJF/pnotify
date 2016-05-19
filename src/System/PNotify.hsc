-----------------------------------------------------------------------------
-- |
-- Module      :  System.PNotify
-- Copyright   :  (c) Javier Jaramago Fernández 2016
-- License     :  GNU GPL3
-- Maintainer  :  jaramago.fernandez.javier@gmail.com
-- Stability   :  experimental
-- Portability :  hc portable, linux only
--
-- A Haskell binding to INotify.
--
-- Use 'initINotify' to get a 'INotify', then use 'addWatch' to
-- add a watch on a file or directory. Select which events you're interested
-- in with 'EventVariety', which corresponds to the 'Event' events.
--
-- Use 'removeWatch' once you don't want to watch a file any more.
--
-----------------------------------------------------------------------------

{-# Language ForeignFunctionInterface #-}

module System.PNotify where

import Data.Bits ((.|.))
import Foreign (ForeignPtr, newForeignPtr, peekByteOff)
import Foreign.C.Types
import Foreign.C.String hiding (peekCString)
import Foreign.Marshal.Alloc (finalizerFree,allocaBytes)
import Foreign.Ptr (Ptr, FunPtr, plusPtr)
import Foreign.ForeignPtr (FinalizerPtr, finalizeForeignPtr)
import GHC.Foreign (peekCString)
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.FD (mkFD)
import GHC.IO.Handle.FD (mkHandleFromFD)
import GHC.IO.Encoding.Types (TextEncoding)
import System.IO (Handle,hGetBufNonBlocking,IOMode(ReadMode))
import System.Posix.IO (fdToHandle)

#include <sys/inotify.h>
#include <limits.h>
#include <unistd.h>

newtype Mask = Mask { unMask :: CUInt }
    deriving (Eq,Show)

(*|*) :: Mask -> Mask -> Mask
(*|*) m1 m2 = Mask $ unMask m1 .|. unMask m2

#{enum Mask, Mask
  , inAccess        = IN_ACCESS
  , inModify        = IN_MODIFY
  , inAttrib        = IN_ATTRIB
  , inCloseWrite    = IN_CLOSE_WRITE
  , inCloseNoWrite  = IN_CLOSE_NOWRITE
  , inOpen          = IN_OPEN
  , inMovedFrom     = IN_MOVED_FROM
  , inMovedTo       = IN_MOVED_TO
  , inCreate        = IN_CREATE
  , inDelete        = IN_DELETE
  , inDeleteSelf    = IN_DELETE_SELF
  , inMoveSelf      = IN_MOVE_SELF
 }

-- CONVENIENCE MASKS
#{enum Mask, Mask
  , inMove          = IN_MOVE
  , inClose         = IN_CLOSE
  , inAllEvents     = IN_ALL_EVENTS
 }

-- SPECIAL MASKS
#{enum Mask, Mask
  , inDontFollow    = IN_DONT_FOLLOW
  , inExclUnlink    = IN_EXCL_UNLINK
  , inMaskAdd       = IN_MASK_ADD
  , inOneShot       = IN_ONESHOT
  , inOnlyDir       = IN_ONLYDIR
  , inIsDir         = IN_ISDIR
  , inIgnored       = IN_IGNORED
  , inQOverflow     = IN_Q_OVERFLOW
  , inUnmount       = IN_UNMOUNT
 }

data INotifyEvent = INotifyEvent { iWD       :: CInt
                                 , iMask     :: Mask
                                 , iCookie   :: CUInt
                                 , iLen      :: CUInt
                                 , iName     :: Maybe String
                                 } deriving (Eq, Show)

data INBuffer  = INBuffer { iNBRaw  :: Ptr INotifyEvent
                          , iNBSize :: Int }

data INotifyHandle = INotifyHandle { iNHRaw :: ForeignPtr INBuffer
                                   , iNHBuf :: INBuffer }

data INFD = INFD { fD :: ForeignPtr CInt }

-- | '_iNotifyInit' initializes a new inotify instance and returns a file
-- descriptor associated with a new inotify event queue
foreign import ccall "inotify_init"
    _iNotifyInit :: IO CInt

-- | 'iNotifyInit1'
foreign import ccall "inotify_init1"
    _iNotifyInit1 :: CInt -> IO CInt

-- | '_iNotifyAddWatch'
foreign import ccall "inotify_add_watch"
    _iNotifyAddWatch :: CInt -> CString -> CUInt -> IO CInt

-- | '_iNotifyAddWatch'
foreign import ccall "inotify_rm_watch"
    _iNotifyRmWatch :: CInt -> CInt -> IO CInt

-- | '_store_fd'
foreign import ccall "store_fd"
    _store_fd :: CInt -> IO (Ptr CInt)

-- | '_close_fd'
foreign import ccall "&close_fd"
    _close_fd :: FunPtr (Ptr CInt -> IO ())

iEventSize :: Int
iEventSize = ((#size struct inotify_event) + #const NAME_MAX) :: Int

iNotifyInit :: IO INFD
iNotifyInit = do
    fd <- _iNotifyInit
    pfd <- _store_fd fd
    fp <- newForeignPtr _close_fd pfd
    return $ INFD fp

iNotifyClose :: INFD -> IO ()
iNotifyClose = finalizeForeignPtr . fD

data IWatchHandle = IWatchHandle { iWFD :: CInt
                                 , iRWH :: CInt
                                 , iWH  :: Handle
                                 , pBuf :: ForeignPtr CChar }

iNotifyAddWatch :: CInt -> String -> CUInt -> IO IWatchHandle
iNotifyAddWatch fd path mask = do
    cpath <- newCString path
    pBuf  <- newForeignPtr finalizerFree cpath
    wd    <- _iNotifyAddWatch fd cpath mask

    let fSocket = False{-is_socket-}
        mStream = Just (Stream,0,0)
    (fD,fdType) <- mkFD (fromIntegral fd) ReadMode mStream fSocket fSocket
    let desc = showString "<inotify handle, fd=" . shows fd $ ">"
    hd <- mkHandleFromFD fD fdType desc ReadMode False{-is_socket-} Nothing

    return $ IWatchHandle fd wd hd pBuf

iNotifyRmWatch :: IWatchHandle -> IO CInt
iNotifyRmWatch iwh = do
     finalizeForeignPtr (pBuf iwh)
     _iNotifyRmWatch (iWFD iwh) (iRWH iwh)

allocaINBuffer :: Int -> (INBuffer -> IO a) -> IO a
allocaINBuffer s f = let size = (s*iEventSize)
                     in allocaBytes size (\ptr -> f $ INBuffer ptr size)

readEventsBuffer :: Ptr INotifyEvent -> Int -> TextEncoding -> IO [INotifyEvent]
readEventsBuffer _ length _ | length <= 0 = return []
readEventsBuffer ptr length enc = do
    let nameP = (#ptr struct inotify_event, name)  ptr
    wd     <- (#peek struct inotify_event, wd)     ptr :: IO CInt
    mask   <- (#peek struct inotify_event, mask)   ptr :: IO CUInt
    cookie <- (#peek struct inotify_event, cookie) ptr :: IO CUInt
    len    <- (#peek struct inotify_event, len)    ptr :: IO CUInt
    nameM  <- if len == 0
                then return Nothing
                else fmap Just $ peekCString enc nameP
    let eventSize = fromIntegral $ (#size struct inotify_event) + len
        event = INotifyEvent wd (Mask mask) cookie len nameM
    events <- readEventsBuffer (ptr `plusPtr` eventSize) (length - eventSize) enc
    return (event:events)

readEvents :: IWatchHandle -> INBuffer -> IO [INotifyEvent]
readEvents iwh buf = do
        let buffer  = iNBRaw buf
            bufSize = iNBSize buf
        enc    <- getFileSystemEncoding
        length <- hGetBufNonBlocking (iWH iwh) buffer bufSize
        readEventsBuffer buffer length enc
