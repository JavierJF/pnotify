{-# Language ForeignFunctionInterface #-}

module System.PNotify where

import Control.Monad.ST
import Control.Concurrent
import Data.Bits ((.|.))
import Foreign
import Foreign.C.Types
import Foreign.C.String hiding (peekCString)
import Foreign.Marshal.Alloc
import Foreign.Ptr
import GHC.Foreign (peekCString)
import GHC.IO.Encoding (getFileSystemEncoding)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.FD (mkFD)
import GHC.IO.Handle.FD (mkHandleFromFD)
import GHC.IO.Encoding.Types (TextEncoding)
import System.IO (Handle,hGetBufNonBlocking,IOMode(ReadMode))
import System.Posix.IO (fdToHandle)
import System.Posix.Types

#include <sys/inotify.h>
#include <limits.h>

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

data INotifyEvent = INotifyEvent { wd       :: CInt
                                 , mask     :: Mask
                                 , cookie   :: CUInt
                                 , length   :: CUInt
                                 , name     :: Maybe String
                                 } deriving (Eq, Show)

data INBuffer  = INBuffer { rawPtr  :: Ptr INotifyEvent
                          , size    :: Int }

data INotifyHandle = INotifyHandle { buffer :: INBuffer }

foreign import ccall "inotify_init"
    iNotifyInit :: IO CInt

foreign import ccall "inotify_init1"
    iNotifyInit1 :: CInt -> IO CInt

foreign import ccall "inotify_add_watch"
    _iNotifyAddWatch :: CInt -> CString -> CUInt -> IO CInt

iEventSize :: Int
iEventSize = ((#size struct inotify_event) + #const NAME_MAX) :: Int

iNotifyAddWatch :: CInt -> CString -> CUInt -> IO Handle
iNotifyAddWatch fd path mask = do
    let desc = showString "<inotify handle, fd=" . shows fd $ ">"
    wd <- _iNotifyAddWatch fd path mask
    (fd,fdType) <- mkFD (fromIntegral fd) ReadMode (Just (Stream,0,0)) False{-is_socket-} False{-is_socket-}
    mkHandleFromFD fd fdType desc ReadMode False{-is_socket-} Nothing

allocaINBuffer :: Int -> (INBuffer -> IO a) -> IO a
allocaINBuffer s f = allocaBytes s (\ptr -> f $ INBuffer ptr s)

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

readEvents :: Handle -> INBuffer -> IO [INotifyEvent]
readEvents fd buf = do
                     let buffer  = rawPtr buf
                         bufSize = size buf
                     enc    <- getFileSystemEncoding
                     length <- hGetBufNonBlocking fd buffer bufSize
                     readEventsBuffer buffer length enc

readLoop :: Handle -> INBuffer -> IO ()
readLoop hd buf = do
                   events <- readEvents hd buf
                   mapM_ print events
                   threadDelay 1000000
                   readLoop hd buf

-- >>= fdToHandle

testFun :: INBuffer -> IO ()
testFun buf = do
               let mask = unMask (inCreate *|* inDelete *|* inModify *|* inMove)
               chd  <- iNotifyInit
               path <- newCString "."
               hd   <- iNotifyAddWatch chd path mask
               readLoop hd buf

test :: IO ()
test = let size = 50*iEventSize
       in allocaINBuffer size testFun
