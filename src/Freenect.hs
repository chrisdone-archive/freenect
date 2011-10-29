{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Interface to the Kinect device.

module Freenect
       (initialize
       ,newContext
       ,shutdown
       ,countDevices
       ,withContext
       ,processEvents
       ,selectSubdevices
       ,newDevice
       ,openDevice
       ,closeDevice
       ,withDevice
       ,setLogLevel
       ,setDepthCallback
       ,startDepth
       ,setTiltDegrees
       ,setDepthMode
       ,Context
       ,FreenectException(..)
       ,Subdevice(..)
       ,LogLevel(..)
       ,Resolution(..)
       ,DepthFormat(..))
       where

import Freenect.FFI

import Control.Exception (bracket,throw,Exception(..))
import Data.Bits
import Data.IORef
import Data.List
import Data.Typeable
import Foreign
import Foreign.C
import Data.Vector.Storable (Vector,unsafeFromForeignPtr)

-- | An acquireable resource. This abstracts the notion of C-level
--   pointers that may or may not refer to something in memory. Avoids
--   segmentation faults and other nasties. Nobody wants segmentation
--   faults in their Haskell code.
data Resource a = Initialized a | Uninitialized a
  deriving Show

-- | A Freenect context.
newtype Context = CPtr (IORef (Resource (Ptr (Ptr ContextStruct))))

-- | A Freenect device.
newtype Device = DPtr (IORef (Resource (Ptr (Ptr DeviceStruct))))

-- | Freenect exception type.
data FreenectException
  = InitFail           -- ^ There was a problem initializing.
  | ShutdownFail       -- ^ There was a problem shutting down.
  | CloseDeviceFail       -- ^ There was a problem closing the device.
  | AlreadyInitializedContext -- ^ Trying to initialize a context that
                              -- was already initialized.
  | AlreadyOpenedDevice -- ^ Trying to open a device that was
                             -- already opened.
  | UseOfUninitializedContext -- ^ Attempt to use an uninitialized
                              --   context.
  | UseOfUninitializedDevice  -- ^ Attempt to use an uninitialized
                              --   device.
  | ProcessEvents CInt       -- ^ Call to process events failed.
  | OpenDeviceFailed Integer -- ^ Opening a device failed.
  | StartDepthProblem        -- ^ Problem starting the depth stream.
  | UnableToSetTilt          -- ^ Unable to set the tilt.
  | SetDepthMode             -- ^ Unable to set the depth mode.
  | DepthModeNotSet          -- ^ You didn't set the depth mode.
    deriving (Show,Typeable)
instance Exception FreenectException

-- | Initialize a Freenect context. Throws exception if already
--   initialized.
initialize :: Context -> IO ()
initialize (CPtr ptrRef) = do
  ptr <- readIORef ptrRef
  case ptr of
    Initialized{} -> throw AlreadyInitializedContext
    Uninitialized ptr -> do
      succeed InitFail (writeIORef ptrRef (Initialized ptr)) $
        freenect_init ptr 0

-- | Create a new Freenect context. Must be initialized before use.
newContext :: IO Context
newContext = new_freenect_context >>= fmap CPtr . newIORef . Uninitialized

-- | Shutdown a Freenect context.
shutdown :: Context -> IO ()
shutdown cptr@(CPtr ptrRef) = flip withC cptr $ \ptr ->
  succeed ShutdownFail
          (writeIORef ptrRef (Uninitialized ptr))
          (peek ptr >>= freenect_shutdown)
  
-- | Count the number of devices on a Freenect context.
countDevices :: Context -> IO Integer
countDevices =
  withC $ \ptr ->
    fmap fromIntegral (peek ptr >>= freenect_num_devices)

-- | Do something with an initialized context, and free the context at
--   the end of the comutation, or on exception.
withContext :: (Context -> IO a) -> IO a
withContext f = bracket newContext shutdown (\c -> do initialize c; f c)

-- | Process events.
processEvents :: Context -> IO ()
processEvents = withC $ \cptr -> do
  cptr <- peek cptr
  result <- freenect_process_events cptr
  case result of
    -- LIBUSB_ERROR_INTERRUPTED 	
    -- System call interrupted (perhaps due to signal).
    -- I think the GHC runtime sends interrupts sometimes, or
    -- otherwise signals are coming from somewhere but are they appear
    -- to be ignorable.
    -10 -> return ()
    _ | result < 0 -> throw (ProcessEvents result)
      | otherwise  -> return ()

-- | Run a computation for which the CInt result is zero (in C this is
--   success), and thrown an exception if the result is non-zero.
succeed :: Exception e => e -> IO () -> IO CInt -> IO ()
succeed e ok m = do
  result <- m
  if result == 0 
     then ok
     else throw e

-- | A sub-device (motor, camera and audio), if supported on the
--   platform.
data Subdevice = Motor | Camera | Auto
  deriving (Show,Eq)

-- | Set which subdevices any subsequent calls to openDevice should
--   open.  This will not affect devices which have already been
--   opened.  The default behavior, should you choose not to call this
--   function at all, is to open all supported subdevices - motor,
--   cameras, and audio, if supported on the platform.
selectSubdevices :: Context -> [Subdevice] -> IO ()
selectSubdevices c (nub -> subdevices) = flip withC c $ \ptr -> do
  ptr <- peek ptr
  freenect_select_subdevices  ptr (foldl1 (.|.) (map toDeviceId subdevices))

  where toDeviceId Motor = 1
        toDeviceId Camera = 2
        toDeviceId Auto = 4

-- | Create a new device.
newDevice :: IO Device
newDevice = new_freenect_device >>= fmap DPtr . newIORef . Uninitialized

-- | Open a Kinect device.
openDevice :: Context -> Device -> Integer -> IO ()
openDevice c (DPtr devptr) index = flip withC c $ \cptr -> do
  dptr <- readIORef devptr
  case dptr of
    Initialized{} -> throw AlreadyOpenedDevice
    Uninitialized dptr -> do
      succeed (OpenDeviceFailed index) (writeIORef devptr (Initialized dptr)) $ do
        cptr <- peek cptr
        freenect_open_device cptr dptr (fromIntegral index)

-- | Close a device.
closeDevice :: Device -> IO ()
closeDevice dptr@(DPtr ptrRef) = do
  flip withD dptr $ \ptr -> do
    succeed CloseDeviceFail
            (writeIORef ptrRef (Uninitialized ptr))
            (peek ptr >>= freenect_close_device)

-- | Do something with an initialized context, and free the context at
--   the end of the comutation, or on exception.
withDevice :: Context -> Integer -> (Device -> IO a) -> IO a
withDevice ctx i f = bracket newDevice closeDevice (\d -> do openDevice ctx d i; f d)

-- | Do something with a device pointer. Unexported.
withD :: (Ptr (Ptr DeviceStruct) -> IO a) -> Device -> IO a
withD cons (DPtr ptr) = do
  ptr <- readIORef ptr
  case ptr of
    Uninitialized{} -> throw UseOfUninitializedDevice
    Initialized ptr -> cons ptr

-- | Do something with a context pointer. Unexported.
withC :: (Ptr (Ptr ContextStruct) -> IO a) -> Context -> IO a
withC cons (CPtr ptr) = do
  ptr <- readIORef ptr
  case ptr of
    Uninitialized{} -> throw UseOfUninitializedContext
    Initialized ptr -> cons ptr

-- | Message logging levels.
data LogLevel
  = LogFatal    -- ^ Crashing/non-recoverable errors
  | LogError    -- ^ Major errors
  | LogWarning  -- ^ Warning messages
  | LogNotice   -- ^ Important messages
  | LogInfo     -- ^ Normal messages
  | LogDebug    -- ^ Useful development messages
  | LogSpew     -- ^ Slightly less useful messages
  | LogFlood    -- ^ EVERYTHING. May slow performance.
  deriving (Show,Eq,Enum)

-- | Set the logging level for the specified context.
setLogLevel :: LogLevel -> Context -> IO ()
setLogLevel level = withC $ \ptr -> do
  ptr <- peek ptr
  freenect_set_log_level ptr (fromIntegral (fromEnum level))

-- | Set callback for depth information received event.
setDepthCallback :: Device -> (Vector Word16 -> Word32 -> IO ()) -> IO ()
setDepthCallback d callback = flip withD d $ \dptr -> do
  dptr <- peek dptr
  resolution <- get_freenect_depth_resolution dptr
  let !size = resolutionToSize (toEnum (fromIntegral resolution))
  callbackPtr <- wrapDepthCallback $ \_ payloadptr timestamp -> do
    fptr <- newForeignPtr_ payloadptr
    let !vector = unsafeFromForeignPtr fptr 0 size
    callback vector timestamp
  freenect_set_depth_callback dptr callbackPtr

-- | Resolution to size.
resolutionToSize :: Resolution -> Int
resolutionToSize Low    = 320  * 240
resolutionToSize Medium = 640  * 480
resolutionToSize High   = 1280 * 1024

-- | Start the depth information stream for a device.
startDepth :: Device -> IO ()
startDepth = withD $ \ptr -> succeed StartDepthProblem (return ()) $ do
  ptr <- peek ptr
  freenect_start_depth ptr

-- | Start the depth information stream for a device.
setTiltDegrees :: Double -> Device -> IO ()
setTiltDegrees angle = withD $ \ptr -> succeed UnableToSetTilt (return ()) $ do
  ptr <- peek ptr
  freenect_set_tilt_degs ptr (realToFrac angle)

data Resolution = Low | Medium | High
  deriving (Enum,Show,Eq,Ord)

data DepthFormat
  = ElevenBit
  | TenBit
  | ElevenBitPacked
  | TenBitPacked
  deriving (Enum,Show,Eq)

-- | Sets the current depth mode for the specified device.  The mode
--    cannot be changed while streaming is active.
setDepthMode :: Device -> Resolution -> DepthFormat -> IO ()
setDepthMode d res fmt = flip withD d $ \dptr -> do
  dptr <- peek dptr
  frameMode <- find_depth_mode_freenect (fromIntegral (fromEnum res))
                                        (fromIntegral (fromEnum fmt))
  succeed SetDepthMode (return ()) $
    set_freenect_depth_mode dptr frameMode
