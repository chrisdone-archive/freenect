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
       ,setLogLevel
       ,Context
       ,FreenectException(..)
       ,Subdevice(..)
       ,LogLevel(..))
       where

import Freenect.FFI

import Data.Bits
import Data.List
import Control.Exception
import Data.Typeable
import Foreign
import Foreign.C
import Data.IORef

-- | An acquireable resource. This abstracts the notion of C-level
--   pointers that may or may not refer to something in memory. Avoids
--   segmentation faults and other nasties. Nobody wants segmentation
--   faults in their Haskell code.
data Resource a = Initialized a | Uninitialized a

-- | A Freenect context.
newtype Context = CPtr (IORef (Resource (Ptr ContextStruct)))

-- | A Freenect device.
newtype Device = DPtr (IORef (Resource (Ptr DeviceStruct)))

-- | Freenect exception type.
data FreenectException
  = InitFail           -- ^ There was a problem initializing.
  | AlreadyInitializedContext -- ^ Trying to initialize a context that
                              -- was already initialized.
  | AlreadyOpenedDevice -- ^ Trying to open a device that was
                             -- already opened.
  | UseOfUninitializedContext -- ^ Attempt to use an uninitialized
                              --   context.
  | UseOfUninitializedDevice  -- ^ Attempt to use an uninitialized
                              --   device.
  | ProcessEvents      -- ^ Call to process events failed.
  | OpenDeviceFailed Integer -- ^ Opening a device failed.
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
        init_freenect_context ptr

-- | Create a new Freenect context. Must be initialized before use.
newContext :: IO Context
newContext = new_freenect_context >>= fmap CPtr . newIORef . Uninitialized

-- | Shutdown a Freenect context.
shutdown :: Context -> IO ()
shutdown = withC (succeed InitFail (return ()) . freenect_shutdown)
  
-- | Count the number of devices on a Freenect context.
countDevices :: Context -> IO Integer
countDevices =
  withC $ \ptr ->
    fmap fromIntegral (freenect_num_devices ptr)

-- | Do something with an initialized context, and free the context at
--   the end of the comutation, or on exception.
withContext :: (Context -> IO a) -> IO a
withContext f = bracket newContext shutdown (\c -> do initialize c; f c)

-- | Process events.
processEvents :: Context -> IO ()
processEvents = withC (succeed ProcessEvents (return ()) . freenect_process_events)

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
  freenect_select_subdevices ptr (foldl1 (.|.) (map toDeviceId subdevices))

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
      succeed (OpenDeviceFailed index) (return ()) $
        open_freenect_device cptr dptr (fromIntegral index)

-- | With a context and a device, do something.
withCD :: Context -> Device -> (Ptr ContextStruct -> Ptr DeviceStruct -> IO a) -> IO a
withCD c d cons = (withC (\cptr -> withD (\dptr -> cons cptr dptr) d) c)

-- | Do something with a device pointer. Unexported.
withD :: (Ptr DeviceStruct -> IO a) -> Device -> IO a
withD cons (DPtr ptr) = do
  ptr <- readIORef ptr
  case ptr of
    Uninitialized{} -> throw UseOfUninitializedDevice
    Initialized ptr -> cons ptr

-- | Do something with a context pointer. Unexported.
withC :: (Ptr ContextStruct -> IO a) -> Context -> IO a
withC cons (CPtr ptr) = do
  ptr <- readIORef ptr
  case ptr of
    Uninitialized{} -> throw UseOfUninitializedContext
    Initialized ptr -> cons ptr

-- | Message logging levels.
data LogLevel
  = LogFatal    -- ^ Log for crashing/non-recoverable errors
  | LogError    -- ^ Log for major errors
  | LogWarning  -- ^ Log for warning messages
  | LogNotice   -- ^ Log for important messages
  | LogInfo     -- ^ Log for normal messages
  | LogDebug    -- ^ Log for useful development messages
  | LogSpew     -- ^ Log for slightly less useful messages
  | LogFlood    -- ^ Log EVERYTHING. May slow performance.
  deriving (Show,Eq,Enum)

-- | Set the logging level for the specified context.
setLogLevel :: LogLevel -> Context -> IO ()
setLogLevel level = withC $ \ptr -> do
  freenect_set_log_level ptr (fromIntegral (fromEnum level))
