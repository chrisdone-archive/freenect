{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Foreign functions, some helpers some from Freenect C lib.

module Freenect.FFI where

import Foreign.C
import Foreign

data ContextStruct
data DeviceStruct
data RawTiltState

--------------------------------------------------------------------------------
-- Freenect lib functions.

foreign import ccall
  "freenect.h freenect_init"
  freenect_init :: Ptr (Ptr ContextStruct) -> CInt -> IO CInt

foreign import ccall
  "freenect.h freenect_shutdown"
  freenect_shutdown :: Ptr ContextStruct -> IO CInt

foreign import ccall
  "freenect.h freenect_num_devices"
  freenect_num_devices :: Ptr ContextStruct -> IO CInt

foreign import ccall
  "freenect.h freenect_process_events"
  freenect_process_events :: Ptr ContextStruct -> IO CInt

foreign import ccall
  "freenect.h freenect_select_subdevices"
  freenect_select_subdevices :: Ptr ContextStruct -> CInt -> IO ()

foreign import ccall
  "freenect.h freenect_set_log_level"
  freenect_set_log_level :: Ptr ContextStruct -> CInt -> IO ()

foreign import ccall
  "freenect.h freenect_close_device"
  freenect_close_device :: Ptr DeviceStruct -> IO CInt

foreign import ccall
  "freenect.h freenect_open_device"
  freenect_open_device :: Ptr ContextStruct -> Ptr (Ptr DeviceStruct) -> CInt
                       -> IO CInt

type DepthCallback = Ptr DeviceStruct -> Ptr Word16 -> Word32 -> IO ()

foreign import ccall
  "freenect.h freenect_set_depth_callback"
  freenect_set_depth_callback
    :: Ptr DeviceStruct
    -> (FunPtr DepthCallback)
    -> IO ()

foreign import ccall "wrapper"  
  wrapDepthCallback :: DepthCallback -> IO (FunPtr DepthCallback)

type VideoCallback = Ptr DeviceStruct -> Ptr Word8 -> Word32 -> IO ()

foreign import ccall
  "freenect.h freenect_set_video_callback"
  freenect_set_video_callback
    :: Ptr DeviceStruct
    -> (FunPtr VideoCallback)
    -> IO ()

foreign import ccall "wrapper"  
  wrapVideoCallback :: VideoCallback -> IO (FunPtr VideoCallback)

foreign import ccall
  "freenect.h freenect_start_depth"
  freenect_start_depth :: Ptr DeviceStruct
                       -> IO CInt

foreign import ccall
  "freenect.h freenect_start_video"
  freenect_start_video :: Ptr DeviceStruct
                       -> IO CInt

foreign import ccall
   "freenect.h freenect_set_led"
   freenect_set_led :: Ptr DeviceStruct -> CInt -> IO CInt


foreign import ccall 
   "freenect.h freenect_set_flag"
   freenect_set_flag :: Ptr DeviceStruct -> CInt -> CInt -> IO CInt


foreign import ccall
  "freenect.h freenect_set_tilt_degs"
  freenect_set_tilt_degs :: Ptr DeviceStruct -> CDouble -> IO CInt

foreign import ccall 
   "freenect.h freenect_update_tilt_state"
   freenect_update_tilt_state :: Ptr DeviceStruct -> IO CInt

foreign import ccall
   "freenect.h freenect_get_tilt_state"
   freenect_get_tilt_state :: Ptr DeviceStruct -> IO (Ptr RawTiltState)

foreign import ccall
   "freenect.h freenect_get_tilt_degs"
   freenect_get_tilt_degs :: Ptr RawTiltState -> IO CDouble

--------------------------------------------------------------------------------
-- Helpers.

foreign import ccall
  "freenect-helpers.h new_freenect_context"
  new_freenect_context :: IO (Ptr (Ptr ContextStruct))

foreign import ccall
  "freenect-helpers.h new_freenect_device"
  new_freenect_device :: IO (Ptr (Ptr DeviceStruct))

foreign import ccall
  "freenect-helpers.h process_events_timeout"
  process_events_timeout :: Ptr ContextStruct -> CInt -> IO CInt

data FrameMode

foreign import ccall
  "freenect.h find_video_mode_freenect"
  find_video_mode_freenect :: Word32
                           -> Word32
                           -> IO (Ptr FrameMode)
foreign import ccall
  "freenect.h set_freenect_video_mode"
  set_freenect_video_mode :: Ptr DeviceStruct -> Ptr FrameMode -> IO CInt
  
foreign import ccall
  "freenect.h get_freenect_video_resolution"
  get_freenect_video_resolution :: Ptr DeviceStruct -> IO CInt

foreign import ccall
  "freenect.h find_depth_mode_freenect"
  find_depth_mode_freenect :: Word32
                           -> Word32
                           -> IO (Ptr FrameMode)

foreign import ccall
  "freenect.h set_freenect_depth_mode"
  set_freenect_depth_mode :: Ptr DeviceStruct -> Ptr FrameMode -> IO CInt

foreign import ccall
  "freenect.h get_freenect_depth_resolution"
  get_freenect_depth_resolution :: Ptr DeviceStruct -> IO CInt
