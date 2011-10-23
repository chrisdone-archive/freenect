{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- | Foreign functions, some helpers some from Freenect C lib.

module Freenect.FFI where

import Foreign.C
import Foreign

data ContextStruct
data DeviceStruct

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

type DepthCallback = Ptr DeviceStruct -> Ptr () -> Word32 -> IO ()

foreign import ccall
  "freenect.h freenect_set_depth_callback"
  freenect_set_depth_callback
    :: Ptr DeviceStruct
    -> (FunPtr DepthCallback)
    -> IO ()

foreign import ccall "wrapper"  
  wrapDepthCallback :: DepthCallback -> IO (FunPtr DepthCallback)

foreign import ccall
  "freenect.h freenect_start_depth"
  freenect_start_depth :: Ptr DeviceStruct
                       -> IO CInt

foreign import ccall
  "freenect.h freenect_set_tilt_degs"
  freenect_set_tilt_degs :: Ptr DeviceStruct -> CDouble -> IO CInt

--------------------------------------------------------------------------------
-- Helpers.

foreign import ccall
  "freenect-helpers.h new_freenect_context"
  new_freenect_context :: IO (Ptr (Ptr ContextStruct))

foreign import ccall
  "freenect-helpers.h new_freenect_device"
  new_freenect_device :: IO (Ptr (Ptr DeviceStruct))
