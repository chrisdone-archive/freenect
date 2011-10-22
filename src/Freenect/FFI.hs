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
  freenect_init :: Ptr ContextStruct -> CInt -> IO CInt

foreign import ccall
  "freenect.h freenect_shutdown"
  freenect_shutdown :: (Ptr ContextStruct) -> IO CInt

foreign import ccall
  "freenect.h freenect_num_devices"
  freenect_num_devices :: (Ptr ContextStruct) -> IO CInt

foreign import ccall
  "freenect.h freenect_process_events"
  freenect_process_events :: (Ptr ContextStruct) -> IO CInt

foreign import ccall
  "freenect.h freenect_select_subdevices"
  freenect_select_subdevices :: (Ptr ContextStruct) -> CInt -> IO ()

foreign import ccall
  "freenect.h freenect_set_log_level"
  freenect_set_log_level :: (Ptr ContextStruct) -> CInt -> IO ()

--------------------------------------------------------------------------------
-- Helpers.

foreign import ccall
  "freenect-helpers.h new_freenect_context"
  new_freenect_context :: IO (Ptr ContextStruct)

foreign import ccall
  "freenect-helpers.h new_freenect_device"
  new_freenect_device :: IO (Ptr DeviceStruct)

foreign import ccall
  "freenect-helpers.h init_freenect_context"
  init_freenect_context :: Ptr ContextStruct -> IO CInt

foreign import ccall
  "freenect-helpers.h open_freenect_device"
  open_freenect_device :: Ptr ContextStruct -> Ptr DeviceStruct -> CInt -> IO CInt
