-- | Freenect examples.

module Main
  (main)
  where

import Freenect
import Text.Printf

-- | Demos some Freenect use.
main :: IO ()
main =
  withContext $ \context -> do
    setLogLevel LogFlood context
    deviceCount <- countDevices context
    printf "Devices: %d\n" deviceCount
    selectSubdevices context devices
    printf "Selected devices: %s\n" (show devices)
    withDevice context index $ \device -> do
      printf "Opened device %d.\n" index
      setDepthCallback device $ \_ _ timestamp ->
        printf "Callback: %d\n" timestamp
      printf "Setted depth callback.\n"
      startDepth device
      printf "Started depth stream.\n"
      processEvents context
      printf "Finished processing events.\n"

  where devices = [Motor,Camera]
        index = 0 :: Integer
