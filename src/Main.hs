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
    device <- newDevice
    openDevice context device index
    printf "Opened device %d.\n" index
    processEvents context
    printf "Finished processing events.\n"

  where devices = [Motor,Camera]
        index = 0
