-- | Freenect examples.

module Main
  (main)
  where

import Freenect
import Text.Printf
import Control.Monad
import Control.Monad.Fix
import Data.IORef

-- | Demos some Freenect use.
main :: IO ()
main =
  withContext $ \context -> do
    setLogLevel LogDebug context
    deviceCount <- countDevices context
    printf "Devices: %d\n" deviceCount
    selectSubdevices context devices
    printf "Selected devices: %s\n" (show devices)
    withDevice context index $ \device -> do
      printf "Opened device %d.\n" index
      done <- newIORef False
      setDepthMode device Medium ElevenBit
      setDepthCallback device $ \payload timestamp -> do
        printf "Payload: %s\n" (take 100 $ show payload)
        writeIORef done True
      printf "Setted depth callback.\n"
      startDepth device
      printf "Started depth stream.\n"
      printf "Processing…\n"
      fix $ \repeat -> do
        processEvents context
        isDone <- readIORef done
        if isDone
           then return ()
           else repeat
      printf "Finished processing events.\n"

  where devices = [Camera]
        index = 0 :: Integer
