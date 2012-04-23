-- | Freenect examples.

module Main
  (main)
  where

import Freenect
import Text.Printf
import Control.Monad
import Control.Monad.Fix
import Data.IORef
import Data.Vector.Storable (Vector,(!))
import Codec.Picture.Types
import Codec.Picture.Bitmap

width, height :: Int
width = 640
height = 480

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
      setVideoMode device Medium Bayer
      setVideoCallback device $ \payload timestamp -> do
        --printf "Payload: %s\n" (take 100 $ show payload)
        let img = ImageRGB8 (generateImage (\x y -> PixelRGB8 (payload ! (y*width + x)) (payload ! (y*width + x + 1)) (payload ! (y*width + x + 2))) width height)
        writeDynamicBitmap "test.bmp" img 
        writeIORef done True
      printf "Setted video callback.\n"
      startVideo device
      printf "Started video stream.\n"
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
