-- | Freenect examples.
-- Grabs a frame from the Kinect video camera and outputs it
-- in the same directory as "output.bmp".

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
      setVideoMode device Medium RGB
      setVideoCallback device $ \payload timestamp -> do
        let at x y i = payload ! ((y*width + x)*3 + i)
        let img = ImageRGB8 (generateImage (\x y -> PixelRGB8 (at x y 0) (at x y 1) (at x y 2)) width height)
        writeDynamicBitmap "output.bmp" img 
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
