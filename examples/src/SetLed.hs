-- | Freenect examples
-- Sets LED from Kinect device

module Main (main) where

import Freenect
import Text.Printf
import Data.Char
import System.IO

main :: IO ()
main = 
   withContext $ \context -> do
      deviceCount <- countDevices context
      printf "Devices: %d\n" deviceCount

      selectSubdevices context devices
      printf "Selected devices: %s\n" (show devices)

      withDevice context index $ \device -> do
         printf "Opened device %d.\n" index

         printf "Please enter the number for next LED status\n" 
         printf " 0 - Off\n"
         printf " 1 - Green\n"
         printf " 2 - Red\n"
         printf " 3 - Yellow\n"
         printf " 4 - BlinkGreen\n"
         printf " 5 - BlinkRedYellow\n"
         printf "Use: "
         hFlush stdout

         ch <- getChar
         
         setLed device (toEnum $ (ord ch) - (ord '0'))

   where 
   devices = [Camera, Motor]
   index = 0 :: Integer
      
