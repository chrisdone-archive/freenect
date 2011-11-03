{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Volume control with the Kinect.

module Main
  where

import           Control.Concurrent
import           Control.Monad
import           Data.Bits
import           Data.IORef
import           Data.Vector.Storable      (Vector,(!))
import qualified Data.Vector.Storable      as V
import           Data.Word
import           Foreign.ForeignPtr
import           Freenect
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLUT          hiding (shift)
import           System.Process            (system)

width, height :: Int
width = 640
height = 480

main :: IO ()
main = do
  depthGrid <- newIORef Nothing
  triggered <- newEmptyMVar
  mode <- newIORef False
  _ <- getDepthThread depthGrid
  glThread depthGrid triggered mode

getDepthThread :: IORef (Maybe (Vector Word16)) -> IO ThreadId
getDepthThread depthGrid = forkOS $ do
  withContext $ \context -> do
    setLogLevel LogFatal context
    selectSubdevices context devices
    withDevice context index $ \device -> do
      setDepthMode device Medium ElevenBit
      setDepthCallback device $ \payload _timestamp -> do
        writeIORef depthGrid (Just payload)
        postRedisplay Nothing
        return ()
      startDepth device
      forever $ processEvents context

  where devices = [Camera]
        index = 0 :: Integer

glThread :: IORef (Maybe (Vector Word16)) -> MVar () -> IORef Bool -> IO ()
glThread depthGrid triggered mode = do
  (_progname,_args) <- getArgsAndInitialize
  _window <- createWindow "Kinect"
  windowSize $= Size (fromIntegral width) (fromIntegral height)
  ortho2D 0 (fromIntegral width) 0 (fromIntegral height)
  displayCallback $= display depthGrid triggered mode
  clearColor $= Color4 0.2 0.2 0.2 0
  mainLoop

n=2^11 - 1000

display ::  IORef (Maybe (Vector Word16)) -> MVar () -> IORef Bool -> IO ()
display depthGrid triggered mode = do
  depthGrid <- readIORef depthGrid
  finished <- isEmptyMVar triggered
  case depthGrid of
    Nothing -> return ()
    Just grid -> do
      clear [ColorBuffer]
      let coords = [(x,y) | x <- [0..width-1], y <- [0..height-1]]
      count <- forFold 0 coords $ \count (x,y) -> do
        let rawDisparity = fromIntegral (grid ! (y*width + x))
            d = rawDisparity/n
        if d<0.8
          then if d<0.5
                  then do patch (x,height-y) (1,d,d); return (count+1)
                  else do patch (x,height-y) (d,d,d); return count
          else return count
      when finished $ do
        when (count > 10000) $ do
           putMVar triggered ()
           forkIO $ do threadDelay $ 1000 * 1000
                       takeMVar triggered
           themode <- readIORef mode
           system ("amixer set Master " ++ if themode then "unmute" else "mute")
           writeIORef mode (not themode)
      swapBuffers

  where forFold nil xs cons = foldM cons nil xs

type PatchColor = (GLfloat,GLfloat,GLfloat)
type Loc = (Int,Int)

patch :: Loc -> PatchColor -> IO ()
patch (x,y) (r,g,b) = do
  color $ Color3 r g b
  rect (Vertex2 xf yf) (Vertex2 (xf+1) (yf+1))
  where xf = fromIntegral x :: GLfloat
        yf = fromIntegral y :: GLfloat
