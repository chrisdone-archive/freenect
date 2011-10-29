{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Freenect example with GLUT.
--   Video demo here: http://www.youtube.com/watch?v=as2syH8Y8yc

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
import           Graphics.UI.GLUT hiding (shift)

width, height :: Int
width = 640
height = 480

main :: IO ()
main = do
  depthGrid <- newMVar Nothing
  _ <- getDepthThread depthGrid
  glThread depthGrid

getDepthThread :: MVar (Maybe (Vector Word16)) -> IO ThreadId
getDepthThread depthGrid = forkOS $ do
  withContext $ \context -> do
    setLogLevel LogFatal context
    selectSubdevices context devices
    withDevice context index $ \device -> do
      setDepthMode device Medium ElevenBit
      setDepthCallback device $ \payload _timestamp -> do
        _ <- swapMVar depthGrid (Just payload)
        postRedisplay Nothing
        return ()
      startDepth device
      forever $ processEvents context

  where devices = [Camera]
        index = 0 :: Integer

glThread :: MVar (Maybe (Vector Word16)) -> IO ()
glThread depthGrid = do
  (_progname,_args) <- getArgsAndInitialize
--  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Kinect"
  windowSize $= Size (fromIntegral width) (fromIntegral height)
  ortho2D 0 (fromIntegral width) 0 (fromIntegral height)
  displayCallback $= display depthGrid
  mainLoop

display ::  MVar (Maybe (Vector Word16)) -> IO ()
display depthGrid = do
  depthGrid <- readMVar depthGrid
  case depthGrid of
    Nothing -> return ()
    Just grid -> do
      forM_ [(x,y) | x <- [0..width-1], y <- [0..height-1]] $ \(x,y) -> do
        let depth = grid ! (y*width + x)
            d = fromIntegral (fromIntegral depth :: Word8)/255
        patch (x,height-y)
              (d,d,d)
      swapBuffers

type PatchColor = (GLfloat,GLfloat,GLfloat)
type Loc = (Int,Int)

patch :: Loc -> PatchColor -> IO ()
patch (x,y) (r,g,b) = do
  color $ Color3 r g b
  rect (Vertex2 xf yf) (Vertex2 (xf+1) (yf+1))
  where xf = fromIntegral x :: GLfloat
        yf = fromIntegral y :: GLfloat
