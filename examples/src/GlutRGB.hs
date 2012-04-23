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
import           Graphics.Rendering.OpenGL hiding (RGB)
import           Graphics.UI.GLUT hiding (shift, RGB)

width, height :: Int
width = 640
height = 480

main :: IO ()
main = do
  videoGrid <- newMVar Nothing
  _ <- getVideoThread videoGrid
  glThread videoGrid

getVideoThread :: MVar (Maybe (Vector Word8)) -> IO ThreadId
getVideoThread videoGrid = forkOS $ do
  withContext $ \context -> do
    setLogLevel LogFatal context
    selectSubdevices context devices
    withDevice context index $ \device -> do
      setVideoMode device Medium RGB
      setVideoCallback device $ \payload _timestamp -> do
        _ <- swapMVar videoGrid (Just payload)
        postRedisplay Nothing
        return ()
      startVideo device
      forever $ processEvents context

  where devices = [Camera]
        index = 0 :: Integer

glThread :: MVar (Maybe (Vector Word8)) -> IO ()
glThread videoGrid = do
  (_progname,_args) <- getArgsAndInitialize
--  initialDisplayMode $= [DoubleBuffered]
  _window <- createWindow "Kinect RGB Video"
  windowSize $= Size (fromIntegral width) (fromIntegral height)
  ortho2D 0 (fromIntegral width) 0 (fromIntegral height)
  displayCallback $= display videoGrid
  mainLoop

display ::  MVar (Maybe (Vector Word8)) -> IO ()
display videoGrid = do
  videoGrid <- readMVar videoGrid
  case videoGrid of
    Nothing -> return ()
    Just grid -> do
      forM_ [(x,y) | x <- [0..width-1], y <- [0..height-1]] $ \(x,y) -> do
        let r = fromIntegral (grid ! (y*width + x + 0))
            g = fromIntegral (grid ! (y*width + x + 1))
            b = fromIntegral (grid ! (y*width + x + 2))
            --d = fromIntegral (fromIntegral video :: Word16)/255
        patch (x,height-y)
              (r,g,b)
      swapBuffers

type PatchColor = (GLubyte,GLubyte,GLubyte)
type Loc = (Int,Int)

patch :: Loc -> PatchColor -> IO ()
patch (x,y) (r,g,b) = do
  color $ Color3 r g b
  rect (Vertex2 xf yf) (Vertex2 (xf+1) (yf+1))
  where xf = fromIntegral x :: GLfloat
        yf = fromIntegral y :: GLfloat
