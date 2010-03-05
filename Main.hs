import Data.IORef
import Graphics.UI.GLUT

import Bindings
import Graphics
import Render
import State
import Util

main :: IO ()
main = do
  -- make pointer to world state
  env <- newIORef initialEnvironment

  -- make the GL window
  initialWindowSize $= Size 640 480
  (_,_) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  wnd <- createWindow "Marlon Moonglow's Animator"

  -- textures need to be in the IO monad, so they aren't part of the Env
  -- textures <- initTextures
  textures <- loadAllTextures

  -- set up callbacks
  displayCallback $= (glRunAs2D $ do
                         clearColor $= Color4 1 0 1 1
                         clear [ColorBuffer, DepthBuffer]
                         e <- readIORef env
                         drawWorld e textures
                         --readIORef env >>= drawWorld
                         flush
                         swapBuffers)

  idleCallback $= Just (idle env)

  let
    moveCursor p = do
      (Env v sp) <- readIORef env
                   --print p
      writeIORef env $ Env v{mousePos = p} sp

    trans :: Position -> IO Position
    trans (Position x y) = do
      (_, Size _ h) <- get viewport
      return ( Position x (conv h - y) )

  keyboardMouseCallback $= Just (keyboardMouse wnd env)

  motionCallback $= Just (\pos ->
                           trans pos >>= motion env)

  passiveMotionCallback $=
    Just (\pos ->
           trans pos >>= moveCursor >> postRedisplay Nothing)

  mainLoop
  -- TODO: stop using all the CPU, silly!

idle :: IORef Env -> IO ()
idle env = do
  e <- get env
  time <- get elapsedTime
  env $= tick time e
  postRedisplay Nothing

-- keep track of how much time has elapsed
tick :: Int -> Env -> Env
tick t (Env v sprs) = Env (v {clock=t}) sprs
