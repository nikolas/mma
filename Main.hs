import Control.Monad
import Data.IORef
import Graphics.UI.GLUT

import Bindings
import Render
import State

main :: IO ()
main = do
	env <- newIORef initialEnvironment

	-- make the GL window
	initialWindowSize $= Size 640 480
	(_,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	wnd <- createWindow "mma"

	-- callbacks
	displayCallback $= (glRunAs2D $ do
		clearColor $= Color4 1 0 1 1
		clear [ColorBuffer, DepthBuffer]
		readIORef env >>= drawWorld
		flush
		swapBuffers)

	idleCallback $= Just (idle env)

	let
		moveCursor p = do
			(Env v sp) <- readIORef env
			print p
			writeIORef env $ Env v{mousePos = p} sp
		trans :: Position -> IO Position
		trans (Position x y) = do
			(_, Size _ h) <- get viewport
			return ( Position x (conv h - y) )

	keyboardMouseCallback $= Just (keyboardMouse wnd env)

	motionCallback $= Just (\pos ->
		trans pos >>= motion env)

	passiveMotionCallback $= Just (\pos ->
		trans pos >>= moveCursor >> postRedisplay Nothing)

	mainLoop

glRunAs2D :: IO () -> IO ()
glRunAs2D draw = do
	matrixMode $= Modelview 0
	loadIdentity

	--matrixMode $=  Projection
	--loadIdentity

	(_, Size w h) <- get viewport

	--ortho (-50) 50 (-50) (50) (-1000) 1000
	ortho 0 (conv w) 0 (conv h) (-1000) 1000

	preservingMatrix draw

idle :: IORef Env -> IO ()
idle env = do
	e <- get env
	time <- get elapsedTime
	env $= tick time e
	postRedisplay Nothing

tick :: Int -> Env -> Env
tick tnew (Env v sprs) = Env v{clock = clock v+elapsed} s
	where
	s = map idleSprite sprs
	elapsed = fromIntegral $ tnew - clock v
	idleSprite z = z
