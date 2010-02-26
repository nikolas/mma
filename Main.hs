import Control.Monad
import Data.IORef
import Graphics.UI.GLUT

import Bindings
import MetaGL (render)
import Render
import State

main :: IO ()
main = do
	env <- newIORef initialEnvironment

	-- make the GL window
	(_,_) <- getArgsAndInitialize
	_ <- initGL

	-- callbacks
	displayCallback $= (display env)

	idleCallback $= Just (idle env)

	let
		moveCursor p = do
			(Env v sp) <- readIORef env
			writeIORef env $ Env v{mousePos = p} sp

	keyboardMouseCallback $= Just (\k st _ pos ->
		do
			--translate pos >>= moveCursor
			moveCursor pos
			when (st == Down) $
				modifyIORef env $ processEnv k
			postRedisplay Nothing)

	motionCallback $= Just (motion env)

	passiveMotionCallback $= Just (passiveMotion env)

	-- just distort it on reshaping, to make sure it's at least still all on
	-- the screen
	--reshapeCallback $= Just reshape

	mainLoop

--__----__----__
-- display callbacks
--__----__----__----_
display :: IORef Env -> IO ()
display env = do
	clear [ColorBuffer, DepthBuffer]
	e <- get env
	render $ world e
	swapBuffers

idle :: IORef Env -> IO ()
idle env = do
	e <- get env
	time <- get elapsedTime
	env $= tick time e
	postRedisplay Nothing

reshape :: Size -> IO ()
reshape s@(Size x y) = do
	viewport $= (Position 0 0 , s)

	matrixMode $= Projection
	loadIdentity
	perspective 45 ((fromIntegral x)/(fromIntegral y)) 0.1 100
	matrixMode $= Modelview 0
--__----__----__----__----__----__----__----__----__----__----__----__--


tick :: Int -> Env -> Env
tick tnew (Env v sprs) = Env v{clock = clock v+elapsed} s
	where
	s = map idleSprite sprs
	elapsed = fromIntegral $ tnew - clock v
	idleSprite z = z

initGL :: IO Window
initGL = do
	initialDisplayMode $= [DoubleBuffered]
	initialWindowSize $= Size 640 480
	window <- createWindow "mma"
	clearColor $= Color4 0 0 0 0

	-- make sure the viewport and perspective are correct when
	-- initialWindowSize is ignored
	s <- get screenSize
	reshape s

	return window
