import Data.IORef
import qualified Graphics.UI.GLUT as GL
import System.Exit

import Bindings
import MetaGL
import State

main = do
	-- make the GL window
	(progname,args) <- GL.getArgsAndInitialize
	wnd <- initGL

	-- pointer to the program state
	env <- newIORef initialEnvironment

	-- callbacks
	GL.displayCallback $= (display env)
	GL.idleCallback $= Just (idle env)
	GL.keyboardMouseCallback $= Just (keyboardMouse wnd env)

	GL.mainLoop


display env = do
	GL.clear [GL.ColorBuffer, GL.DepthBuffer]
	e <- GL.get env
	render $ world e
	GL.swapBuffers

idle env = do
	e <- GL.get env
	time <- GL.get GL.elapsedTime
	env $= tick time e
	GL.postRedisplay Nothing

tick :: Int -> Env -> Env
tick tnew (Env 0 sprites) = Env tnew sprites
tick tnew (Env told sprites) = Env tnew s
	where
	s = map updateSprites sprites
	elapsed = fromIntegral $ tnew - told
	updateSprites z = z

initGL = do
	GL.initialDisplayMode $= [GL.DoubleBuffered]
	GL.initialWindowSize $= GL.Size 640 480
	window <- GL.createWindow "mma"
	GL.clearColor $= GL.Color4 0 0 0 0
	GL.viewport $= (GL.Position 0 0 , GL.Size 640 680)
	GL.matrixMode $= GL.Projection
	GL.loadIdentity
	GL.perspective 45 ((fromIntegral 640)/(fromIntegral 480)) 0.1 100
	GL.matrixMode $= GL.Modelview 0
	return window

($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
