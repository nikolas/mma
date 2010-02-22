import Data.IORef
import qualified Graphics.UI.GLUT as GL
import System.Exit

import Bindings
import MetaGL (render)
import Render
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
	GL.motionCallback $= Just (motion env)
	GL.passiveMotionCallback $= Just (passiveMotion env)

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
tick tnew (Env v sprites) = Env (setClock (clock v+elapsed) v) s
	where
	s = map idleSprite sprites
	elapsed = fromIntegral $ tnew - clock v
	idleSprite z = z

initGL = do
	GL.initialDisplayMode $= [GL.DoubleBuffered]
	GL.initialWindowSize $= GL.Size 640 480
	window <- GL.createWindow "mma"
	GL.clearColor $= GL.Color4 0 0 0 0

	sz <- GL.get GL.screenSize
	GL.viewport $= (GL.Position 0 0 , sz)

	GL.matrixMode $= GL.Projection
	GL.loadIdentity
	GL.perspective 45 ((fromIntegral 640)/(fromIntegral 480)) 0.1 100
	GL.matrixMode $= GL.Modelview 0
	return window

($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
