import Data.IORef
import qualified Graphics.UI.GLUT as GL

import Bindings
import MetaGL (render)
import Render
import State

main = do
	-- make the GL window
	(_,_) <- GL.getArgsAndInitialize
	wnd <- initGL

	-- pointer to the program state
	env <- newIORef initialEnvironment

	-- callbacks
	GL.displayCallback $= (display env)
	GL.idleCallback $= Just (idle env)
	GL.keyboardMouseCallback $= Just (keyboardMouse wnd env)
	GL.motionCallback $= Just (motion env)
	GL.passiveMotionCallback $= Just (passiveMotion env)

	-- just distort it on reshaping, to make sure it's at least still all on
	-- the screen
	--GL.reshapeCallback $= Just reshape

	GL.mainLoop

--__----__----__
-- display callbacks
--__----__----__----_
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

reshape s@(GL.Size x y) = do
	GL.viewport $= (GL.Position 0 0 , s)

	GL.matrixMode $= GL.Projection
	GL.loadIdentity
	GL.perspective 45 ((fromIntegral x)/(fromIntegral y)) 0.1 100
	GL.matrixMode $= GL.Modelview 0
--__----__----__----__----__----__----__----__----__----__----__----__--


tick :: Int -> Env -> Env
tick tnew (Env v sprs) = Env v{clock = clock v+elapsed} s
	where
	s = map idleSprite sprs
	elapsed = fromIntegral $ tnew - clock v
	idleSprite z = z

initGL = do
	GL.initialDisplayMode $= [GL.DoubleBuffered]
	GL.initialWindowSize $= GL.Size 640 480
	window <- GL.createWindow "mma"
	GL.clearColor $= GL.Color4 0 0 0 0

	-- make sure the viewport and perspective are correct when
	-- initialWindowSize is ignored
	s <- GL.get GL.screenSize
	reshape s

	return window


($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
