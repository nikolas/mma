-- let's copy (some of) Raincat for some basic gfx....

import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Bindings

main = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]

	createWindow "mma"
	reshapeCallback $= Just reshape

	angle <- newIORef (0.0::GLfloat)
	delta <- newIORef (0.1::GLfloat)
	position <- newIORef (0.0::GLfloat, 0.0)
	mousePos <- newIORef (Position 0 0)

	-- input
	keyboardMouseCallback $= Just (keyboardMouse delta position)
	motionCallback $= Just (mouseAct mousePos)

	idleCallback $= Just (idle angle delta)
	displayCallback $= (display angle position)
	mainLoop
