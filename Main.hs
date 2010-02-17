import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Bindings
import State
import Points

main = do
	(progname,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]

	createWindow "mma"
	reshapeCallback $= Just reshape

	startState <- guiInit

	globalState <- newIORef startState
	angle <- newIORef (0.0::GLfloat)
	delta <- newIORef (0.1::GLfloat)
	position <- newIORef (0.0::GLfloat, 0.0)
	mousePos <- newIORef (Position 0 0)

	-- input
	keyboardMouseCallback $= Just (keyboardMouse delta position)
--	motionCallback $= Just (mouseAct (IORef mousePos))

	idleCallback $= Just (idle angle delta)
	displayCallback $= (display angle position)
	mainLoop

guiInit :: IO (GlobalState)
guiInit = do
	keys <- newIORef $ KeysState False False False False False False False
	mp <- newIORef $ MousePos 0 0
	return (GlobalState keys mp (points 8))
