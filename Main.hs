import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import Bindings
import Display
import State
import Points

main = do
	(progname,args) <- getArgsAndInitialize

	keystate <- newIORef []

	state <- progInit
	stateRef <- newIORef state

	initialWindowSize $= Size 640 480
	initialDisplayMode $= [RGBAMode,DoubleBuffered]

	wnd <- createWindow "Marlon Moonglow's Animator"

	displayCallback $= dispProc stateRef
	keyboardMouseCallback $= Just (keyProc keystate)

	initMatrix
	mainLoop

	--reshapeCallback $= Just reshape
	--startState <- guiInit
	{-globalState <- newIORef startState
	angle <- newIORef (0.0::GLdouble)
	delta <- newIORef (0.1::GLdouble)
	position <- newIORef (0.0::GLdouble, 0.0)
	mousePos <- newIORef (Position 0 0)

	-- input
	keyboardMouseCallback $= Just (keyboardMouse delta position)
--	motionCallback $= Just (mouseAct (IORef mousePos))

	idleCallback $= Just (idle angle delta)
	displayCallback $= (display angle position)
	mainLoop-}

keyProc keystate key ks mod pos =
	case (key,ks) of
		(Char 'q',_) -> exitSuccess
		(Char '\ESC',_) -> exitSuccess
		(_,_) -> return ()

initMatrix = do
	viewport $= (Position 0 0, Size 640 480)
	matrixMode $= Projection
	loadIdentity
	perspective 30.0 (4/3) 600 1400
	lookAt (Vertex3 0 0 (927::GLdouble)) (Vertex3 0 0 (0::GLdouble)) (Vector3 0 1 (0::GLdouble))

progInit :: IO (State)
progInit = do
	return (State (Variables 0) [] [])

dispProc :: IORef State -> IO ()
dispProc ref = do
	r <- readIORef ref

	clear [ColorBuffer]
	matrixMode $= Modelview 0
	loadIdentity
	renderState r
	swapBuffers
	--flush

mainProc :: Variables -> IORef [Key] -> IO Scene
mainProc vars ks = do
	keystate <- readIORef ks
	clear [ColorBuffer]
	matrixMode $= Modelview 0
	loadIdentity

	swapBuffers
	return $ Scene $ mainProc vars ks

data Scene = Scene (IO Scene)

{-guiInit :: IO (State)
guiInit = do
	keys <- newIORef $ KeysState False False False False False False False
	mp <- newIORef $ MousePos 0 0
	return (GlobalState keys mp (points 8))-}
