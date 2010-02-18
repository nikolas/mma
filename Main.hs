import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Bindings
import State
import Points

main = do
	(progname,args) <- getArgsAndInitialize

	keystate <- newIORef []

	cp <- newIORef (openingProc 0

	initialWindowSize $= Size 640 480
	initialDisplayMode $= [RGBAMode,DoubleBuffered]

	wnd <- createWindow "Marlon Moonglow's Animator"

	displayCallback $= dispProc cp
	keyboardMouseCallback $= Just (keyProc keystate)

	initMatrix
	mainLoop

	--reshapeCallback $= Just reshape
	--startState <- guiInit
	{-globalState <- newIORef startState
	angle <- newIORef (0.0::GLfloat)
	delta <- newIORef (0.1::GLfloat)
	position <- newIORef (0.0::GLfloat, 0.0)
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

initMatrix = do
	viewport $= (Position 0 0, Size 640 480)
	matrixMode $= Projection
	loadIdentity
	perspective 30.0 (4/3) 600 1400
	lookAt (Vertex3 0 0 (927::GLfloat)) (Vertex3 0 0 (0::GLfloat)) (Vector3 0 1 (0::GLfloat))

dispProc cp = do
	m <- get cp
	Scene next <- m
	cp $= next

guiInit :: IO (GlobalState)
guiInit = do
	keys <- newIORef $ KeysState False False False False False False False
	mp <- newIORef $ MousePos 0 0
	return (GlobalState keys mp (points 8))
