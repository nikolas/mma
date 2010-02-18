import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
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
	--modifyIORef gs (updateState keystate)
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
