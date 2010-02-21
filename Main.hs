{-# OPTIONS -fglasgow-exts #-}

import Complex
import Data.IORef
import qualified Graphics.UI.GLUT as GL
import System.Exit

import State

main = do
	(progname,args) <- GL.getArgsAndInitialize
	wnd <- initGL
	env <- newIORef initialEnvironment
	GL.displayCallback $= (display env)
	GL.idleCallback $= Just (idle env)
	GL.keyboardMouseCallback $= Just (keyboardMouse wnd env)
	GL.mainLoop

	{-keystate <- newIORef []

	state <- progInit
	stateRef <- newIORef state

	initialWindowSize $= Size 640 480
	initialDisplayMode $= [RGBAMode,DoubleBuffered]

	wnd <- createWindow "Marlon Moonglow's Animator"

	displayCallback $= dispProc stateRef
	keyboardMouseCallback $= Just (keyProc keystate)

	initMatrix
	mainLoop-}

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

keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
	GL.destroyWindow window
	exitSuccess
keyboardMouse _ env key state modifiers position = do
	e <- GL.get env
	env $= userAction e key state

userAction (Env t sprites) (GL.Char 'a') GL.Down = Env t s
	where
	s = (Square (replicate 10 ((newP::GL.GLdouble):+(newP::GL.GLdouble)))):sprites
	newP = fromIntegral $ length sprites * 3
userAction e _ _ = e

tick :: Int -> Env -> Env
tick tnew (Env 0 sprites) = Env tnew sprites
tick tnew (Env told sprites) = Env tnew s
	where
	s = map updateSprites sprites
	elapsed = fromIntegral $ tnew - told
	updateSprites z = z

($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)

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

-- metavar's dsl. do it this way until i know what can be removed
class GLCommand a where
	render :: a -> IO ()

data GLC = forall a. GLCommand a => GLC a
instance GLCommand GLC where
	render (GLC a) = render a

rotate :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
rotate a v1 v2 v3 = GLC $ ActionRotate a (GL.Vector3 v1 v2 v3)
scale :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
scale x y z = GLC $ ActionScale x y z
translate :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
translate x y z = GLC $ ActionTranslate (GL.Vector3 x y z)
identity = GLC $ (ActionIdentity :: MatrixActions GL.GLdouble)

data GL.MatrixComponent a => MatrixActions a = ActionRotate a (GL.Vector3 a)
	| ActionScale a a a
	| ActionTranslate (GL.Vector3 a)
	| ActionIdentity

instance GL.MatrixComponent a => GLCommand (MatrixActions a) where
	render = matrixActions
matrixActions (ActionRotate angle vec) = GL.rotate angle vec
matrixActions (ActionScale x y z) = GL.scale x y z
matrixActions (ActionTranslate vec) = GL.translate vec
matrixActions (ActionIdentity) = GL.loadIdentity

data GL.VertexComponent a => VertexActions a = ActionVertex (GL.Vertex3 a)
instance GL.VertexComponent a => GLCommand (VertexActions a) where
	render = vertexActions
vertexActions (ActionVertex vec) = GL.vertex vec
vertex :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
vertex x y z = GLC $ ActionVertex (GL.Vertex3 x y z)

data GL.ColorComponent a => ColorActions a = ActionColor (GL.Color3 a)
instance GL.ColorComponent a => GLCommand (ColorActions a) where
	render = colorActions
colorActions (ActionColor c) = GL.color c
color :: GL.GLdouble -> GL.GLdouble -> GL.GLdouble -> GLC
color r g b = GLC $ ActionColor (GL.Color3 r g b)

triangles commands = GLC $ RenderTriangles commands
quads commands = GLC $ RenderQuads commands
serial commands = GLC $ RenderSerial commands
parallel commands = GLC $ RenderParallel commands

data RenderActions = RenderTriangles [GLC]
	| RenderQuads [GLC]
	| RenderSerial [GLC]
	| RenderParallel [GLC]
instance GLCommand RenderActions where
	render = renderActions
renderActions (RenderTriangles commands) = GL.renderPrimitive GL.Triangles $ mapM_ render commands
renderActions (RenderQuads commands) = GL.renderPrimitive GL.Quads $ mapM_ render commands
renderActions (RenderSerial commands) = mapM_ render commands
renderActions (RenderParallel commands) = mapM_ (GL.preservingMatrix . render) commands

world (Env t s) = serial [ identity,
	--rotate (playerRotation s) 0 1 0,
	--translate (0 - playerX s) 0 (0 - playerY s),
	--translate 0 0 (-4),
	--rotate trotation 0 1 0,
	translate (0 - realPart pos) 0 (0 - imagPart pos),
	translate 0 0 (-4),
	quads [ color 1 0 0,
		vertex 0 0 0,
		vertex 1 0 0,
		vertex 0 1 0,
		vertex 1 1 0 ]
	{-triangles [ color 1 0 0,
		vertex 0 1 0,
		color 0 1 0,
		vertex (-1) 0 0,
		color 0 0 1,
		vertex 1 0 0]-}
	]
	where
	trotation = (fromIntegral t) / 10
	pos = head (spritePath (head s))

{- keyProc keystate key ks mod pos =
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

data Scene = Scene (IO Scene) -}

{-guiInit :: IO (State)
guiInit = do
	keys <- newIORef $ KeysState False False False False False False False
	mp <- newIORef $ MousePos 0 0
	return (GlobalState keys mp (points 8))-}
