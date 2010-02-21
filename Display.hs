module Display (
) where
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import Data.IORef
import Cube
import Points
import State

{-display angle position = do
	clear [ColorBuffer]
	loadIdentity
	(x,y) <- get position
	translate $ Vector3 x y 0

	preservingMatrix $ do
		a <- get angle
		rotate a $ Vector3 0 0 (1::GLfloat)
		scale 0.7 0.7 (0.7::GLfloat)
		mapM_ (\(x,y,z) -> preservingMatrix $ do
			color $ Color3 ((x+1.0)/2.0) ((y+1.0)/2.0) ((z+1.0)/2.0)
			translate $ Vector3 x y z
			cube (0.1::GLfloat)
			) $ points 50
	swapBuffers-}

idle angle delta = do
	a <- get angle
	d <- get delta
	angle $= a + d
	postRedisplay Nothing -- Only required on Mac OS X, which double-buffers internally

{-data Rect = Rect
	{
		rectX :: Double,
		rectY :: Double,
		rectWidth :: Double,
		rectHeight :: Double
	} deriving Show-}

{-renderState :: State -> IO ()
renderState (State variables sprites _) = do
	putStrLn $ (show (length sprites)) ++ " sprites"
	mapM_ renderSprite sprites
	preservingMatrix $ do
		translate (Vector3 (-300) (220) (0::GLfloat))
		renderWithShade (Color3 1 1 (1::GLfloat)) (Color3 0 0 (1::GLfloat)) $ do
			scale (0.2::GLfloat) 0.2 0.2
			renderString Helvetica12 clockStr
	where
	clockStr = show $ elapsedTimeInSeconds variables

	renderSprite :: Sprite -> IO ()

	renderSprite (Square p c) = preservingMatrix $ do
		color c
		cube (0.1::GLfloat)

	renderSprite _ = return ()


	renderWithShade :: (ColorComponent a) => Color3 a -> Color3 a -> IO () -> IO ()
	renderWithShade colorA colorB renderer = do
		color colorB
		preservingMatrix $ do
			translate $ Vector3 1 (-1) (-1::GLfloat)
			renderer
		color colorA
		preservingMatrix renderer-}
