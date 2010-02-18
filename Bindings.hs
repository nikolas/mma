import Data.IORef
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import System.Exit
import Display


keyboardAct a p (Char ' ') Down = do
	a' <- get a
	a $= -a'
keyboardAct a p (Char '+') Down = do
	a' <- get a
	a $= 2*a'
keyboardAct a p (Char '-') Down = do
	a' <- get a
	a $= a'/2
keyboardAct a p (SpecialKey KeyLeft) Down = do
	(x,y) <- get p
	p $= (x-0.1,y)
keyboardAct a p (SpecialKey KeyRight) Down = do
	(x,y) <- get p
	p $= (x+0.1,y)
keyboardAct a p (SpecialKey KeyUp) Down = do
	(x,y) <- get p
	p $= (x,y+0.1)
keyboardAct a p (SpecialKey KeyDown) Down = do
	(x,y) <- get p
	p $= (x,y-0.1)

-- exit program
keyboardAct a p (Char 'q') Down = do
	exitSuccess

keyboardAct _ _ _ _ = return ()


mouseAct (Position x y) = do
	return ()

keyboardMouse angle pos key state modifiers position = do
	keyboardAct angle pos key state
