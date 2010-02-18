module State (
	State(..),
	Variables(..),
--	KeysState(KeysState),
--	MousePos(MousePos),
	elapsedTimeInSeconds,
	Sprite(..)
) where
--import Data.IORef
import Data.Complex
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

data State = State
	{
		--keysState :: IORef KeysState,
		--mousePos :: IORef MousePos,
		variable :: Variables,
		sprites :: [Sprite],
		keys :: [[Key]]
	}

data Variables = Variables {
	clock :: Int
}

elapsedTimeInSeconds :: Variables -> Int
elapsedTimeInSeconds (Variables i) = i

{-data KeysState = KeysState
	{
		leftKeyDown     :: Bool,
		rightKeyDown    :: Bool,
		downKeyDown     :: Bool,
		upKeyDown       :: Bool,
		leftMouseDown   :: Bool,
		spaceKeyDown    :: Bool,
		escKeyDown      :: Bool
	}

data MousePos = MousePos
	{
		mouseX  :: Int,
		mouseY  :: Int
	}-}

-- for now
data Sprite =
	Square {
		spritePos :: Complex GLfloat,
		spriteColor :: Color3 GLfloat
	}
