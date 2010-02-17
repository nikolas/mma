module State (
	GlobalState(GlobalState),
	KeysState(KeysState),
	MousePos(MousePos)
) where
import Data.IORef
import Graphics.Rendering.OpenGL

data GlobalState = GlobalState
	{
		keysState :: IORef KeysState,
		mousePos :: IORef MousePos,
		sprites :: [Sprite]
	}

data KeysState = KeysState
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
	}

-- for now
type Sprite = (GLfloat, GLfloat, GLfloat)
