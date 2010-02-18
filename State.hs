module State (
	State(State),
	KeysState(KeysState),
	MousePos(MousePos)
) where
import Data.IORef
import Data.Complex
import Graphics.Rendering.OpenGL

data State = State
	{
		--keysState :: IORef KeysState,
		--mousePos :: IORef MousePos,
		variable :: Variables
		sprites :: [Sprite]
	}

data Variables = Variables {
	clock :: Int
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
data Sprite =
	Square {
		position :: Complex GLfloat,
		color :: Color3 GLfloat
	}
