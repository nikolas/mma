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
		variables :: Variables,
		sprites :: [Sprite]
	}

data Variables = Variables {
	clock :: Int
}

elapsedTimeInSeconds :: Variables -> Int
elapsedTimeInSeconds (Variables i) = i

data Sprite =
	Square {
		spritePos :: Complex GLfloat,
		spriteColor :: Color3 GLfloat
	}

updateState :: [Key] -> State -> State
updateState keys (State variable sprites) = State newVars newSprites
	where
	-- add a sprite to the screen
	if addSpriteButton `elem` keys then 

addSpriteButton = Char 'z'
