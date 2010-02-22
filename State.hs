module State (
	Env(..),
	initialEnvironment,
	Sprite(..)
) where
import qualified Graphics.UI.GLUT as GL

data Env = Env
	{
		clock :: Int,
		sprites :: [Sprite]
	} deriving (Show)

initialEnvironment = Env 0 [
	Square (GL.Position 1 1) [] False
	]

data Sprite =
	Square {
		currentPos :: GL.Position,
		spritePath :: [GL.Position],
		sticky :: Bool
--		spriteColor :: Color3 GLdouble
	} deriving (Show)
