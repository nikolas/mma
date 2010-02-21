module State (
	Env(..),
	initialEnvironment,
	elapsedTimeInSeconds,
	Sprite(..)
) where
import Complex
import qualified Graphics.UI.GLUT as GL

data Env = Env
	{
		clock :: Int,
		sprites :: [Sprite]
	} deriving (Show)

initialEnvironment = Env 0 [Square (replicate 10 ((1.0::GL.GLdouble):+(1.0::GL.GLdouble)))]

elapsedTimeInSeconds :: Env -> Int
elapsedTimeInSeconds (Env i _) = i

data Sprite =
	Square {
		spritePath :: [Complex GL.GLdouble]
--		spriteColor :: Color3 GLdouble
	} deriving (Show)
