module State (
	Env(..),
	initialEnvironment,

	Vars(..),

	Sprite(..),
	toggleSticky,
	isMouseOverSprite,
) where
import qualified Graphics.UI.GLUT as GL

data Env = Env
	{
		vars :: Vars,
		sprites :: [Sprite]
	} deriving (Show)

initialEnvironment :: Env
initialEnvironment = Env
	( Vars 0 (GL.Position 0 0) False False initialMenu )
	[ Square (GL.Position 0 0) [] False ]

data Vars = Vars
	{
		clock :: Int,
		mousePos :: GL.Position,
		playing :: Bool,
		recording :: Bool,
		menu :: Menu
	} deriving (Show)

data Menu = Menu
	{
		playButton :: Button,
		recButton :: Button,

		-- stepper
		prevFrameButton :: Button,
		nextFrameButton :: Button,

		-- sprite chooser
		prevSpriteButton :: Button,
		nextSpriteButton :: Button
	} deriving (Show)

type Button = Bool
initialMenu :: Menu
initialMenu = Menu {
	playButton = False,
	recButton = False,
	prevFrameButton = False,
	nextFrameButton = False,
	prevSpriteButton = False,
	nextSpriteButton = False
}

data Sprite =
	Square {
		currentPos :: GL.Position,

		-- recorded path
		spritePath :: [GL.Position],

		-- is it being dragged?
		sticky :: Bool
	} deriving (Show)

toggleSticky :: Sprite -> Sprite
toggleSticky (Square pos path s) = Square pos path $ not s

isMouseOverSprite :: Vars -> Sprite -> Bool
isMouseOverSprite v s = (sx >= vx - size) && (sx <= vx + size)
				&& (sy >= vy - size) && (sy <= sy + size)
				where
				(GL.Position sx sy) = currentPos s
				(GL.Position vx vy) = mousePos v
				size = 40
