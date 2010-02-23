module State (
	Env(..),
	initialEnvironment,

	Vars(..),
	setClock,

	Sprite(..),
	toggleSticky,
) where
import qualified Graphics.UI.GLUT as GL

data Env = Env
	{
		vars :: Vars,
		sprites :: [Sprite]
	} deriving (Show)

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

setClock :: Int -> Vars -> Vars
setClock i (Vars _ mp p r m) = Vars i mp p r m

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
