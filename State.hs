module State (
	Env(..),
	initialEnvironment,

	Vars(..),
	setClock,
	setMousePos,

	Sprite(..),
	toggleSticky,
	isMouseOverSprite,
	setSpritePos,
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

setMousePos :: GL.Position -> Vars -> Vars
setMousePos pos (Vars c _ p r m) = Vars c pos p r m

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

isMouseOverSprite :: Vars -> Sprite -> Bool
isMouseOverSprite v s = (sx >= vx - size) && (sx <= vx + size)
				&& (sy >= vy - size) && (sy <= sy + size)
				where
				(GL.Position sx sy) = currentPos s
				(GL.Position vx vy) = mousePos v
				size = 40

setSpritePos :: GL.Position -> Sprite -> Sprite
setSpritePos p (Square _ sp st) = Square p sp st
