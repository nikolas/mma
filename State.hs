module State (
	Env(..),
	initialEnvironment,

	Vars(..),

	Sprite(..),
	makeSprite,
	spritePoints,
	toggleSticky,
	isMouseOverSprite,

	conv,
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
	[ ]

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

		size :: GL.GLdouble,

		-- recorded path
		spritePath :: [GL.Position],

		-- is it being dragged?
		sticky :: Bool
	} deriving (Show)

makeSprite :: GL.Position -> Sprite
makeSprite pos = Square pos 10 [] False

spritePoints :: Sprite -> [GL.Vertex2 GL.GLdouble]
spritePoints s =
	[(GL.Vertex2 (x-sz) (y+sz))
	, (GL.Vertex2 (x-sz) (y-sz))
	, (GL.Vertex2 (x+sz) (y-sz))
	, (GL.Vertex2 (x+sz) (y+sz))]
	where
	-- TODO: better way to do this
	x = conv px; y = conv py
	(GL.Position px py) = currentPos s
	sz :: GL.GLdouble
	sz = 10

toggleSticky :: Sprite -> Sprite
toggleSticky (Square pos sz path s) = Square pos sz path $ not s

isMouseOverSprite :: GL.Position -> Sprite -> Bool
isMouseOverSprite p s = (sx >= px - sz) && (sx <= px + sz)
				&& (sy >= py - sz) && (sy <= py + sz)
				where
				sx = conv csx; sy = conv csy; px = conv ppx; py = conv ppy
				(GL.Position csx csy) = currentPos s
				(GL.Position ppx ppy) = p
				sz = size s

-- generalize an Integral
conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger
