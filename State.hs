module State (
	Env(..),
	initialEnvironment,

	Vars(..),

	MmaMenu(..),

	Sprite(..),
	makeSprite,
	spritePoints,
	toggleSticky,
	within,

	-- misc functions that belong.... elsewhere???
	vertexRect,
	vertexRect',
	conv,
) where
import qualified Graphics.UI.GLUT as GL

data Env = Env
	{
		vars :: Vars,
		sprites :: [Sprite]
	} deriving (Show, Eq)

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
		menu :: MmaMenu
	} deriving (Show, Eq)

data MmaMenu = MmaMenu
	{
		playButton :: Button,
		recButton :: Button,

		-- stepper
		prevFrameButton :: Button,
		nextFrameButton :: Button,

		-- sprite chooser
		prevSpriteButton :: Button,
		nextSpriteButton :: Button
	} deriving (Show, Eq)

type Button = Bool
initialMenu :: MmaMenu
initialMenu = MmaMenu {
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
	} deriving (Show, Eq)

makeSprite :: GL.Position -> Sprite
makeSprite pos = Square pos 10 [] False

spritePoints :: Sprite -> [GL.Vertex2 GL.GLdouble]
spritePoints s = vertexRect' (currentPos s) 10 10

toggleSticky :: Sprite -> Sprite
toggleSticky (Square pos sz path s) = Square pos sz path $ not s

-- returns True if the point lies within the sprite's area
within :: GL.Position -> Sprite -> Bool
within (GL.Position ppx ppy) s = (sx >= px - sz) && (sx <= px + sz)
				&& (sy >= py - sz) && (sy <= py + sz)
				where
				sx = conv csx; sy = conv csy; px = conv ppx; py = conv ppy
				(GL.Position csx csy) = currentPos s
				sz = size s

-- return a list of four vertices for a rectangle, given position and size
vertexRect :: (GL.GLdouble,GL.GLdouble) -> GL.GLdouble -> GL.GLdouble
	-> [GL.Vertex2 GL.GLdouble]
vertexRect (x,y) width height =
	[(GL.Vertex2 (x-w) (y+h))
	, (GL.Vertex2 (x-w) (y-h))
	, (GL.Vertex2 (x+w) (y-h))
	, (GL.Vertex2 (x+w) (y+h))]
	where
	w = width / 2
	h = height / 2
	--gld z = z :: GL.GLdouble

-- same thing with different point format
vertexRect' :: GL.Position -> GL.GLdouble -> GL.GLdouble -> [GL.Vertex2 GL.GLdouble]
vertexRect' (GL.Position x y) w h = vertexRect (conv x,conv y) w h

-- generalize an Integral
conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger
