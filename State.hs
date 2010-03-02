module State (
	Env(..),
	initialEnvironment,

	Vars(..),

	MmaMenu(..),
	MmaButton(..),

	Sprite(..),
	makeSprite,
	spritePoints,
	toggleSticky,
	within,

	MmaTexture(..),
	loadTexture,
	freeTexture,
	drawTexture,
	drawTextureFlip,

	-- misc functions that belong.... elsewhere???
	Rectangle(..),
	vertexRect,
	vertexRect',
	conv,
) where
import Control.Monad
import Graphics.UI.GLUT
import Graphics.UI.SDL.Image as SDLImage
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video (freeSurface)

data Env = Env
	{
		vars :: Vars,
		sprites :: [Sprite]
	} deriving Show

initialEnvironment :: Env
initialEnvironment = Env
	( Vars 0 (Position 0 0) False False initialMenu )
	[ ]

data Vars = Vars
	{
		clock :: Int,
		mousePos :: Position,
		playing :: Bool,
		recording :: Bool,
		menu :: MmaMenu
	} deriving Show

data MmaMenu = MmaMenu
	{
		playMmaButton :: MmaButton
		{-
		recMmaButton :: MmaButton,

		-- stepper
		prevFrameMmaButton :: MmaButton,
		nextFrameMmaButton :: MmaButton,

		-- sprite chooser
		prevSpriteMmaButton :: MmaButton,
		nextSpriteMmaButton :: MmaButton
		-}
	} deriving Show

initialMenu :: MmaMenu
initialMenu = MmaMenu b
	where
	b = MmaButton (Rectangle 20 20 50 70) False

data MmaButton = MmaButton
	{
		buttonRect :: Rectangle,

		--buttonTex :: MmaTexture,

		buttonState :: Bool
	} deriving Show

data Sprite =
	Square {
		currentPos :: Position,

		size :: GLdouble,

		-- recorded path
		spritePath :: [Position],

		-- is it being dragged?
		sticky :: Bool
	} deriving (Show, Eq)

makeSprite :: Position -> Sprite
makeSprite pos = Square pos 10 [] False

spritePoints :: Sprite -> [Vertex2 GLdouble]
spritePoints s = vertexRect' (currentPos s) 10 10

toggleSticky :: Sprite -> Sprite
toggleSticky (Square pos sz path s) = Square pos sz path $ not s

-- returns True if the point lies within the sprite's area
within :: Position -> Sprite -> Bool
within (Position ppx ppy) s = (sx >= px - sz) && (sx <= px + sz)
				&& (sy >= py - sz) && (sy <= py + sz)
				where
				sx = conv csx; sy = conv csy; px = conv ppx; py = conv ppy
				(Position csx csy) = currentPos s
				sz = size s

data MmaTexture = MmaTexture
	{
		textureWidth :: GLsizei,
		textureHeight :: GLsizei,
		-- textures have to be in the IO monad
		textureObject :: TextureObject
	} deriving Show

loadTexture :: String -> IO MmaTexture
loadTexture filepath = do
	surface <- SDLImage.loadTyped filepath SDLImage.PNG

	let width = fromIntegral (surfaceGetWidth surface)
	let height = fromIntegral (surfaceGetHeight surface)
	let surfaceSize = TextureSize2D width height

	textureObj <- liftM head (genObjectNames 1)
	textureBinding Texture2D $= Just textureObj
	textureWrapMode Texture2D S $= (Repeated, Repeat)
	textureWrapMode Texture2D T $= (Repeated, Repeat)
	textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
	surfacePixels <- surfaceGetPixels surface

	let pixelData = PixelData RGBA UnsignedByte surfacePixels
	texImage2D Nothing NoProxy 0 RGBA' surfaceSize 0 pixelData

	--freeSurface surface

	return (MmaTexture width height textureObj)

freeTexture :: MmaTexture -> IO ()
freeTexture tex = do
	deleteObjectNames ([textureObject tex])

drawTexture :: GLdouble -> GLdouble -> MmaTexture -> GLdouble -> IO ()
drawTexture x y tex alpha = do
	drawTextureFlip x y tex alpha False

drawTextureFlip :: GLdouble -> GLdouble -> MmaTexture -> GLdouble -> Bool -> IO ()
drawTextureFlip x y tex alpha flp = do
	texture Texture2D $= Enabled
	textureBinding Texture2D $= Just (textureObject tex)

	let
		texWidth = fromIntegral $ textureWidth tex
		texHeight = fromIntegral $ textureHeight tex

		texCoord2f = texCoord :: TexCoord2 GLdouble -> IO ()
		vertex3f = vertex :: Vertex3 GLdouble -> IO ()
		color4f = color :: Color4 GLdouble -> IO ()
		col = color4f (Color4 (1.0::GLdouble) (1.0::GLdouble) (1.0::GLdouble) alpha)

		texCoordX = if flp then (-1) else 1

	renderPrimitive Quads $ do
		texCoord2f (TexCoord2 0 1); vertex3f (Vertex3 x y 0.0); col
		texCoord2f (TexCoord2 0 0); vertex3f (Vertex3 x (y + texHeight) 0.0); col
		texCoord2f (TexCoord2 texCoordX 0); vertex3f (Vertex3 (x + texWidth) (y + texHeight) 0.0); col
		texCoord2f (TexCoord2 texCoordX 1); vertex3f (Vertex3 (x + texWidth) y 0.0); col

	texture Texture2D $= Disabled

data Rectangle = Rectangle
	{
		rectX :: GLdouble,
		rectY :: GLdouble,
		rectWidth :: GLdouble,
		rectHeight :: GLdouble
	} deriving (Show, Eq)

-- return a list of four vertices for a rectangle, given position and size
vertexRect :: (GLdouble,GLdouble) -> GLdouble -> GLdouble
	-> [Vertex2 GLdouble]
vertexRect (x,y) width height =
	[(Vertex2 (x-w) (y+h))
	, (Vertex2 (x-w) (y-h))
	, (Vertex2 (x+w) (y-h))
	, (Vertex2 (x+w) (y+h))]
	where
	w = width / 2
	h = height / 2

-- same thing with different point format
vertexRect' :: Position -> GLdouble -> GLdouble -> [Vertex2 GLdouble]
vertexRect' (Position x y) w h = vertexRect (conv x,conv y) w h

-- generalize an Integral
conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger
