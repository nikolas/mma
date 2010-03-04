module Graphics (
	glRunAs2D,
	loadAllTextures,

	loadTexture,
	freeTexture,
	drawTexture,
	drawTextureFlip,
) where

import Control.Monad
import Graphics.UI.GLUT
import Graphics.UI.SDL.Image as SDLImage
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video (freeSurface)

import State
import Util

glRunAs2D :: IO () -> IO ()
glRunAs2D draw = do
	matrixMode $= Modelview 0
	loadIdentity

	matrixMode $=  Projection
	loadIdentity

	(_, Size w h) <- get viewport

	ortho 0 (conv w) 0 (conv h) (-1000) 1000

	preservingMatrix draw

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

	freeSurface surface

	return (MmaTexture width height textureObj)

loadAllTextures :: IO MmaTextures
loadAllTextures = do
	introtex <- loadTexture "intro.png"
	playtex <- loadTexture "play.png"

	return ( MmaTextures {
		introTexture = introtex,
		playTexture = playtex
		} )

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
