module Util (
    conv,
    posConv,
    posOp,
    Pos,
    MmaTexture(..),
    MmaTextures(..),
    Rectangle(..),
) where
import Graphics.UI.GLUT

-- generalize an Integral
conv :: (Integral a, Num b) => a -> b
conv = fromInteger . toInteger


{- TODO: GL uses `Position' but I don't know how to get the x and y values out
 - of a Position without pattern matching, because it's defined as:
 -    data Position = Position !GLint !GLint
 - So I made a tuple type for co-ordinates but I'm worried it's unnecessary and
 - it's annoying to have another type to worry about converting to and from.
 -}

type Pos = (GLdouble,GLdouble)

-- convert from a GL Position
posConv :: Position -> Pos
posConv (Position x y) = (conv x,conv y)

-- apply a binary function on a Pos
posOp :: (GLdouble -> GLdouble -> GLdouble) -> Pos -> Pos -> Pos
posOp op (a,b) (x,y) = (a `op` x, b `op` y)

data Rectangle =
  Rectangle
  {
    rectX :: GLdouble,
    rectY :: GLdouble,
    rectWidth :: GLdouble,
    rectHeight :: GLdouble
  } deriving (Show, Eq)

data MmaTexture = MmaTexture
                  {
                    textureWidth :: GLsizei,
                    textureHeight :: GLsizei,
                    textureObject :: TextureObject
                  } deriving Show

data MmaTextures = MmaTextures
                   {
                     introTexture :: MmaTexture,
                     menuTexture :: MmaTexture,
                     playButtonTexture :: MmaTexture,
                     saveButtonTexture :: MmaTexture,
                     nextSprtButtonTexture :: MmaTexture,
                     prevSprtButtonTexture :: MmaTexture,
                     nextBgButtonTexture :: MmaTexture,
                     prevBgButtonTexture :: MmaTexture,
                     nextFrameButtonTexture :: MmaTexture,
                     prevFrameButtonTexture :: MmaTexture
                   } deriving Show
