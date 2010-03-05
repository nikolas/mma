module Sprite (
  Sprite(..),
  Rectangle(..),
  makeSprite,
  spritePoints,
  selectSprite,
  dragSprite,
  within,

  vertexRect,
) where
import Graphics.UI.GLUT

import Util

data Sprite =
  Sprite {
    rectangle :: Rectangle,
    -- recorded path
    spritePath :: [Position],
    -- is it being dragged?
    sticky :: Bool,
    -- mouse offset for dragging
    offset :: Pos
    } deriving (Show, Eq)

data Rectangle =
  Rectangle
  {
    rectX :: GLdouble,
    rectY :: GLdouble,
    rectWidth :: GLdouble,
    rectHeight :: GLdouble
  } deriving (Show, Eq)

makeSprite :: Position -> Sprite
makeSprite (Position x y) =
  Sprite (Rectangle (conv x) (conv y) 20 20) [] False (0,0)

spritePoints :: Sprite -> [Vertex2 GLdouble]
spritePoints s = vertexRect (rectangle s)

selectSprite :: Position -> Sprite -> Sprite
selectSprite offs s =
  s { sticky = True, offset = posOp (-) myPos (posConv offs) }
    where
      myPos = ( (rectX$rectangle$s),(rectY$rectangle$s) )

dragSprite :: Position -> Sprite -> Sprite
dragSprite p s =
  if sticky s
    then s{ rectangle = newRect }
    else s
      where
        newRect :: Rectangle
        newRect = (rectangle s){rectX = newX, rectY = newY}

        (newX,newY) = posOp (+) (posConv p) (offset s)

-- returns True if the point lies within the sprite's area
within :: Position -> Sprite -> Bool
within (Position px py) (Sprite (Rectangle rx ry rw rh) _ _ _) =
  (x <= (rx+(rw/2))) && (x >= (rx-(rw/2)))
  && (y <= (ry+(rh/2))) && (y >= (ry-(rh/2)))
    where
      x = conv px
      y = conv py

-- return a list of four vertices for a rectangle, given position and size
vertexRect :: Rectangle -> [Vertex2 GLdouble]
vertexRect (Rectangle x y width height) =
    [(Vertex2 (x-w) (y+h))
    , (Vertex2 (x-w) (y-h))
    , (Vertex2 (x+w) (y-h))
    , (Vertex2 (x+w) (y+h))]
    where
    w = width / 2
    h = height / 2
