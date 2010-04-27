module Sprite (
  Sprite(..),
  makeSprite,
  spritePoints,
  selectPoints,
  initDragSprite,
  dragSprite,
  dragSpriteUpdatingPath,
) where
import Graphics.UI.GLUT

import Rectangle
import Util

data Sprite =
  Sprite {
    rectangle :: Rectangle,

    -- recorded path
    spritePath :: [Position],

    -- is it being dragged?
    sticky :: Bool,

    -- is it selected?
    selected :: Bool,

    -- mouse offset for dragging
    offset :: Pos
    } deriving (Show, Eq)

makeSprite :: Position -> Sprite
makeSprite (Position x y) =
  Sprite (Rectangle (conv x) (conv y) 20 20) [] False False (0,0)

spritePoints :: Sprite -> [Vertex2 GLdouble]
spritePoints s = vertexRect $ rectangle s

-- calculate a rectangle around the sprite
selectPoints :: Sprite -> [Vertex2 GLdouble]
selectPoints s = vertexRect $ boxAroundRect (rectangle s) 4.0

-- start dragging a sprite
initDragSprite :: Position -> Sprite -> Sprite
initDragSprite offs s =
  s { sticky = True, offset = posOp (-) myPos (posConv offs) }
    where
      myPos = ( (rectX$rectangle$s),(rectY$rectangle$s) )

dragSprite :: Position -> Sprite -> Sprite
dragSprite p s = s{ rectangle = newRect }
  where
    newRect :: Rectangle
    newRect = (rectangle s){rectX = newX, rectY = newY}

    (newX,newY) = posOp (+) (posConv p) (offset s)

dragSpriteUpdatingPath :: Position -> Sprite -> Sprite
dragSpriteUpdatingPath p s = dragSprite p s { spritePath = spritePath s ++ [p] }