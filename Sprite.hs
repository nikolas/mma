module Sprite (
    Sprite(..),
    makeSprite,
    spritePoints,
    toggleSticky,
    vertexRect,
    vertexRect',
    within,
) where
import Graphics.UI.GLUT

import Util

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

-- returns True if the point lies within the sprite's area
within :: Position -> Sprite -> Bool
within (Position ppx ppy) s = (sx >= px - sz) && (sx <= px + sz)
                && (sy >= py - sz) && (sy <= py + sz)
                where
                sx = conv csx; sy = conv csy; px = conv ppx; py = conv ppy
                (Position csx csy) = currentPos s
                sz = size s
