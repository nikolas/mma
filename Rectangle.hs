module Rectangle (
  Rectangle(..),
  boxAroundRect,
  vertexRect,
  within,
) where
import Graphics.UI.GLUT

import Util

data Rectangle =
  Rectangle
  {
    rectX :: GLdouble,
    rectY :: GLdouble,
    rectWidth :: GLdouble,
    rectHeight :: GLdouble
  } deriving (Show, Eq)

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

boxAroundRect :: Rectangle -> GLdouble -> Rectangle
boxAroundRect r i = r {
                        rectWidth = rectWidth r + i,
                        rectHeight = rectHeight r + i 
                      }

-- returns True if the point lies within the rect's area
within :: Position -> Rectangle -> Bool
within (Position px py) (Rectangle rx ry rw rh) =
  (x <= (rx+(rw/2))) && (x >= (rx-(rw/2)))
  && (y <= (ry+(rh/2))) && (y >= (ry-(rh/2)))
    where
      x = conv px
      y = conv py
