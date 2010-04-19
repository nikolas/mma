module Rectangle (
  Rectangle(..),
  boxAroundRect,
  vertexRect,
) where
import Graphics.UI.GLUT

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
