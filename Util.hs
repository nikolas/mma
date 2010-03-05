module Util (
    conv,

    Pos,
    posConv,
    posOp,
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

-- apply a binary function over Pos
posOp :: (GLdouble -> GLdouble -> GLdouble) -> Pos -> Pos -> Pos
posOp op (a,b) (x,y) = (a `op` x, b `op` y)