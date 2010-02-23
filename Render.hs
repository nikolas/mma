module Render (
	world
) where
import qualified Graphics.UI.GLUT as GL

import MetaGL
import State


world (Env v s) = serial $ concat $ renderMenu v : map renderSprite s

renderSprite :: Sprite -> [GLC]
renderSprite s = [
	identity,
	translate x y dist,
	quads [ color 1 0 0,
		vertex 0 1,
		vertex 0 0,
		vertex 1 0,
		vertex 1 1 ]
	]
	where
	x = fromIntegral p :: GL.GLdouble
	y = fromIntegral q :: GL.GLdouble
	(GL.Position p q) = currentPos s

-- TODO: make some images to put in these quads
renderMenu :: Vars -> [GLC]
renderMenu v = concat $ map renderButton [0 .. 5]
	where
	m = menu v

renderButton :: Int -> [GLC]
renderButton i = [
	identity,
	translate (x!!i) (-20) dist,
	quads [ color 0.8 0 0.5,
		vertex 0 5,
		vertex 0 0,
		vertex 9 0,
		vertex 9 5 ]
	]
	where
	x = [-30,-20 ..]


-- distance from camera
dist = (-50)
