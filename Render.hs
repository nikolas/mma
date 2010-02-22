module Render (
	world
) where
import qualified Graphics.UI.GLUT as GL

import MetaGL
import State


world (Env v ss) = serial $ concat $ map renderSprite ss

renderSprite :: Sprite -> [GLC]
renderSprite s = [
	identity,
	translate (0 - x) 0 (0 - y),
	translate 0 0 (-4),
	quads [ color 1 0 0,
		vertex 0 0 0,
		vertex 1 0 0,
		vertex 0 1 0,
		vertex 1 1 0 ]
	]
	where
	x = fromIntegral p :: GL.GLdouble
	y = fromIntegral q :: GL.GLdouble
	(GL.Position p q) = currentPos s
