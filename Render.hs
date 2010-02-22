module Render (
	world
) where
import qualified Graphics.UI.GLUT as GL

import MetaGL
import State


world (Env v sprites) = parallel $ concat $ (renderMenu v : map renderSprite sprites)

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

-- TODO: make some images to put in these quads
-- TODO: learn how to use quads
renderMenu :: Vars -> [GLC]
renderMenu v = [
	identity,
	translate (0 - 0) 0 (0 - 440),
	translate 0 0 (-4),
	quads [ color 1 0 1,
		vertex 0 0 0,
		vertex 1 0 0,
		vertex 0 1 0,
		vertex 1 1 0 ],
	identity,
	translate (0 - 40) 0 (0 - 440),
	translate 0 0 (-4),
	quads [ color 1 0 1,
		vertex 0 0 0,
		vertex 1 0 0,
		vertex 0 1 0,
		vertex 1 1 0 ],
	identity,
	translate (0 - 80) 0 (0 - 440),
	translate 0 0 (-4),
	quads [ color 1 0 1,
		vertex 0 0 0,
		vertex 1 0 0,
		vertex 0 1 0,
		vertex 1 1 0 ]
	]
	where
	m = menu v
