module Render (
	drawWorld,
) where
import Graphics.UI.GLUT (($=))
import qualified Graphics.UI.GLUT as GL

import State


{-world :: Env -> GLC
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
renderMenu _ = concat $ map renderButton [0 .. 5]

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
	x = [-30,-20 ..]-}

drawWorld :: Env -> IO ()
drawWorld e = do
	GL.currentColor $= GL.Color4 1 0.25 0.5 0
	mapM_ (drawSprite) $ sprites e

drawSprite :: Sprite -> IO ()
drawSprite s = GL.renderPrimitive GL.Quads $ mapM_ GL.vertex $ spritePoints s
	where
	squarePoints :: [GL.Vertex2 GL.GLdouble]
	squarePoints =
		[(GL.Vertex2 0 sz)
		, (GL.Vertex2 0 0)
		, (GL.Vertex2 sz 0)
		, (GL.Vertex2 sz sz)]

	sz = size s
