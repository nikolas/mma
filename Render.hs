module Render (
	drawWorld,
) where
import Graphics.UI.GLUT

import State

drawWorld :: Env -> IO ()
drawWorld e = do
	currentColor $= Color4 0.2 0 0.3 0
	mapM_ (drawSprite) $ sprites e
	currentColor $= Color4 0.8 0.1 0.65 0
	drawMenu (menu $ vars $ e)

drawSprite :: Sprite -> IO ()
drawSprite s = renderPrimitive Quads $ mapM_ vertex $ spritePoints s

drawMenu :: MmaMenu -> IO ()
drawMenu _ = mapM_ drawButton [0..5]

drawButton :: Int -> IO ()
drawButton i = do
	--loadIdentity
	renderPrimitive Quads $ mapM_ vertex $ vertexRect (conv i*85+60,50) 80 55
