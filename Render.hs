module Render (
    drawWorld,
) where
import Graphics.UI.GLUT

import Graphics
import Sprite
import State
import Util

drawWorld :: Env -> MmaTextures -> IO ()
drawWorld e =
    case (mode $ vars $ e) of
        Intro -> do
            drawIntro e
        Animator -> do
            drawAnimator e

drawAnimator :: Env -> MmaTextures -> IO ()
drawAnimator e t = do
    currentColor $= Color4 0.2 0 0.3 0
    mapM_ (drawSprite) $ sprites e
    --currentColor $= Color4 0.8 0.1 0.65 0
    drawMenu (menu $ vars $ e) t

drawIntro :: Env -> MmaTextures -> IO ()
drawIntro e t = drawTexture 0 y (introTexture t) 1
    where
    y = (-) 480$ (conv $ clock $ vars $ e) / 10

drawSprite :: Sprite -> IO ()
drawSprite s = renderPrimitive Quads $ mapM_ vertex $ spritePoints s

drawMenu :: MmaMenu -> MmaTextures -> IO ()
--drawMenu _ = mapM_ drawButton [0..5]
drawMenu m t = drawButton (playMmaButton m) (playTexture t)

drawButton :: MmaButton -> MmaTexture -> IO ()
drawButton (MmaButton r _) tex = do
    drawTexture (rectX r) (rectY r) tex 1

    --loadIdentity
    --renderPrimitive Quads $ mapM_ vertex $ vertexRect (conv i*85+60,50) 80 55
