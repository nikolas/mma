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
    drawMenu (menu $ vars $ e) t

drawIntro :: Env -> MmaTextures -> IO ()
drawIntro e t = do
  (_, Size _ h) <- get viewport
  let y = conv $ (conv h) -
          (((clock $ vars $ e) `div` 10) `mod` (conv h*2))
  drawTexture 0 0 (introTexture t) 0.25
  drawTexture 0 y (introTexture t) 1


drawSprite :: Sprite -> IO ()
drawSprite s = renderPrimitive Quads $ mapM_ vertex $ spritePoints s

drawMenu :: MmaMenu -> MmaTextures -> IO ()
drawMenu m t = do
  drawTexture 0 0 (menuTexture t) 1
  drawButton (playMmaButton m) (playTexture t)

drawButton :: MmaButton -> MmaTexture -> IO ()
drawButton (MmaButton r _) tex = do
  drawTexture (rectX r) (rectY r) tex 1
