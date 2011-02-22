module Render (
  drawWorld,
) where
import Graphics.UI.GLUT

import Graphics
import Menu
import Rectangle
import Sprite
import State
import Util

drawWorld :: Env -> MmaTextures -> IO ()
drawWorld e =
  case (mode $ vars $ e) of
    Animator -> drawAnimator e
    Intro -> drawIntro e
    Play -> drawAnimator e
    Record -> drawAnimator e

drawAnimator :: Env -> MmaTextures -> IO ()
drawAnimator e t = do
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
drawSprite s = do
  if selected s
    then do
      currentColor $= Color4 0.85 0 0.4 0
      renderPrimitive Quads $ mapM_ vertex $ selectPoints s
    else return ()

  currentColor $= Color4 0.2 0 0.3 0
  renderPrimitive Quads $ mapM_ vertex $ spritePoints s

drawMenu :: MmaMenu -> MmaTextures -> IO ()
drawMenu m t = do
  drawButton (playButton m) (playButtonTexture t)
  drawButton (saveButton m) (saveButtonTexture t)
  drawButton (nextSprtButton m) (nextSprtButtonTexture t)
  drawButton (prevSprtButton m) (prevSprtButtonTexture t)
  drawButton (nextBgButton m) (nextBgButtonTexture t)
  drawButton (prevBgButton m) (prevBgButtonTexture t)
  drawButton (nextFrameButton m) (nextFrameButtonTexture t)
  drawButton (prevFrameButton m) (prevFrameButtonTexture t)

drawButton :: MmaButton -> MmaTexture -> IO ()
drawButton b tex = do
  -- draw selected button highlight
  if buttonState b
    then do
      currentColor $= Color4 0.85 0 0.4 0
      renderPrimitive Quads $ mapM_ vertex $ vertexRect $ selectButtonRect b
    else
      return ()

  currentColor $= Color4 0.4 0.4 0.8 0
  renderPrimitive Quads $ mapM_ vertex $ vertexRect $ buttonRect b
  --drawTexture (rectX (buttonRect b)) (rectY (buttonRect b)) tex 1
