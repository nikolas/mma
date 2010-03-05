module Bindings (
    keyboardMouse,
    motion,
) where
import Data.IORef
import Data.List ((\\))
import Graphics.UI.GLUT

import State
import Sprite

keyboardMouse _ env key state modifiers pos = do
  e <- get env

  let
    dispatchAction = case (mode $ vars $ e) of
      Intro -> introAction
      Animator -> animatorAction

  env $= dispatchAction e key state

motion :: IORef Env -> Position -> IO ()
motion env pos = do
  e <- get env

  let
    dispatchMotion = case (mode $ vars $ e) of
      Intro -> introMotion
      Animator -> animatorMotion

  env $= dispatchMotion e pos


{-
 - keyboard/mouse buttons
 -}

introAction :: Env -> Key -> KeyState -> Env
introAction (Env v s) (MouseButton _) Down =
    Env (v {mode = Animator}) s
introAction e _ _ = e

animatorAction :: Env -> Key -> KeyState -> Env
-- place a sprite
animatorAction e (MouseButton RightButton) Down =
    e { sprites = (makeSprite (mousePos $ vars $ e))  : sprites e }


animatorAction e (MouseButton LeftButton) Down =
  e { sprites = (map (selectSprite mp) spritesUnder ++ theRest) }
    where
      mp :: Position
      mp = mousePos $ vars $ e

      spritesUnder :: [Sprite]
      spritesUnder = filter (within mp) (sprites e)

      theRest = (sprites e) \\ spritesUnder

animatorAction e (MouseButton LeftButton) Up =
  Env (vars e) $ map (\s -> s {sticky = False}) (sprites e)

animatorAction e _ _ = e


{-
 - mouse motion
 -}

introMotion :: Env -> Position -> Env
introMotion e _ = e

-- drag a sprite
animatorMotion :: Env -> Position -> Env
animatorMotion (Env v s) p =
  -- update mouse position and any sticky sprites
  Env v{mousePos = p} $ map (dragSprite p) s
