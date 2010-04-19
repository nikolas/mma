module Bindings (
    keyboardMouse,
    motion,
) where
import Data.IORef
import Data.List ((\\))
import Graphics.UI.GLUT

import Rectangle
import State
import Sprite

keyboardMouse :: Window -> IORef Env -> Key -> KeyState -> Modifiers -> Position
                 -> IO ()
keyboardMouse _ env key state _ _ = do
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
  e { sprites = (updateSelected . updateDragged) (sprites e) }
      -- TODO: map over MmaMenu?
      --vars { menu 
    where
      -- TODO: just look at this mess!
      updateSelected :: [Sprite] -> [Sprite]
      updateSelected ss = map (\s -> s {selected=True}) (spriteUnder ss) ++
                          map (\s -> s {selected=False}) (ss \\ (spriteUnder ss))

      updateDragged :: [Sprite] -> [Sprite]
      updateDragged ss = map (initDragSprite mp) (spriteUnder ss) ++
                         (ss \\ spriteUnder ss)

      spriteUnder :: [Sprite] -> [Sprite]
      spriteUnder ss = oneOrNone $ filter ((within mp) . rectangle) ss

      -- stupid... Maybe I should learn how to use Maybe?
      oneOrNone :: [a] -> [a]
      oneOrNone x = if length x >= 1 then [head x] else []

      mp :: Position
      mp = mousePos $ vars $ e

animatorAction e (MouseButton LeftButton) Up =
  e { sprites = unsticky (sprites e) }
    where
      unsticky :: [Sprite] -> [Sprite]
      unsticky = map (\s -> s {sticky = False})

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
