module Bindings (
    keyboardMouse,
    motion,
) where
import Data.IORef
import Data.List ((\\))
import Graphics.UI.GLUT

import Menu
import Rectangle
import State
import Sprite

keyboardMouse :: Window -> IORef Env -> Key -> KeyState -> Modifiers -> Position
                -> IO ()
keyboardMouse _ env key state _ _ = do
  e <- get env

  -- TODO: put these modes in different files, maybe
  let
    dispatchAction = case (mode$vars$e) of
      Animator -> animatorAction
      Intro -> introAction
      Play -> playAction
      Record -> recordAction

  env $= dispatchAction e key state

motion :: IORef Env -> Position -> IO ()
motion env pos = do
  e <- get env

  let
    dispatchMotion = case (mode$vars$e) of
      Animator -> animatorMotion
      Intro -> introMotion
      Play -> playMotion
      Record -> recordMotion

  env $= dispatchMotion e pos


{-
 - keyboard/mouse buttons
 -}

animatorAction, introAction, playAction, recordAction :: Env -> Key -> KeyState -> Env
-- place a sprite
animatorAction e (MouseButton RightButton) Down =
  e { sprites = ( (makeSprite (mousePos$vars$e)) { spritePath = mkPath } )
                : sprites e }
    where
      -- Make the initial animation path for this sprite, using (1000,1000) as
      -- a dummy value for "not visible"
      mkPath :: [Position]
      mkPath = replicate (animClock$vars$e) (Position 1000 1000) ++ [(mousePos$vars$e)]

animatorAction e (MouseButton LeftButton) Down =
  --
  -- TODO: just look at this mess!
  --
  e { sprites = (updateSelected . updateDragged) (sprites e),
      vars = (vars e) { menu = buttonMap updateButton (menu$vars$e) }
    }
    where
      updateSelected :: [Sprite] -> [Sprite]
      updateSelected ss = map (\s -> s {selected=True}) (spriteUnder ss) ++
                          map (\s -> s {selected=False}) (ss \\ (spriteUnder ss))

      updateDragged :: [Sprite] -> [Sprite]
      updateDragged ss = map (initDragSprite mp) (spriteUnder ss) ++
                         (ss \\ spriteUnder ss)

      spriteUnder :: [Sprite] -> [Sprite]
      spriteUnder ss = oneOrNone $ filter ((within mp) . rectangle) ss

      updateButton :: MmaButton -> MmaButton
      updateButton b = b { buttonState = if within mp (buttonRect b)
                                         then not $ buttonState b
                                         else buttonState b }

      updateWindow :: MmaWindow -> MmaWindow
      updateWindow w = w

      -- stupid... Maybe I should learn how to use Maybe?
      oneOrNone :: [a] -> [a]
      oneOrNone x = if length x >= 1 then [head x] else []

      mp :: Position
      mp = mousePos $ vars $ e

animatorAction e (MouseButton LeftButton) Up =
  e { sprites = unsticky (sprites e),
      vars = (vars e) { menu = buttonMap deactButton (menu$vars$e) }
    }
    where
      unsticky :: [Sprite] -> [Sprite]
      unsticky = map (\s -> s {sticky = False})

      deactButton :: MmaButton -> MmaButton
      deactButton b = b { buttonState = False }

animatorAction e _ _ = e

introAction (Env v s) (MouseButton _) Down =
  Env (v {mode = Animator}) s
introAction e _ _ = e

playAction e _ _ = e

recordAction e _ _ = e

{-
 - mouse motion (mouse button held down)
 -}

-- drag a sprite
animatorMotion, introMotion, playMotion, recordMotion :: Env -> Position -> Env
animatorMotion (Env v s) p =
  -- update mouse position and any sticky sprites
  Env v{mousePos = p} $ map (\x -> if sticky x then dragSprite p x else x) s

introMotion e _ = e

playMotion e _ = e

recordMotion (Env v s) p =
  -- update mouse position and any sticky sprites
  Env v{mousePos = p}
  $ map (\x -> if sticky x then dragSpriteUpdatingPath p x else x) s