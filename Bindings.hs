module Bindings (
	keyboardMouse,
	motion,
) where
import Data.IORef
import Data.List (delete)
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
 - keyboar/mouse buttons
 -}

introAction :: Env -> Key -> KeyState -> Env
introAction (Env v s) (MouseButton _) Down =
	Env (v {mode = Animator}) s
introAction e _ _ = e

animatorAction :: Env -> Key -> KeyState -> Env
-- place a sprite
animatorAction e (MouseButton RightButton) Down =
	e { sprites = (makeSprite (mousePos $ vars $ e))  : sprites e }

-- start dragging a sprite
animatorAction e (MouseButton LeftButton) Down =
	if length spritesWithin > 0
		then e { sprites =
			(toggleSticky selected) : (delete selected (sprites e))
		}
		else e
	where
	pos :: Position
	pos = (mousePos $ vars $ e)

	selected :: Sprite
	selected = head spritesWithin

	spritesWithin :: [Sprite]
	spritesWithin = filter (within pos) (sprites e)
animatorAction e _ _ = e


{-
 - mouse motion
 -}

introMotion :: Env -> Position -> Env
introMotion e _ = e

animatorMotion :: Env -> Position -> Env
animatorMotion (Env v s) pos = Env v{mousePos = pos}
	$ map updateSprite s
	where
	-- drag a sprite
	updateSprite q =
		if sticky q
		then q{ currentPos = pos }
		else q
