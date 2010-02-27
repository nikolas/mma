module Bindings (
	keyboardMouse,
	motion,
) where

import Data.IORef
import Graphics.UI.GLUT

import State

keyboardMouse _ env key state modifiers pos = do
	e <- get env
	print $ sprites e
	env $= userAction e key state

motion :: IORef Env -> Position -> IO ()
motion env pos = do
	e <- get env
	env $= mouseMotion e pos


userAction :: Env -> Key -> KeyState -> Env

-- place a sprite
userAction e (MouseButton RightButton) Down =
	e { sprites = (makeSprite (mousePos $ vars $ e))  : sprites e }

-- start dragging a sprite
userAction e (MouseButton LeftButton) Down =
	let pos = (mousePos $ vars $ e) in
		e { sprites =
			(map toggleSticky (filter (isMouseOverSprite pos) (sprites e))
			++ filter (not . isMouseOverSprite pos) (sprites e))
		}

userAction e _ _ = e


mouseMotion :: Env -> Position -> Env
mouseMotion (Env v s) pos = Env v{mousePos = pos}
	$ map updateSprite s
	where
	-- drag a sprite
	updateSprite q =
		if sticky q
		then q{ currentPos = pos }
		else q
