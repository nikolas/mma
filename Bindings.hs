module Bindings (
	keyboardMouse,
	motion,
) where

import Data.IORef
import Data.List
import Graphics.UI.GLUT

import Sprite
import State

keyboardMouse _ env key state modifiers pos = do
	e <- get env
	--print $ sprites e
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
