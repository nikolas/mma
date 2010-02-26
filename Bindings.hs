module Bindings (
	motion,
	passiveMotion,
	processEnv,
) where

import Data.IORef
import Graphics.UI.GLUT

import State

motion :: IORef Env -> Position -> IO ()
motion env pos = do
	e <- get env
	--pos $= oglToGlut pos
	print $ sprites e
	env $= mouseMotion e pos

passiveMotion :: IORef Env -> Position -> IO ()
passiveMotion env pos = do
	e <- get env
	--pos $= oglToGlut pos
	env $= passiveMouseMotion e pos

processEnv :: Key -> Env -> Env
processEnv k e = case k of
	(MouseButton LeftButton) 
		-> e { sprites = (Square (mousePos $ vars $ e) [] False) : sprites e }
	_ -> e

-- start dragging a sprite
--userAction (Env v sprts)
--	(MouseButton LeftButton) _ = Env v $
--		map toggleSticky (filter (isMouseOverSprite v) sprts)
--		++ filter (not . isMouseOverSprite v) sprts


mouseMotion :: Env -> Position -> Env
mouseMotion (Env v s) pos = Env v{mousePos = pos}
	$ map updateSprite s
	where
	-- drag a sprite
	updateSprite q =
		if sticky q
		then q{ currentPos = pos }
		else q

passiveMouseMotion :: Env -> Position -> Env
passiveMouseMotion (Env v s) pos = Env v{mousePos = pos} s
