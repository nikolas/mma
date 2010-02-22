module Bindings (
	keyboardMouse,
	motion,
	passiveMotion
) where

import qualified Graphics.UI.GLUT as GL
import System.Exit

import State

--__----__----__
-- callbacks
--__----__----__----_
keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
	GL.destroyWindow window
	exitSuccess
keyboardMouse _ env key state modifiers position = do
	e <- GL.get env
	env $= userAction e key state
motion env pos = do
	e <- GL.get env
	env $= mouseMotion e pos
passiveMotion env pos = do
	e <- GL.get env
	env $= passiveMouseMotion e pos
--__----__----__----__----__----__----__----__----__----__----__----__--


-- add a sprite
userAction (Env v sprites) (GL.Char 'a') GL.Down = Env v s
	where
	s = (Square (GL.Position newPos newPos) [] False):sprites
	newPos = (fromIntegral $ length sprites * 2) :: GL.GLint

-- drag a sprite
userAction (Env v sprites)
	(GL.MouseButton GL.LeftButton) _ = Env v $ map toggleSticky sprites

-- drag the camera
userAction (Env v sprites)
	(GL.MouseButton GL.RightButton) _ = Env v $ map toggleSticky sprites


userAction e _ _ = e

mouseMotion (Env v s) newpos = Env v $ map updateSprite s
	where
	updateSprite q =
		if sticky q
		then (Square newpos [] True)
		else q

passiveMouseMotion (Env v s) pos = Env v s


-- TODO :P
($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
