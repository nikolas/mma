module Bindings (
	keyboardMouse,
	motion,
	passiveMotion
) where

import qualified Graphics.UI.GLUT as GL
import System.Exit

import State

--__----__----__
-- input callbacks
--__----__----__----_
keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
	GL.destroyWindow window
	exitWith ExitSuccess
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
	(GL.MouseButton GL.LeftButton) _ = Env v $
		map toggleSticky (filter (isMouseOverSprite v) sprites)
		++ filter (not . isMouseOverSprite v) sprites

userAction e _ _ = e

mouseMotion (Env v s) pos = Env (setMousePos pos v)
	$ map updateSprite s
	where
	updateSprite q =
		if sticky q
		then setSpritePos pos q
		else q

passiveMouseMotion (Env v s) pos = Env (setMousePos pos v) s


-- TODO :P
($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
