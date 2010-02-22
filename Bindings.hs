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
userAction (Env t sprites) (GL.Char 'a') GL.Down = Env t s
	where
	s = (Square (GL.Position newPos newPos) [] False):sprites
	newPos = (fromIntegral $ length sprites + 10) :: GL.GLint

userAction e _ _ = e

mouseMotion (Env t s) pos = Env t $ map updateSprite s
	where
	updateSprite q = if sticky q
			then (Square pos [] True)
			else q

passiveMouseMotion (Env t s) pos = Env t s


-- TODO :P
($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
