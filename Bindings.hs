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
	print $ clock $ vars $ e
	env $= userAction e key state
motion env pos = do
	e <- GL.get env
	--pos $= oglToGlut pos
	print $ sprites e
	env $= mouseMotion e pos
passiveMotion env pos = do
	e <- GL.get env
	--pos $= oglToGlut pos
	env $= passiveMouseMotion e pos
--__----__----__----__----__----__----__----__----__----__----__----__--

-- OpenGL and GLUT don't use the same co-ordinates, lol
--oglToGlut :: GL.Position -> GL.Position
--oglToGlut (GL.Position x y) = GL.Position (x - (640/2)) (y - (480-2))

-- add a sprite
userAction (Env v sprites) (GL.Char 'a') GL.Down = Env v s
	where
	s = (Square (GL.Position x y) [] False):sprites
	-- get random new position from clock
	-- yeah...... doesn't really work :P
	x = fromIntegral $ clock v `mod` 20 :: GL.GLint
	y = fromIntegral $ (clock v + 8) `mod` 20 :: GL.GLint

-- start dragging a sprite
userAction (Env v sprites)
	(GL.MouseButton GL.LeftButton) _ = Env v $
		map toggleSticky (filter (isMouseOverSprite v) sprites)
		++ filter (not . isMouseOverSprite v) sprites

userAction e _ _ = e

mouseMotion (Env v s) pos = Env (setMousePos pos v)
	$ map updateSprite s
	where
	-- drag a sprite
	updateSprite q =
		if sticky q
		then setSpritePos pos q
		else q

passiveMouseMotion (Env v s) pos = Env (setMousePos pos v) s


-- TODO :P
($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
