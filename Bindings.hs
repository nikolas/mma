module Bindings (
	keyboardMouse
) where

import Complex
import qualified Graphics.UI.GLUT as GL
import System.Exit

import State

keyboardMouse window _ (GL.Char '\ESC') GL.Down _ _ = do
	GL.destroyWindow window
	exitSuccess
keyboardMouse _ env key state modifiers position = do
	e <- GL.get env
	env $= userAction e key state

userAction (Env t sprites) (GL.Char 'a') GL.Down = Env t s
	where
	s = (Square (replicate 10 ((newP::GL.GLdouble):+(newP::GL.GLdouble)))):sprites
	newP = fromIntegral $ length sprites * 3
userAction e _ _ = e

-- TODO :P
($=) :: (GL.HasSetter s) => s a -> a -> IO ()
($=) = (GL.$=)
