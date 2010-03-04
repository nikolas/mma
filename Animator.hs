module Animator (
	animator,
) where

import Data.IORef
import Graphics.UI.GLUT

import Bindings
import Graphics
import Render
import State
import Util

animator :: IORef Env -> MmaTexture -> Window -> IO ()
animator env textures wnd = do
	-- set up callbacks
	displayCallback $= (glRunAs2D $ do
		clearColor $= Color4 1 0 1 1
		clear [ColorBuffer, DepthBuffer]
		e <- readIORef env
		drawWorld e textures
		--readIORef env >>= drawWorld
		flush
		swapBuffers)

	idleCallback $= Just (idle env)

	let
		moveCursor p = do
			(Env v sp) <- readIORef env
			--print p
			writeIORef env $ Env v{mousePos = p} sp
		trans :: Position -> IO Position
		trans (Position x y) = do
			(_, Size _ h) <- get viewport
			return ( Position x (conv h - y) )

	keyboardMouseCallback $= Just (keyboardMouse wnd env)

	motionCallback $= Just (\pos ->
		trans pos >>= motion env)

	passiveMotionCallback $= Just (\pos ->
		trans pos >>= moveCursor >> postRedisplay Nothing)

	mainLoop

idle :: IORef Env -> IO ()
idle env = do
	e <- get env
	time <- get elapsedTime
	env $= tick time e
	postRedisplay Nothing

-- keep track of how much time has elapsed
tick :: Int -> Env -> Env
tick tnew (Env v sprs) = Env v sprs
	{-where
	s = map idleSprite sprs
	elapsed = fromIntegral $ tnew - clock v
	idleSprite z = z-}
