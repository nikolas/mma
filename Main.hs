import Data.IORef
import Graphics.UI.GLUT

import Animator
import Graphics
import State

main :: IO ()
main = do
	-- make pointer to world state
	env <- newIORef initialEnvironment

	-- make the GL window
	initialWindowSize $= Size 640 480
	(_,_) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered]
	wnd <- createWindow "Marlon Moonglow's Animator"

	-- textures need to be in the IO monad, so they aren't part of the Env
	-- textures <- initTextures
	textures <- loadTexture "play.png"

	intro
	animator env textures wnd

intro :: IO ()
intro = do
	print "hi"
	{-displayCallback $= (glRunAs2D $ do
		clearColor $= Color4 1 0 1 1
		clear [ColorBuffer, DepthBuffer]
		e <- readIORef env
		drawWorld e textures
		--readIORef env >>= drawWorld
		flush
		swapBuffers)-}

	--keyboardMouseCallback $= Just (animator)

