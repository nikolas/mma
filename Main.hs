-- let's copy (some of) Raincat for some basic gfx....

import Graphics.UI.GLUT
import System.Exit

main :: IO ()
main = do
	initWindow (Size (truncate 800.0) (truncate 600.0)) "mma"
	mainLoop

exitMain :: IO ()
exitMain = do
	exitSuccess


initWindow :: Size -> [Char] -> IO ()
initWindow windowSize windowTitle = do
	getArgsAndInitialize

	initialWindowSize $= windowSize
	initialDisplayMode $= [DoubleBuffered]

	createWindow windowTitle

	return ()
