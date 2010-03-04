module State (
	Env(..),
	initialEnvironment,

	Vars(..),

	MmaMenu(..),
	MmaButton(..),

	MmaTexture(..),

	-- misc functions that belong.... elsewhere???
	Rectangle(..),
) where
import Graphics.UI.GLUT

import Sprite

data Env = Env
	{
		vars :: Vars,
		sprites :: [Sprite]
	} deriving Show

initialEnvironment :: Env
initialEnvironment = Env
	( Vars (Position 0 0) False False initialMenu )
	[ ]

data Vars = Vars
	{
		mousePos :: Position,
		playing :: Bool,
		recording :: Bool,
		menu :: MmaMenu
	} deriving Show

data MmaMenu = MmaMenu
	{
		playMmaButton :: MmaButton
		{-
		recMmaButton :: MmaButton,

		-- stepper
		prevFrameMmaButton :: MmaButton,
		nextFrameMmaButton :: MmaButton,

		-- sprite chooser
		prevSpriteMmaButton :: MmaButton,
		nextSpriteMmaButton :: MmaButton
		-}
	} deriving Show

initialMenu :: MmaMenu
initialMenu = MmaMenu b
	where
	b = MmaButton (Rectangle 20 20 50 70) False

data MmaButton = MmaButton
	{
		buttonRect :: Rectangle,

		--buttonTex :: MmaTexture,

		buttonState :: Bool
	} deriving Show

data MmaTexture = MmaTexture
	{
		textureWidth :: GLsizei,
		textureHeight :: GLsizei,
		textureObject :: TextureObject
	} deriving Show

data Rectangle = Rectangle
	{
		rectX :: GLdouble,
		rectY :: GLdouble,
		rectWidth :: GLdouble,
		rectHeight :: GLdouble
	} deriving (Show, Eq)
