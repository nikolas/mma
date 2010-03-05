module State (
    Env(..),
    initialEnvironment,

    Vars(..),
    Mode(..),

    MmaMenu(..),
    MmaButton(..),

    MmaTextures(..),
    MmaTexture(..),
) where
import Graphics.UI.GLUT

import Sprite

data Env = Env
    {
        vars :: Vars,
        sprites :: [Sprite]
    } deriving Show

initialEnvironment :: Env
initialEnvironment =
    Env ( Vars {
        clock = 0,
        mousePos = Position 0 0,
        menu = initialMenu,
        mode = Intro }
        )
    [ ]

data Vars = Vars
    {
        clock :: Int,
        mousePos :: Position,
        menu :: MmaMenu,
        mode :: Mode
    } deriving Show

data Mode = Animator | Intro
    deriving (Show, Eq)

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
    b = MmaButton (Rectangle 20 15 50 70) False

data MmaButton = MmaButton
    {
        buttonRect :: Rectangle,

        --buttonTex :: MmaTexture,

        buttonState :: Bool
    } deriving Show

-- just a dictionary, really
data MmaTextures = MmaTextures
    {
        introTexture :: MmaTexture,
        playTexture :: MmaTexture,
        menuTexture :: MmaTexture
    } deriving Show

data MmaTexture = MmaTexture
    {
        textureWidth :: GLsizei,
        textureHeight :: GLsizei,
        textureObject :: TextureObject
    } deriving Show
