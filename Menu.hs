module Menu (
  MmaButton(..),
  MmaMenu(..),
  MmaWindow(..),
  initialMenu,
  selectButtonRect,
) where
import Graphics.UI.GLUT

import Rectangle
import Util

data MmaMenu = MmaMenu
               {
                 playButton :: MmaButton,

                 -- sprite chooser
                 sprtWindow :: MmaWindow,
                 nextSprtButton :: MmaButton,
                 prevSprtButton :: MmaButton,

                 -- background chooser
                 bgWindow :: MmaWindow,
                 nextBgButton :: MmaButton,
                 prevBgButton :: MmaButton,

                 -- stepper
                 frameWindow :: MmaWindow,
                 nextFrameButton :: MmaButton,
                 prevFrameButton :: MmaButton,

                 saveButton :: MmaButton
               } deriving Show

initialMenu :: MmaMenu
initialMenu = MmaMenu {
  playButton = MmaButton (Rectangle (m!!0) 105 140 60) False,

  sprtWindow = MmaWindow (Rectangle (m!!1) 105 140 60) [] (-1),
  nextSprtButton = MmaButton (Rectangle (m!!2) 105 60 60) False,
  prevSprtButton = MmaButton (Rectangle ((m!!2)+70) 105 60 60) False,

  saveButton = MmaButton (Rectangle (m!!3) 105 140 60) False,

  bgWindow = MmaWindow (Rectangle (m!!0) 35 140 60) [] (-1),
  nextBgButton = MmaButton (Rectangle (m!!1) 35 60 60) False,
  prevBgButton = MmaButton (Rectangle ((m!!1)+70) 35 60 60) False,

  frameWindow = MmaWindow (Rectangle (m!!2) 35 140 60) [] (-1),
  nextFrameButton = MmaButton (Rectangle (m!!3) 35 60 60) False,
  prevFrameButton = MmaButton (Rectangle ((m!!3)+70) 35 60 60) False
  }
  where
    m = mkMenu 640

-- a list of button x-positions that fit in a window of width x
mkMenu :: GLdouble -> [GLdouble]
mkMenu x = [sp, (sp*2)+wd .. x]
  where
    sp = 15  -- free space between buttons
    wd = 140 -- button width

  {-
instance Functor MmaMenu where
  fmap f m = m {
    playButton = f (playButton m),
    nextSprtButton = f (nextSprtButton m),
    prevSprtButton = f (prevSprtButton m),
    nextBgButton = f (nextBgButton m),
    prevBgButton = f (prevBgButton m),
    nextFrameButton = f (nextFrameButton m),
    prevFrameButton = f (prevFrameButton m),
    saveButton = f (saveButton m)
  }
  -}

data MmaButton = MmaButton
                 {
                   buttonRect :: Rectangle,

        --buttonTex :: MmaTexture,

                   buttonState :: Bool
                 } deriving Show

-- calculate a rectangle around the button
selectButtonRect :: MmaButton -> Rectangle
selectButtonRect b = boxAroundRect (buttonRect b) 4.0

data MmaWindow = MmaWindow
                 {
                   windowRect :: Rectangle,

                   -- all possible textures for this window
                   windowTextures :: [MmaTexture],

                   -- the current texture
                   windowState :: Int
                 } deriving Show
