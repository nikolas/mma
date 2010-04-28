module Menu (
  MmaButton(..),
  MmaMenu(..),
  MmaWindow(..),
  buttonMap,
  initialMenu,
  menuButtons,
  selectButtonRect,
  windowInc,
  windowDec,
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

  bgWindow = MmaWindow (Rectangle (m!!0) 35 140 60) [] (-1),
  nextBgButton = MmaButton (Rectangle (m!!1) 35 60 60) False,
  prevBgButton = MmaButton (Rectangle ((m!!1)+70) 35 60 60) False,

  frameWindow = MmaWindow (Rectangle (m!!2) 35 140 60) [] (-1),
  nextFrameButton = MmaButton (Rectangle (m!!3) 35 60 60) False,
  prevFrameButton = MmaButton (Rectangle ((m!!3)+70) 35 60 60) False,
  saveButton = MmaButton (Rectangle (m!!3) 105 140 60) False
  }
  where
    m = mkMenu 640 140 15

buttonMap :: (MmaButton -> MmaButton) -> MmaMenu -> MmaMenu
buttonMap f m = m {
  playButton = f $ playButton m,
  nextSprtButton = f $ nextSprtButton m,
  prevSprtButton = f $ prevSprtButton m,
  nextBgButton = f $ nextBgButton m,
  prevBgButton = f $ prevBgButton m,
  nextFrameButton = f $ nextFrameButton m,
  prevFrameButton = f $ prevFrameButton m,
  saveButton = f $ saveButton m
  }

menuButtons :: MmaMenu -> [MmaButton]
menuButtons m = [
  playButton m,
  nextSprtButton m,
  prevSprtButton m,
  nextBgButton m,
  prevBgButton m,
  nextFrameButton m,
  prevFrameButton m,
  saveButton m
  ]

-- A list of button x-positions that fit in a window of width wwd. bwd is
-- button width, and sp is space between buttons.
mkMenu :: GLdouble -> GLdouble -> GLdouble -> [GLdouble]
mkMenu wwd bwd sp = [sp, (sp*2)+bwd .. wwd]

data MmaButton = MmaButton
                 {
                   buttonRect :: Rectangle,

                   --buttonTex :: MmaTexture,

                   buttonState :: Bool
                 } deriving (Show, Eq)

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

windowInc, windowDec :: MmaWindow -> MmaWindow
windowInc w = if windowState w >= length (windowTextures w) - 1
              then w { windowState = 0 }
              else w { windowState = (windowState w) + 1 }

windowDec w = if windowState w <= 0
              then w { windowState = length (windowTextures w) - 1 }
              else w { windowState = (windowState w) - 1 }