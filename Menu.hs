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
  playButton = MmaButton (Rectangle (x!!0) (y!!1) longWidth height) False,

  sprtWindow = MmaWindow (Rectangle (x!!1) (y!!1) longWidth height) [] (-1),
  nextSprtButton = MmaButton (Rectangle (x!!2) 120 shortWidth shortHeight) False,
  prevSprtButton = MmaButton (Rectangle (x!!2) 90 shortWidth shortHeight) False,

  saveButton = MmaButton (Rectangle (x!!0) (y!!0) longWidth height) False,

  bgWindow = MmaWindow (Rectangle (x!!1) (y!!0) longWidth height) [] (-1),
  nextBgButton = MmaButton (Rectangle (x!!2) 50 shortWidth shortHeight) False,
  prevBgButton = MmaButton (Rectangle (x!!2) 20 shortWidth shortHeight) False,

  frameWindow = MmaWindow (Rectangle (x!!3) (y!!0) longWidth height) [] (-1),
  nextFrameButton = MmaButton (Rectangle (x!!4) 50 shortWidth shortHeight) False,
  prevFrameButton = MmaButton (Rectangle (x!!4) 20 shortWidth shortHeight) False
  }
  where
    -- button grid
    x = [70, 215, 325, 435, 545]
    y = [40, 100]
    longWidth = 100
    shortWidth = 50
    height = 50
    shortHeight = 20

buttonMap :: (MmaButton -> MmaButton) -> MmaMenu -> MmaMenu
buttonMap f m = m {
  playButton = f $ playButton m,
  nextSprtButton = f $ nextSprtButton m,
  prevSprtButton = f $ prevSprtButton m,
  saveButton = f $ saveButton m,
  nextBgButton = f $ nextBgButton m,
  prevBgButton = f $ prevBgButton m,
  nextFrameButton = f $ nextFrameButton m,
  prevFrameButton = f $ prevFrameButton m
  }

menuButtons :: MmaMenu -> [MmaButton]
menuButtons m = [
  playButton m,
  nextSprtButton m,
  prevSprtButton m,
  saveButton m,
  nextBgButton m,
  prevBgButton m,
  nextFrameButton m,
  prevFrameButton m
  ]

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
