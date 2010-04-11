module Menu (
  MmaButton(..),
  MmaMenu(..),
  MmaWindow(..),
  initialMenu,
  ) where

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
                 prevFrameButton :: MmaButton,
                 nextFrameButton :: MmaButton,

                 saveButton :: MmaButton
               } deriving Show

initialMenu :: MmaMenu
initialMenu = MmaMenu {
  playButton = MmaButton (Rectangle 15 95 140 70) False,

  sprtWindow = MmaWindow (Rectangle 165 95 140 70) [] (-1),
  nextSprtButton = MmaButton (Rectangle 315 95 60 70) False,
  prevSprtButton = MmaButton (Rectangle 385 95 60 70) False,

  saveButton = MmaButton (Rectangle 455 95 140 70) False,

  bgWindow = MmaWindow (Rectangle 15 15 140 70) [] (-1),
  nextBgButton = MmaButton (Rectangle 165 15 60 70) False,
  prevBgButton = MmaButton (Rectangle 235 15 60 70) False,

  frameWindow = MmaWindow (Rectangle 315 15 140 70) [] (-1),
  nextFrameButton = MmaButton (Rectangle 465 15 60 70) False,
  prevFrameButton = MmaButton (Rectangle 535 15 60 70) False
  }

data MmaButton = MmaButton
                 {
                   buttonRect :: Rectangle,

        --buttonTex :: MmaTexture,

                   buttonState :: Bool
                 } deriving Show

data MmaWindow = MmaWindow
                 {
                   windowRect :: Rectangle,

                   -- all possible textures for this window
                   windowTextures :: [MmaTexture],

                   -- the current texture
                   windowState :: Int
                 } deriving Show
