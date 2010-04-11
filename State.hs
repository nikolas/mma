module State (
  Env(..),
  initialEnvironment,
  Vars(..),
  Mode(..),
  ) where
import Graphics.UI.GLUT

import Menu
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