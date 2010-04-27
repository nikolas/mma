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
           animClock = 0,
           mousePos = Position 0 0,
           menu = initialMenu,
           mode = Intro }
      )
  [ ]

data Vars = Vars
            {
              -- the uptime
              clock :: Int,

              -- current position of the Animator's clock
              animClock :: Int,

              mousePos :: Position,

              menu :: MmaMenu,

              mode :: Mode
            } deriving Show

data Mode = Animator | Intro | Play | Record
          deriving (Show, Eq)
