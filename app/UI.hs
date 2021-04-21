module UI where

import Types
import World


data UI = UI
  { windowSize :: PixelSize
  , debug :: String
  , world :: World
  }
