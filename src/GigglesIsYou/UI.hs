module GigglesIsYou.UI where

import GigglesIsYou.Types
import GigglesIsYou.World


data UI = UI
  { windowSize :: PixelSize
  , debug :: String
  , world :: World
  }
