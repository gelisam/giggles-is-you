module Assets where

import Graphics.Gloss

import CharChart
import Level
import Types


data Assets = Assets
  { giggles   :: Picture
  , sheets    :: Picture
  , charChart :: CharChart
  }


cellPixelSize :: PixelSize
cellPixelSize = (48, 48)

level1 :: Level
level1 = parseLevel
  [ "S A   "
  , "GBB   "
  , "      "
  , "  C   "
  ]
