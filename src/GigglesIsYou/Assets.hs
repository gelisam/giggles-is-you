module GigglesIsYou.Assets where

import Graphics.Gloss

import GigglesIsYou.CharChart
import GigglesIsYou.Level


data Assets = Assets
  { giggles   :: Picture
  , sheets    :: Picture
  , charChart :: CharChart
  }


cellPixelSize :: Num a => a
cellPixelSize = 48

level1 :: Level
level1 = parseLevel
  [ ".t H     "
  , ".i    h  "
  , ".p G i   "
  , ".     y  "
  , ".hogiy   "
  ]
