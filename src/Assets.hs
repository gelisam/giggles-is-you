module Assets where

import GHC.Arr
import Graphics.Gloss

import CharChart
import Types


data Assets = Assets
  { giggles   :: Picture
  , sheets    :: Picture
  , charChart :: CharChart
  }


levelCellSize :: CellSize
levelCellSize = (3, 3)

cellPixelSize :: PixelSize
cellPixelSize = (48, 48)

stringLevel :: [String]
stringLevel
  = [ "   "
    , "GB "
    , "   "
    ]

level1 :: Level
level1 = array ((0,0), levelCellSize - 1)
  [ ((x,y), c)
  | (y, row) <- zip [0..] (reverse stringLevel)
  , (x, c) <- zip [0..] row
  ]
