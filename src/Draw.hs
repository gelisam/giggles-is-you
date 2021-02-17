{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Draw where

import GHC.Arr
import Graphics.Gloss

import Assets
import CharChart
import Level
import Pictures
import Types
import World


drawSprite :: Assets -> Name -> Picture
drawSprite (Assets {..}) 'S'
  = boxed (128, 128) cellPixelSize sheets
drawSprite (Assets {..}) 'G'
  = boxed (128, 127) cellPixelSize giggles
drawSprite (Assets {..}) c
  = boxedText charChart [c] (cellPixelSize - 8)

drawLevel :: Assets -> Level -> Picture
drawLevel assets lvl@(Level {..})
  = translate2D (negate (recenter cellPixelSize (totalPixelSize lvl)))
  $ mconcat
  [ translate2D p $ ( color (greyN 0.8)
                    $ uncurry rectangleWire cellPixelSize
                    )
                 <> drawSprite assets (levelArray ! (i,j))
  | (i,j) <- indices levelArray
  , let p = cellPixelSize * (fromIntegral i, fromIntegral j)
  ]

totalPixelSize :: Level -> PixelSize
totalPixelSize lvl = cellPixelSize * fromIntegral2D (levelCellSize lvl)

displayWorld :: Assets -> World -> Picture
displayWorld assets@(Assets {..}) (World {..})
  = drawLevel assets level
