{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Draw where

import GHC.Arr
import Graphics.Gloss

import Assets
import CharChart
import Pictures
import Types


drawCell :: Assets -> Char -> Picture
drawCell (Assets {..}) 'S'
  = boxed (128, 128) cellPixelSize sheets
drawCell (Assets {..}) 'G'
  = boxed (128, 127) cellPixelSize giggles
drawCell (Assets {..}) c
  = boxedText charChart [c] (cellPixelSize - 8)

drawLevel :: Assets -> Level -> Picture
drawLevel assets lvl
  = translate2D (negate (recenter cellPixelSize totalPixelSize))
  $ mconcat
  [ translate2D p $ ( color (greyN 0.8)
                    $ uncurry rectangleWire cellPixelSize
                    )
                 <> drawCell assets (lvl ! (i,j))
  | (i,j) <- indices lvl
  , let p = cellPixelSize * (fromIntegral i, fromIntegral j)
  ]

totalPixelSize :: PixelSize
totalPixelSize = cellPixelSize * fromIntegral2D levelCellSize

displayWorld :: Assets -> World -> Picture
displayWorld assets@(Assets {..}) (World {..})
  = drawLevel assets level
 <> ( translate2D (negate (recenter cellPixelSize totalPixelSize))
    $ translate2D (fromIntegral2D playerPos * cellPixelSize)
    $ circle 10
    )
