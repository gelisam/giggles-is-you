module Pictures where

import Graphics.Gloss

import Types


translate2D :: (Float, Float) -> Picture -> Picture
translate2D = uncurry translate

scale2D :: (Float, Float) -> Picture -> Picture
scale2D = uncurry scale


-- assuming an item of size 'smallSize' is centered at the origin, with the rest
-- of 'bigSize' around it, returns a vector pointing towards the top-right which
-- causes the big item to be centered instead of the small item.
recenter :: PixelSize -> PixelSize -> PixelVector
recenter smallSize bigSize
  = negate (smallSize / 2)
  + bigSize / 2

boxed :: PixelSize -> PixelSize -> Picture -> Picture
boxed (w, h) (boxW, boxH)
  = scale2D (realToFrac (min scaleVertically scaleHorizontally))
  where
    scaleVertically :: Float
    scaleVertically = boxH / h

    scaleHorizontally :: Float
    scaleHorizontally = boxW / w
