module Pictures where

import Graphics.Gloss


translate2D :: (Float, Float) -> Picture -> Picture
translate2D = uncurry translate

scale2D :: (Float, Float) -> Picture -> Picture
scale2D = uncurry scale
