{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module CharChart where

import Data.Char
import Data.Traversable
import Graphics.Gloss
import Graphics.UI.GLUT.Fonts

import Pictures
import Types


type CharChart = [(Char, PixelSize)]

loadCharChart :: IO CharChart
loadCharChart = do
  for [32..127] $ \i -> do
    let c = chr i
    let s = [c]
    w <- stringWidth Roman s
    h <- pure 100
    pure (c, (fromIntegral w, h))

textPixelSize :: CharChart -> String -> PixelSize
textPixelSize _ []
  = 0
textPixelSize charChart (c:s)
  = let (charWidth, charHeight) = charPixelSize charChart c
        (strWidth, strHeight) = textPixelSize charChart s
    in (charWidth + strWidth, max charHeight strHeight)

charPixelSize :: CharChart -> Char -> PixelSize
charPixelSize charChart c = case lookup c charChart of
  Nothing
    -> error $ "you need to add " ++ show c ++ " to loadCharChart"
  Just pixelSize
    -> pixelSize

centeredText :: CharChart -> String -> Picture
centeredText charChart s
  = translate2D (negate (textPixelSize charChart s / 2))
  $ text s
