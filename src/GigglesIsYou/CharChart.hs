module GigglesIsYou.CharChart where

import Data.Char
import Data.Traversable
import Graphics.Gloss
import qualified Graphics.UI.GLUT.Fonts as GLUT

import GigglesIsYou.Pictures
import GigglesIsYou.Types


type CharChart = [(Char, PixelSize)]

fontHeight :: Float
fontHeight = 100

loadCharChart :: IO CharChart
loadCharChart = do
  for [32..127] $ \i -> do
    let c = chr i
    let s = [c]
    w <- GLUT.stringWidth GLUT.Roman s
    pure (c, (fromIntegral w, fontHeight))

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

-- Takes a word with newlines in it, e.g. "BA\nBA", "BOX", or "GIG\nGLES", and
-- draws it over multiple lines so it fits snuggly inside the given box.
boxedText :: CharChart -> String -> PixelSize -> Picture
boxedText charChart multilineString boxSize
  = boxed (totalWidth, totalHeight) boxSize
  $ translate2D (recenter (0, fontHeight) (0, totalHeight))
  $ mconcat
  $ [ translate2D (fromIntegral y * (0, negate lineHeight))
                  (centeredText charChart str)
    | (y, str) <- zip [(0::Int)..] strs
    ]
  where
    strs :: [String]
    strs = lines multilineString

    gapBetweenLines :: Float
    gapBetweenLines = 50

    lineHeight :: Float
    lineHeight = fontHeight + gapBetweenLines

    totalHeight :: Float
    totalHeight = fontHeight * fromIntegral (length strs)
                + gapBetweenLines * fromIntegral (length strs - 1)

    totalWidth :: Float
    totalWidth
      | null strs
        = 1
      | otherwise
        = maximum [ w
                  | s <- strs
                  , let (w, _h) = textPixelSize charChart s
                  ]
