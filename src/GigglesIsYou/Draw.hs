{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module GigglesIsYou.Draw where

import GHC.Arr
import Graphics.Gloss hiding (Text)

import GigglesIsYou.Assets
import GigglesIsYou.CharChart
import GigglesIsYou.Level
import GigglesIsYou.Pictures
import GigglesIsYou.Types
import GigglesIsYou.UI
import GigglesIsYou.World


drawGenericText :: CharChart -> String -> Picture
drawGenericText charChart s
  = boxedText charChart s (cellPixelSize - 8)

drawGenericObject :: CharChart -> String -> Picture
drawGenericObject charChart s
  = color green (circleSolid ((cellPixelSize - 8) / 2))
 <> boxedText charChart s ((cellPixelSize - 8) / realToFrac (sqrt 2 :: Float))

drawSprite :: Assets -> Entity -> Picture
drawSprite (Assets {..}) (Object SheetsName)
  = boxed (128, 128) cellPixelSize sheets
drawSprite (Assets {..}) (Object GigglesName)
  = boxed (128, 127) cellPixelSize giggles
drawSprite (Assets {..}) (Object TextName)
  = drawGenericObject charChart "Text"
drawSprite (Assets {..}) (Object (CharName c))
  = drawGenericObject charChart [c]
drawSprite (Assets {..}) (Text GigglesName)
  = drawGenericText charChart "Giggles"
drawSprite (Assets {..}) (Text SheetsName)
  = drawGenericText charChart "Sheets"
drawSprite (Assets {..}) (Text TextName)
  = drawGenericText charChart "Text"
drawSprite (Assets {..}) (Text (CharName c))
  = drawGenericText charChart [c]

drawLevel :: Assets -> Level -> Picture
drawLevel assets lvl@(Level {..})
  = translate2D (negate (recenter cellPixelSize (totalPixelSize lvl)))
  $ mconcat
  [ translate2D p $ ( color (greyN 0.8)
                    $ uncurry rectangleWire cellPixelSize
                    )
                 <> foldMap (drawSprite assets) (levelArray ! (i,j))
  | (i,j) <- indices levelArray
  , let p = cellPixelSize * (fromIntegral i, fromIntegral j)
  ]

totalPixelSize :: Level -> PixelSize
totalPixelSize lvl = cellPixelSize * fromIntegral2D (levelCellSize lvl)

displayWorld :: Assets -> World -> Picture
displayWorld assets@(Assets {..}) (World {..})
  = drawLevel assets level

displayUI :: Assets -> UI -> Picture
displayUI assets@(Assets {..}) (UI {..})
  = displayWorld assets world
 <> translate2D
      (0, snd windowSize / 2 - debugHeight / 2 - 3)
      (boxedText charChart debug (fst windowSize - 6, debugHeight))
  where
    debugHeight = 30
