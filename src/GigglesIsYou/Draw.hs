{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module GigglesIsYou.Draw where

import GHC.Arr
import Graphics.Gloss hiding (Text)

import GigglesIsYou.Assets
import GigglesIsYou.CharChart
import GigglesIsYou.Level
import GigglesIsYou.Pictures
import GigglesIsYou.Stack
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
drawSprite (Assets {..}) (Object AName)
  = drawGenericObject charChart "A"
drawSprite (Assets {..}) (Object BName)
  = drawGenericObject charChart "B"
drawSprite (Assets {..}) (Object CName)
  = drawGenericObject charChart "C"
drawSprite (Assets {..}) (Text (NameWord GigglesName))
  = drawGenericText charChart "Giggles"
drawSprite (Assets {..}) (Text (NameWord SheetsName))
  = drawGenericText charChart "Sheets"
drawSprite (Assets {..}) (Text (NameWord TextName))
  = drawGenericText charChart "Text"
drawSprite (Assets {..}) (Text (NameWord AName))
  = drawGenericText charChart "A"
drawSprite (Assets {..}) (Text (NameWord BName))
  = drawGenericText charChart "B"
drawSprite (Assets {..}) (Text (NameWord CName))
  = drawGenericText charChart "C"
drawSprite (Assets {..}) (Text IsWord)
  = drawGenericText charChart "is"
drawSprite (Assets {..}) (Text OnWord)
  = drawGenericText charChart "on"
drawSprite (Assets {..}) (Text YouWord)
  = drawGenericText charChart "You"
drawSprite (Assets {..}) (Text StopWord)
  = drawGenericText charChart "Stop"
drawSprite (Assets {..}) (Text PushWord)
  = drawGenericText charChart "Push"

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
displayWorld assets (World {..})
  = drawLevel assets level

displayUI :: Assets -> UI -> Picture
displayUI assets@(Assets {..}) (UI {..})
  = displayWorld assets world
 <> translate2D
      (0, snd windowSize / 2 - debugHeight / 2 - 3)
      (boxedText charChart debug (fst windowSize - 6, debugHeight))
  where
    debugHeight = 30
