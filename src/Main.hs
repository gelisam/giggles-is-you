{-# LANGUAGE ViewPatterns #-}
module Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import GHC.Arr
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy

import CharChart
import Pictures
import Types


levelCellSize :: CellSize
levelCellSize = (3, 3)

cellPixelSize :: PixelSize
cellPixelSize = (32, 32)

stringLevel :: [String]
stringLevel
  = [ "SSG"
    , "SGG"
    , "SSS"
    ]

level :: Level
level = array ((0,0), levelCellSize - 1)
  [ ((x,y), c)
  | (y, row) <- zip [0..] (reverse stringLevel)
  , (x, c) <- zip [0..] row
  ]

drawLevel :: CharChart -> Level -> Picture
drawLevel charChart lvl
  = translate2D (negate (recenter cellPixelSize totalPixelSize))
  $ mconcat
  [ translate2D p $ rectangleWire 32 32
                 <> scale2D 0.2 (centeredText charChart [lvl ! (i,j)])
  | (i,j) <- indices lvl
  , let p = cellPixelSize * (fromIntegral i, fromIntegral j)
  ]
  where
    totalPixelSize = cellPixelSize * fromIntegral2D levelCellSize


main :: IO ()
main = do
  r <- runExceptT main'
  case r of
    Left err -> do
      error err
    Right () -> do
      pure ()


mustBeJust :: Maybe a -> M a
mustBeJust Nothing = ExceptT $ pure $ Left "Nothing encountered"
mustBeJust (Just a) = pure a

data Dir = N | E | W | S
  deriving (Eq, Show)

isDirKey :: SpecialKey -> Maybe Dir
isDirKey KeyRight = Just E
isDirKey KeyLeft  = Just W
isDirKey KeyUp    = Just N
isDirKey KeyDown  = Just S
isDirKey _        = Nothing

unitVector :: Num a => Dir -> (a, a)
unitVector N = (0,1)
unitVector E = (1,0)
unitVector W = (-1,0)
unitVector S = (0,-1)

reactWorld :: Event -> W -> W
reactWorld (EventKey (SpecialKey (isDirKey -> Just dir)) Down _ _) w
  = w + 10 * unitVector dir
reactWorld _ w = w

main' :: M ()
main' = do
  r1 <- ExceptT $ readImage "assets/images/giggles.png"
  r2 <- ExceptT $ readImage "assets/images/sheets.png"
  giggles <- mustBeJust $ fromDynamicImage r1
  sheets  <- mustBeJust $ fromDynamicImage r2
  charChart <- lift loadCharChart
  --let displayWorld :: W -> Picture
  --    displayWorld (x,y) = translate x y giggles
  --                      <> drawLevel charChart level
  let displayWorld :: W -> Picture
      displayWorld (x,y) = boxedText charChart "GIG\nGLES" cellPixelSize
                        <> uncurry rectangleWire cellPixelSize
                        <> circle 2

  let stepWorld :: Float -> W -> W
      stepWorld dt (x,y) = (x, y)

  lift $ play (InWindow "Giggles is you" (200, 200) (-10, 10))
              white
              30
              (0, 0)
              displayWorld
              reactWorld
              stepWorld
