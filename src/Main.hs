{-# LANGUAGE FlexibleInstances, ViewPatterns #-}
module Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import GHC.Arr
import Graphics.Gloss.Juicy
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact


type M = ExceptT String IO
type P = (Float, Float)
type W = (Float, Float)
type I = (Int, Int)
type Level = Array I Char

levelSize :: I
levelSize = (3, 3)

cellSize :: P
cellSize = (32, 32)

stringLevel :: [String]
stringLevel
  = [ "SSG"
    , "SGG"
    , "SSS"
    ]

level :: Level
level = array ((0,0), levelSize - 1)
  [ ((x,y), c)
  | (y, row) <- zip [0..] (reverse stringLevel)
  , (x, c) <- zip [0..] row
  ]

drawLevel :: Level -> Picture
drawLevel lvl
  = translate2D (negate (totalSize / 2))
  $ translate2D (cellSize / 2)
  $ mconcat
  [ translate2D p $ rectangleWire 32 32
                 <> scale2D 0.2 (text [lvl ! (i,j)])
  | (i,j) <- indices lvl
  , let p = cellSize * (fromIntegral i, fromIntegral j)
  ]
  where
    totalSize = cellSize * fromIntegral2D levelSize


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

unitVector :: Dir -> P
unitVector N = (0,1)
unitVector E = (1,0)
unitVector W = (-1,0)
unitVector S = (0,-1)

instance Num a => Num (a,a) where
  (x1,y1) + (x2,y2) = (x1+x2, y1+y2)
  (x1,y1) * (x2,y2) = (x1*x2, y1*y2)
  abs (x,y) = (abs x, abs y)
  fromInteger x = (fromInteger x, fromInteger x)
  negate (x,y) = (negate x, negate y)
  signum (x,y) = (signum x, signum y)

instance Fractional a => Fractional (a,a) where
  fromRational x = (fromRational x, fromRational x)
  recip (x,y) = (recip x, recip y)

fromIntegral2D :: (Integral a, Num b)
               => (a, a) -> (b, b)
fromIntegral2D (x, y) = (fromIntegral x, fromIntegral y)

translate2D :: (Float, Float) -> Picture -> Picture
translate2D = uncurry translate

scale2D :: (Float, Float) -> Picture -> Picture
scale2D = uncurry scale

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
  let animation :: Float -> Picture
      animation t = translate (10*t) 0 giggles
                 <> sheets
  let displayWorld :: W -> Picture
      displayWorld (x,y) = translate x y giggles
                        <> drawLevel level
  let stepWorld :: Float -> W -> W
      stepWorld dt (x,y) = (x, y)

  lift $ play (InWindow "Giggles is you" (200, 200) (-10, 10))
              white
              30
              (0, 0)
              displayWorld
              reactWorld
              stepWorld
