{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module Types where

import Control.Monad.Trans.Except
import GHC.Arr


type M = ExceptT String IO

type PixelPos = (Float, Float)
type PixelVector = (Float, Float)
type PixelSize = (Float, Float)
type CellPos = (Int, Int)
type CellSize = (Int, Int)

type Level = Array CellPos Char

data World = World
  { level     :: Level
  , playerPos :: CellPos
  }


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
