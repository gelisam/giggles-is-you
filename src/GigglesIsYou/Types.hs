{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module GigglesIsYou.Types where

import Control.Monad.Trans.Except


type M = ExceptT String IO

data Name
  = GigglesName
  | SheetsName
  | TextName
  | CharName Char
  deriving (Eq, Ord, Read, Show)

data Word
  = NameWord Name
  | IsWord
  | YouWord
  | StopWord
  | PushWord
  deriving (Eq, Ord, Read, Show)

type PixelPos = (Float, Float)
type PixelVector = (Float, Float)
type PixelSize = (Float, Float)
type CellPos = (Int, Int)
type CellSize = (Int, Int)


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
