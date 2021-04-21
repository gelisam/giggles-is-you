{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module World where

import Level
import Types


data Rule
  = NameIsYou Name
  | NameIsStop Name
  deriving (Eq, Show, Read)

data World = World
  { windowSize :: PixelSize
  , debug :: String
  , level :: Level
  , rules :: [Rule]
  }
