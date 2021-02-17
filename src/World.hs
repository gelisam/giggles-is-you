{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module World where

import Level


data Rule
  = NameIsYou Name
  deriving Show

data World = World
  { level :: Level
  , rules :: [Rule]
  }
