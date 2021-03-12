{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wno-orphans #-}
module World where

import Level


data Rule
  = NameIsYou Name
  | NameIsStop Name
  deriving (Eq, Show)

data World = World
  { level :: Level
  , rules :: [Rule]
  }
