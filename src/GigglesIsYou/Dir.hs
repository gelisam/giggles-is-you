module GigglesIsYou.Dir where

import Graphics.Gloss.Interface.IO.Interact


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

flipDir :: Dir -> Dir
flipDir N = S
flipDir E = W
flipDir W = E
flipDir S = N
