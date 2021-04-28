{-# LANGUAGE RecordWildCards #-}
module World where

import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import Level
import Rules


data World = World
  { level :: Level
  , rules :: Set Rule
  }

runCommand :: String -> World -> Maybe World
runCommand cmd w@(World {..})
  | take 1 cmd == "!"
    , Just rule <- readMaybe (drop 1 cmd)
    = Just $ w { rules = Set.delete rule rules }
  | Just rule <- readMaybe cmd
    = Just $ w { rules = Set.insert rule rules }
  | otherwise
    = Nothing
