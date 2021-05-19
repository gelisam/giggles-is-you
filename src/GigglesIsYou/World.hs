{-# LANGUAGE RecordWildCards #-}
module GigglesIsYou.World where

import Text.Read (readMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

import GigglesIsYou.Level
import GigglesIsYou.Rules


data World = World
  { level :: Level
  , rules :: Set Rule
  }

enableRule :: Rule -> World -> World
enableRule rule w@(World {..})
  = w { rules = Set.insert rule rules }

disableRule :: Rule -> World -> World
disableRule rule w@(World {..})
  = w { rules = Set.delete rule rules }

runCommand :: String -> World -> Maybe World
runCommand cmd w
  | take 1 cmd == "!"
    , Just rule <- readMaybe (drop 1 cmd)
    = Just $ disableRule rule w
  | Just rule <- readMaybe cmd
    = Just $ enableRule rule w
  | otherwise
    = Nothing
