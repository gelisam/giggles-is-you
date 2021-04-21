{-# LANGUAGE RecordWildCards #-}
module World where

import Text.Read (readMaybe)

import Level
import Rules


data World = World
  { level :: Level
  , rules :: [Rule]
  }

runCommand :: String -> World -> Maybe World
runCommand cmd w@(World {..})
  | take 1 cmd == "!"
    , Just rule <- readMaybe (drop 1 cmd)
    = Just $ w { rules = filter (/= rule) rules }
  | Just rule <- readMaybe cmd
    = Just $ w { rules = rule : rules }
  | otherwise
    = Nothing
