{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
module Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Juicy

import Assets
import CharChart
import Dir
import Draw
import Level
import Types
import World


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

moveSprites :: Dir -> Name -> Level -> Level
moveSprites dir name level
  | Just playerPos <- findSprite name level
  , let newPlayerPos = playerPos + unitVector dir
  , inBounds newPlayerPos level
  = moveSpriteTo playerPos newPlayerPos level
moveSprites _ _ level = level

applyRule :: Dir -> Rule -> Level -> Level
applyRule dir (NameIsYou name) = moveSprites dir name

reactWorld :: Event -> World -> World
reactWorld (EventKey (SpecialKey (isDirKey -> Just dir)) Down _ _)
           w@(World {..})
  = w
  { level = foldr (applyRule dir) level rules
  }
reactWorld (EventKey (Char c) Down _ _) w
  = w
  { rules = [NameIsYou c]
  }
reactWorld _ w = w

main' :: M ()
main' = do
  r1 <- ExceptT $ readImage "assets/images/giggles.png"
  r2 <- ExceptT $ readImage "assets/images/sheets.png"
  giggles <- mustBeJust $ fromDynamicImage r1
  sheets  <- mustBeJust $ fromDynamicImage r2
  charChart <- lift loadCharChart
  let assets = Assets {..}
  let stepWorld :: Float -> World -> World
      stepWorld _dt world = world

  lift $ play (InWindow "Giggles is you" (200, 200) (-10, 10))
              white
              30
              (World level1 [])
              (displayWorld assets)
              reactWorld
              stepWorld
