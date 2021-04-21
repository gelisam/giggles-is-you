{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Graphics.Gloss hiding (Text)
import Graphics.Gloss.Interface.IO.Interact hiding (Text)
import Graphics.Gloss.Juicy

import Assets
import CharChart
import Dir
import Draw
import Rules
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

reactWorld :: Event -> World -> World
reactWorld (EventKey (SpecialKey (isDirKey -> Just dir)) Down _ _)
           w@(World {..})
  = w
  { level = moveYou rules dir level
  }
reactWorld (EventKey (Char c) Down _ _) w@(World {..})
  = w
  { debug = debug ++ [c]
  }
reactWorld (EventKey (SpecialKey KeySpace) Down _ _) w@(World {..})
  = w
  { debug = debug ++ " "
  }
reactWorld (EventKey (SpecialKey KeyDelete) Down _ _) w@(World {..})
  = w
  { debug = take (length debug - 1) debug
  }
reactWorld (EventKey (SpecialKey KeyEnter) Down _ _) w@(World {..})
  = case runCommand debug w of
      Just w'
        -> w' { debug = "" }
      Nothing
        -> w
reactWorld (EventResize windowSize) w
  = w
  { windowSize = fromIntegral2D windowSize
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

  lift $ play (InWindow "Giggles is you" (400, 300) (-10, 10))
              white
              30
              (World
                (error "EventSize was not triggered before the first draw event")
                ""
                level1
                [ NameIsYou "B"
                , NameIsYou "Giggles"
                , NameIsStop "Text"
                ])
              (displayWorld assets)
              reactWorld
              stepWorld
