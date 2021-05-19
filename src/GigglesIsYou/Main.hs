{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module GigglesIsYou.Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Graphics.Gloss hiding (Text)
import Graphics.Gloss.Interface.IO.Interact hiding (Text)
import Graphics.Gloss.Juicy
import qualified Data.Set as Set

import GigglesIsYou.Assets
import GigglesIsYou.CharChart
import GigglesIsYou.Dir
import GigglesIsYou.Draw
import GigglesIsYou.Rules
import GigglesIsYou.Types
import GigglesIsYou.UI
import GigglesIsYou.World

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
reactWorld _ w
  = w

reactUI :: Event -> UI -> UI
reactUI (EventKey (Char c) Down _ _) ui@(UI {..})
  = ui
  { debug = debug ++ [c]
  }
reactUI (EventKey (SpecialKey KeySpace) Down _ _) ui@(UI {..})
  = ui
  { debug = debug ++ " "
  }
reactUI (EventKey (SpecialKey KeyDelete) Down _ _) ui@(UI {..})
  = ui
  { debug = take (length debug - 1) debug
  }
reactUI (EventKey (SpecialKey KeyEnter) Down _ _) ui@(UI {..})
  = case runCommand debug world of
      Just w'
        -> ui
         { debug = ""
         , world = w'
         }
      Nothing
        -> ui
reactUI (EventResize windowSize) ui
  = ui
  { windowSize = fromIntegral2D windowSize
  }
reactUI event ui@(UI {..})
  = ui
  { world = reactWorld event world
  }

main' :: M ()
main' = do
  r1 <- ExceptT $ readImage "assets/images/giggles.png"
  r2 <- ExceptT $ readImage "assets/images/sheets.png"
  giggles <- mustBeJust $ fromDynamicImage r1
  sheets  <- mustBeJust $ fromDynamicImage r2
  charChart <- lift loadCharChart
  let assets = Assets {..}

  lift $ play (InWindow "Giggles is you" (400, 300) (-10, 10))
              white
              30
              (UI
                (error "EventSize was not triggered before the first draw event")
                ""
                (World
                  level1
                  (Set.fromList
                    [ NameIsYou (CharName 'B')
                    , NameIsYou GigglesName
                    , NameIsStop TextName
                    ])))
              (displayUI assets)
              reactUI
              (\_ ui -> ui)
