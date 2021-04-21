{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
module Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Graphics.Gloss hiding (Text)
import Graphics.Gloss.Interface.IO.Interact hiding (Text)
import Graphics.Gloss.Juicy
import Text.Read (readMaybe)

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

compose :: [a -> a] -> a -> a
compose = foldr (.) id

isYou :: [Rule] -> Entity -> Bool
isYou [] _
  = False
isYou (NameIsYou you : rules) e@(Object name)
  = (you == name)
 || isYou rules e
isYou (NameIsYou you : rules) e@(Text _)
  = (you == "Text")
 || isYou rules e
isYou (_ : rules) name
  = isYou rules name

isStop :: [Rule] -> Entity -> Bool
isStop [] _
  = False
isStop (NameIsStop you : rules) e@(Object name)
  = (you == name)
 || isStop rules e
isStop (NameIsStop you : rules) e@(Text _)
  = (you == "Text")
 || isStop rules e
isStop (_ : rules) name
  = isStop rules name

moveYou :: [Rule] -> Dir -> Level -> Level
moveYou rules dir lvl = compose
    [ \lvl -> case moveChunk (reverse chunk) lvl of
                Nothing -> lvl
                Just lvl' -> lvl'
    | chunk <- levelChunks
    ]
    lvl
  where
    levelChunks :: [[CellPos]]
    levelChunks = foldMap rowChunks (directedLevelIndices dir lvl)

    hasYous :: CellPos -> Bool
    hasYous p = any (isYou rules) (spriteAt lvl p)

    hasStops :: CellPos -> Bool
    hasStops p = any (isStop rules) (spriteAt lvl p)

    -- X Y Z S
    --
    --     Y
    --   X Z S
    --
    --
    --     Y
    -- W X S Z
    --
    --   W
    --   X S Y Z
    rowChunks :: [CellPos] -> [[CellPos]]
    rowChunks = filter (not . null) . go []
      where
        go :: [CellPos] -> [CellPos] -> [[CellPos]]
        go reversedChunk []
          = [reverse reversedChunk]
        go reversedChunk (p:ps)
          | hasYous p && hasStops p
            = reverse (drop 1 reversedChunk) : go [p] ps
          | hasYous p
            = go (p:reversedChunk) ps
          | hasStops p
            = reverse (drop 1 reversedChunk) : go [] ps
          | otherwise
            = reverse reversedChunk : go [] ps

    moveChunk :: [CellPos] -> Level -> Maybe Level
    moveChunk [] lvl = do
      pure lvl
    moveChunk (p:chunk) lvl = do
      lvl' <- moveSpriteTo
                (isYou rules)
                p
                (p + unitVector dir)
                lvl
      moveChunk chunk lvl'

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
  | take 1 debug == "!"
  , Just rule <- readMaybe (drop 1 debug)
  = w
  { debug = ""
  , rules = filter (/= rule) rules
  }
reactWorld (EventKey (SpecialKey KeyEnter) Down _ _) w@(World {..})
  | Just rule <- readMaybe debug
  = w
  { debug = ""
  , rules = rule : rules
  }
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
