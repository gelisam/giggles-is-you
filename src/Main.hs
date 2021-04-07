{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
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

    rowChunks :: [CellPos] -> [[CellPos]]
    rowChunks = filter (not . null) . go []
      where
        go :: [CellPos] -> [CellPos] -> [[CellPos]]
        go chunk []
          = [chunk]
        go chunk (p:ps)
          | hasYous p && hasStops p
            = go [p] ps
          | hasYous p
            = go (chunk ++ [p]) ps
          | hasStops p
            = go [] ps
          | otherwise
            = chunk : go [] ps

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
reactWorld (EventKey (Char (parseEntity -> Just (Object name))) Down _ _)
           w@(World {..})
  | isYou rules (Object name)
    = w
    { rules = filter (/= NameIsYou name) rules
    }
  | otherwise
    = w
    { rules = NameIsYou name : rules
    }
reactWorld (EventKey (Char (parseEntity -> Just (Text "Text"))) Down _ _)
           w@(World {..})
  | isYou rules (Text "whatever")
    = w
    { rules = filter (/= NameIsYou "Text") rules
    }
  | otherwise
    = w
    { rules = NameIsYou "Text" : rules
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
                level1
                [ NameIsYou "B"
                , NameIsYou "Giggles"
                , NameIsStop "Text"
                ])
              (displayWorld assets)
              reactWorld
              stepWorld
