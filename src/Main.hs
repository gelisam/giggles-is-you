{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
{-# OPTIONS -Wno-name-shadowing #-}
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

compose :: [a -> a] -> a -> a
compose = foldr (.) id

isYou :: [Rule] -> Name -> Bool
isYou [] _
  = False
isYou (NameIsYou you : rules) name
  = (name == you) || isYou rules name
isYou (_ : rules) name
  = isYou rules name

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

    isInChunk :: CellPos -> Bool
    isInChunk p = isYou rules (spriteAt lvl p)

    rowChunks :: [CellPos] -> [[CellPos]]
    rowChunks row = case span (not . isInChunk) row of
      ([], []) -> []
      (_, rest) -> case span isInChunk rest of
        (chunk, rest) -> chunk : rowChunks rest

    moveChunk :: [CellPos] -> Level -> Maybe Level
    moveChunk [] lvl = do
      pure lvl
    moveChunk (p:chunk) lvl = do
      lvl' <- moveSpriteTo p (p + unitVector dir) lvl
      moveChunk chunk lvl'

reactWorld :: Event -> World -> World
reactWorld (EventKey (SpecialKey (isDirKey -> Just dir)) Down _ _)
           w@(World {..})
  = w
  { level = moveYou rules dir level
  }
reactWorld (EventKey (Char c) Down _ _)
           w@(World {..})
  | isYou rules c
    = w
    { rules = filter (/= NameIsYou c) rules
    }
  | otherwise
    = w
    { rules = NameIsYou c : rules
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
              (World level1 [NameIsYou 'B', NameIsYou 'G'])
              (displayWorld assets)
              reactWorld
              stepWorld
