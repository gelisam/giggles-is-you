{-# OPTIONS -Wno-name-shadowing #-}
module Rules where

import Dir
import Level
import Types


data Rule
  = NameIsYou Name
  | NameIsStop Name
  deriving (Eq, Show, Read)

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

compose :: [a -> a] -> a -> a
compose = foldr (.) id

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

