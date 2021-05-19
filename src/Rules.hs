{-# OPTIONS -Wno-name-shadowing #-}
module Rules where

import Data.Set (Set)
import qualified Data.Set as Set

import Dir
import Level
import Types


data Rule
  = NameIsYou Name
  | NameIsStop Name
  deriving (Eq, Ord, Read, Show)

hasNameIsRule
  :: (Name -> Rule)
  -> Set Rule -> Entity -> Bool
hasNameIsRule nameIsRule rules (Object name)
  = nameIsRule name `Set.member` rules
hasNameIsRule nameIsRule rules (Text _)
  = nameIsRule TextName `Set.member` rules

isYou :: Set Rule -> Entity -> Bool
isYou = hasNameIsRule NameIsYou

isStop :: Set Rule -> Entity -> Bool
isStop = hasNameIsRule NameIsStop

compose :: [a -> a] -> a -> a
compose = foldr (.) id

moveYou :: Set Rule -> Dir -> Level -> Level
moveYou rules dir lvl = compose
    [ \lvl -> case moveChunk (reverse chunk) lvl of
                Nothing -> lvl
                Just lvl' -> lvl'
    | chunk <- levelChunks
    ]
    lvl
  where
    -- all the chunks which are moving throughout the level
    levelChunks :: [[CellPos]]
    levelChunks = foldMap rowChunks (directedLevelIndices dir lvl)

    hasYous :: CellPos -> Bool
    hasYous p
      = inBounds p lvl
     && any (isYou rules) (spritesAt lvl p)

    hasStops :: CellPos -> Bool
    hasStops p
      = not (inBounds p lvl)
     || any (isStop rules) (spritesAt lvl p)

    -- the chunks which are moving within the row
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

