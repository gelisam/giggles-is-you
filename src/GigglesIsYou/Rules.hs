{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wno-name-shadowing #-}
module GigglesIsYou.Rules where

import Data.Set (Set)
import qualified Data.Set as Set

import GigglesIsYou.Dir
import GigglesIsYou.Level
import GigglesIsYou.Types


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

-- an active cell, that is, a cell containing some entities
-- (those which are "you") which move in the direction the player
-- pressed.
data ActiveEntity = ActiveEntity
  { activeCellPos :: CellPos
  , isActive :: Entity -> Bool
  }

moveYou :: Set Rule -> Dir -> Level -> Level
moveYou rules dir lvl = compose
    [ \lvl -> case moveActiveEntity activeEntity lvl of
                Nothing -> lvl
                Just lvl' -> lvl'
    | activeEntity <- reverse levelActiveEntities
    ]
    lvl
  where
    -- all the active entities within the level
    levelActiveEntities :: [ActiveEntity]
    levelActiveEntities
      = foldMap
          rowActiveEntities
          (directedLevelIndices (flipDir dir) lvl)

    isNonYouStop :: Entity -> Bool
    isNonYouStop e
      = isStop rules e
     && not (isYou rules e)

    hasNonYouStops :: CellPos -> Bool
    hasNonYouStops p
      = any isNonYouStop (spritesAt lvl p)

    hasStops :: CellPos -> Bool
    hasStops p
      = any (isStop rules) (spritesAt lvl p)

    hasYous :: CellPos -> Bool
    hasYous p
      = any (isYou rules) (spritesAt lvl p)

    -- the active entities within the row
    rowActiveEntities :: [CellPos] -> [ActiveEntity]
    rowActiveEntities = go True
      where
        -- the Bool indicates whether movement is blocked in the
        -- direction 'dir'
        go :: Bool -> [CellPos] -> [ActiveEntity]
        go _ []
          = []
        go True (p:ps)
          = go (hasStops p) ps
        go False (p:ps)
          | hasYous p
            = ActiveEntity p (isYou rules)
            : go (hasNonYouStops p) ps
          | otherwise
            = go (hasStops p) ps

    moveActiveEntity :: ActiveEntity -> Level -> Maybe Level
    moveActiveEntity (ActiveEntity {..})
      = moveSpriteTo
          isActive
          activeCellPos
          (activeCellPos + unitVector dir)
