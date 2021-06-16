{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wno-name-shadowing #-}
module GigglesIsYou.Rules where

import Data.Set (Set)
import qualified Data.Set as Set

import GigglesIsYou.Dir
import GigglesIsYou.Level
import GigglesIsYou.Types


data Rule
  = NameIsPush Name
  | NameIsStop Name
  | NameIsYou Name
  deriving (Eq, Ord, Read, Show)

hasNameIsRule
  :: (Name -> Rule)
  -> Set Rule -> Entity -> Bool
hasNameIsRule nameIsRule rules (Object name)
  = nameIsRule name `Set.member` rules
hasNameIsRule nameIsRule rules (Text _)
  = nameIsRule TextName `Set.member` rules

isPush :: Set Rule -> Entity -> Bool
isPush = hasNameIsRule NameIsPush

isStop :: Set Rule -> Entity -> Bool
isStop = hasNameIsRule NameIsStop

isYou :: Set Rule -> Entity -> Bool
isYou = hasNameIsRule NameIsYou

-- an active cell, that is, a cell containing some entities which move in the
-- direction the player pressed.
data ActiveCell = ActiveCell
  { activeCellPos :: CellPos
  , isActive :: Entity -> Bool
  }

moveYou :: Set Rule -> Dir -> Level -> Level
moveYou rules dir lvl
  = moveSpritesTo
      [ ( activeCellPos
        , isActive
        , activeCellPos + unitVector dir
        )
      | ActiveCell {..} <- reverse levelActiveCells
      ]
      lvl
  where
    -- all the active cells within the level
    levelActiveCells :: [ActiveCell]
    levelActiveCells
      = foldMap
          rowActiveCells
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

    -- the active cells within the row
    rowActiveCells :: [CellPos] -> [ActiveCell]
    rowActiveCells = go True
      where
        -- the Bool indicates whether movement is blocked in the
        -- direction 'dir'
        go :: Bool -> [CellPos] -> [ActiveCell]
        go _ []
          = []
        go True (p:ps)
          = go (hasStops p) ps
        go False (p:ps)
          | hasYous p
            = ActiveCell p (isYou rules)
            : go (hasNonYouStops p) ps
          | otherwise
            = go (hasStops p) ps
