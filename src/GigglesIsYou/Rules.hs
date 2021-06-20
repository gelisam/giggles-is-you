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

type Chunk = [ActiveCell]

moveYou :: Set Rule -> Dir -> Level -> Level
moveYou rules dir lvl
  = moveSpritesTo
      [ ( activeCellPos
        , isActive
        , activeCellPos + unitVector dir
        )
      | chunk <- levelChunks
      , ActiveCell {..} <- chunk
      ]
      lvl
  where
    -- all the active cells within the level
    levelChunks :: [Chunk]
    levelChunks
      = foldMap
          rowChunks
          (directedLevelIndices (flipDir dir) lvl)

    isNonPushStop :: Entity -> Bool
    isNonPushStop e
      = isStop rules e
     && not (isPush rules e)

    isNonYouStop :: Entity -> Bool
    isNonYouStop e
      = isStop rules e
     && not (isYou rules e)

    hasNonPushStops :: CellPos -> Bool
    hasNonPushStops p
      = any isNonPushStop (spritesAt lvl p)

    hasNonYouStops :: CellPos -> Bool
    hasNonYouStops p
      = any isNonYouStop (spritesAt lvl p)

    hasPushes :: CellPos -> Bool
    hasPushes p
      = any (isPush rules) (spritesAt lvl p)

    hasStops :: CellPos -> Bool
    hasStops p
      = any (isStop rules) (spritesAt lvl p)

    hasYous :: CellPos -> Bool
    hasYous p
      = any (isYou rules) (spritesAt lvl p)

    -- the active cells within the row
    rowChunks :: [CellPos] -> [Chunk]
    rowChunks = go Nothing
      where
        -- Nothing means the movement is blocked in the direction
        -- in which the characters are moving
        go :: Maybe Chunk -> [CellPos] -> [Chunk]
        go _ []
          = []
        go Nothing (p:ps)
          = go (if hasStops p then Nothing else Just []) ps
        go (Just chunk) (p:ps)
          | hasYous p
            = (ActiveCell p (isYou rules) : chunk)
            : go (if hasNonYouStops p
                  then Nothing
                  else Just [currentActiveCell])
                 ps
          | hasPushes p
            = go (if hasNonPushStops p
                  then Nothing
                  else Just (currentActiveCell : chunk))
                 ps
          | otherwise
            = go (if hasStops p
                  then Nothing
                  else Just [])
                 ps
          where
            currentActiveCell :: ActiveCell
            currentActiveCell
              = ActiveCell p (isPush rules)
