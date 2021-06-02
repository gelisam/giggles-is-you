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
type ActiveCell = CellPos

moveYou :: Set Rule -> Dir -> Level -> Level
moveYou rules dir lvl = compose
    [ \lvl -> case moveActiveCell activeCell lvl of
                Nothing -> lvl
                Just lvl' -> lvl'
    | activeCell <- reverse levelActiveCells
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
      = not (inBounds p lvl)
     || any isNonYouStop (spritesAt lvl p)

    hasStops :: CellPos -> Bool
    hasStops p
      = not (inBounds p lvl)
     || any (isStop rules) (spritesAt lvl p)

    hasYou :: CellPos -> Bool
    hasYou p
      = inBounds p lvl
     && any (isYou rules) (spritesAt lvl p)

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
          | hasYou p
            = p : go (hasNonYouStops p) ps
          | otherwise
            = go (hasStops p) ps

    moveActiveCell :: ActiveCell -> Level -> Maybe Level
    moveActiveCell p
      = moveSpriteTo
          (isYou rules)
          p
          (p + unitVector dir)
