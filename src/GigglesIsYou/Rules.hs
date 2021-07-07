{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wno-name-shadowing #-}
module GigglesIsYou.Rules where

import Prelude hiding (Word)

import Control.Category ((>>>))
import Data.List (tails)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Safe.Exact (takeExactMay)
import qualified Data.Set as Set

import GigglesIsYou.Dir
import GigglesIsYou.Level
import GigglesIsYou.Types


data Subject
  = NameSubject Name
  deriving (Eq, Ord, Read, Show)

data Rule
  = SubjectIsPush Subject
  | SubjectIsStop Subject
  | SubjectIsYou Subject
  deriving (Eq, Ord, Read, Show)

nameIsPush, nameIsStop, nameIsYou :: Name -> Rule
nameIsPush = SubjectIsPush . NameSubject
nameIsStop = SubjectIsStop . NameSubject
nameIsYou = SubjectIsYou . NameSubject

hasSubjectIsRule
  :: (Subject -> Rule)
  -> Set Rule -> Entity -> Bool
hasSubjectIsRule subjectIsRule rules (Object name)
  = subjectIsRule (NameSubject name) `Set.member` rules
hasSubjectIsRule subjectIsRule rules (Text _)
  = subjectIsRule (NameSubject TextName) `Set.member` rules

isPush :: Set Rule -> Entity -> Bool
isPush = hasSubjectIsRule SubjectIsPush

isStop :: Set Rule -> Entity -> Bool
isStop = hasSubjectIsRule SubjectIsStop

isYou :: Set Rule -> Entity -> Bool
isYou = hasSubjectIsRule SubjectIsYou

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

windows
  :: Int -> [a] -> [[a]]
windows n
      -- "abc"
    = tails
      -- ["abc", "bc", "c", ""]
  >>> fmap (takeExactMay n)
      -- [Just "ab", Just "bc", Nothing, Nothing]
  >>> catMaybes
      -- ["ab", "bc"]
