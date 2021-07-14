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
import GigglesIsYou.Stack
import GigglesIsYou.Types


data Subject
  = NameSubject Name
  | On Name Subject
  deriving (Eq, Ord, Read, Show)

data Adjective
  = Push
  | Stop
  | You
  deriving (Eq, Ord, Read, Show)

data Rule
  = Is Subject Adjective
  deriving (Eq, Ord, Read, Show)

nameIsPush, nameIsStop, nameIsYou :: Name -> Rule
nameIsPush name = NameSubject name `Is` Push
nameIsStop name = NameSubject name `Is` Stop
nameIsYou  name = NameSubject name `Is` You

selectSubject
  :: Subject
  -> Stack Entity
  -> Stack Bool
selectSubject (NameSubject name) stack
  = selectName name stack
selectSubject (name `On` subject) stack
    = (&&)
  <$> selectBelowLt (selectName name stack)
  <*> selectSubject subject stack

selectAdjective
  :: Adjective
  -> Set Rule
  -> Stack Entity
  -> Stack Bool
selectAdjective expectedAdjective rules stack
  = fmap or
  $ sequenceA
      [ selectSubject subject stack
      | subject `Is` actualAdjective <- Set.toList rules
      , actualAdjective == expectedAdjective
      ]

selectPush :: Set Rule -> Stack Entity -> Stack Bool
selectPush = selectAdjective Push

selectStop :: Set Rule -> Stack Entity -> Stack Bool
selectStop = selectAdjective Stop

selectYou :: Set Rule -> Stack Entity -> Stack Bool
selectYou = selectAdjective You

-- an active cell, that is, a cell containing some entities which move in the
-- direction the player pressed.
data ActiveCell = ActiveCell
  { activeCellPos :: CellPos
  , isActive :: Stack Bool
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

    selectNonPushStop :: Stack Entity -> Stack Bool
    selectNonPushStop stack
        = (&&)
      <$> selectStop rules stack
      <*> (not <$> selectPush rules stack)

    selectNonYouStop :: Stack Entity -> Stack Bool
    selectNonYouStop stack
        = (&&)
      <$> selectStop rules stack
      <*> (not <$> selectYou rules stack)

    hasNonPushStops :: CellPos -> Bool
    hasNonPushStops
      = selectsAny selectNonPushStop . stackAt lvl

    hasNonYouStops :: CellPos -> Bool
    hasNonYouStops
      = selectsAny selectNonYouStop . stackAt lvl

    hasPushes :: CellPos -> Bool
    hasPushes
      = selectsAny (selectPush rules) . stackAt lvl

    hasStops :: CellPos -> Bool
    hasStops
      = selectsAny (selectStop rules) . stackAt lvl

    hasYous :: CellPos -> Bool
    hasYous
      = selectsAny (selectYou rules) . stackAt lvl

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
            = (ActiveCell p (selectYou rules stack) : chunk)
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
            stack :: Stack Entity
            stack
              = stackAt lvl p

            currentActiveCell :: ActiveCell
            currentActiveCell
              = ActiveCell p (selectPush rules stack)

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
