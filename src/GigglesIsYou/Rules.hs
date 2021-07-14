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

isName
  :: Name
  -> Entity
  -> Bool
isName expected (Object actual)
  = actual == expected
isName TextName (Text _)
  = True
isName _ _
  = False

data ExpectedLocation
  = OnTop
  | Anywhere
  deriving (Eq, Show)

stackHasSubject
  :: ExpectedLocation
  -> Subject
  -> Stack
  -> Bool
stackHasSubject Anywhere subject stack
  = any (stackHasSubject OnTop subject) (tails stack)
stackHasSubject OnTop (NameSubject name) (entity : _)
  = isName name entity
stackHasSubject OnTop (name `On` subject) (entity : stack)
  = isName name entity
 && stackHasSubject Anywhere subject stack
stackHasSubject _ _ _
  = False

stackHasAdjective
  :: Adjective
  -> ExpectedLocation
  -> Set Rule
  -> Stack
  -> Bool
stackHasAdjective expectedAdjective expectedLocation rules stack
  = or
      [ stackHasSubject expectedLocation subject stack
      | subject `Is` actualAdjective <- Set.toList rules
      , actualAdjective == expectedAdjective
      ]

stackHasPush :: ExpectedLocation -> Set Rule -> Stack -> Bool
stackHasPush = stackHasAdjective Push

stackHasStop :: ExpectedLocation -> Set Rule -> Stack -> Bool
stackHasStop = stackHasAdjective Stop

stackHasYou :: ExpectedLocation -> Set Rule -> Stack -> Bool
stackHasYou = stackHasAdjective You

-- an active cell, that is, a cell containing some entities which move in the
-- direction the player pressed.
data ActiveCell = ActiveCell
  { activeCellPos :: CellPos
  , isActive :: Stack -> Bool
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

    stackHasNonPushStopOnTop :: Stack -> Bool
    stackHasNonPushStopOnTop e
      = stackHasStop OnTop rules e
     && not (stackHasPush OnTop rules e)

    stackHasNonYouStopOnTop :: Stack -> Bool
    stackHasNonYouStopOnTop e
      = stackHasStop OnTop rules e
     && not (stackHasYou OnTop rules e)

    hasNonPushStops :: CellPos -> Bool
    hasNonPushStops p
      = any stackHasNonPushStopOnTop 
      $ tails
      $ stackAt lvl p

    hasNonYouStops :: CellPos -> Bool
    hasNonYouStops p
      = any stackHasNonYouStopOnTop
      $ tails
      $ stackAt lvl p

    hasPushes :: CellPos -> Bool
    hasPushes p
      = stackHasPush Anywhere rules
      $ stackAt lvl p

    hasStops :: CellPos -> Bool
    hasStops p
      = stackHasStop Anywhere rules
      $ stackAt lvl p

    hasYous :: CellPos -> Bool
    hasYous p
      = stackHasYou Anywhere rules
      $ stackAt lvl p

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
            = (ActiveCell p (stackHasYou OnTop rules) : chunk)
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
              = ActiveCell p (stackHasPush OnTop rules)

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
