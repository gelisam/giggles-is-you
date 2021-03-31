{-# LANGUAGE RecordWildCards #-}
module Level where

import Control.Monad
import Data.Char
import Data.List
import GHC.Arr

import Dir
import Types


type Name = String

data Entity
  = Object Name
  | Text Name
  deriving (Show, Eq)

data Level = Level
  { levelArray :: Array CellPos (Maybe Entity)
  , levelList  :: [(Entity, CellPos)]
  }

parseEntity :: Char -> Maybe Entity
parseEntity ' '
  = Nothing
parseEntity 'G'
  = Just (Object "Giggles")
parseEntity 'S'
  = Just (Object "Sheets")
parseEntity 'g'
  = Just (Text "Giggles")
parseEntity 's'
  = Just (Text "Sheets")
parseEntity 't'
  = Just (Text "Text")
parseEntity c
  | isUpper c
    = Just (Object [c])
  | otherwise
    = Just (Text [toUpper c])

parseLevel :: [String] -> Level
parseLevel stringLevel = Level {..}
  where
    stringLevelCellSize :: CellSize
    stringLevelCellSize
      = (length (head stringLevel), length stringLevel)

    levelArray :: Array CellPos (Maybe Entity)
    levelArray = array ((0,0), stringLevelCellSize - 1)
      [ ((x,y), parseEntity c)
      | (y, row) <- zip [0..] (reverse stringLevel)
      , (x, c) <- zip [0..] row
      ]

    levelList :: [(Entity, CellPos)]
    levelList =
      [ (e, i)
      | (i, Just e) <- assocs levelArray
      ]

spriteAt :: Level -> CellPos -> Maybe Entity
spriteAt (Level {..}) p = levelArray ! p

findSprite :: Entity -> Level -> Maybe CellPos
findSprite name (Level {..}) = lookup name levelList

levelBounds :: Level -> (CellPos, CellPos)
levelBounds = bounds . levelArray

inBounds :: CellPos -> Level -> Bool
inBounds p lvl = inRange (levelBounds lvl) p

levelCellSize :: Level -> CellSize
levelCellSize lvl = snd (levelBounds lvl) + 1

-- >>> levelRows lvl
-- [(0,2), (1,2), (2,2), (3,2)]
-- [(0,1), (1,1), (2,1), (3,1)]
-- [(0,0), (1,0), (2,0), (3,0)]
levelRows :: Level -> [[CellPos]]
levelRows lvl
  = [ [ (x, y)
      | x <- [loX..hiX]
      ]
    | y <- [hiY, (hiY-1) .. loY]
    ]
  where
    ((loX, loY), (hiX, hiY)) = levelBounds lvl

-- >>> levelCols lvl
-- [(0,2), (0,1), (0,0)]
-- [(1,2), (1,1), (1,0)]
-- [(2,2), (2,1), (2,0)]
-- [(3,2), (3,1), (3,0)]
levelCols :: Level -> [[CellPos]]
levelCols = transpose . levelRows

-- >>> directedLevelIndices N lvl
-- [(0,0), (0,1), (0,2)]
-- [(1,0), (1,1), (1,2)]
-- [(2,0), (2,1), (2,2)]
-- [(3,0), (3,1), (3,2)]
-- >>> directedLevelIndices E lvl
-- [(0,2), (1,2), (2,2), (3,2)]
-- [(0,1), (1,1), (2,1), (3,1)]
-- [(0,0), (1,0), (2,0), (3,0)]
-- >>> directedLevelIndices W lvl
-- [(3,2), (2,2), (1,2), (0,2)]
-- [(3,1), (2,1), (1,1), (0,1)]
-- [(3,0), (2,0), (1,0), (0,0)]
-- >>> directedLevelIndices S lvl
-- [(0,2), (0,1), (0,0)]
-- [(1,2), (1,1), (1,0)]
-- [(2,2), (2,1), (2,0)]
-- [(3,2), (3,1), (3,0)]
directedLevelIndices :: Dir -> Level -> [[CellPos]]
directedLevelIndices N = fmap reverse . levelCols
directedLevelIndices E = levelRows
directedLevelIndices W = fmap reverse . levelRows
directedLevelIndices S = levelCols

moveSpriteTo :: CellPos -> CellPos -> Level -> Maybe Level
moveSpriteTo src dst lvl@(Level {..}) = do
  entity <- levelArray ! src
  guard (dst `inBounds` lvl)
  guard (levelArray ! dst == Nothing)
  pure $ Level
    { levelArray = levelArray // [(src, Nothing), (dst, Just entity)]
    , levelList  = ((entity, dst):)
                 . delete (entity, src)
                 $ levelList
    }
