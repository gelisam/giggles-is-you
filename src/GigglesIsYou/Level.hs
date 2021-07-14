{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections, ViewPatterns #-}
module GigglesIsYou.Level where

import Prelude hiding (Word)

import Data.List
import Data.Maybe
import Data.Tuple
import GHC.Arr
import qualified Data.Map as Map

import GigglesIsYou.Dir
import GigglesIsYou.Types


data Entity
  = Object Name
  | Text Word
  deriving (Eq, Show)

type Stack = [Entity]  -- top to bottom

data Level = Level
  { levelArray :: Array CellPos [Entity]  -- bottom to top
  , levelList  :: [(Entity, CellPos)]
  }

specialChars :: [(Char, Entity)]
specialChars
  = [ ('G', Object GigglesName)
    , ('g', Text   (NameWord GigglesName))
    , ('H', Object SheetsName)
    , ('h', Text   (NameWord SheetsName))
    , ('A', Object AName)
    , ('a', Text   (NameWord AName))
    , ('B', Object BName)
    , ('b', Text   (NameWord BName))
    , ('C', Object CName)
    , ('c', Text   (NameWord CName))
    , ('t', Text   (NameWord TextName))
    , ('i', Text   IsWord)
    , ('o', Text   OnWord)
    , ('y', Text   YouWord)
    , ('s', Text   StopWord)
    , ('p', Text   PushWord)
    ]

specialEntities :: [(Entity, Char)]
specialEntities
  = fmap swap specialChars

isSpecialChar :: Char -> Maybe Entity
isSpecialChar c
  = lookup c specialChars

isSpecialEntity :: Entity -> Maybe Char
isSpecialEntity entity
  = lookup entity specialEntities

parseEntity :: Char -> Maybe Entity
parseEntity ' '
  = Nothing
parseEntity (isSpecialChar -> Just entity)
  = Just entity
parseEntity c
  = error
  $ "level contains unrecognized character "
 ++ show c

pprintEntity :: Maybe Entity -> Char
pprintEntity Nothing
  = ' '
pprintEntity (Just (isSpecialEntity -> Just c))
  = c
pprintEntity x
  = error $ "pprintEntity: pattern-match non-exhaustive: "
         ++ show x

-- ground floor first in the input,
-- ground floor first in the output.
pprintRow :: [[Entity]] -> [String]
pprintRow row
  | all null row
    = []
  | otherwise
    = map (pprintEntity . listToMaybe) row
    : pprintRow (fmap (drop 1) row)

-- biggest Y first, ground floor last.
-- ground floor is prefixed with '.'
pprintLevel :: Level -> [String]
pprintLevel lvl
  = reverse
  $ concat
  [ zipWith (:) ('.' : repeat ' ')
  $ pprintRow [ spritesAt lvl (x,y)
              | x <- [x0..xZ]
              ]
  | y <- [y0..yZ]
  ]
  where
    ((x0,y0), (xZ,yZ)) = levelBounds lvl

parseRow :: [String] -> ([[Entity]], [String])
parseRow ((' ' : stringRow) : stringLevel)
  = let (row, stringLevel') = parseRow stringLevel
        entityRow = map parseEntity stringRow
        snoc entityStack Nothing
          = entityStack
        snoc entityStack (Just entity)
          = entityStack ++ [entity]
        row' = zipWith snoc row entityRow
    in (row', stringLevel')
parseRow (('.' : stringRow) : stringLevel)
  = (map (maybeToList . parseEntity) stringRow, stringLevel)
parseRow stringLevel
  = error $ "expected a row beginning with ' ' or '.', found "
         ++ show stringLevel

-- ground floor last in the input,
-- ground floor first in the output.
parseRows :: [String] -> [[[Entity]]]
parseRows []
  = []
parseRows stringLevel
  = let (row, stringLevel') = parseRow stringLevel
    in row : parseRows stringLevel'

-- biggest Y first, ground floor last.
-- ground floor is prefixed with '.'
parseLevel :: [String] -> Level
parseLevel = convertLevel . parseRows

-- biggest Y first.
convertLevel :: [[[Entity]]] -> Level
convertLevel rows = Level {..}
  where
    stringLevelCellSize :: CellSize
    stringLevelCellSize
      = (length (head rows), length rows)

    levelArray :: Array CellPos [Entity]
    levelArray = array ((0,0), stringLevelCellSize - 1)
      [ ((x,y), entityStack)
      | (y, row) <- zip [0..] (reverse rows)
      , (x, entityStack) <- zip [0..] row
      ]

    levelList :: [(Entity, CellPos)]
    levelList = autoLevelList levelArray

autoLevelList :: Array CellPos [Entity] -> [(Entity, CellPos)]
autoLevelList lvlArray =
  [ (e, i)
  | (i, es) <- assocs lvlArray
  , e <- es
  ]

spritesAt :: Level -> CellPos -> [Entity]
spritesAt (Level {..}) p = levelArray ! p

stackAt :: Level -> CellPos -> Stack
stackAt lvl = reverse . spritesAt lvl

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
    | y <- [hiY, hiY-1 .. loY]
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
--
-- >>> directedLevelIndices E lvl
-- [(0,2), (1,2), (2,2), (3,2)]
-- [(0,1), (1,1), (2,1), (3,1)]
-- [(0,0), (1,0), (2,0), (3,0)]
--
-- >>> directedLevelIndices W lvl
-- [(3,2), (2,2), (1,2), (0,2)]
-- [(3,1), (2,1), (1,1), (0,1)]
-- [(3,0), (2,0), (1,0), (0,0)]
--
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

(///) :: forall i e. (Ix i, Ord i)
      => Array i e -> [(i, e -> e)] -> Array i e
es /// ifs = es // ies
  where
    ifs' :: [(i, e -> e)]
    ifs' = Map.toList
         . Map.fromListWith (.)
         $ ifs

    ies :: [(i, e)]
    ies = fmap if2ie ifs'

    if2ie :: (i, e -> e) -> (i, e)
    if2ie (i, f) = (i, f (es ! i))

moveSpritesTo
  :: [(CellPos, Stack -> Bool, CellPos)]
  -> Level
  -> Level
moveSpritesTo moves lvl@(Level {..}) =
  let removed :: [(CellPos, [Entity] -> [Entity])]
      removed
        = [ (src, mapMaybe listToMaybe . filter (not . isMoving) . tails . reverse)
          | (src, isMoving, _dst) <- moves
          ]

      added :: [(CellPos, [Entity])]
      added
        = [ (dst, addedEntities)
          | (src, isMoving, dst) <- moves
          , let addedEntities
                  = reverse
                  $ fmap head
                  $ filter isMoving
                  $ tails
                  $ stackAt lvl src
          ]

      assocs_ :: [(CellPos, Stack -> Stack)]
      assocs_
        = removed
       ++ [ (p, (++ es))
          | (p, es) <- added
          ]

      levelArray' :: Array CellPos [Entity]
      levelArray' = levelArray /// assocs_

      levelList' :: [(Entity, CellPos)]
      levelList' = autoLevelList levelArray'
  in Level
       { levelArray = levelArray'
       , levelList  = levelList'
       }
