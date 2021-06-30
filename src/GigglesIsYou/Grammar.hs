{-# LANGUAGE LambdaCase, RecursiveDo #-}
module GigglesIsYou.Grammar where

import Prelude hiding (Word)

import Control.Applicative
import Data.Set (Set)
import Text.Earley
import qualified Data.Set as Set

import GigglesIsYou.Dir
import GigglesIsYou.Level
import GigglesIsYou.Rules
import GigglesIsYou.Types


grammar
  :: Grammar r (Prod r String Word Rule)
grammar = mdo
  rule $ NameIsPush <$> name <* token IsWord <* token PushWord
     <|> NameIsStop <$> name <* token IsWord <* token StopWord
     <|> NameIsYou  <$> name <* token IsWord <* token YouWord
  where
    name :: Prod r e Word Name
    name = terminal $ \case
      NameWord word
        -> Just word
      _ -> Nothing

parseWords
  :: [Word] -> [Rule]
parseWords
  = fst
  . fullParses (parser grammar)


detectRules
  :: Level -> Set Rule
detectRules lvl
  = Set.fromList
      [ rule_
      | dir <- [E, S]
      , row <- directedLevelIndices dir lvl
      , threeCells <- windows 3 row
      , let wordsList = fmap getWords threeCells
      , input <- sequenceA wordsList
      , rule_ <- parseWords input
      ]
  where
    getWords :: CellPos -> [Word]
    getWords p =
      [ word
      | Text word <- spritesAt lvl p
      ]
