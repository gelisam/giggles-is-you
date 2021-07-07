{-# LANGUAGE LambdaCase, RecursiveDo #-}
module GigglesIsYou.Grammar where

import Prelude hiding (Word)

import Control.Applicative
import Control.Monad (guard)
import Data.Foldable (asum)
import Data.Set (Set)
import Text.Earley
import qualified Data.Set as Set

import GigglesIsYou.Dir
import GigglesIsYou.Level
import GigglesIsYou.Rules
import GigglesIsYou.Types


grammar
  :: Grammar r (Prod r String [Word] Rule)
grammar = mdo
  subject <- rule
     $ NameSubject <$> name
   <|> NameOnSubject <$> (name <* word OnWord) <*> subject
  rule_ <- rule
     $ SubjectIsPush <$> subject <* word IsWord <* word PushWord
   <|> SubjectIsStop <$> subject <* word IsWord <* word StopWord
   <|> SubjectIsYou  <$> subject <* word IsWord <* word YouWord
  rule $ many skip *> rule_ <* many skip
  where
    name :: Prod r e [Word] Name
    name = asum
      [ name_ <$ word (NameWord name_)
      | name_ <- [minBound..maxBound]
      ]

    word :: Word -> Prod r e [Word] ()
    word w = terminal $ \ws -> do
      guard (w `elem` ws)

    skip :: Prod r e [Word] ()
    skip = terminal $ \_ -> do
      pure ()

parseWords
  :: [[Word]] -> [Rule]
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
      , rule_ <- parseWords (fmap getWords row)
      ]
  where
    getWords :: CellPos -> [Word]
    getWords p =
      [ word
      | Text word <- spritesAt lvl p
      ]
