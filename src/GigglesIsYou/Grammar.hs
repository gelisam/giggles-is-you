{-# LANGUAGE LambdaCase, RecursiveDo #-}
module GigglesIsYou.Grammar where

import Prelude hiding (Word)

import Control.Applicative
import Text.Earley

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
