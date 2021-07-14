{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module GigglesIsYou.Stack where

import Prelude hiding (Word)

import Control.Applicative

import GigglesIsYou.Types


data Entity
  = Object Name
  | Text Word
  deriving (Eq, Show)

isName
  :: Name
  -> Entity
  -> Bool
isName TextName (Text _)
  = True
isName expectedName (Object actualName)
  = expectedName == actualName
isName _ _
  = False


newtype Stack a = Stack
  { unStack :: ZipList a  -- top-to-bottom
  }
  deriving (Show, Functor, Applicative)


fromTopToBottom
  :: [a]
  -> Stack a
fromTopToBottom
  = Stack . ZipList

fromBottomToTop
  :: [a]
  -> Stack a
fromBottomToTop
  = fromTopToBottom . reverse

toTopToBottom
  :: Stack a
  -> [a]
toTopToBottom
  = getZipList . unStack

toBottomToTop
  :: Stack a
  -> [a]
toBottomToTop
  = reverse . toTopToBottom


addOnTop
  :: Entity
  -> Stack Entity
  -> Stack Entity
addOnTop e (Stack (ZipList es))
  = Stack (ZipList (e : es))

addStackOnTop
  :: Stack Entity
  -> Stack Entity
  -> Stack Entity
addStackOnTop (Stack (ZipList xs)) (Stack (ZipList ys))
  = Stack (ZipList (xs ++ ys))


selectAll
  :: Stack Entity
  -> Stack Bool
selectAll
  = (True <$)

selectNone
  :: Stack Entity
  -> Stack Bool
selectNone
  = (False <$)

selectName
  :: Name
  -> Stack Entity
  -> Stack Bool
selectName name
  = fmap (isName name)

selectBelowLeq
  :: Stack Bool
  -> Stack Bool
selectBelowLeq (Stack (ZipList bs0))
  = Stack (ZipList (go bs0))
  where
    go :: [Bool] -> [Bool]
    go []
      = []
    go (True : bs)
      = True : (True <$ bs)
    go (False : bs)
      = False : go bs

selectBelowLt
  :: Stack Bool
  -> Stack Bool
selectBelowLt (Stack (ZipList bs0))
  = Stack (ZipList (go bs0))
  where
    go :: [Bool] -> [Bool]
    go []
      = []
    go (True : bs)
      = False : (True <$ bs)
    go (False : bs)
      = False : go bs


takeSelection
  :: Stack Bool
  -> Stack Entity
  -> Stack Entity
takeSelection (Stack (ZipList bs)) (Stack (ZipList es))
  = Stack
  $ ZipList
  $ fmap snd
  $ filter fst
  $ zip bs es

dropSelection
  :: Stack Bool
  -> Stack Entity
  -> Stack Entity
dropSelection
  = takeSelection . fmap not

selectsAny
  :: (Stack Entity -> Stack Bool)
  -> Stack Entity  -- ^ to make sure the ZipList is finite
  -> Bool
selectsAny f es
  = or $ toTopToBottom (f es <* es)

selectsNone
  :: (Stack Entity -> Stack Bool)
  -> Stack Entity  -- ^ to make sure the ZipList is finite
  -> Bool
selectsNone f
  = not . selectsAny f
