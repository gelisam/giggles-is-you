{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module Main where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable (for_)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Set as Set

import Dir
import Level
import Rules
import World


newtype MyTest a = MyTest
  { unMyTest :: ExceptT [String] (State World) a
  } deriving (Functor, Applicative, Monad)

runTest :: [String] -> MyTest () -> IO ()
runTest stringLevel myTest = do
  let s0 = World
         { level = parseLevel stringLevel
         , rules = Set.empty
         }
  case evalState (runExceptT $ unMyTest myTest) s0 of
    Left errorLines -> do
      for_ errorLines $ \errorLine -> do
        hPutStrLn stderr errorLine
      exitFailure
    Right () -> do
      pure ()

move :: Dir -> MyTest ()
move dir = MyTest $ do
  modify $ \(w@World {..}) -> w
    { level = moveYou rules dir level
    }

enable :: Rule -> MyTest ()
enable rule = MyTest $ do
  modify $ enableRule rule

disable :: Rule -> MyTest ()
disable rule = MyTest $ do
  modify $ disableRule rule

check :: [String] -> MyTest ()
check expected = MyTest $ do
  lvl <- lift (gets level)
  let actual = pprintLevel lvl
  unless (actual == expected) $ do
    throwE $ ["expected:"]
          ++ (fmap ("  " ++) expected)
          ++ ["got:"]
          ++ (fmap ("  " ++) actual)

walkOntoObstacleMidLevel :: IO ()
walkOntoObstacleMidLevel
  = runTest [ "   Y  "
            , ".WXSZ "
            ] $ do
      for_ ["W", "X", "Y", "Z"] $ \name -> do
        enable $ NameIsYou name
      enable $ NameIsStop "Sheets"
      move E
      check [ "  W   "
            , ". XSYZ"
            ]

walkOntoObstacleAtWorldsEnd :: IO ()
walkOntoObstacleAtWorldsEnd
  = runTest [ "   Y "
            , ".WXSZ"
            ] $ do
      for_ ["W", "X", "Y", "Z"] $ \name -> do
        enable $ NameIsYou name
      enable $ NameIsStop "Sheets"
      move E
      check [ "  W Y"
            , ". XSZ"
            ]

failingTest :: IO ()
failingTest
  = runTest [ "     "
            , ".GGG "
            ] $ do
      enable $ NameIsYou "Giggles"
      move W
      check [ " G   "
            , ".GG  "
            ]


--myTest :: MyTest ()
--myTest
--  = runTest [ "    "
--            , "XYZS"
--            ] $ do
--      for_ ["X", "Y", "Z"] $ \name -> do
--        addRule $ NameIsYou name
--      move E
--      withLocalRules $ do
--        addRule $ NameIsYou "Y"
--        move N
--        check [ "  Y "
--              , " XZS"
--              ]
--        move S


--
--
--     Y
-- W X S Z
--
--   W
--   X S Y Z
main :: IO ()
main = do
  walkOntoObstacleMidLevel
  walkOntoObstacleAtWorldsEnd
