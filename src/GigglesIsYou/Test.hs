{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}
module GigglesIsYou.Test where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable (for_)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Set as Set

import GigglesIsYou.Dir
import GigglesIsYou.Level
import GigglesIsYou.Rules
import GigglesIsYou.Types
import GigglesIsYou.World


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
      for_ ['W', 'X', 'Y', 'Z'] $ \name -> do
        enable $ NameIsYou (CharName name)
      enable $ NameIsStop SheetsName
      move E
      check [ "  W   "
            , ". XSYZ"
            ]

walkOntoObstacleAtWorldsEnd :: IO ()
walkOntoObstacleAtWorldsEnd
  = runTest [ "   Y "
            , ".WXSZ"
            ] $ do
      for_ ['W', 'X', 'Y', 'Z'] $ \name -> do
        enable $ NameIsYou (CharName name)
      enable $ NameIsStop SheetsName
      move E
      check [ "  W Y"
            , ". XSZ"
            ]

walkOntoObstacleWhileStop :: IO ()
walkOntoObstacleWhileStop
  = runTest [ "        G "
            , ".G GGS GS "
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsStop GigglesName
      enable $ NameIsStop SheetsName
      move E
      check [ ". GGGS GSG"
            ]

walkOntoObstacleWhileSomeAreStop :: IO ()
walkOntoObstacleWhileSomeAreStop
  = runTest [ ".ABS BAS"
            ] $ do
      enable $ NameIsYou (CharName 'A')
      enable $ NameIsYou (CharName 'B')
      enable $ NameIsStop (CharName 'B')
      enable $ NameIsStop SheetsName
      move E
      check [ "      B "
            , ".ABS  AS"
            ]

youAndStopMoveInUnison :: IO ()
youAndStopMoveInUnison
  = runTest [ "     "
            , ".GGG "
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsStop GigglesName
      move E
      check [ ". GGG"
            ]

youAndStopStopInUnison :: IO ()
youAndStopStopInUnison
  = runTest [ ".GGGS"
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsStop GigglesName
      enable $ NameIsStop SheetsName
      move E
      check [ ".GGGS"
            ]

pushTest :: IO ()
pushTest
  = runTest [ "                      G  G  "
            , ".GA  GAS GB  GBS GAA  B  BA "
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsPush (CharName 'A')
      enable $ NameIsPush (CharName 'B')
      enable $ NameIsStop (CharName 'B')
      enable $ NameIsStop SheetsName
      move E
      check [ "      G                     "
            , ". GA  AS  GB GBS  GAA BG BGA"
            ]

testAll :: IO ()
testAll = do
  walkOntoObstacleMidLevel
  walkOntoObstacleAtWorldsEnd
  walkOntoObstacleWhileStop
  walkOntoObstacleWhileSomeAreStop
  youAndStopMoveInUnison
  youAndStopStopInUnison
  pushTest
  putStrLn "PASSED"
