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
            , ".WXHZ "
            ] $ do
      for_ ['W', 'X', 'Y', 'Z'] $ \name -> do
        enable $ NameIsYou (CharName name)
      enable $ NameIsStop SheetsName
      move E
      check [ "  W   "
            , ". XHYZ"
            ]

walkOntoObstacleAtWorldsEnd :: IO ()
walkOntoObstacleAtWorldsEnd
  = runTest [ "   Y "
            , ".WXHZ"
            ] $ do
      for_ ['W', 'X', 'Y', 'Z'] $ \name -> do
        enable $ NameIsYou (CharName name)
      enable $ NameIsStop SheetsName
      move E
      check [ "  W Y"
            , ". XHZ"
            ]

walkOntoObstacleWhileStop :: IO ()
walkOntoObstacleWhileStop
  = runTest [ "        G "
            , ".G GGH GH "
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsStop GigglesName
      enable $ NameIsStop SheetsName
      move E
      check [ ". GGGH GHG"
            ]

walkOntoObstacleWhileSomeAreStop :: IO ()
walkOntoObstacleWhileSomeAreStop
  = runTest [ ".ABH BAH"
            ] $ do
      enable $ NameIsYou (CharName 'A')
      enable $ NameIsYou (CharName 'B')
      enable $ NameIsStop (CharName 'B')
      enable $ NameIsStop SheetsName
      move E
      check [ "      B "
            , ".ABH  AH"
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
  = runTest [ ".GGGH"
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsStop GigglesName
      enable $ NameIsStop SheetsName
      move E
      check [ ".GGGH"
            ]

pushTest :: IO ()
pushTest
  = runTest [ "                      G  G  "
            , ".GA  GAH GB  GBH GAA  B  BA "
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsPush (CharName 'A')
      enable $ NameIsPush (CharName 'B')
      enable $ NameIsStop (CharName 'B')
      enable $ NameIsStop SheetsName
      move E
      check [ "      G                     "
            , ". GA  AH  GB GBH  GAA BG BGA"
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
