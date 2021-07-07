{-# LANGUAGE GeneralizedNewtypeDeriving, MultiWayIf, RecordWildCards #-}
module GigglesIsYou.Test where

import Prelude hiding (Word)

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable (for_)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Set as Set
import qualified Text.Earley as Earley

import GigglesIsYou.Assets
import GigglesIsYou.Dir
import GigglesIsYou.Grammar
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
  = runTest [ "   B  "
            , ".GAHC "
            ] $ do
      enable $ nameIsYou GigglesName
      enable $ nameIsYou AName
      enable $ nameIsYou BName
      enable $ nameIsYou CName
      enable $ nameIsStop SheetsName
      move E
      check [ "  G   "
            , ". AHBC"
            ]

walkOntoObstacleAtWorldsEnd :: IO ()
walkOntoObstacleAtWorldsEnd
  = runTest [ "   B "
            , ".GAHC"
            ] $ do
      enable $ nameIsYou GigglesName
      enable $ nameIsYou AName
      enable $ nameIsYou BName
      enable $ nameIsYou CName
      enable $ nameIsStop SheetsName
      move E
      check [ "  G B"
            , ". AHC"
            ]

walkOntoObstacleWhileStop :: IO ()
walkOntoObstacleWhileStop
  = runTest [ "        G "
            , ".G GGH GH "
            ] $ do
      enable $ nameIsYou GigglesName
      enable $ nameIsStop GigglesName
      enable $ nameIsStop SheetsName
      move E
      check [ ". GGGH GHG"
            ]

walkOntoObstacleWhileSomeAreStop :: IO ()
walkOntoObstacleWhileSomeAreStop
  = runTest [ ".ABH BAH"
            ] $ do
      enable $ nameIsYou AName
      enable $ nameIsYou BName
      enable $ nameIsStop BName
      enable $ nameIsStop SheetsName
      move E
      check [ "      B "
            , ".ABH  AH"
            ]

youAndStopMoveInUnison :: IO ()
youAndStopMoveInUnison
  = runTest [ "     "
            , ".GGG "
            ] $ do
      enable $ nameIsYou GigglesName
      enable $ nameIsStop GigglesName
      move E
      check [ ". GGG"
            ]

youAndStopStopInUnison :: IO ()
youAndStopStopInUnison
  = runTest [ ".GGGH"
            ] $ do
      enable $ nameIsYou GigglesName
      enable $ nameIsStop GigglesName
      enable $ nameIsStop SheetsName
      move E
      check [ ".GGGH"
            ]

pushTest :: IO ()
pushTest
  = runTest [ "                      G  G  "
            , ".GA  GAH GB  GBH GAA  B  BA "
            ] $ do
      enable $ nameIsYou GigglesName
      enable $ nameIsPush AName
      enable $ nameIsPush BName
      enable $ nameIsStop BName
      enable $ nameIsStop SheetsName
      move E
      check [ "      G                     "
            , ". GA  AH  GB GBH  GAA BG BGA"
            ]


checkParser
  :: [[Word]]
  -> [Rule]
  -> IO ()
checkParser input expected = do
  let (actual, report)
        = Earley.fullParses (Earley.parser grammar) input
  if | Earley.unconsumed report /= [] -> do
         hPutStrLn stderr $ show report
         exitFailure
     | Set.fromList actual /= Set.fromList expected -> do
         hPutStrLn stderr "expected:"
         hPutStrLn stderr $ "  " ++ show (Set.fromList expected)
         hPutStrLn stderr "actual:"
         hPutStrLn stderr $ "  " ++ show (Set.fromList actual)
         exitFailure
     | otherwise -> do
         pure ()

grammarTest :: IO ()
grammarTest = do
  checkParser
    [[NameWord GigglesName], [IsWord], [YouWord]]
    [nameIsYou GigglesName]
  checkParser
    [[NameWord SheetsName], [IsWord], [StopWord]]
    [nameIsStop SheetsName]
  checkParser
    [[NameWord TextName], [IsWord], [PushWord]]
    [nameIsPush TextName]
  checkParser
    [ [NameWord GigglesName, NameWord SheetsName]
    , [IsWord, StopWord]
    , [YouWord, PushWord]
    ]
    [ nameIsYou GigglesName
    , nameIsPush GigglesName
    , nameIsYou SheetsName
    , nameIsPush SheetsName
    ]
  checkParser
    [[NameWord GigglesName], [OnWord], [NameWord BName], [IsWord], [YouWord]]
    [ (GigglesName `On` NameSubject BName) `Is` You
    , nameIsYou BName
    ]

ruleDetectionTest
  :: IO ()
ruleDetectionTest = do
  let expected = Set.fromList
        [ nameIsYou GigglesName
        , nameIsPush TextName
        ]
  let actual = detectRules level1
  when (expected /= actual) $ do
    hPutStrLn stderr $ "expected: " ++ show expected
    hPutStrLn stderr $ "expected: " ++ show actual
    exitFailure


testAll :: IO ()
testAll = do
  walkOntoObstacleMidLevel
  walkOntoObstacleAtWorldsEnd
  walkOntoObstacleWhileStop
  walkOntoObstacleWhileSomeAreStop
  youAndStopMoveInUnison
  youAndStopStopInUnison
  pushTest
  grammarTest
  ruleDetectionTest
  putStrLn "PASSED"
