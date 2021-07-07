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
      enable $ NameIsYou GigglesName
      enable $ NameIsYou AName
      enable $ NameIsYou BName
      enable $ NameIsYou CName
      enable $ NameIsStop SheetsName
      move E
      check [ "  G   "
            , ". AHBC"
            ]

walkOntoObstacleAtWorldsEnd :: IO ()
walkOntoObstacleAtWorldsEnd
  = runTest [ "   B "
            , ".GAHC"
            ] $ do
      enable $ NameIsYou GigglesName
      enable $ NameIsYou AName
      enable $ NameIsYou BName
      enable $ NameIsYou CName
      enable $ NameIsStop SheetsName
      move E
      check [ "  G B"
            , ". AHC"
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
      enable $ NameIsYou AName
      enable $ NameIsYou BName
      enable $ NameIsStop BName
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
      enable $ NameIsPush AName
      enable $ NameIsPush BName
      enable $ NameIsStop BName
      enable $ NameIsStop SheetsName
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
    [NameIsYou GigglesName]
  checkParser
    [[NameWord SheetsName], [IsWord], [StopWord]]
    [NameIsStop SheetsName]
  checkParser
    [[NameWord TextName], [IsWord], [PushWord]]
    [NameIsPush TextName]
  checkParser
    [ [NameWord GigglesName, NameWord SheetsName]
    , [IsWord, StopWord]
    , [YouWord, PushWord]
    ]
    [ NameIsYou GigglesName
    , NameIsPush GigglesName
    , NameIsYou SheetsName
    , NameIsPush SheetsName
    ]

ruleDetectionTest
  :: IO ()
ruleDetectionTest = do
  let expected = Set.fromList
        [ NameIsYou GigglesName
        , NameIsPush TextName
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
