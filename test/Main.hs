module Main where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable (for_)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Level
import World


newtype MyTest a = MyTest
  { unMyTest :: ExceptT [String] (State World) a
  }

runTest :: [String] -> MyTest () -> IO ()
runTest stringLevel myTest = do
  let s0 = World
         { level = parseLevel stringLevel
         , rules = []
         }
  case evalState (runExceptT $ unMyTest myTest) s0 of
    Left errorLines -> do
      for_ errorLines $ \errorLine -> do
        hPutStrLn stderr errorLine
      exitFailure
    Right () -> do
      pure ()

check :: [String] -> MyTest ()
check stringLevel = MyTest $ do
  lvl <- lift (gets level)
  unless ( levelArray lvl
        == levelArray (parseLevel stringLevel)
         ) $ do
    throwE $ ["unexpected level state:"]
          ++ (fmap ("  " ++) stringLevel)

passingTest :: IO ()
passingTest
  = runTest [ "X"
            ] $ do
      check [ "X"
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
  passingTest
