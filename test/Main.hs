module Main where

import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified Data.Set as Set

import Level
import World


newtype MyTest a = MyTest
  { unMyTest :: ExceptT [String] (State World) a
  }

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

check :: [String] -> MyTest ()
check expected = MyTest $ do
  lvl <- lift (gets level)
  let actual = pprintLevel lvl
  unless (actual == expected) $ do
    throwE $ ["expected:"]
          ++ (fmap ("  " ++) expected)
          ++ ["got:"]
          ++ (fmap ("  " ++) actual)

passingTest :: IO ()
passingTest
  = runTest [ "  X "
            , ".GgG"
            ] $ do
      check [ "  X "
            , ".GgG"
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
