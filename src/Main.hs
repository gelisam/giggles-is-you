module Main where

import Codec.Picture
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Graphics.Gloss.Juicy
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact


main :: IO ()
main = do
  r <- runExceptT main'
  case r of
    Left err -> do
      error err
    Right () -> do
      pure ()

type M = ExceptT String IO
type W = (Float, Float)


mustBeJust :: Maybe a -> M a
mustBeJust Nothing = ExceptT $ pure $ Left "Nothing encountered"
mustBeJust (Just a) = pure a

reactWorld :: Event -> W -> W
reactWorld (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x+10, y)
reactWorld _ w = w

main' :: M ()
main' = do
  r1 <- ExceptT $ readImage "assets/images/giggles.png"
  r2 <- ExceptT $ readImage "assets/images/sheets.png"
  giggles <- mustBeJust $ fromDynamicImage r1
  sheets  <- mustBeJust $ fromDynamicImage r2
  let animation :: Float -> Picture
      animation t = translate (10*t) 0 giggles
                 <> sheets
  let displayWorld :: W -> Picture
      displayWorld (x,y) = translate x y giggles
                        <> sheets
  let stepWorld :: Float -> W -> W
      stepWorld dt (x,y) = (x, y)

  lift $ play (InWindow "Giggles is you" (200, 200) (-10, 10))
              white
              30
              (0, 0)
              displayWorld
              reactWorld
              stepWorld
