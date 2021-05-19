module Main where

import LifeGame
import LifeGame.Console

main :: IO ()
main = do
  loopOut =<< getLifeGameFromFile "data/penta-decathlon.lifegame"
  return ()

