module Main where

import LifeGame

main :: IO ()
main = do
  loopOut =<< getLifeGameFromFile "data/penta-decathlon.lifegame"
  return ()

