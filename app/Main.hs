module Main where

import Zipper

main :: IO ()
main = do
  let z = listToZipper [0..3]
  print z
  print . left $ z
  print . right $ z
  print . extract $ z
