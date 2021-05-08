module Main where

import Zipper
import ZipperT
import Zipper2

main :: IO ()
main = do
  let z = listToZipper [0..3] :: Zipper Integer
  let zz = listToZipper2 [
           [0..2],
           [10..13],
           [20..23]
           ] :: Zipper2 Integer
  print zz
  print . duplicate $ zz
  print . neighbourhood $ zz
