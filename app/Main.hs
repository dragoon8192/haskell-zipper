module Main where

import Zipper
import Zipper2

main :: IO ()
main = do
  let z = listToZipper [0..3] :: Zipper Integer
  let zz = listToZipper2 [
           [0..2],
           [10..12],
           [20..22]
           ] :: Zipper2 Integer
  print zz
  print . duplicate $ zz
  putStr "neighbourhood"
  print . neighbourhood $ zz
  print . neighbourhood .duplicate $ zz
