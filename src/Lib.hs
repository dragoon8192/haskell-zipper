module Lib
    ( someFunc
    ) where
import Control.Comonad
import Zipper

someFunc :: IO ()
someFunc = do
  let z = MkZipper [4,3..1] 5 [6..9]
  print z
  print . left $ z
  print . right $ z
  print . extract $ z
  print . duplicate $ z
