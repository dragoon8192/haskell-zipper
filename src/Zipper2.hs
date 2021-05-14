module Zipper2 (
    Zipper2, listToZipper2
  ) where
import Zipper ( listToZipper, zipperToList, Zipper )
import ZipperT ( ZipperT(..) )

type Zipper2 = ZipperT Zipper

listToZipper2 :: [[a]] -> Zipper2 a
listToZipper2 = MkZipperT . listToZipper . map listToZipper
zipperToList2 :: Zipper2 a -> [[a]]
zipperToList2 = map zipperToList . zipperToList . runZipperT
