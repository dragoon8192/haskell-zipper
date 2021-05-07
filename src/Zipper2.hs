module Zipper2 (
    Zipper2, listToZipper2
  ) where
import Control.Comonad
import Control.Monad
import Data.Maybe
import Data.List
import Zipper
import ZipperT

--------------------------
-- Zipper2
--------------------------
type Zipper2 = ZipperT Zipper
--newtype Zipper2 a where
--  MkZipper2 :: Zipper (Zipper a) -> Zipper2 a
--  deriving (Functor,Show)
--instance Comonad Zipper2 where
--  extract (MkZipper2 zz) = extract . extract $ zz
--  duplicate (MkZipper2 zz) = fmap MkZipper2 $ MkZipper2 $ extend duplicate zz

listToZipper2 :: [[a]] -> Zipper2 a
listToZipper2 = MkZipperT . listToZipper . map listToZipper
zipperToList2 :: Zipper2 a -> [[a]]
zipperToList2 = map zipperToList . zipperToList . runZipperT
