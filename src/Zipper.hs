module Zipper (
    Zipper(MkZipper), left, right, listToZipper, zipperToList,
    iterateZipMb, iterateZip,
    Comonad(..), Nbhd(..),
  ) where
import Control.Comonad ( Comonad(..) )
import Data.Maybe ( isJust, catMaybes, fromJust )
import Data.Functor.Classes ( showsPrec1, Show1(liftShowsPrec) )

data Zipper a
  = MkZipper {leftList :: [a], center :: a, rightList :: [a]}
  deriving (Functor)

instance Show1 Zipper where
  liftShowsPrec sp sl d (MkZipper ls c rs) =
    (sl . reverse $ ls)
    . ('<':) . sp d c . ('>':)
    . sl rs
instance (Show a) => Show (Zipper a) where
  showsPrec = showsPrec1
instance Applicative Zipper where
  pure x = MkZipper (repeat x) x (repeat x)
  -- 自信ないなあ
  (<*>) (MkZipper fls fc frs) (MkZipper xls xc xrs)
    = MkZipper  (zipWith ($) fls xls)
                (fc xc)
                (zipWith ($) frs xrs)

instance Comonad Zipper where
  extract (MkZipper _ c _)  = c
  duplicate = iterateZipMb left right

class Nbhd w where
  neighbourhood :: w a -> [a]
instance Nbhd Zipper where
  neighbourhood z = fmap extract zs
    where
      zs = catMaybes [f z | f <- [left,Just,right]]

fromZipJust :: Zipper (Maybe a) -> Zipper a
fromZipJust = fromJust . zipMbToMbZip
  where
    zipMbToMbZip :: Zipper (Maybe a) -> Maybe (Zipper a)
    -- zipMbToMbZip (MkZipper _ Nothing _) = Nothing
    zipMbToMbZip = Just . zMap (catMaybes . takeWhile isJust)
      -- Nothing の連続に対応するため takeWhile isJust

zMap :: ([a] -> [b]) -> Zipper a -> Zipper b
zMap f (MkZipper ls c rs) = MkZipper (f ls) (head . f $ [c]) (f rs)

iterateZip :: (a -> a) -> (a -> a) -> a -> Zipper a
-- fl . fr = id を想定
iterateZip fl fr x = MkZipper (iterate fl (fl x)) x (iterate fr (fr x))

iterateZipMb :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Zipper a
iterateZipMb fl fr = fromZipJust . iterateZip (fl=<<) (fr=<<) . Just

left, right :: Zipper a -> Maybe (Zipper a)
left  (MkZipper [] _ _) = Nothing
left  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
right (MkZipper _ _ []) = Nothing
right (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs

listToZipper :: [a] -> Zipper a
listToZipper (x:xs) = MkZipper [] x xs
zipperToList :: Zipper a -> [a]
zipperToList (MkZipper ls c rs) = reverse ls ++ c : rs
