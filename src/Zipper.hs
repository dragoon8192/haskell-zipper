module Zipper (
    Zipper(MkZipper), left, right, listToZipper, zipperToList,
    iterateMaybe,
    Nbhd(neighbourhood),
    duplicate,
  ) where
import Control.Comonad ( Comonad(duplicate, extract) )
import Data.Maybe ( catMaybes, isJust )
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
  (<*>) (MkZipper fls fc frs) (MkZipper xls xc xrs)
    = MkZipper  (zipWith ($) fls xls)
                (fc xc)
                (zipWith ($) frs xrs)
instance Comonad Zipper where
  extract (MkZipper _ c _)  = c
  duplicate z = MkZipper ls z rs
    where
      ls = tail . iterateMaybe left $ z
      rs = tail . iterateMaybe right $ z

class Nbhd w where
  neighbourhood :: w a -> [a]
instance Nbhd Zipper where
  neighbourhood z = fmap extract zs
    where
      zs = catMaybes [f z | f <- [left,Just,right]]

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f =
  catMaybes . takeWhile isJust . iterate (f=<<) . Just
--Nothing の連続に対応するため takeWhile isJust

left, right :: Zipper a -> Maybe (Zipper a)
left  (MkZipper [] _ _) = Nothing
left  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
right (MkZipper _ _ []) = Nothing
right (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs

listToZipper :: [a] -> Zipper a
listToZipper (x:xs) = MkZipper [] x xs
zipperToList :: Zipper a -> [a]
zipperToList (MkZipper ls c rs) = reverse ls ++ c : rs
