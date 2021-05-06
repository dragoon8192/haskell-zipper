module Zipper (
    Zipper(MkZipper), left, right, listToZipper, zipperToList,
    iterateMaybe,
    module Control.Comonad
  ) where
import Control.Comonad
import Control.Monad
import Data.Maybe
import Data.List

--------------------------
-- Zipper
--------------------------
data Zipper a = MkZipper [a] a [a]
  deriving (Functor)

instance (Show a) => Show (Zipper a) where
  show (MkZipper ls c rs) =
    (show . reverse) ls ++ '+' : show c ++ '+' : show rs

instance Comonad Zipper where
  extract (MkZipper _ c _)  = c
  duplicate z = MkZipper ls z rs
    where
      ls = tail . iterateMaybe left $ z
      rs = tail . iterateMaybe right $ z

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
