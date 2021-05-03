module Zipper (
    Zipper(MkZipper), left, right, listToZipper,
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
  show (MkZipper ls c rs) = (show . reverse) ls ++"+"++ show c ++"+"++ show rs

instance Comonad Zipper where
  extract (MkZipper _ c _)  = c
  duplicate z = MkZipper ls z rs
    where
      ls = tail . iterateMaybe left $ z
      rs = tail . iterateMaybe right $ z

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = catMaybes . takeWhile isJust . iterateM f . Just
--Nothing の連続に対応するため takeWhile isJust
  where
    iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
    iterateM = iterate . (=<<)

left, right :: Zipper a -> Maybe (Zipper a)
left  (MkZipper [] _ _) = Nothing
left  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
right (MkZipper _ _ []) = Nothing
right (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs

listToZipper :: [a] -> Zipper a
listToZipper (x:xs) = MkZipper [] x xs

--innerDupulicate :: (Functor f) => f (Zipper a) -> Zipper (f (Zipper a))
--innerDupulicate fz = MkZipper ls fz rs
--  where
--    ls = tail . iterateMaybe leftF $ fz
--    rs = tail . iterateMaybe rightF $ fz
--
--leftF, rightF :: (Functor f) => f (Zipper a) -> Maybe (f (Zipper a))
--leftF = fmap left
--leftF  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
--rightF (MkZipper _ _ []) = Nothing
--rightF (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs
