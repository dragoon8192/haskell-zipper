module Zipper
  ( Zipper (MkZipper), left, right
  ) where
import Control.Comonad
import Control.Monad
import Data.Maybe

data Zipper a = MkZipper [a] a [a]
  deriving (Show, Functor)

left, right :: Zipper a -> Maybe (Zipper a)
left  (MkZipper [] _ _) = Nothing
left  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
right (MkZipper _ _ []) = Nothing
right (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs

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
