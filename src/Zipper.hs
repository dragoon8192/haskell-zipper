module Zipper where
import Control.Comonad
import Data.Maybe

data Zipper a = MkZipper [a] a [a]
  deriving Show

left, right :: Zipper a -> Maybe (Zipper a)
left  (MkZipper [] _ _) = Nothing
left  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
right (MkZipper _ _ []) = Nothing
right (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs

instance Functor Zipper where
  fmap f (MkZipper ls c rs) = MkZipper (fmap f ls) (f c) (fmap f rs)

instance Comonad Zipper where
  extract (MkZipper _ c _)  = c
  duplicate z = MkZipper (tail . iterateMaybe left $ z) z (tail . iterateMaybe right $ z)

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f = catMaybes . takeWhile isJust . iterateM f . Just
  --Nothing の連続に対応するため takeWhile isJust
  where iterateM :: (Monad m) => (a -> m a) -> m a -> [m a]
        iterateM f = iterate (>>=f)
