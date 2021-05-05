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
iterateMaybe f = catMaybes . takeWhile isJust . iterate (f=<<) . Just
--Nothing の連続に対応するため takeWhile isJust

left, right :: Zipper a -> Maybe (Zipper a)
left  (MkZipper [] _ _) = Nothing
left  (MkZipper (l:ls) c rs) = Just $ MkZipper ls l (c:rs)
right (MkZipper _ _ []) = Nothing
right (MkZipper ls c (r:rs)) = Just $ MkZipper (c:ls) r rs

listToZipper :: [a] -> Zipper a
listToZipper (x:xs) = MkZipper [] x xs

innerDupulicate :: (Comonad w) => w (Zipper a) -> Zipper (w (Zipper a))
innerDupulicate wz = MkZipper ls wz rs
  where
    ls = tail . iterateMaybe leftW $ wz
    rs = tail . iterateMaybe rightW $ wz
    leftW, rightW :: (Comonad w) => w (Zipper a) -> Maybe (w (Zipper a))
    leftW = wMb2MbW . fmap left
    rightW= wMb2MbW . fmap right

  -- MaybeT w ~= w ○ Maybe の w と Maybe を交換
wMb2MbW  :: (Comonad w) => w (Maybe a)-> Maybe(w a)
wMb2MbW wMbA =
  if isJust . extract $ wMbA
  then Just . fmap fromJust $ wMbA
  else const Nothing $ wMbA
  -- extract の結果を信用して他も取り出すので危険
