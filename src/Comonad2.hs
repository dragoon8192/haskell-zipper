module Comonad2 (
  ) where
import Control.Comonad

newtype W10 w1 w0 a where
  MkW10 :: w1 (w0 a) -> W10 w1 w0  a
instance (Functor w1, Functor w0) => Functor (W10 w1 w0) where
  fmap f (MkW10 w1w0)= MkW10 $ fmap10 f w1w0
    where
      fmap10 = fmap1 . fmap0
      fmap0 = fmap
      fmap1 = fmap

--instance (Comonad w1, Comonad w0) => Comonad (W10 w1 w0) where
--  extract (MkW10 w1w0) = extract10 w1w0
--    where
--      extract10 = extract1 . extract0
--      extract0  = extract
--      extract1  = extract
--  duplicate (MkW10 w1w0) = fmap MkW10 . MkW10 . duplicate10 $ w1w0
--    where
--      -- Todo
--      duplicate10 = duplicate0 . duplicate1
--      duplicate0 = duplicate
--      duplicate1 = duplicate

