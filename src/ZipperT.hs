module ZipperT (
    ZipperT(MkZipperT,runZipperT),
    duplicate,
  ) where
import Control.Comonad ( Comonad(duplicate, extract) )
import Control.Comonad.Trans.Class ( ComonadTrans(..) )
import Control.Monad ( (<=<) )
import Data.Maybe (catMaybes,isJust,fromJust)
import Data.Functor.Classes (Show1,liftShowsPrec,showsPrec1,showsUnaryWith,liftShowList)
import Zipper ( Nbhd(..), Zipper(..), iterateMaybe, left, right )

newtype ZipperT w a = MkZipperT {runZipperT :: w (Zipper a)}
  deriving (Functor)
instance ComonadTrans ZipperT where
  lower = fmap extract . runZipperT
instance (Comonad w) => Comonad (ZipperT w) where
  extract = extract . extract . runZipperT
  duplicate = fmap MkZipperT . MkZipperT
              . fmap innerDupulicate . duplicate . runZipperT
instance (Show1 w) => Show1 (ZipperT w) where
  liftShowsPrec sp sl d (MkZipperT wZa) =
    showsUnaryWith (liftShowsPrec sp' sl') "ZipperT" d wZa
     where
       sp' = liftShowsPrec sp sl
       sl' = liftShowList sp sl
instance (Show1 w, Show a) => Show (ZipperT w a) where
  showsPrec = showsPrec1

instance (Nbhd w) => Nbhd (ZipperT w) where
  neighbourhood = neighbourhood <=< neighbourhood . runZipperT

innerDupulicate :: (Comonad w) => w (Zipper a) -> Zipper (w (Zipper a))
innerDupulicate wz = MkZipper ls wz rs
  where
    ls = tail . iterateMaybe leftW $ wz
    rs = tail . iterateMaybe rightW $ wz
    leftW, rightW :: (Comonad w) => w (Zipper a) -> Maybe (w (Zipper a))
    leftW = wMb2MbW . fmap left
    rightW= wMb2MbW . fmap right
    -- w ○ Maybe の w と Maybe を交換
    wMb2MbW  :: (Comonad w) => w (Maybe a)-> Maybe(w a)
    wMb2MbW wMbA =
      if isJust . extract $ wMbA
      then Just . fmap fromJust $ wMbA
      else Nothing
      -- extract の結果を信用して他も取り出すので危険
