module LifeGame (
  LifeCell(..), LifeGame(..),
  life, lifeGoes,
  getLifeGameFromFile,
  ) where
import Zipper2 ( listToZipper2, Nbhd(neighbourhood), Zipper2 )
import Control.Comonad ( Comonad(extend, extract) )
import Control.Monad
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )

data LifeCell = Dead | Birth | Live | Dying
  deriving (Show, Enum)

boolToLifeCell :: Bool -> LifeCell
boolToLifeCell True = Live
boolToLifeCell False = Dead

charToLifeCell :: Char -> LifeCell
charToLifeCell = boolToLifeCell . ('1'==)

strListToLifeGame :: [[Char]] -> LifeGame
strListToLifeGame = listToZipper2 . map (map charToLifeCell)

lifeCellToBool :: LifeCell -> Bool
lifeCellToBool Birth = True
lifeCellToBool Live = True
lifeCellToBool Dying = False
lifeCellToBool Dead = False

type LifeGame = Zipper2 LifeCell

life :: LifeGame -> LifeCell
life zzc
  | cellIs      && elem nbhdsCount liveCounts   = Live
  | cellIs      && otherwise                    = Dying
  | not cellIs  && elem nbhdsCount birthCounts  = Birth
  | otherwise                                   = Dead
  where
    cellIs = lifeCellToBool . extract $ zzc
    nbhdsCount = length . filter lifeCellToBool . neighbourhood $ zzc
    liveCounts = [3, 4]
    -- 自分自身も含んでいる
    birthCounts = [3]

lifeGoes :: LifeGame -> LifeGame
lifeGoes = extend life

------
-- IO
------

getLifeGameFromFile :: String -> IO LifeGame
getLifeGameFromFile =
  flip openFile ReadMode
  >=> hGetContents
  >=> return . strListToLifeGame . lines

--getLifeGameFromFile filename = do
--  contents <- hGetContents =<<
--    openFile filename ReadMode
--  return . strListToLifeGame . lines $ contents
