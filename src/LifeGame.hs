module LifeGame (
  LifeCell(..), LifeGame(..),
  life, lifeGoes,
  getLifeGameFromFile, loopOut
  ) where
import Zipper2
    ( Nbhd(neighbourhood), listToZipper2, zipperToList2, Zipper2 )
import Control.Comonad ( Comonad(extend, extract) )
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import System.Console.ANSI
    ( clearFromCursorToScreenEnd, cursorUpLine )
import Control.Concurrent ( threadDelay )

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
  | cellIs && nbhdsCount > 2 && nbhdsCount < 5  = Live
  | cellIs && otherwise                         = Dying
  | not cellIs && nbhdsCount == 3               = Birth
  | otherwise                                   = Dead
  where
    cellIs = lifeCellToBool . extract $ zzc
    nbhdsCount = length . filter lifeCellToBool . neighbourhood $ zzc

lifeGoes :: LifeGame -> LifeGame
lifeGoes = extend life

------
-- IO
------

getLifeGameFromFile :: String -> IO LifeGame
getLifeGameFromFile filename = do
  contents <- hGetContents =<<
    openFile filename ReadMode
  return . strListToLifeGame . lines $ contents

showIcon :: LifeCell -> String
showIcon Dead   = "  "
showIcon Birth  = "🌱"
showIcon Live   = "🍄"
showIcon Dying  = " ."

showIcons :: LifeGame -> String
showIcons = unlines . fmap concat . zipperToList2 . fmap showIcon

loopOut :: LifeGame -> IO LifeGame
loopOut lg = loopLc n lg
  where
    n = length . zipperToList2 $ lg
    loopLc m x = do
      putStr . showIcons $ x
      threadDelay $ 500 * 1000
      cursorUpLine n
      clearFromCursorToScreenEnd
      loopLc m . lifeGoes $ x
