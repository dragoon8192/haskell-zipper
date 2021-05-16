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

showIcon :: LifeCell -> String
showIcon Dead   = "  "
showIcon Birth  = "🌱"
showIcon Live   = "🍄"
showIcon Dying  = "--"

charToBool :: Char -> Bool
charToBool = ('1'==)
boolToLifeCell :: Bool -> LifeCell
boolToLifeCell True = Live
boolToLifeCell False = Dead
charToLifeCell :: Char -> LifeCell
charToLifeCell = boolToLifeCell . charToBool

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

lifeGoes = extend life

showIcons :: LifeGame -> String
showIcons = unlines . fmap concat . zipperToList2 . fmap showIcon

------
-- IO
------
getLifeGameFromFile :: String -> IO LifeGame
getLifeGameFromFile filename = do
  contents <- hGetContents =<<
    openFile filename ReadMode
  return . strListToLifeGame . lines $ contents

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
