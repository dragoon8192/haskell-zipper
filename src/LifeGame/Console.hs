module LifeGame.Console (
  loopOut
  ) where
import LifeGame
import Zipper2
import System.Console.ANSI
    ( clearFromCursorToScreenEnd, cursorUpLine )
import Control.Concurrent ( threadDelay )

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
