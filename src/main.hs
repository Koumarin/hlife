import System.Console.ANSI
import System.IO
import Data.Maybe

pointAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointAdd (y, x) (dy, dx) = (y + dy, x + dx)

screenSize :: IO (Int, Int)
screenSize = do
  saveCursor
  -- XTerm resize does it like this i think.
  setCursorPosition 999 999
  size <- getCursorPosition
  restoreCursor
  -- TODO: Learn how maybe works lmao
  if isJust size
    then return (fromJust size)
    else return (80, 24)

main :: IO ()
main = do
  setTitle "Conway's Game of Life"
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  showCursor
  (height, width) <- screenSize
  let middle = (div height 2, div width 2)
    in mainloop middle
  -- Be friendly and reset the terminal state lol.
  setSGR [Reset]
  -- Put the cursor at the lowest line we can.
  setCursorPosition 999 0

mainloop :: (Int, Int) -> IO ()
mainloop cursor = let (y, x) = cursor
                  in do
  setCursorPosition y x
  c <- hGetChar stdin
  case c of
    'h' -> move cursor ( 0, -1)
    'l' -> move cursor ( 0,  1)
    'j' -> move cursor ( 1,  0)
    'k' -> move cursor (-1,  0)
    'y' -> move cursor (-1, -1)
    'u' -> move cursor (-1,  1)
    'b' -> move cursor ( 1, -1)
    'n' -> move cursor ( 1,  1)
    'q' -> return ()
    _   -> mainloop cursor

move :: (Int, Int) -> (Int, Int) -> IO ()
move pos delta = mainloop (pointAdd pos delta)
