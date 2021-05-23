module Main where

import System.Console.ANSI
import System.IO
import Data.Maybe

main :: IO ()
main = do
  setTitle "Conway's Game of Life"
  hSetEcho stdin False
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  showCursor
  size <- screenSize
  let middle = (\(h, w) -> (div h 2, div w 2)) size
    in mainloop middle size
  -- Be friendly and reset the terminal state lol.
  setSGR [Reset]
  -- Put the cursor at the lowest line we can.
  setCursorPosition 999 0

mainloop :: (Int, Int) -> (Int, Int) -> IO ()
mainloop cursor size = let (y, x) = cursor
                       in do
  setCursorPosition y x
  c <- hGetChar stdin
  case c of
    -- Movement keys     Y,  X
    'h' -> move cursor ( 0, -1)
    'l' -> move cursor ( 0,  1)
    'j' -> move cursor ( 1,  0)
    'k' -> move cursor (-1,  0)
    'y' -> move cursor (-1, -1)
    'u' -> move cursor (-1,  1)
    'b' -> move cursor ( 1, -1)
    'n' -> move cursor ( 1,  1)
    -- I'm thinkin space should maybe be a step? Or maybe pause/unpause
    -- if i can figure out how to add input timeout.
    ' ' -> do print cursor; mainloop cursor size
    -- Add a live cell at cursor position.
    's' -> do putStr "#"; mainloop cursor size
    -- Quit.
    'q' -> return ()
    _   -> mainloop cursor size
  where
    -- Move cursor by an increment.
    move :: (Int, Int) -> (Int, Int) -> IO ()
    move pos delta = mainloop (pointAdd pos delta) size

-- Either get terminal size when our terminal understands the keycode
-- or we default to 80x24.
screenSize :: IO (Int, Int)
screenSize = do
  size <- getTerminalSize
  if isJust size
    then return (fromJust size)
    else return (24, 80)

pointAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
pointAdd (y, x) (dy, dx) = (y + dy, x + dx)

------------------------------------------------------------
-- Example states
------------------------------------------------------------

-- The state after glider is glider'.
glider = [
  [False, False, False, False, False],
  [False, False, False,  True, False],
  [False,  True, False,  True, False],
  [False, False,  True,  True, False],
  [False, False, False, False, False]]

glider' = [
  [False, False, False, False, False],
  [False,  True, False, False, False],
  [False, False,  True,  True, False],
  [False,  True,  True, False, False],
  [False, False, False, False, False]]

------------------------------------------------------------
-- Drawing
------------------------------------------------------------

lifeToString :: (Int, Int) -> [[Bool]] -> String
lifeToString (height, width) state =
  foldr (\line rest -> (draw line) ++ "\n" ++ rest) "" screen
  where
    -- Outer frame isn't printed.
    screen = sliceBox (1, 1) (height, width) state

    draw :: [Bool] -> String
    draw [] = ""
    draw (x:xs)
      | x         = '#' : draw xs
      | otherwise = ' ' : draw xs

------------------------------------------------------------
-- Rules
------------------------------------------------------------

-- Count the number of cells in a 3x3 centered in (y, x).
mooreNeighbors :: (Int, Int) -> [[Bool]] -> Int
mooreNeighbors (y, x) state = count neighbors
  where
    -- Get only the 3x3 square around (y, x).
    neighbors = sliceBox (y - 1, x - 1) (y + 1, x + 1) state
    -- Count the number of alive cells.
    count :: [[Bool]] -> Int
    count blist = foldr (+) 0 (map (\line -> length $ filter (\b -> b) line)
                                   blist)

------------------------------------------------------------
-- Functions for utilizing arrays.
------------------------------------------------------------

-- Take the matrix from origin to corner.
sliceBox :: (Int, Int) -> (Int, Int) -> [[a]] -> [[a]]
sliceBox (y0, x0) (y, x) matrix = map (slice x0 x) (slice y0 y matrix)

slice :: Int -> Int -> [a] -> [a]
slice from to list = take (to - from + 1) (drop from list)

at :: Int -> [a] -> a
at 0 (x:_)  = x
at n (_:xs) = at (n - 1) xs
at _ []     = error "Out of bounds!"

atPut :: Int -> a -> [a] -> [a]
atPut 0 x' (_:xs) = x' : xs
atPut n x' (x:xs) = x  : (atPut (n - 1) x' xs)
atPut _ _  []     = error "Out of bounds!"

atyx :: (Int, Int) -> [[a]] -> a
atyx (y, x) lst = at x (at y lst)

atyxPut :: (Int, Int) -> a -> [[a]] -> [[a]]
atyxPut (y, x) elt lst = atPut y (atPut x elt (at y lst)) lst
