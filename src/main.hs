module Main where

import System.Console.ANSI
import System.IO
import Data.Maybe

main :: IO ()
main = do
  ansi <- hSupportsANSI stdout
  if (not ansi)
    then putStr "I'm sorry, this game only runs on ANSI terminals.\n"
    else do
    setTitle "Conway's Game of Life"
    hSetEcho      stdin  False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    showCursor
    size <- screenSize
    let (height, width) = size
        middle          = (div height 2, div width 2)
        blank           = replicate height (replicate width False)
      in do
      draw size blank
      mainloop middle size blank
    -- Be friendly and reset the terminal state lol.
    setSGR [Reset]
    -- Put the cursor at the lowest line we can.
    setCursorPosition 999 0

mainloop :: (Int, Int) -> (Int, Int) -> [[Bool]] -> IO ()
mainloop cursor size state = let (y, x) = cursor
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
    ' ' -> step
    -- Add a live cell at cursor position.
    's' -> setCell True
    -- Make cell at cursor position die.
    'd' -> setCell False
    -- Quit.
    'q' -> return ()
    _   -> mainloop cursor size state
  where
    -- Move cursor by an increment.
    -- TODO: Add bounds checking.
    move :: (Int, Int) -> (Int, Int) -> IO ()
    move pos delta = mainloop (pointAdd pos delta) size
    -- Set cell under cursor position.
    setCell :: Bool -> IO ()
    setCell b = let nextState = atyxPut cursor b state
                in do
      draw size nextState
      mainloop cursor size nextState
    -- Perform a step (ideally this should have been pause/unpause).
    step = let nextState = lifeStep state size
           in do
      draw size nextState
      mainloop cursor size nextState

draw :: (Int, Int) -> [[Bool]] -> IO ()
draw size state = do
  clearScreen
  setCursorPosition 0 0
  putStr $ lifeToString size state

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

within :: (Int, Int) -> Int -> Bool
within (low, hi) n = n >= low &&
                     n <= hi

------------------------------------------------------------
-- Example states
------------------------------------------------------------

-- Successive applications of lifeStep on each glider state.
glider = [
  [False, False, False, False, False],
  [False, False,  True, False, False],
  [ True, False,  True, False, False],
  [False,  True,  True, False, False],
  [False, False, False, False, False]]

glider' = [
  [False, False, False, False, False],
  [False,  True, False, False, False],
  [False, False,  True,  True, False],
  [False,  True,  True, False, False],
  [False, False, False, False, False]]

glider'' = [
  [False, False, False, False, False],
  [False, False,  True, False, False],
  [False, False, False,  True, False],
  [False,  True,  True,  True, False],
  [False, False, False, False, False]]

glider''' = [
  [False, False, False, False, False],
  [False, False, False, False, False],
  [False,  True, False,  True, False],
  [False, False,  True,  True, False],
  [False, False,  True, False, False]]

------------------------------------------------------------
-- Drawing
------------------------------------------------------------

lifeToString :: (Int, Int) -> [[Bool]] -> String
lifeToString (height, width) state =
  foldr (\line rest -> (draw line) ++ "\n" ++ rest) "" state
  where
    draw :: [Bool] -> String
    draw [] = ""
    draw (x:xs)
      | x         = '#' : draw xs
      | otherwise = ' ' : draw xs

------------------------------------------------------------
-- Rules
------------------------------------------------------------

lifeStep :: [[Bool]] -> (Int, Int) -> [[Bool]]
lifeStep state (height, width) =
  map (\i -> map (\j -> lives (i, j))
                 [0 .. width - 1])
      [0 .. height - 1]
  where
    lives :: (Int, Int) -> Bool
    lives cell
      | (&&) (dead cell)
             (neighbors == 3)          = True
      | (&&) (live cell)
             (within (3, 4) neighbors) = True
      | otherwise                      = False
      where
        neighbors = mooreNeighbors cell state
    live = (\c -> atyx c state)
    dead = (\c -> not $ live c)

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
slice from to list = take (to + 1 - (max from 0)) (drop from list)

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
