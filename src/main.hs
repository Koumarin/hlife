module Main where

import System.Console.ANSI
import System.IO
import Control.Exception (catch, throwIO, AsyncException(UserInterrupt))
import Data.Maybe

main :: IO ()
main = do
  ansi <- hSupportsANSI stdout
  if (not ansi)
    then putStr "I'm sorry, this game only runs on ANSI terminals.\n"
    else catch (do saveTitle
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
                     resetTerminal)
         (\e -> if e == UserInterrupt
                then resetTerminal
                else throwIO e)

-- Be friendly and reset the terminal state.
resetTerminal = do
    setSGR [Reset]
    restoreTitle
    -- Put the cursor at the lowest line we can.
    setCursorPosition 999 0
    putStr "\n"

mainloop :: (Int, Int) -> (Int, Int) -> [[Bool]] -> IO ()
mainloop cursor size state = let (y, x) = cursor
                             in do
  setCursorPosition y x
  c <- hGetChar stdin
  case c of
    -- Movement   Y,  X
    'h' -> move ( 0, -1)
    'l' -> move ( 0,  1)
    'j' -> move ( 1,  0)
    'k' -> move (-1,  0)
    'y' -> move (-1, -1)
    'u' -> move (-1,  1)
    'b' -> move ( 1, -1)
    'n' -> move ( 1,  1)
    -- I'm thinkin space should maybe be a step? Or maybe pause/unpause
    -- if i can figure out how to add input timeout.
    ' ' -> step
    -- Add a live cell at cursor position.
    's' -> setCell True
    -- Make cell at cursor position die.
    'd' -> setCell False
    -- Generate a random state.
    'r' -> do setCursorPosition 999 0
              clearLine
              putStr "Sorry I didn't write the random state generator yet :("
              mainloop cursor size state
    -- Load a pattern (displays it under the cursor and lets you place it with 's').
    'e' -> do setCursorPosition 999 0
              clearLine
              putStr "Sorry I didn't write the pattern loader yet :("
              mainloop cursor size state
    -- Quit.
    'q' -> return ()
    _   -> mainloop cursor size state
  where
    -- Move cursor by an increment.
    -- TODO: Add bounds checking.
    move :: (Int, Int) -> IO ()
    move delta = mainloop (pointAdd cursor delta) size state
    -- Set cell under cursor position.
    setCell :: Bool -> IO ()
    setCell b = let nextState = atyxPut cursor b state
                in do
      -- Only redraw the character we're setting.
      putStr (if b then "#" else " ")
      mainloop cursor size nextState
    -- Perform a step (ideally this should have been pause/unpause).
    step = let nextState = lifeStep state size
           in do
      draw size nextState
      mainloop cursor size nextState

draw :: (Int, Int) -> [[Bool]] -> IO ()
draw size state = do
  hideCursor
  saveCursor
  setCursorPosition 0 0
  putStr $ lifeToString size state
  restoreCursor
  showCursor

-- Either get terminal size when our terminal understands the keycode
-- or we default to 80x24.
screenSize :: IO (Int, Int)
screenSize = do
  size <- getTerminalSize
  if isJust size
    then return (fromJust size)
    else return (24, 80)

-- ANSI sequences I didn't find on the library.
saveTitle =    do hPutStr stdout "\ESC[22;0t"; hFlush stdout
restoreTitle = do hPutStr stdout "\ESC[23;0t"; hFlush stdout

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

-- Some cool patterns (maybe add support to load them?)
patternHeart = [
  [ True, False, False,  True],
  [ True,  True,  True, False]]

patternGlider = [
  [False, False,  True],
  [ True, False,  True],
  [False,  True,  True]]

patternC2Ortho = [
  [False,  True, False, False,  True],
  [ True, False, False, False, False],
  [ True, False, False, False,  True],
  [ True,  True,  True,  True, False]]

patternPrePulsar = [
  [ True,  True,  True, False, False, False,  True,  True,  True],
  [ True, False,  True, False, False, False,  True, False,  True],
  [ True,  True,  True, False, False, False,  True,  True,  True]]

patternQueenBeeShuttle = [
  [False, False, False, False, False, False, False, False, False,  True],
  [False, False, False, False, False, False, False,  True, False,  True],
  [False, False, False, False, False, False,  True, False,  True, False],
  [ True,  True, False, False, False,  True, False, False,  True, False, False, False, False, False, False,
   False, False, False, False, False,  True,  True],
  [ True,  True, False, False, False, False,  True, False,  True, False, False, False, False, False, False,
   False, False, False, False, False,  True,  True],
  [False, False, False, False, False, False, False,  True, False,  True],
  [False, False, False, False, False, False, False, False, False,  True]]

------------------------------------------------------------
-- Drawing
------------------------------------------------------------

lifeToString :: (Int, Int) -> [[Bool]] -> String
lifeToString (height, width) state =
  foldr (\line rest -> (draw line) ++ rest) "" state
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
