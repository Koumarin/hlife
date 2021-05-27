module Main where

import System.Console.ANSI
import System.IO
import Control.Exception (catch, throw, SomeException)
import Data.Maybe

data Cell = Alive | Dead
  deriving (Eq)

-- TODO: add a way to read these back.
instance Show Cell where
  show Alive = "#"
  show Dead  = " "

main :: IO ()
main = do
  ansi <- hSupportsANSI stdout
  if (not ansi)
    then putStr "I'm sorry, this game only runs on ANSI terminals.\n"
    else catch (do saveTitle
                   setTitle "Conway's Game of Life"
                   hSetEcho      stdin  False
                   hSetBuffering stdin  NoBuffering
                   showCursor
                   size <- screenSize
                   let (height, width) = size
                       middle          = (div height 2, div width 2)
                       blank           = replicate height
                                                   (replicate width Dead)
                     in do
                     scrollPageUp (height - 1)
                     drawScreen blank
                     mainloop middle size blank
                     resetTerminal)
               -- Make sure to reset terminal state no matter what happens.
               (\e -> do resetTerminal; throw (e :: SomeException))
  where
    resetTerminal = do
      setSGR [Reset]
      restoreTitle
      -- Put the cursor at lowest line we can, so screen isn't cut.
      setCursorPosition 999 0
      putStr "\n"
      showCursor
    -- ANSI sequences I didn't find on the library.
    saveTitle    = hPutStr stdout "\ESC[22;0t"
    restoreTitle = hPutStr stdout "\ESC[23;0t"

mainloop :: (Int, Int) -> (Int, Int) -> [[Cell]] -> IO ()
mainloop cursor size state = let (y, x) = cursor
                             in do
  setCursorPosition y x
  hFlush stdout
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
    's' -> setCell Alive
    -- Make cell at cursor position die.
    'd' -> setCell Dead
    -- Generate a random state.
    'r' -> do setCursorPosition 999 0
              clearLine
              putStr "Sorry I didn't write the random state generator yet :("
              mainloop cursor size state
    -- Load a pattern.
    -- (displays it under the cursor and lets you place it with 's')
    'e' -> do setCursorPosition 999 0
              clearLine
              putStr "Sorry I didn't write the pattern loader yet :("
              mainloop cursor size state
    -- Quit.
    'q' -> return ()
    _   -> mainloop cursor size state
  where
    -- Move cursor by an increment.
    move = \delta -> let maybeCursor = pointAdd cursor delta
                         newCursor   = let origin = (0, 0)
                                           corner = pointAdd size (-1, -1)
                                       in if withinSquare origin
                                                          corner
                                                          maybeCursor
                                          then maybeCursor
                                          else cursor
                     in mainloop newCursor size state
    -- Set cell under cursor position.
    setCell = \cell -> let nextState = atyxPut cursor cell state
                       in do
      -- Only redraw the character we're setting.
      putStr (show cell)
      mainloop cursor size nextState
    -- Perform a step (ideally this should have been pause/unpause).
    step = let nextState = lifeStep state size
           in do
      drawScreen nextState
      mainloop cursor size nextState

drawScreen :: [[Cell]] -> IO ()
drawScreen state = do
  hideCursor -- Prevent visible cursor travelling on screen.
  setCursorPosition 0 0
  putStr $ lifeToString state
  showCursor

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

withinSquare :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
withinSquare (lowy, lowx) (hiy, hix) (y, x) = within (lowy, hiy) y &&
                                              within (lowx, hix) x

------------------------------------------------------------
-- Example states
------------------------------------------------------------

-- Successive applications of lifeStep on each glider state.
glider = [
  [ Dead,  Dead,  Dead,  Dead,  Dead],
  [ Dead,  Dead, Alive,  Dead,  Dead],
  [Alive,  Dead, Alive,  Dead,  Dead],
  [ Dead, Alive, Alive,  Dead,  Dead],
  [ Dead,  Dead,  Dead,  Dead,  Dead]]

glider' = [
  [ Dead,  Dead,  Dead,  Dead,  Dead],
  [ Dead, Alive,  Dead,  Dead,  Dead],
  [ Dead,  Dead, Alive, Alive,  Dead],
  [ Dead, Alive, Alive,  Dead,  Dead],
  [ Dead,  Dead,  Dead,  Dead,  Dead]]

glider'' = [
  [ Dead,  Dead,  Dead,  Dead,  Dead],
  [ Dead,  Dead, Alive,  Dead,  Dead],
  [ Dead,  Dead,  Dead, Alive,  Dead],
  [ Dead, Alive, Alive, Alive,  Dead],
  [ Dead,  Dead,  Dead,  Dead,  Dead]]

glider''' = [
  [ Dead,  Dead,  Dead,  Dead,  Dead],
  [ Dead,  Dead,  Dead,  Dead,  Dead],
  [ Dead, Alive,  Dead, Alive,  Dead],
  [ Dead,  Dead, Alive, Alive,  Dead],
  [ Dead,  Dead, Alive,  Dead,  Dead]]

-- Some cool patterns (maybe add support to load them?)
patternHeart = [
  [Alive,  Dead,  Dead, Alive],
  [Alive, Alive, Alive,  Dead]]

patternGlider = [
  [ Dead,  Dead, Alive],
  [Alive,  Dead, Alive],
  [ Dead, Alive, Alive]]

patternC2Ortho = [
  [ Dead, Alive,  Dead,  Dead, Alive],
  [Alive,  Dead,  Dead,  Dead,  Dead],
  [Alive,  Dead,  Dead,  Dead, Alive],
  [Alive, Alive, Alive, Alive,  Dead]]

patternPrePulsar = [
  [Alive, Alive, Alive,  Dead,  Dead,  Dead, Alive, Alive, Alive],
  [Alive,  Dead, Alive,  Dead,  Dead,  Dead, Alive,  Dead, Alive],
  [Alive, Alive, Alive,  Dead,  Dead,  Dead, Alive, Alive, Alive]]

patternQueenBeeShuttle = [
  [ Dead,  Dead, Dead, Dead, Dead,  Dead,  Dead,  Dead,  Dead, Alive],
  [ Dead,  Dead, Dead, Dead, Dead,  Dead,  Dead, Alive,  Dead, Alive],
  [ Dead,  Dead, Dead, Dead, Dead,  Dead, Alive,  Dead, Alive,  Dead],
  [Alive, Alive, Dead, Dead, Dead, Alive,  Dead,  Dead, Alive,  Dead,  Dead,
    Dead,  Dead, Dead, Dead, Dead,  Dead,  Dead,  Dead,  Dead, Alive, Alive],
  [Alive, Alive, Dead, Dead, Dead,  Dead, Alive,  Dead, Alive,  Dead,  Dead,
    Dead,  Dead, Dead, Dead, Dead,  Dead,  Dead,  Dead,  Dead, Alive, Alive],
  [ Dead,  Dead, Dead, Dead, Dead,  Dead,  Dead, Alive,  Dead, Alive],
  [ Dead,  Dead, Dead, Dead, Dead,  Dead,  Dead,  Dead,  Dead, Alive]]

------------------------------------------------------------
-- Drawing
------------------------------------------------------------

lifeToString :: [[Cell]] -> String
lifeToString state =
  foldr (\line rest -> (draw line) ++ rest) "" state
  where
    draw :: [Cell] -> String
    draw []     = ""
    draw (x:xs) = show x ++ draw xs

------------------------------------------------------------
-- Rules
------------------------------------------------------------

lifeStep :: [[Cell]] -> (Int, Int) -> [[Cell]]
lifeStep state (height, width) =
  map (\i -> map (\j -> lives (i, j))
                 [0 .. width - 1])
      [0 .. height - 1]
  where
    lives :: (Int, Int) -> Cell
    lives cell
      | (&&) (dead (atyx cell state))
             (neighbors == 3)          = Alive
      | (&&) (alive (atyx cell state))
             (within (3, 4) neighbors) = Alive
      | otherwise                      = Dead
      where
        neighbors = mooreNeighbors cell state

alive :: Cell -> Bool
alive Alive = True
alive _     = False

dead :: Cell -> Bool
dead Dead = True
dead _    = False

-- Count the number of cells in a 3x3 centered in (y, x).
mooreNeighbors :: (Int, Int) -> [[Cell]] -> Int
mooreNeighbors (y, x) state = count neighbors
  where
    -- Get only the 3x3 square around (y, x).
    neighbors = sliceBox (y - 1, x - 1) (y + 1, x + 1) state
    -- Count the number of alive cells.
    count :: [[Cell]] -> Int
    count cells = foldr (+) 0 (map (\line -> length $ filter alive line)
                                   cells)

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
