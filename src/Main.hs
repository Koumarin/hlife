module Main where

import System.Console.ANSI
import System.IO
import System.Random
import Data.Maybe

import Life
import Terminal
import Util

main :: IO ()
main = do
  ansi <- hSupportsANSI stdout
  if (not ansi)
    then putStr "I'm sorry, this game only runs on ANSI terminals.\n"
    else withTerminal [Name "Conway's Game of Life",
                       NoEcho]
           (do hSetBuffering stdin  NoBuffering
               showCursor
               gen  <- getStdGen
               size <- screenSize
               let (height, width) = size
                   middle          = (div height 2, div width 2)
                   blank           = blankLife size
                 in do
                 scrollPageUp (height - 1)
                 drawScreen blank
                 mainloop middle size blank gen)

mainloop :: (Int, Int) -> (Int, Int) -> [[Cell]] -> StdGen -> IO ()
mainloop cursor size state gen = let (y, x) = cursor
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
    'r' -> let (newState, gen') = randomLife size gen
           in do drawScreen newState
                 mainloop cursor size newState gen'
    -- Load a pattern.
    -- (displays it under the cursor and lets you place it with 's')
    'e' -> do setCursorPosition 999 0
              clearLine
              putStr "Sorry I didn't write the pattern loader yet :("
              mainloop cursor size state gen
    -- Clear the whole canvas.
    'c' -> clean
    -- Quit.
    'q' -> return ()
    _   -> mainloop cursor size state gen
  where
    -- Move cursor by an increment.
    move = \delta -> let maybeCursor = pointAdd cursor delta
                         newCursor   = let origin = (0, 0)
                                           corner = pointAdd size (-1, -1)
                                           -- Check against screen boundaries.
                                       in if withinSquare origin
                                                          corner
                                                          maybeCursor
                                          then maybeCursor
                                          else cursor
                     in mainloop newCursor size state gen
    -- Set cell under cursor position.
    setCell = \cell -> let nextState = atyxPut cursor cell state
                       in do
      -- Only redraw the character we're setting.
      putStr (drawCell cell)
      mainloop cursor size nextState gen
    -- Perform a step (ideally this should have been pause/unpause).
    step = let nextState = lifeStep state size
           in do
      drawScreen nextState
      mainloop cursor size nextState gen
    clean = let blank = blankLife size
            in do
      drawScreen blank
      mainloop cursor size blank gen

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

