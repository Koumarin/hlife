module Main where

import System.Console.ANSI
import System.IO
import Data.Maybe

import Rogue
import Terminal
import Util

main :: IO ()
main = do
  ansi <- hSupportsANSI stdout
  if (not ansi)
    then putStr "I'm sorry, this game only runs on ANSI terminals.\n"
    else withTerminal [Name "NetHeck",
                       NoEcho]
           (do hSetBuffering stdin  NoBuffering
               showCursor
               size <- screenSize
               let (height, width) = size
                   middle          = (div height 2, div width 2)
                   dungeon         = atyxPut (12, 39) Wall (blankDungeon size)
                 in do
                 scrollPageUp (height - 1)
                 drawScreen dungeon
                 mainloop middle size dungeon)

mainloop :: (Int, Int) -> (Int, Int) -> [[Tile]] -> IO ()
mainloop cursor size dungeon = let (y, x) = cursor
                               in do
  setCursorPosition y x
  drawScreen dungeon
  atPrint cursor "@"
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
    -- Quit.
    'q' -> return ()
    _   -> mainloop cursor size dungeon
  where
    -- Move cursor by an increment.
    move = \delta -> let maybeCursor = pointAdd cursor delta
                         newCursor   =
                           let origin = (0, 0)
                               corner = pointAdd size (-1, -1)
                           -- Check against screen boundaries and collision.
                           in if and [withinSquare origin
                                                   corner
                                                   maybeCursor,
                                      walkable (atyx maybeCursor dungeon)]
                              then maybeCursor
                              else cursor
                     in mainloop newCursor size dungeon

drawScreen :: [[Tile]] -> IO ()
drawScreen dungeon = do
  hideCursor -- Prevent visible cursor travelling on screen.
  saveCursor
  setCursorPosition 0 0
  putStr $ dungeonToString dungeon
  restoreCursor
  showCursor

atPrint :: (Int, Int) -> String -> IO ()
atPrint (y, x) msg = do
  saveCursor
  setCursorPosition y x
  putStr msg
  restoreCursor

-- Either get terminal size when our terminal understands the keycode
-- or we default to 80x24.
screenSize :: IO (Int, Int)
screenSize = do
  size <- getTerminalSize
  if isJust size
    then return (fromJust size)
    else return (24, 80)

blankDungeon :: (Int, Int) -> [[Tile]]
blankDungeon (height, width) = replicate height $ replicate width Floor
