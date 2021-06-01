module Life where

import System.Console.ANSI
import Util

data Cell = Alive | Dead
  deriving (Eq)

-- TODO: add a way to read these back.
instance Show Cell where
  show Alive = "O"
  show Dead  = "."

------------------------------------------------------------
-- Drawing
------------------------------------------------------------

lifeToString :: [[Cell]] -> String
lifeToString state =
  foldr (\line rest -> (draw line) ++ rest) "" state
  where
    draw :: [Cell] -> String
    draw []     = ""
    draw (x:xs) = drawCell x ++ draw xs

drawCell :: Cell -> String
drawCell Alive = setSGRCode [SetColor Background Vivid Blue]  ++ " "
drawCell Dead  = setSGRCode [SetColor Background Dull  Black] ++ " "

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
    lives point
      | and [dead cell,
             neighbors == 3]          = Alive
      | and [alive cell,
             within (3, 4) neighbors] = Alive
      | otherwise                     = Dead
      where
        cell      = atyx point state
        neighbors = mooreNeighbors point state

alive :: Cell -> Bool
alive = (== Alive)

dead :: Cell -> Bool
dead = not . alive

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
-- Example states
------------------------------------------------------------

blankLife :: (Int, Int) -> [[Cell]]
blankLife (height, width) = replicate height $ replicate width Dead

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
