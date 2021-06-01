module Rogue
  (walkable,
   dungeonToString,
   Tile (..))
where

import System.Random

import Util

data Tile
  = Floor
  | Wall
  deriving (Eq, Show)

walkable :: Tile -> Bool
walkable Floor = True
walkable Wall  = False

makeCave :: (Int, Int) -> StdGen -> ([[Tile]], StdGen)
makeCave size gen = (cave', gen')
  where (cave, gen') = randomTiles size gen
        cave' = erode 5 size cave

erode :: Int -> (Int, Int) -> [[Tile]] -> [[Tile]]
erode times (height, width) cave = applyTimes times erode' cave
  where erode' = \cave -> map (\i -> map (\j -> newTile(i, j))
                                         [0 .. width - 1])
                              [0 .. height - 1]
        newTile = \point -> if mooreNeighbors point Wall cave >= 5
                            then Wall
                            else Floor

-- Count the number of tiles in a 3x3 square centered in (y, x).
mooreNeighbors :: (Int, Int) -> Tile -> [[Tile]] -> Int
mooreNeighbors (y, x) tile cave = count neighbors
  -- Get only the 3x3 square around (y, x).
  where neighbors = sliceBox (y - 1, x - 1) (y + 1, x + 1) cave
  -- Count the number of times the tile appears in the neighborhood.
        count = \tiles ->
                  foldr (+) 0 (map (\line -> length $ filter (== tile) line)
                                   tiles)

-- Creates a map out of random tiles.
randomTiles :: (Int, Int) -> StdGen -> ([[Tile]], StdGen)
randomTiles size gen = randomTiles' [] size gen
  where
    randomTiles' :: [[Tile]] -> (Int, Int) -> StdGen -> ([[Tile]], StdGen)
    randomTiles' acc (     0,     _) gen = (acc, gen)
    randomTiles' acc (height, width) gen =
      let (row, gen') = makeRow [] width gen
      in randomTiles' (row : acc) (height - 1, width) gen'
    makeRow :: [Tile] -> Int -> StdGen -> ([Tile], StdGen)
    makeRow acc   0 gen = (acc, gen)
    makeRow acc len gen = let (t, gen') = randomTile gen
                          in makeRow (t : acc) (len - 1) gen'

-- This should be an instance of Random but i cannot figure out how to do it.
randomTile :: StdGen -> (Tile, StdGen)
randomTile gen =
  let (idx, gen') = randomR (0, 1) gen :: (Int, StdGen)
  in (at idx [Floor, Wall], gen')

dungeonToString :: [[Tile]] -> String
dungeonToString dungeon =
  foldr (\line rest -> (draw line) ++ rest) "" dungeon
  where
    draw []     = ""
    draw (x:xs) = drawTile x ++ draw xs

drawTile :: Tile -> String
drawTile Floor = "."
drawTile Wall  = "#"
