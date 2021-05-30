module Rogue
  (walkable,
   dungeonToString,
   Tile (..))
where

data Tile
  = Floor
  | Wall
  deriving (Eq)

walkable :: Tile -> Bool
walkable Floor = True
walkable Wall  = False

dungeonToString :: [[Tile]] -> String
dungeonToString dungeon =
  foldr (\line rest -> (draw line) ++ rest) "" dungeon
  where
    draw []     = ""
    draw (x:xs) = drawTile x ++ draw xs

drawTile :: Tile -> String
drawTile Floor = "."
drawTile Wall  = "#"
