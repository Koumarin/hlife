module Rogue
  (dungeonToString,
   Tile (..))
where

data Tile
  = Floor
  | Wall
  deriving (Eq)

dungeonToString :: [[Tile]] -> String
dungeonToString dungeon =
  foldr (\line rest -> (draw line) ++ rest) "" dungeon
  where
    draw []     = ""
    draw (x:xs) = drawTile x ++ draw xs

drawTile :: Tile -> String
drawTile Floor = "."
drawTile Wall  = "#"
