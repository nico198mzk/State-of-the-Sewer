-- src/WorldGen.hs
module WorldGen where

import System.Random (StdGen, randomR, mkStdGen)
import Types

w, h :: Int
w = 200
h = 200

generateMap :: IO TileMap
generateMap = do
  let tiles =
        [ [ tileAt (x,y)
          | x <- [0 .. w-1]
          ]
        | y <- [0 .. h-1]
        ]
  return tiles

tileAt :: (Int, Int) -> Tile
tileAt (x,y) =
  let seed      = x * 928371 + y * 123455
      (r,_g)    = randomR (1,10) (mkStdGen seed) :: (Int,StdGen)
  in if r <= 2 then WallTile else FloorTile