-- src/WorldGen.hs
module WorldGen where

import System.Random (StdGen, randomR, split)
import Types

w, h :: Int
w = 200
h = 200

-- Genera un mapa aleatorio usando StdGen como semilla
generateMap :: StdGen -> TileMap
generateMap gen = tiles
  where
    tiles = [ generateRow y rowGen | (y, rowGen) <- zip [0..h-1] rowGens ]
    rowGens = splitN (h-1) gen

-- Genera una fila de tiles
generateRow :: Int -> StdGen -> [Tile]
generateRow y gen = [ generateTile x y tileGen | (x, tileGen) <- zip [0..w-1] tileGens ]
  where
    tileGens = splitN (w-1) gen

-- Genera un tile aleatorio
generateTile :: Int -> Int -> StdGen -> Tile
generateTile x y gen =
  let (r, _) = randomR (1, 10) gen :: (Int, StdGen)
  in if r <= 2 then WallTile else FloorTile

-- Divide un generador en n+1 generadores
splitN :: Int -> StdGen -> [StdGen]
splitN 0 g = [g]
splitN n g = let (g1, g2) = split g in g1 : splitN (n-1) g2