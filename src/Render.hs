-- src/Render.hs
module Render where

import Graphics.Gloss
import Types

cameraOffset :: GameState -> (Float, Float)
cameraOffset gs =
  let (px, py) = pPos (gsPlayer gs)
  in (-px, -py)

render :: GameState -> Picture
render gs =
  let (camX, camY) = cameraOffset gs
  in translate camX camY $
       pictures
         [ renderMap gs
         , renderEnemies gs
         , renderItems gs
         , renderPlayer gs
         ]

renderPlayer :: GameState -> Picture
renderPlayer gs =
  let (x,y) = pPos (gsPlayer gs)
  in translate x y (aPlayer (gsAssets gs))

renderEnemies :: GameState -> Picture
renderEnemies gs =
  pictures
    [ translate x y (aEnemy (gsAssets gs))
    | Enemy (x,y) _ _ <- gsEnemies gs
    ]

renderItems :: GameState -> Picture
renderItems gs =
  pictures
    [ translate x y (aItemFood (gsAssets gs))
    | ((x,y), _) <- gsItems gs
    ]

renderMap :: GameState -> Picture
renderMap gs =
  let (px, py) = pPos (gsPlayer gs)
      playerTileX = floor (px / tileSize)
      playerTileY = floor (-(py) / tileSize)
      cullRadius = 15
      manhattanDist x y = abs (x - playerTileX) + abs (y - playerTileY)
  in pictures
       [ translate (fromIntegral x * tileSize)
                   (-(fromIntegral y * tileSize))
                   (tilePic gs tile)
       | (y, row) <- zip [0..] (gsMap gs)
       , (x, tile) <- zip [0..] row
       , manhattanDist x y <= cullRadius
       ]

-- Función auxiliar segura para acceder a lista
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 || n >= length xs = Nothing
  | otherwise               = Just (xs !! n)

tilePic :: GameState -> Tile -> Picture
tilePic gs (FloorTile variant) =
  case safeIndex (aTileFloors (gsAssets gs)) variant of
    Just pic -> pic
    Nothing  -> color green (rectangleSolid tileSize tileSize)  -- Fallback
tilePic gs WallTile  = aTileWall (gsAssets gs)
tilePic _ StairUp    = color yellow  (circleSolid 10)
tilePic _ StairDown  = color magenta (circleSolid 10)