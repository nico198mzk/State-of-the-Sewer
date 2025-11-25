-- src/Render.hs (reemplazar renderMap y render completamente)
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
      -- Convertir posición del jugador a coordenadas de tile
      playerTileX = floor (px / tileSize)
      playerTileY = floor (-(py) / tileSize)
      -- Radio de culling en tiles (15 tiles = ~480 pixeles)
      cullRadius = 15
      -- Función para calcular distancia Manhattan
      manhattanDist x y = abs (x - playerTileX) + abs (y - playerTileY)
  in pictures
       [ translate (fromIntegral x * tileSize)
                   (-(fromIntegral y * tileSize))
                   (tilePic gs tile)
       | (y, row) <- zip [0..] (gsMap gs)
       , (x, tile) <- zip [0..] row
       , manhattanDist x y <= cullRadius  -- Culling: solo dibujar tiles cercanos
       ]

tilePic :: GameState -> Tile -> Picture
tilePic gs FloorTile = aTileFloor (gsAssets gs)
tilePic gs WallTile  = aTileWall  (gsAssets gs)
tilePic _ StairUp    = color yellow  (circleSolid 10)
tilePic _ StairDown  = color magenta (circleSolid 10)