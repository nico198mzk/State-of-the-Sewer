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
      baseScene =
        translate camX camY $
          pictures
            [ renderMap gs
            , renderEnemies gs
            , renderItems gs
            , renderPlayer gs
            ]

      makeThickText msg col =
        let base = text msg
            thick =
              pictures
                [ translate dx dy base
                | dx <- [-3, -2, -1, 0, 1, 2, 3]
                , dy <- [-3, -2, -1, 0, 1, 2, 3]
                ]
        in color col thick

      overlay =
        case gsPhase gs of
          GameOver ->
            translate (-260) 0 $
              scale 0.3 0.3 $
                makeThickText "GAME OVER" red

          Victory  ->
            translate (-240) 0 $
              scale 0.3 0.3 $
                makeThickText "VICTORIA" white
          
          BossFight ->
            if gsBossMsgTime gs > 0
              then
                translate (-180) 200 $
                  scale 0.2 0.2 $
                    makeThickText "BOSS FIGHT" red
            else Blank

          Playing  -> Blank
      
      hud = renderHUD gs
  in pictures [baseScene, overlay, hud]

renderPlayer :: GameState -> Picture
renderPlayer gs =
  let p = gsPlayer gs
      (x,y) = pPos p
      facing = pFacing p
      timer = pAttackTimer p
      assets = gsAssets gs
      
      -- Imagen del jugador
      playerImg = aPlayer assets
      
      -- Indicador de dirección (triángulo) - Aparece frente al jugador
      directionIndicator = color (makeColorI 255 255 0 150) $ 
        case facing of
          DirUp    -> translate 0 40 $ polygon [(0, 8), (-5, 0), (5, 0)]
          DirDown  -> translate 0 (-40) $ polygon [(0, -8), (-5, 0), (5, 0)]
          DirLeft  -> translate (-30) 0 $ polygon [(-8, 0), (0, -5), (0, 5)]
          DirRight -> translate 30 0 $ polygon [(8, 0), (0, -5), (0, 5)]
      
      -- Espada (solo si está atacando)
      swordPic = if timer > 0
                   then let sword = aSword assets
                            -- Rotar según dirección (sprite apunta arriba)
                            rotation = case facing of
                              DirUp    -> 0
                              DirDown  -> 180
                              DirLeft  -> -90
                              DirRight -> 90
                            -- Posición relativa de la espada
                            (sx, sy) = case facing of
                              DirUp    -> (0, 20)
                              DirDown  -> (0, -20)
                              DirLeft  -> (-20, 0)
                              DirRight -> (20, 0)
                        in translate sx sy $ rotate rotation sword
                   else Blank
      
  in translate x y $ pictures [playerImg, directionIndicator, swordPic]

renderEnemies :: GameState -> Picture
renderEnemies gs =
  pictures
    [ if gsPhase gs == BossFight
        then
          translate x y $
            scale 1.5 1.5 $           -- boss más grande
              aBoss (gsAssets gs)     -- sprite del boss
        else
          translate x y $
            aEnemySlime (gsAssets gs) -- slimes normales
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

--Funcion para la barra de vida del jugador
renderHUD :: GameState -> Picture
renderHUD gs = 
  let p      = gsPlayer gs
      hp     = fromIntegral (pHP p)    ::Float
      maxHP  = fromIntegral (pMaxHP p) :: Float
      ratio  = max 0 (min 1 (hp / maxHP))

      --ancho y alto de la barra
      barW = 200
      barH = 20
      margin =10

      x = -screenWidth / 2 + barW / 2 + margin
      y =  screenHeight / 2 - barH / 2 - margin

      hpText = show (pHP p) ++ " / " ++ show (pMaxHP p)
      atkText = "ATK: " ++ show (pAtk p)
      spdText = "SPD: " ++ show (round (pSpeed p):: Int)

  in translate x y $
      pictures
        [color (greyN 0.3) (rectangleSolid barW barH) --fondo de la barra
        , translate 0 0 $ --bara de vida roja
            color red (rectangleSolid (barW * ratio) (barH - 4))
        , translate (-barW/2 +5)(-6) $ -- Texto barra de vida
            scale 0.1 0.1 $
            color white $
              text hpText
        , translate (-barW/2)(-barH -10) $
            scale 0.1 0.1 $
            color white (text atkText)
        , translate (-barW/2)(-barH -30) $
            scale 0.1 0.1 $
              color white (text spdText)
        ]

-- Función auxiliar segura para acceder a lista
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 || n >= length xs = Nothing
  | otherwise               = Just (xs !! n)

tilePic :: GameState -> Tile -> Picture
tilePic _ Void = Blank  -- No renderizar tiles vacíos
tilePic gs (FloorTile variant) =
  case safeIndex (aTileFloors (gsAssets gs)) variant of
    Just pic -> pic
    Nothing  -> color green (rectangleSolid tileSize tileSize)  -- Fallback
tilePic gs (WallTile variant) =
  case safeIndex (aTileWalls (gsAssets gs)) variant of
    Just pic -> pic
    Nothing  -> color red (rectangleSolid tileSize tileSize)  -- Fallback
tilePic _ StairUp    = color yellow  (circleSolid 10)
tilePic _ StairDown  = color magenta (circleSolid 10)
