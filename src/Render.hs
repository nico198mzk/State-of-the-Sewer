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
  case gsPhase gs of

    StartScreen ->
      pictures
        [ color black $ rectangleSolid screenWidth screenHeight
        , scale 2 1.5 $ 
            aStartScreen (gsAssets gs)
        ]
    ControlsScreen ->
      pictures
        [ color black $ rectangleSolid screenWidth screenHeight
        , scale 2 1.5 $
            aControlsScreen (gsAssets gs)
        ]

    -- Resto de fases: dibujamos el mundo con cámara + overlays si quieres
    _ ->
      let (camX, camY) = cameraOffset gs

          baseScene =
            translate camX camY $
              pictures
                [ renderMap gs
                , renderEnemies gs
                , renderItems gs
                , renderPlayer gs
                ]

          -- Por ahora, overlay vacío (luego puedes re-agregar GAME OVER, BOSS FIGHT, etc.)
          overlay = Blank

      in pictures
           [ baseScene
           , overlay
           ]

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
      -- Dibujar tiles del mapa
      mapPics = 
        [ translate (fromIntegral x * tileSize)
                    (-(fromIntegral y * tileSize))
                    (tilePic gs tile)
        | (y, row) <- zip [0..] (gsMap gs)
        , (x, tile) <- zip [0..] row
        , manhattanDist x y <= cullRadius
        ]
      -- Dibujar escalera (solo visible cuando el jefe fue derrotado)
      (escX, escY) = posEscalera gs
      stairsPic = if jefeDerrotado gs
                    then translate escX escY (aStairs (gsAssets gs))
                    else Blank  -- Invisible mientras el jefe está vivo
  in pictures (mapPics ++ [stairsPic])

-- Obtener los tiles correctos según el nivel actual
obtenerTilesDelNivel :: GameState -> [Picture]
obtenerTilesDelNivel gs =
  let assets = gsAssets gs
  in case nivelActual gs of
       1 -> aTileFloors assets   -- Piso 1
       2 -> aTileFloors2 assets  -- Piso 2
       3 -> aTileFloors3 assets  -- Piso 3
       _ -> aTileFloors assets   -- Default: Piso 1

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
      
      -- Indicador de nivel actual
      nivel = nivelActual gs
      nivelText = "PISO " ++ show nivel ++ "/3"
      jefeDerr = if jefeDerrotado gs then " [ESCALERA ACTIVA]" else ""

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
        , translate (-barW/2)(-barH -50) $
            scale 0.1 0.1 $
              color yellow (text (nivelText ++ jefeDerr))
        ]

-- Función auxiliar segura para acceder a lista
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 || n >= length xs = Nothing
  | otherwise               = Just (xs !! n)

tilePic :: GameState -> Tile -> Picture
tilePic _ Void = Blank  -- No renderizar tiles vacíos
tilePic gs (FloorTile variant) =
  let tiles = obtenerTilesDelNivel gs  -- Usar tiles según nivel actual
  in case safeIndex tiles variant of
       Just pic -> pic
       Nothing  -> color green (rectangleSolid tileSize tileSize)  -- Fallback
tilePic gs (WallTile variant) =
  case safeIndex (aTileWalls (gsAssets gs)) variant of
    Just pic -> pic
    Nothing  -> color red (rectangleSolid tileSize tileSize)  -- Fallback
tilePic _ StairUp    = color yellow  (circleSolid 10)
tilePic _ StairDown  = color magenta (circleSolid 10)
