-- src/Render.hs
module Render where

import Graphics.Gloss
import Types

-- Calcula el desplazamiento de cámara para centrarla en el jugador.
cameraOffset :: GameState -> (Float, Float)
cameraOffset gs =
  let (px, py) = pPos (gsPlayer gs)
  in (-px, -py)

-- Dibuja toda la escena según la fase actual del juego.
render :: GameState -> Picture

-- Dibuja texto con efecto de sombra/negrita multiplicando varias capas.
boldText :: Color -> Float -> Float -> String -> Picture
boldText col sx sy txt =
  pictures
    [ translate dx dy $ scale sx sy $ color col $ text txt
    | dx <- [-2, 0, 2]
    , dy <- [-2, 0, 2]
    ]
render gs =
  case gsPhase gs of

    -- pantalla de inicio
    StartScreen ->
      pictures
        [ color black $ rectangleSolid screenWidth screenHeight
        , scale 2 1.5 $
            aStartScreen (gsAssets gs)
        ]
    
    -- pantalla de lore
    LoreScreen ->
      pictures
        [ color black $ rectangleSolid screenWidth screenHeight
        , scale 2 1.5 $
            aLoreScreen (gsAssets gs)
        ]

    -- pantalla de controles
    ControlsScreen ->
      pictures
        [ color black $ rectangleSolid screenWidth screenHeight
        , scale 2 1.5 $
            aControlsScreen (gsAssets gs)
        ]

    -- pantalla final (despues de terminar los 3 pisos)
    Victory ->
      pictures
        [ -- Fondo negro semitransparente
          color (makeColorI 0 0 0 200) $ rectangleSolid screenWidth screenHeight
        , -- Imagen de fondo (opcional)
          scale 1 1 $ aFinalScreen (gsAssets gs)
        , -- Texto de victoria
          translate (-280) 50 $
            boldText green 0.35 0.35 "ESCAPED!"
        , translate (-320) 0 $
            boldText yellow 0.25 0.25 "FLOORS CLEARED: 3/3"
        , translate (-320) (-50) $
            boldText white 0.2 0.2 "PRESS ESC TO QUIT"
        ]

    GameOver ->
      pictures
        [ color black $ rectangleSolid screenWidth screenHeight
        , translate (-200) 0 $
            boldText red 0.5 0.5 "GAME OVER"
        ]

    -- Resto de fases
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
          hudPic = renderHUD gs

          -- Overlay para mensaje de boss
          bossOverlay =
            if gsPhase gs == BossFight && gsBossMsgTime gs > 0
              then
                translate (-220) 200 $
                  boldText (makeColorI 200 0 0 255) 0.35 0.35 "BOSS FIGHT"
              else Blank

          overlay = pictures
            [ bossOverlay
            ]

      in pictures
           [ baseScene
           , hudPic
           , overlay
           ]

-- Dibuja al jugador, su dirección y la espada si está atacando.
renderPlayer :: GameState -> Picture
renderPlayer gs =
  let p = gsPlayer gs
      (x,y) = pPos p
      facing = pFacing p
      timer = pAttackTimer p
      assets = gsAssets gs
      
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

-- Dibuja todos los enemigos (o al jefe si es fase BossFight).
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

-- Dibuja los ítems presentes en el mapa.
renderItems :: GameState -> Picture
renderItems gs =
  pictures
    [ translate x y $
        scale 0.35 0.35 $           
          itemSprite (gsAssets gs) item
    | ((x,y), item) <- gsItems gs
    ]

-- Selecciona el sprite correcto para un ítem según su tipo.
itemSprite :: Assets -> Item -> Picture
itemSprite a (Heal _      ) = aItemFood  a
itemSprite a (BoostAtk  _ ) = aItemAtk   a
itemSprite a (BoostSpeed _) = aItemSpeed a

-- Dibuja el mapa visible cerca del jugador y la escalera si está activa.
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

-- Elige la lista de sprites de piso según el nivel actual.
obtenerTilesDelNivel :: GameState -> [Picture]
obtenerTilesDelNivel gs =
  let assets = gsAssets gs
  in case nivelActual gs of
       1 -> aTileFloors assets   -- Piso 1: tiles base
       2 -> aTileFloors2 assets  -- Piso 2: tiles alternativos
       3 -> aTileFloors3 assets  -- Piso 3: tiles finales
       _ -> aTileFloors assets   -- Default

-- Dibuja la interfaz (vida, ataque, velocidad, nivel, escalera activa).
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

-- Acceso seguro a un índice de lista (evita crasheos por índices inválidos).
safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n
  | n < 0 || n >= length xs = Nothing
  | otherwise               = Just (xs !! n)

-- Selecciona el sprite apropiado para un tile (piso, pared, escalera).
tilePic :: GameState -> Tile -> Picture
tilePic _ Void = Blank  -- No renderizar tiles vacíos
tilePic gs (FloorTile variant) =
  let tiles = obtenerTilesDelNivel gs  -- Usar tiles según nivel actual
  in case safeIndex tiles variant of
       Just pic -> pic
       Nothing  -> color green (rectangleSolid tileSize tileSize)  
tilePic gs (WallTile variant) =
  case safeIndex (aTileWalls (gsAssets gs)) variant of
    Just pic -> pic
    Nothing  -> color red (rectangleSolid tileSize tileSize)  
tilePic _ StairUp    = color yellow  (circleSolid 10)
tilePic _ StairDown  = color magenta (circleSolid 10)
