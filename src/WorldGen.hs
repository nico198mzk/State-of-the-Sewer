-- src/WorldGen.hs
module WorldGen where

import System.Random (StdGen, randomR, split, mkStdGen)
import Types hiding (Rect)

-- Dimensiones del mapa
mapWidth, mapHeight :: Int
mapWidth = 100
mapHeight = 100

-- Representa una habitación rectangular
data Rect = Rect
  { rectX :: Int
  , rectY :: Int
  , rectW :: Int
  , rectH :: Int
  } deriving (Show, Eq)

-- Centro de una habitación
center :: Rect -> (Int, Int)
center r = (rectX r + rectW r `div` 2, rectY r + rectH r `div` 2)

-- Determina si dos salas se superponen (incluye margen para corredores).
overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 =
  let x1 = rectX r1 - 2
      y1 = rectY r1 - 2
      x2 = rectX r1 + rectW r1 + 2
      y2 = rectY r1 + rectH r1 + 2
      x3 = rectX r2 - 2
      y3 = rectY r2 - 2
      x4 = rectX r2 + rectW r2 + 2
      y4 = rectY r2 + rectH r2 + 2
  in not (x2 < x3 || x4 < x1 || y2 < y3 || y4 < y1)

-- Calcula la distancia entre los centros de dos salas.
roomDistance :: Rect -> Rect -> Float
roomDistance r1 r2 =
  let (x1, y1) = center r1
      (x2, y2) = center r2
      dx = fromIntegral (x2 - x1)
      dy = fromIntegral (y2 - y1)
  in sqrt (dx * dx + dy * dy)

-- Genera un mapa completo con salas, pasillos, enemigos y sala del boss.
-- Devuelve: (mapa, posición inicial, enemigos, posición sala del boss)
generateMap :: StdGen -> (TileMap, Vec2, [Enemy], Vec2)
generateMap gen =
  let -- Dividir generador para diferentes propósitos
      (roomGen, enemyGen) = split gen
      
      emptyMap = createVoidMap
      
      (rooms, gen1) = generateRooms roomGen [] 15
      
      mapWithRooms = foldl carveRoom emptyMap rooms
      
      (mapWithCorridors, gen2) = connectRooms mapWithRooms rooms gen1
      
      finalMap = autoTileWalls mapWithCorridors gen2
      
      enemies = spawnEnemiesInRooms (tail rooms) enemyGen
      
      -- Posicion inicial (centro de la primera sala)
      startPos = roomToWorldPos (head rooms)
      
      -- Posicion de la sala del boss (ultima sala generada)
      bossRoomPos = if null rooms 
                      then startPos  
                      else roomToWorldPos (last rooms)
      
  in (finalMap, startPos, enemies, bossRoomPos)

-- Convierte coordenadas de sala (tiles) a coordenadas del mundo (pixeles).
roomToWorldPos :: Rect -> Vec2
roomToWorldPos room =
  let (cx, cy) = center room
  in (fromIntegral cx * tileSize, -(fromIntegral cy * tileSize))

-- Escoge una variante de tile de piso según probabilidad.
getWeightedFloorVariant :: StdGen -> (Int, StdGen)
getWeightedFloorVariant gen =
  let (roll, gen') = randomR (1, 100) gen :: (Int, StdGen)
      variant | roll <= 70  = 0  -- 70% probabilidad
              | roll <= 90  = 1  -- 20% probabilidad
              | roll <= 95  = 2  -- 5% probabilidad
              | otherwise   = 3  -- 5% probabilidad
  in (variant, gen')

-- Genera enemigos dentro de varias salas.
spawnEnemiesInRooms :: [Rect] -> StdGen -> [Enemy]
spawnEnemiesInRooms [] _ = []
spawnEnemiesInRooms (room:rest) gen =
  let (gen1, gen2) = split gen
      enemiesInRoom = spawnEnemiesInRoom room gen1
  in enemiesInRoom ++ spawnEnemiesInRooms rest gen2

-- Genera enemigos dentro de una sala según su tamaño.
spawnEnemiesInRoom :: Rect -> StdGen -> [Enemy]
spawnEnemiesInRoom room gen =
  let area = rectW room * rectH room
      count = max 1 (area `div` 25)  
      (enemies, _) = generateNEnemies count room gen
  in enemies

-- Genera N enemigos dentro de una sala.
generateNEnemies :: Int -> Rect -> StdGen -> ([Enemy], StdGen)
generateNEnemies 0 _ gen = ([], gen)
generateNEnemies n room gen =
  let (enemy, gen1) = generateOneEnemy room gen
      (rest, gen2) = generateNEnemies (n - 1) room gen1
  in (enemy : rest, gen2)

-- Crea un enemigo con posición y atributos aleatorios en una sala.
generateOneEnemy :: Rect -> StdGen -> (Enemy, StdGen)
generateOneEnemy room gen =
  let -- Posición aleatoria dentro de la sala (con margen de 1 tile)
      x1 = rectX room + 1
      y1 = rectY room + 1
      x2 = rectX room + rectW room - 2
      y2 = rectY room + rectH room - 2
      
      (tileX, gen1) = randomR (x1, max x1 x2) gen
      (tileY, gen2) = randomR (y1, max y1 y2) gen1
      
      -- Convertir a coordenadas del mundo
      worldX = fromIntegral tileX * tileSize
      worldY = -(fromIntegral tileY * tileSize)
      
      -- Atributos aleatorios para variedad
      (hp, gen3) = randomR (30, 60) gen2  -- HP entre 30 y 60
      (atk, gen4) = randomR (8, 15) gen3  -- Ataque entre 8 y 15
      
  in (Enemy (worldX, worldY) hp atk, gen4)


-- Crea un mapa inicial relleno completamente de Void.
createVoidMap :: TileMap
createVoidMap = replicate mapHeight (replicate mapWidth Void)

-- Genera una lista de salas sin superposición y con distancia controlada.
generateRooms :: StdGen -> [Rect] -> Int -> ([Rect], StdGen)
generateRooms gen rooms 0 = (rooms, gen)
generateRooms gen rooms n =
  let (room, gen') = generateRoom gen rooms
      validRoom = null rooms || 
                  (not (any (overlaps room) rooms) &&
                   roomDistance room (last rooms) <= 20)
  in if validRoom
       then generateRooms gen' (rooms ++ [room]) (n - 1)
       else if n > 0  -- Intenta de nuevo pero no infinitamente
              then generateRooms gen' rooms (n - 1)
              else (rooms, gen')

-- Genera una nueva sala cerca de la última, o centrada si es la primera.
generateRoom :: StdGen -> [Rect] -> (Rect, StdGen)
generateRoom gen rooms =
  let (w, gen1) = randomR (6, 12) gen
      (h, gen2) = randomR (6, 12) gen1
      (x, y, gen3) = if null rooms
                       then -- Primera sala: en el centro
                            let x0 = mapWidth `div` 2 - w `div` 2
                                y0 = mapHeight `div` 2 - h `div` 2
                            in (x0, y0, gen2)
                       else -- Salas siguientes: cerca de la última
                            let (lastX, lastY) = center (last rooms)
                                (dx, g1) = randomR (-15, 15) gen2
                                (dy, g2) = randomR (-15, 15) g1
                                x0 = max 1 (min (mapWidth - w - 1) (lastX + dx))
                                y0 = max 1 (min (mapHeight - h - 1) (lastY + dy))
                            in (x0, y0, g2)
  in (Rect x y w h, gen3)

-- Rellena una sala en el mapa convirtiendo Void en tiles de piso.
carveRoom :: TileMap -> Rect -> TileMap
carveRoom tmap room =
  let x1 = rectX room
      y1 = rectY room
      x2 = x1 + rectW room
      y2 = y1 + rectH room
      gen = mkStdGen (x1 + y1 * 1000)
      (finalMap, _) = foldl processRow ([], gen) (zip [0..] tmap)
      processRow (rows, g) (y, row) =
        let (newRow, g') = foldl (processCell y) ([], g) (zip [0..] row)
        in (rows ++ [newRow], g')
      processCell y (cells, gCell) (x, tile) =
        if x >= x1 && x < x2 && y >= y1 && y < y2
          then let (variant, gCell') = getWeightedFloorVariant gCell
               in (cells ++ [FloorTile variant], gCell')
          else (cells ++ [tile], gCell)
  in finalMap

-- Conecta todas las salas mediante pasillos anchos.
connectRooms :: TileMap -> [Rect] -> StdGen -> (TileMap, StdGen)
connectRooms tmap [] gen = (tmap, gen)
connectRooms tmap [_] gen = (tmap, gen)
connectRooms tmap (r1:r2:rest) gen =
  let (tmap', gen') = createWideCorridor tmap r1 r2 gen
  in connectRooms tmap' (r2:rest) gen'

-- Conecta dos salas con un pasillo de 2 tiles de grosor.
createWideCorridor :: TileMap -> Rect -> Rect -> StdGen -> (TileMap, StdGen)
createWideCorridor tmap r1 r2 gen =
  let (x1, y1) = center r1
      (x2, y2) = center r2
      (horizontal, gen') = randomR (True, False) gen
      tmap' = if horizontal
                then carveWideHorizontalCorridor tmap x1 x2 y1
                else carveWideVerticalCorridor tmap y1 y2 x1
      tmap'' = if horizontal
                 then carveWideVerticalCorridor tmap' y1 y2 x2
                 else carveWideHorizontalCorridor tmap' x1 x2 y2
  in (tmap'', gen')

-- Crea un pasillo horizontal de 2 tiles.
carveWideHorizontalCorridor :: TileMap -> Int -> Int -> Int -> TileMap
carveWideHorizontalCorridor tmap x1 x2 y =
  let xStart = min x1 x2
      xEnd = max x1 x2
      gen = mkStdGen (xStart + y * 1000)
      (finalMap, _) = foldl processRow ([], gen) (zip [0..] tmap)
      processRow (rows, g) (yIdx, row) =
        let (newRow, g') = foldl (processCell yIdx) ([], g) (zip [0..] row)
        in (rows ++ [newRow], g')
      processCell yIdx (cells, gCell) (xIdx, tile) =
        if xIdx >= xStart && xIdx <= xEnd && (yIdx == y || yIdx == y + 1)
          then let (variant, gCell') = getWeightedFloorVariant gCell
               in (cells ++ [FloorTile variant], gCell')
          else (cells ++ [tile], gCell)
  in finalMap

-- Crea un pasillo vertical de 2 tiles.
carveWideVerticalCorridor :: TileMap -> Int -> Int -> Int -> TileMap
carveWideVerticalCorridor tmap y1 y2 x =
  let yStart = min y1 y2
      yEnd = max y1 y2
      gen = mkStdGen (x + yStart * 1000)
      (finalMap, _) = foldl processRow ([], gen) (zip [0..] tmap)
      processRow (rows, g) (yIdx, row) =
        let (newRow, g') = foldl (processCell yIdx) ([], g) (zip [0..] row)
        in (rows ++ [newRow], g')
      processCell yIdx (cells, gCell) (xIdx, tile) =
        if yIdx >= yStart && yIdx <= yEnd && (xIdx == x || xIdx == x + 1)
          then let (variant, gCell') = getWeightedFloorVariant gCell
               in (cells ++ [FloorTile variant], gCell')
          else (cells ++ [tile], gCell)
  in finalMap

-- Convierte Void en muros cuando están junto a tiles de piso.
autoTileWalls :: TileMap -> StdGen -> TileMap
autoTileWalls tmap gen =
  let (finalMap, _) = foldl processRow ([], gen) (zip [0..] tmap)
  in finalMap
  where
    processRow (rows, g) (y, row) =
      let (newRow, g') = foldl (processCell y) ([], g) (zip [0..] row)
      in (rows ++ [newRow], g')
    
    processCell y (cells, g) (x, tile) =
      case tile of
        Void -> if hasFloorNeighbor x y tmap
                  then let (variant, g') = randomR (0, 3) g
                       in (cells ++ [WallTile variant], g')
                  else (cells ++ [Void], g)
        _ -> (cells ++ [tile], g)

-- Comprueba si una celda de Void tiene suelo al lado (para auto-tiling).
hasFloorNeighbor :: Int -> Int -> TileMap -> Bool
hasFloorNeighbor x y tmap =
  let neighbors = [ (x-1, y), (x+1, y), (x, y-1), (x, y+1) ]
      h = length tmap
      w = if null tmap then 0 else length (head tmap)
  in any (isFloorAt w h) neighbors
  where
    isFloorAt w h (nx, ny)
      | nx < 0 || ny < 0 || nx >= w || ny >= h = False
      | otherwise = case (tmap !! ny !! nx) of
                      FloorTile _ -> True
                      _ -> False
