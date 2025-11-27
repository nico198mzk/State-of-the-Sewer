-- src/WorldGen.hs
module WorldGen where

import System.Random (StdGen, randomR, split, mkStdGen)
import Types

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

-- Verifica si dos habitaciones se superponen (con margen de 2 para pasillos)
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

-- Calcula la distancia entre dos salas
roomDistance :: Rect -> Rect -> Float
roomDistance r1 r2 =
  let (x1, y1) = center r1
      (x2, y2) = center r2
      dx = fromIntegral (x2 - x1)
      dy = fromIntegral (y2 - y1)
  in sqrt (dx * dx + dy * dy)

-- Genera un mapa con salas y pasillos, devuelve (mapa, posición inicial)
generateMap :: StdGen -> (TileMap, Vec2)
generateMap gen =
  let -- Paso 1: Inicializar mapa vacío (todo Void)
      emptyMap = createVoidMap
      -- Paso 2: Generar salas con restricción de distancia
      (rooms, gen1) = generateRooms gen [] 15
      -- Paso 3: Esculpir salas (colocar suelo)
      mapWithRooms = foldl carveRoom emptyMap rooms
      -- Paso 4: Esculpir pasillos anchos (2 tiles)
      (mapWithCorridors, gen2) = connectRooms mapWithRooms rooms gen1
      -- Paso 5: Generar muros automáticamente
      finalMap = autoTileWalls mapWithCorridors gen2
      -- Posición inicial (centro de la primera sala)
      startPos = roomToWorldPos (head rooms)
  in (finalMap, startPos)

-- Convierte posición de sala a coordenadas del mundo
roomToWorldPos :: Rect -> Vec2
roomToWorldPos room =
  let (cx, cy) = center room
  in (fromIntegral cx * tileSize, -(fromIntegral cy * tileSize))

-- Selecciona variante de suelo con ponderación (weighted randomness)
-- Variante 0: 70%, Variante 1: 20%, Variante 2: 5%, Variante 3: 5%
getWeightedFloorVariant :: StdGen -> (Int, StdGen)
getWeightedFloorVariant gen =
  let (roll, gen') = randomR (1, 100) gen :: (Int, StdGen)
      variant | roll <= 70  = 0  -- 70% probabilidad
              | roll <= 90  = 1  -- 20% probabilidad
              | roll <= 95  = 2  -- 5% probabilidad
              | otherwise   = 3  -- 5% probabilidad
  in (variant, gen')

-- Crea un mapa lleno de Void
createVoidMap :: TileMap
createVoidMap = replicate mapHeight (replicate mapWidth Void)

-- Genera una lista de habitaciones sin superposición y con distancia controlada
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

-- Genera una habitación aleatoria cerca de la última sala
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

-- Talla una habitación en el mapa (la rellena con suelo)
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

-- Conecta todas las habitaciones con pasillos anchos
connectRooms :: TileMap -> [Rect] -> StdGen -> (TileMap, StdGen)
connectRooms tmap [] gen = (tmap, gen)
connectRooms tmap [_] gen = (tmap, gen)
connectRooms tmap (r1:r2:rest) gen =
  let (tmap', gen') = createWideCorridor tmap r1 r2 gen
  in connectRooms tmap' (r2:rest) gen'

-- Crea un pasillo ancho (2 tiles) entre dos habitaciones
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

-- Talla un pasillo horizontal ancho (2 tiles de grosor)
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

-- Talla un pasillo vertical ancho (2 tiles de grosor)
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

-- Auto-tiling: convierte Void en muros si están adyacentes a suelo
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

-- Verifica si una celda tiene al menos un vecino de suelo
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
