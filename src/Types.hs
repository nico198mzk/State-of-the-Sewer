module Types where

import Graphics.Gloss
import System.Random (StdGen)

screenWidth, screenHeight :: Float
screenWidth  = 800
screenHeight = 600

-- Tamaño en píxeles de cada tile del mapa.
tileSize :: Float
tileSize = 32

-- Par (Float, Float) representando una posición 2D.
type Vec2 = (Float, Float)
-- Rectángulo con posición (x,y) y tamaño (w,h).
data Rect = Rect { rX :: Float, rY :: Float, rW :: Float, rH :: Float } deriving (Show, Eq)

-- Construye un rectángulo (hitbox) centrado en una posición dada.
hitboxFromCenter :: Vec2 -> Float -> Float -> Rect
hitboxFromCenter = rectFromCenter

-- Reposiciona al jugador en un nuevo punto y reinicia temporizadores.
initPlayerAtPos :: Vec2 -> Player -> Player
initPlayerAtPos newPos p = p
  { pPos         = newPos
  , pCooldown    = 0
  , pAttackTimer = 0
  }

-- Verifica si un hitbox rectangular puede caminarse (sin chocar con paredes).
isHitboxWalkable :: Rect -> TileMap -> Bool
isHitboxWalkable = isRectWalkable


-- direccion para combate direccional
data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

data Tile
  = Void           -- Espacio vacio (no jugable)
  | FloorTile Int  -- Variante de textura (0-3)
  | WallTile Int   -- Variante de muro (0-3)
  | StairUp
  | StairDown
  deriving (Eq,Show)

-- Matriz de tiles que constituye el mapa del juego.
type TileMap = [[Tile]]

-- Representa al jugador con posición, stats, velocidad y hitbox.
data Player = Player
  { pPos         :: Vec2
  , pHP          :: Int
  , pMaxHP       :: Int
  , pAtk         :: Int
  , pSpeed       :: Float
  , pCooldown    :: Float
  , pInventory   :: [Item]
  , pFacing      :: Direction
  , pAttackTimer :: Float
  , pHalfW       :: Float   -- mitad de ancho del hitbox
  , pHalfH       :: Float   -- mitad de alto del hitbox
  }
  deriving (Show)

-- Representa un enemigo con posición, vida y ataque.
data Enemy = Enemy
  { ePos :: Vec2
  , eHP  :: Int
  , eAtk :: Int
  }
  deriving (Show)

-- Representa un enemigo con posición, vida y ataque.
data Item
  = Heal Int
  | BoostAtk Int
  | BoostSpeed Float
  deriving (Show,Eq)

-- Estados globales del juego (menús, jugando, jefe, victoria).
data GamePhase
  = StartScreen
  | LoreScreen
  | ControlsScreen
  | Playing
  | BossFight
  | GameOver
  | Victory
  deriving (Eq, Show)

-- Todas las imágenes cargadas y usadas para dibujar el juego.
data Assets = Assets
  { aPlayer       :: Picture
  , aEnemy        :: Picture
  , aEnemySlime   :: Picture
  , aBoss         :: Picture
  , aTileFloors   :: [Picture]
  , aTileFloors2  :: [Picture]
  , aTileFloors3  :: [Picture]
  , aTileWalls    :: [Picture]
  , aItemFood     :: Picture
  , aItemSpeed    :: Picture
  , aItemAtk      :: Picture
  , aSword        :: Picture
  , aStairs       :: Picture
  , aFinalScreen  :: Picture
  , aStartScreen  :: Picture
  , aLoreScreen   :: Picture
  , aControlsScreen :: Picture
  }

type KeysDown = (Bool, Bool, Bool, Bool)

-- Estado completo del juego: jugador, enemigos, mapa, RNG, assets, etc.
data GameState = GameState
  { gsPlayer       :: Player
  , gsEnemies      :: [Enemy]
  , gsItems        :: [(Vec2, Item)]
  , gsMap          :: TileMap
  , gsFloor        :: Int
  , gsAssets       :: Assets
  , gsKeys         :: KeysDown
  , gsRng          :: StdGen
  , gsPhase        :: GamePhase
  , gsBossMsgTime  :: Float
  , nivelActual    :: Int
  , posEscalera    :: Vec2
  , jefeDerrotado  :: Bool
  }

-- Construye un Rect usando coordenadas de su centro y medios anchos/altos.
rectFromCenter :: Vec2 -> Float -> Float -> Rect
rectFromCenter (cx,cy) hw hh = Rect (cx - hw) (cy - hh) (2*hw) (2*hh)

-- Devuelve todas las coordenadas de tiles que toca un rectángulo.
tilesCoveredByRect :: Float -> Rect -> [(Int,Int)]
tilesCoveredByRect tileSize (Rect x y w h) =
  let eps = 1e-6
      -- para X: incluimos una pequeña corrección en la parte izquierda
      left   = floor ((x + eps) / tileSize)
      right  = floor ((x + w - eps) / tileSize)
      -- para Y usamos la convención del proyecto (ty = floor (-(py) / tileSize))
      -- aplicamos eps de forma simétrica: restamos eps en el tope y sumamos eps en el fondo
      top    = floor (-(y + h - eps) / tileSize)
      bottom = floor (-(y + eps) / tileSize)
  in [ (tx,ty) | tx <- [left..right], ty <- [top..bottom] ]

-- Comprueba si un rectángulo colisiona con paredes del mapa.
isRectWalkable :: Rect -> TileMap -> Bool
isRectWalkable rect tiles =
  let tilesIdx = tilesCoveredByRect tileSize rect
      h  = length tiles
      w  = if null tiles then 0 else length (head tiles)
      inBounds (tx,ty) = tx >= 0 && ty >= 0 && tx < w && ty < h
      tileOk (tx,ty) =
        if not (inBounds (tx,ty)) then False
        else let t = tiles !! ty !! tx in tileIsWalkable t
  in all tileOk tilesIdx

-- VERSIÓN PUNTUAL (compatibilidad) para módulos que aún la usan (Combat, etc.)
-- Comprueba si un punto (px, py) está en un tile caminable.
isWalkable :: Vec2 -> TileMap -> Bool
isWalkable (px,py) tiles =
  let tx = floor ((px + tileSize / 2) / tileSize)
      ty = floor ((-py + tileSize / 2) / tileSize)
      h  = length tiles
      w  = if null tiles then 0 else length (head tiles)
  in tx >= 0 && ty >= 0 && tx < w && ty < h && tileIsWalkable (tiles !! ty !! tx)

-- Determina si un Tile es "transitable" — adapta según tus constructores de Tile
tileIsWalkable :: Tile -> Bool
tileIsWalkable t = case t of
  FloorTile _ -> True
  StairUp     -> True
  StairDown   -> True
  -- Void, WallTile y otros no caminables
  _           -> False

