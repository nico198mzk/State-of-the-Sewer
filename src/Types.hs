-- src/Types.hs (arreglado por Kei)
module Types where

import Graphics.Gloss
import System.Random (StdGen)

screenWidth, screenHeight :: Float
screenWidth  = 800
screenHeight = 600

tileSize :: Float
tileSize = 32

type Vec2 = (Float, Float)
data Rect = Rect { rX :: Float, rY :: Float, rW :: Float, rH :: Float } deriving (Show, Eq)

hitboxFromCenter :: Vec2 -> Float -> Float -> Rect
hitboxFromCenter = rectFromCenter

isHitboxWalkable :: Rect -> TileMap -> Bool
isHitboxWalkable = isRectWalkable

-- Reintroduzco initPlayerAtPos que algunos módulos esperan
initPlayerAtPos :: Vec2 -> Player -> Player
initPlayerAtPos newPos p = p
  { pPos         = newPos
  , pCooldown    = 0
  , pAttackTimer = 0
  }


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

type TileMap = [[Tile]]

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

data Enemy = Enemy
  { ePos :: Vec2
  , eHP  :: Int
  , eAtk :: Int
  }
  deriving (Show)

data Item
  = Heal Int
  | BoostAtk Int
  | BoostSpeed Float
  deriving (Show,Eq)

data GamePhase
  = StartScreen
  | LoreScreen
  | ControlsScreen
  | Playing
  | BossFight
  | GameOver
  | Victory
  deriving (Eq, Show)

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


rectFromCenter :: Vec2 -> Float -> Float -> Rect
rectFromCenter (cx,cy) hw hh = Rect (cx - hw) (cy - hh) (2*hw) (2*hh)

tilesCoveredByRect :: Float -> Rect -> [(Int,Int)]
tilesCoveredByRect tileSize (Rect x y w h) =
  let left   = floor (x / tileSize)
      right  = floor ((x + w - 1e-6) / tileSize)
      top    = floor (-(y + h - 1e-6) / tileSize)
      bottom = floor (-(y) / tileSize)
  in [ (tx,ty) | tx <- [left..right], ty <- [top..bottom] ]




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
isWalkable :: Vec2 -> TileMap -> Bool
isWalkable (px,py) tiles =
  let tx = floor (px / tileSize)
      ty = floor (-(py) / tileSize)  -- convención Y del proyecto
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

