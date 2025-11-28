-- src/Types.hs
module Types where

import Graphics.Gloss
import System.Random (StdGen)

screenWidth, screenHeight :: Float
screenWidth  = 800
screenHeight = 600

tileSize :: Float
tileSize = 32

type Vec2 = (Float, Float)

-- Dirección para combate direccional
data Direction = DirUp | DirDown | DirLeft | DirRight
  deriving (Show, Eq)

data Tile
  = Void           -- Espacio vacío (no jugable)
  | FloorTile Int  -- Variante de textura (0-3)
  | WallTile Int   -- Modificado: ahora acepta variante (0-3)
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
  , pFacing      :: Direction    -- Nueva: dirección hacia donde mira
  , pAttackTimer :: Float        -- Nueva: tiempo de animación de ataque
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

data GamePhase  -- Nuevo: ahora el juego tiene fase 
  = Playing
  | BossFight
  | GameOver
  | Victory
  deriving (Eq, Show)

data Assets = Assets
  { aPlayer       :: Picture
  , aEnemy        :: Picture
  , aEnemySlime   :: Picture  -- Nueva: imagen para enemigos Slime
  , aBoss         :: Picture  -- nueva: imagen para el boss 
  , aTileFloors   :: [Picture]  -- Lista de variantes de suelo (Piso 1)
  , aTileFloors2  :: [Picture]  -- Tiles del Piso 2
  , aTileFloors3  :: [Picture]  -- Tiles del Piso 3
  , aTileWalls    :: [Picture]  -- Modificado: ahora es una lista de variantes
  , aItemFood     :: Picture
  , aSword        :: Picture    -- Nueva: imagen de la espada
  , aStairs       :: Picture    -- Sprite de escalera
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
  , gsPhase        :: GamePhase -- nuevo
  , gsBossMsgTime  :: Float
  , nivelActual    :: Int           -- Nivel actual (1, 2 o 3)
  , posEscalera    :: Vec2          -- Posición fija de la escalera
  , jefeDerrotado  :: Bool          -- True si el jefe del nivel fue derrotado
  }

isWalkable :: Vec2 -> TileMap -> Bool
isWalkable (px,py) tiles =
  let tx = floor (px / tileSize)
      ty = floor (-(py) / tileSize)
      h  = length tiles
      w  = if null tiles then 0 else length (head tiles)
  in tx >= 0 && ty >= 0 && tx < w && ty < h && isFloor (tiles !! ty !! tx)
  where
    isFloor (FloorTile _) = True
    isFloor _             = False
    