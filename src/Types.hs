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

data Tile
  = Void           -- Espacio vacío (no jugable)
  | FloorTile Int  -- Variante de textura (0-3)
  | WallTile Int   -- Modificado: ahora acepta variante (0-3)
  | StairUp
  | StairDown
  deriving (Eq,Show)

type TileMap = [[Tile]]

data Player = Player
  { pPos       :: Vec2
  , pHP        :: Int
  , pMaxHP     :: Int
  , pAtk       :: Int
  , pSpeed     :: Float
  , pCooldown  :: Float
  , pInventory :: [Item]
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

data Assets = Assets
  { aPlayer     :: Picture
  , aEnemy      :: Picture
  , aTileFloors :: [Picture]  -- Lista de variantes de suelo
  , aTileWalls  :: [Picture]  -- Modificado: ahora es una lista de variantes
  , aItemFood   :: Picture
  }

type KeysDown = (Bool, Bool, Bool, Bool)

data GameState = GameState
  { gsPlayer  :: Player
  , gsEnemies :: [Enemy]
  , gsItems   :: [(Vec2, Item)]
  , gsMap     :: TileMap
  , gsFloor   :: Int
  , gsAssets  :: Assets
  , gsKeys    :: KeysDown
  , gsRng     :: StdGen
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