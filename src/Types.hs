-- src/Types.hs
module Types where

import Graphics.Gloss

screenWidth, screenHeight :: Float
screenWidth  = 800
screenHeight = 600

tileSize :: Float
tileSize = 32

type Vec2 = (Float, Float)

data Tile
  = FloorTile
  | WallTile
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
  { aPlayer    :: Picture
  , aEnemy     :: Picture
  , aTileFloor :: Picture
  , aTileWall  :: Picture
  , aItemFood  :: Picture
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
  }

-- src/Types.hs (reemplazar isWalkable)
isWalkable :: Vec2 -> TileMap -> Bool
isWalkable (px,py) tiles =
  let tx = floor (px / tileSize)
      ty = floor (-(py) / tileSize)
      h  = length tiles
      w  = if null tiles then 0 else length (head tiles)
  in tx >= 0 && ty >= 0 && tx < w && ty < h && (tiles !! ty !! tx) /= WallTile