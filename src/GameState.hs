-- src/GameState.hs 
module GameState where

import Types

initPlayer :: Player
initPlayer = Player
  { pPos       = (0, 0)
  , pHP        = 100
  , pMaxHP     = 100
  , pAtk       = 10
  , pSpeed     = 60
  , pCooldown  = 0
  , pInventory = []
  }

emptyState :: Assets -> TileMap -> GameState
emptyState assets m = GameState
  { gsPlayer  = initPlayer
  , gsEnemies =
      [ Enemy (  80,   -40) 40 20
      , Enemy ( 200, -180) 40 20
      , Enemy (  20,  -140) 40 20
      ]
  , gsItems   = []
  , gsMap     = m
  , gsFloor   = 0
  , gsAssets  = assets
  , gsKeys    = (False, False, False, False)
  }