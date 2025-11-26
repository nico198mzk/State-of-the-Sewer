-- src/GameState.hs 
module GameState where

import Types
import System.Random (StdGen)

-- Modificado para aceptar posición inicial
initPlayer :: Vec2 -> Player
initPlayer startPos = Player
  { pPos       = startPos
  , pHP        = 100
  , pMaxHP     = 100
  , pAtk       = 10
  , pSpeed     = 60
  , pCooldown  = 0
  , pInventory = []
  }

-- Modificado para usar posición inicial del mapa
emptyState :: Assets -> TileMap -> Vec2 -> StdGen -> GameState
emptyState assets m startPos gen = GameState
  { gsPlayer  = initPlayer startPos  -- Usar posición generada
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
  , gsRng     = gen
  }