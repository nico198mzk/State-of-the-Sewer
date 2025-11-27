-- src/GameState.hs 
module GameState where

import Types
import WorldGen (generateMap)
import System.Random (StdGen, next)


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
emptyState assets m startPos gen = 
  let (sx,sy)= startPos
  in GameState
    { gsPlayer  = initPlayer startPos  -- Usar posición generada
    , gsEnemies =
        [ Enemy (  80,   -40) 40 20
        , Enemy ( 200, -180) 40 20
        , Enemy (  20,  -140) 40 20
        ]
    , gsItems = 
        [ ((sx + 40, sy),   Heal 30)
        , ((sx + 80, sy),   BoostAtk 5)
        , ((sx + 120, sy),  BoostSpeed 30)
        ] -- Nuevo los 3 items los coloque al lado de donde aparece al jugador mas o menos
    , gsMap     = m
    , gsFloor   = 0
    , gsAssets  = assets
    , gsKeys    = (False, False, False, False)
    , gsRng     = gen
    , gsPhase   = Playing -- Nuevo
    }

--Permite reiniciar generando nuevo mapa 
resetGame :: GameState -> GameState
resetGame gs =
  let oldGen = gsRng gs
      (newSeed, newGen) = next oldGen
      assets = gsAssets gs

      -- Generar nuevo mapa y posición inicial
      (newMap, startPos) = generateMap newGen

  in emptyState assets newMap startPos newGen