-- src/GameState.hs 
module GameState where

import Types
import WorldGen (generateMap)
import System.Random (StdGen, next)


-- Modificado para aceptar posición inicial
initPlayer :: Vec2 -> Player
initPlayer startPos = Player
  { pPos         = startPos
  , pHP          = 100
  , pMaxHP       = 100
  , pAtk         = 10
  , pSpeed       = 60
  , pCooldown    = 0
  , pInventory   = []
  , pFacing      = DirDown      -- Iniciar mirando hacia abajo
  , pAttackTimer = 0            -- Sin animación de ataque al inicio
  }

-- Modificado para usar posición inicial del mapa y lista de enemigos generados
emptyState :: Assets -> TileMap -> Vec2 -> [Enemy] -> StdGen -> GameState
emptyState assets m startPos enemies gen = 
  let (sx,sy)= startPos
  in GameState
    { gsPlayer  = initPlayer startPos  -- Usar posición generada
    , gsEnemies = enemies  -- Usar enemigos generados por WorldGen
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

      -- Generar nuevo mapa, posición inicial y enemigos
      (newMap, startPos, enemies) = generateMap newGen

  in emptyState assets newMap startPos enemies newGen