-- src/GameState.hs 
module GameState where

import Types
import WorldGen (generateMap)
import System.Random (StdGen, split)


-- Crea un jugador inicial usando una posición dada.
initPlayer :: Vec2 -> Player
initPlayer startPos = Player
  { pPos         = startPos
  , pHP          = 100
  , pMaxHP       = 100
  , pAtk         = 10
  , pSpeed       = 80
  , pCooldown    = 0
  , pInventory   = []
  , pFacing      = DirDown
  , pAttackTimer = 0
  , pHalfW       = 12.0   -- medio ancho del hitbox (ajusta si tu sprite es otro)
  , pHalfH       = 12.0   -- medio alto del hitbox
  }


-- Construye el estado inicial completo del juego con mapa, jugador, enemigos y sala del boss.
emptyState :: Assets -> TileMap -> Vec2 -> [Enemy] -> Vec2 -> StdGen -> GameState
emptyState assets m startPos enemies bossRoomPos gen = 
  let (sx,sy) = startPos
  in GameState
    { gsPlayer       = initPlayer startPos  -- Usar posición generada
    , gsEnemies      = enemies  -- Usar enemigos generados por WorldGen
    , gsItems        = [] 
    , gsMap          = m
    , gsFloor        = 0
    , gsAssets       = assets
    , gsKeys         = (False, False, False, False)
    , gsRng          = gen
    , gsPhase        = StartScreen 
    , gsBossMsgTime  = 0
    , nivelActual    = 1          -- Inicia en nivel 1
    , posEscalera    = bossRoomPos  -- Escalera en el centro de la sala del boss
    , jefeDerrotado  = False      -- Jefe no derrotado al inicio
    }

-- Reinicia el juego generando un nuevo mapa y restableciendo el estado.
resetGame :: GameState -> GameState
resetGame gs =
  let oldGen = gsRng gs
      (mapGen, newGen) = split oldGen
      assets = gsAssets gs

      -- Generar nuevo mapa, posición inicial, enemigos y posición del boss
      (newMap, startPos, enemies, bossRoomPos) = generateMap newGen

  in emptyState assets newMap startPos enemies bossRoomPos newGen
  