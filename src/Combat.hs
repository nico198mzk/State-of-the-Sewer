-- src/Combat.hs
module Combat where

import Types
import Control.Monad.State

attackRange :: Float
attackRange = 40

-- Fuerza de retroceso de la espada de madera
woodSwordKnockback :: Float
woodSwordKnockback = 40.0

-- Funciones puras de daño (sin cambios, usadas por funciones monádicas)
enemyTakeDamage :: Enemy -> Int -> Enemy
enemyTakeDamage e dmg = e { eHP = eHP e - dmg }

playerTakeDamage :: Player -> Int -> Player
playerTakeDamage p dmg =
  let newHP = pHP p - dmg
  in p { pHP = max 0 newHP }

-- Aplicar ataque del jugador a enemigos usando la mónada State
-- Ahora usa ataque direccional con knockback
applyPlayerAttack :: State GameState ()
applyPlayerAttack = do
  gs <- get
  let p        = gsPlayer gs
      posP     = pPos p
      facing   = pFacing p
      atk      = pAtk p
      tmap     = gsMap gs
      enemies' = map (hitEnemyDirectional posP facing atk tmap) (gsEnemies gs)
  modify $ \s -> s { gsEnemies = enemies' }

-- Función auxiliar pura para golpear un enemigo direccionalmente con knockback
hitEnemyDirectional :: Vec2 -> Direction -> Int -> TileMap -> Enemy -> Enemy
hitEnemyDirectional (px,py) facing atk tmap e =
  let (ex,ey) = ePos e
      -- Calcular punto de impacto desplazado en la dirección
      offset = 30  -- Distancia del punto de impacto desde el jugador
      (impactX, impactY) = case facing of
        DirUp    -> (px, py + offset)
        DirDown  -> (px, py - offset)
        DirLeft  -> (px - offset, py)
        DirRight -> (px + offset, py)
      -- Calcular distancia desde punto de impacto al enemigo
      dist = sqrt ((impactX-ex)^2 + (impactY-ey)^2)
  in if dist <= attackRange
        then applyKnockback (px,py) (enemyTakeDamage e atk) tmap
        else e

-- Aplicar knockback al enemigo alejándolo del jugador
applyKnockback :: Vec2 -> Enemy -> TileMap -> Enemy
applyKnockback (px,py) e tmap =
  let (ex,ey) = ePos e
      -- Vector desde jugador hacia enemigo
      dx = ex - px
      dy = ey - py
      dist = sqrt (dx*dx + dy*dy)
  in if dist > 0.1  -- Evitar división por cero
       then let -- Vector unitario
                unitX = dx / dist
                unitY = dy / dist
                -- Nueva posición con knockback
                newX = ex + unitX * woodSwordKnockback
                newY = ey + unitY * woodSwordKnockback
                newPos = (newX, newY)
            -- Verificar si la nueva posición es válida
            in if isWalkable newPos tmap
                 then e { ePos = newPos }
                 else e  -- No mover si hay pared
       else e  -- No aplicar knockback si están en la misma posición