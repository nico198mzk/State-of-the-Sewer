-- src/Combat.hs
module Combat where

import Types
import Control.Monad.State

attackRange :: Float
attackRange = 40

-- Fuerza de retroceso de la espada de madera
woodSwordKnockback :: Float
woodSwordKnockback = 40.0

-- Resta vida al enemigo.
enemyTakeDamage :: Enemy -> Int -> Enemy
enemyTakeDamage e dmg = e { eHP = eHP e - dmg }

-- Resta vida al jugador sin dejar que baje de 0.
playerTakeDamage :: Player -> Int -> Player
playerTakeDamage p dmg =
  let newHP = pHP p - dmg
  in p { pHP = max 0 newHP }

-- Aplica el ataque del jugador a todos los enemigos según dirección.
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

-- Determina si un enemigo está en rango y aplica daño y knockback.
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

-- Empuja al enemigo lejos del jugador si la nueva posición es válida.
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