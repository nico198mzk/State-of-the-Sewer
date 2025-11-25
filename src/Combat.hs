-- src/Combat.hs
module Combat where

import Types
import Control.Monad.State

attackRange :: Float
attackRange = 40

-- Funciones puras de daño (sin cambios, usadas por funciones monádicas)
enemyTakeDamage :: Enemy -> Int -> Enemy
enemyTakeDamage e dmg = e { eHP = eHP e - dmg }

playerTakeDamage :: Player -> Int -> Player
playerTakeDamage p dmg =
  let newHP = pHP p - dmg
  in p { pHP = max 0 newHP }

-- Aplicar ataque del jugador a enemigos usando la mónada State
applyPlayerAttack :: State GameState ()
applyPlayerAttack = do
  gs <- get
  let p        = gsPlayer gs
      posP     = pPos p
      atk      = pAtk p
      enemies' = map (hitEnemy posP atk) (gsEnemies gs)
  modify $ \s -> s { gsEnemies = enemies' }

-- Función auxiliar pura para golpear un enemigo
hitEnemy :: Vec2 -> Int -> Enemy -> Enemy
hitEnemy (px,py) atk e =
  let (ex,ey) = ePos e
      dist    = sqrt ((px-ex)^2 + (py-ey)^2)
  in if dist <= attackRange
        then enemyTakeDamage e atk
        else e