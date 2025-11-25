-- src/Combat.hs
module Combat where

import Types

attackRange :: Float
attackRange = 40

enemyTakeDamage :: Enemy -> Int -> Enemy
enemyTakeDamage e dmg = e { eHP = eHP e - dmg }

playerTakeDamage :: Player -> Int -> Player
playerTakeDamage p dmg =
  let newHP = pHP p - dmg
  in p { pHP = max 0 newHP }

applyPlayerAttack :: GameState -> GameState
applyPlayerAttack gs =
  let p        = gsPlayer gs
      posP     = pPos p
      atk      = pAtk p
      enemies' = map (hitEnemy posP atk) (gsEnemies gs)
  in gs { gsEnemies = enemies' }

hitEnemy :: Vec2 -> Int -> Enemy -> Enemy
hitEnemy (px,py) atk e =
  let (ex,ey) = ePos e
      dist    = sqrt ((px-ex)^2 + (py-ey)^2)
  in if dist <= attackRange
        then enemyTakeDamage e atk
        else e