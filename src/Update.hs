-- src/Update.hs
module Update where

import Types
import Combat

updateWorld dt gs =
  let gs1 = movePlayerByKeys dt gs
      gs2 = gs1 { gsEnemies = updateEnemies dt gs1 }
      gs3 = enemyDealDamage dt gs2       -- ← OBLIGATORIO
      gs4 = cleanupDeadEnemies gs3
  in gs4

-- src/Update.hs (reemplazar movePlayerByKeys)
movePlayerByKeys :: Float -> GameState -> GameState
movePlayerByKeys dt gs =
  let (u,d,l,r) = gsKeys gs
      p  = gsPlayer gs
      sp = pSpeed p * dt
      (x,y) = pPos p
      dx = (if r then 1 else 0) - (if l then 1 else 0)
      dy = (if u then 1 else 0) - (if d then 1 else 0)
      newPos = (x + dx*sp, y + dy*sp)
  in if isWalkable newPos (gsMap gs)
        then gs { gsPlayer = p { pPos = newPos } }
        else gs

updateEnemies :: Float -> GameState -> [Enemy]
updateEnemies dt gs =
  map (advanceEnemy dt gs) (gsEnemies gs)

advanceEnemy :: Float -> GameState -> Enemy -> Enemy
advanceEnemy dt gs e =
  let (px,py) = pPos (gsPlayer gs)
      (ex,ey) = ePos e
      dx   = px - ex
      dy   = py - ey
      dist = sqrt (dx*dx + dy*dy)
      speed = 25 * dt
  in if dist < 250 && dist > 20
       then
         let newPos = (ex + dx/dist * speed, ey + dy/dist * speed)
         in if isWalkable newPos (gsMap gs)
              then e { ePos = newPos }
              else e
       else e

enemyDealDamage :: Float -> GameState -> GameState
enemyDealDamage dt gs =
  let p  = gsPlayer gs
      p' = foldl (applyEnemyHit dt) p (gsEnemies gs)
  in gs { gsPlayer = p' }

applyEnemyHit :: Float -> Player -> Enemy -> Player
applyEnemyHit dt p e =
  let (px,py) = pPos p
      (ex,ey) = ePos e
      dist    = sqrt ((px-ex)^2 + (py-ey)^2)
      rawDmg  = fromIntegral (eAtk e) * dt
      dmg     = if rawDmg < 1 && rawDmg > 0 then 1 else floor rawDmg
  in if dist < 30
        then playerTakeDamage p dmg
        else p

cleanupDeadEnemies :: GameState -> GameState
cleanupDeadEnemies gs =
  gs { gsEnemies = filter (\e -> eHP e > 0) (gsEnemies gs) }