-- src/Update.hs
module Update where

import Types
import Combat
import Control.Monad.State
import Inventory
import Data.List (partition)

-- Función principal compatible con Gloss, usa execState internamente
updateWorld :: Float -> GameState -> GameState
updateWorld dt gs = execState (updateWorldM dt) gs

-- Lógica de actualización usando la mónada State
updateWorldM :: Float -> State GameState ()
updateWorldM dt = do
  gs <- get  -- Nuevo
  case gsPhase gs of -- Nuevo
    GameOver -> return () -- Nuevo
    Victory  -> return () -- Nuevo
    Playing  -> do -- Nuevo
      movePlayerByKeys dt
      updateEnemies dt
      enemyDealDamage dt
      cleanupDeadEnemies

-- Mover al jugador basado en las teclas presionadas
movePlayerByKeys :: Float -> State GameState ()
movePlayerByKeys dt = do
  gs <- get
  let (u,d,l,r) = gsKeys gs
      p  = gsPlayer gs
      sp = pSpeed p * dt
      (x,y) = pPos p
      dx = (if r then 1 else 0) - (if l then 1 else 0)
      dy = (if u then 1 else 0) - (if d then 1 else 0)
      newPos = (x + dx*sp, y + dy*sp)
  if isWalkable newPos (gsMap gs) --poder hacer que el jugador recoja los items
    then do
      let pMoved = p {pPos = newPos}
          (picked, remaining) =
            partition (\(pos, _) -> distance pos newPos < 16) (gsItems gs)
          
          pickedItems = map snd picked
        
          pUpdated = foldl applyItem pMoved pickedItems
      put gs {gsPlayer = pUpdated, gsItems = remaining }
    else 
      return()



-- Actualizar posiciones de todos los enemigos
updateEnemies :: Float -> State GameState ()
updateEnemies dt = do
  gs <- get
  let enemies' = map (advanceEnemy dt gs) (gsEnemies gs)
  modify $ \s -> s { gsEnemies = enemies' }

-- Avanzar un enemigo hacia el jugador
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

-- Los enemigos causan daño al jugador si están cerca
enemyDealDamage :: Float -> State GameState ()
enemyDealDamage dt = do
  gs <- get
  let p  = gsPlayer gs
      enemies = gsEnemies gs
      p' = foldl (applyEnemyHit dt) p enemies
      phase' = if pHP p' <= 0 then GameOver else gsPhase gs --Nuevo
  put gs { gsPlayer = p', gsPhase = phase'}  --Nuevo 

-- Aplicar daño de un enemigo al jugador
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

-- Eliminar enemigos muertos
cleanupDeadEnemies :: State GameState ()
cleanupDeadEnemies = do
  gs <- get 
  let alive = filter (\e -> eHP e > 0) (gsEnemies gs)
      hadEnemies = not (null (gsEnemies gs))

      newPhase =
        if hadEnemies && null alive && gsPhase gs == Playing
          then Victory
          else gsPhase gs
  put gs { gsEnemies = alive, gsPhase = newPhase}

--funcion de distancia para saber cuando el jugador esta cerca del item
distance :: Vec2 -> Vec2 -> Float
distance (x1,y1) (x2,y2) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2)