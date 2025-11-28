-- src/Update.hs
module Update where

import Types
import Combat
import Control.Monad (when)
import Control.Monad.State
import Inventory
import Data.List (partition)
import System.Random (StdGen, randomR, split)
import WorldGen (generateMap)

updateWorld :: Float -> GameState -> GameState
updateWorld dt gs = execState (updateWorldM dt) gs

updateWorldM :: Float -> State GameState ()
updateWorldM dt = do
  gs <- get
  case gsPhase gs of
    StartScreen ->
      return ()

    LoreScreen  -> 
      return ()

    ControlsScreen ->    
      return ()

    GameOver ->
      return ()

    Victory  ->
      return ()

    Playing  -> do
      movePlayerByKeys dt
      updateEnemies dt
      enemyDealDamage dt
      cleanupDeadEnemies
      checkStairTransition

    BossFight -> do
      let t = max 0 (gsBossMsgTime gs - dt)
      put gs { gsBossMsgTime = t }

      gs2 <- get
      movePlayerByKeys dt
      updateEnemies dt
      enemyDealDamage dt
      cleanupDeadEnemies



-- Mover al jugador basado en las teclas presionadas
movePlayerByKeys :: Float -> State GameState ()
movePlayerByKeys dt = do
  -- movimiento por ejes usando hitbox (X primero, luego Y)
  gs <- get
  let (u,d,l,r) = gsKeys gs
      p  = gsPlayer gs
      sp = pSpeed p * dt
      (x,y) = pPos p
      dx = (if r then 1 else 0) - (if l then 1 else 0)
      dy = (if u then 1 else 0) - (if d then 1 else 0)
      desiredX = x + dx * sp
      desiredY = y + dy * sp
      newTimer = max 0 (pAttackTimer p - dt)

      hbAfterX  = hitboxFromCenter (desiredX, y) (pHalfW p) (pHalfH p)
      hbAfterY  = hitboxFromCenter (x, desiredY) (pHalfW p) (pHalfH p)
      hbAfterXY = hitboxFromCenter (desiredX, desiredY) (pHalfW p) (pHalfH p)

      tiles = gsMap gs

  -- función local para aplicar movimiento y recoger items
  let applyMoveAndPick :: (Float,Float) -> State GameState ()
      applyMoveAndPick newPos = do
        gs' <- get
        let pMoved = p { pPos = newPos, pAttackTimer = newTimer }
            (picked, remaining) = partition (\(pos, _) -> distance pos newPos < 16) (gsItems gs')
            pickedItems = map snd picked
            pUpdated = foldl applyItem pMoved pickedItems
        put gs' { gsPlayer = pUpdated, gsItems = remaining }

  -- comprobaciones de hitbox y llamadas a la función monádica
  if isHitboxWalkable hbAfterX tiles
    then if isHitboxWalkable hbAfterXY tiles
           then applyMoveAndPick (desiredX, desiredY)
           else applyMoveAndPick (desiredX, y)
    else if isHitboxWalkable hbAfterY tiles
           then applyMoveAndPick (x, desiredY)
           else modify $ \s -> s { gsPlayer = p { pAttackTimer = newTimer } }


-- actualizar posiciones de todos los enemigos
updateEnemies :: Float -> State GameState ()
updateEnemies dt = do
  gs <- get
  let enemies' = map (advanceEnemy dt gs) (gsEnemies gs)
  modify $ \s -> s { gsEnemies = enemies' }

-- avanzar un enemigo hacia el jugador
advanceEnemy :: Float -> GameState -> Enemy -> Enemy
advanceEnemy dt gs e =
  let (px,py) = pPos (gsPlayer gs)
      (ex,ey) = ePos e
      dx   = px - ex
      dy   = py - ey
      dist = sqrt (dx*dx + dy*dy)
      baseSpeed =
        if gsPhase gs == BossFight
          then 60
          else 25
      speed = baseSpeed * dt
  in if dist < 400 && dist > 5
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
      phase' = if pHP p' <= 0 then GameOver else gsPhase gs 
  put gs { gsPlayer = p', gsPhase = phase'}  

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

--genera un boss en el centro de la sala del boss (ultima sala)
spawnBoss :: GameState -> Enemy
spawnBoss gs =
  let -- El boss aparece en el centro de la sala del boss 
      bossRoomCenter = posEscalera gs
      tiles = gsMap gs
      -- Buscar posición valida cercana al centro
      safePos = encontrarSueloCercano tiles bossRoomCenter
  in Enemy
      { ePos = safePos
      , eHP  = 300
      , eAtk = 20
      }

-- Si el centro es pared, busca el vecino ms cercano que sea suelo
encontrarSueloCercano :: TileMap -> Vec2 -> Vec2
encontrarSueloCercano mapa (x, y)
    | isWalkable (x, y) mapa       = (x, y)           -- Centro OK
    | isWalkable (x + 32, y) mapa  = (x + 32, y)      -- Derecha
    | isWalkable (x - 32, y) mapa  = (x - 32, y)      -- Izquierda
    | isWalkable (x, y + 32) mapa  = (x, y + 32)      -- Arriba
    | isWalkable (x, y - 32) mapa  = (x, y - 32)      -- Abajo
    | isWalkable (x + 32, y + 32) mapa = (x + 32, y + 32)  -- Diagonal
    | isWalkable (x - 32, y - 32) mapa = (x - 32, y - 32)  -- Diagonal
    | isWalkable (x + 32, y - 32) mapa = (x + 32, y - 32)  -- Diagonal
    | isWalkable (x - 32, y + 32) mapa = (x - 32, y + 32)  -- Diagonal
    | otherwise = (x, y)  -- Fallback (no debería pasar con salas bien generadas)


-- Eliminar enemigos muertos
cleanupDeadEnemies :: State GameState ()
cleanupDeadEnemies = do
  gs <- get

  let allEnemies          = gsEnemies gs
      (dead, alive0)      = partition (\e -> eHP e <= 0) allEnemies
      hadEnemies          = not (null allEnemies)

      gen0                = gsRng gs
      (newItems, genFinal) =
        if null dead
          then ([], gen0)
          else spawnDrops dead gen0

      items'              = gsItems gs ++ newItems

  case gsPhase gs of

    -- Fase normal: si mueren todos los enemigos → boss
    Playing ->
      if hadEnemies && null alive0
        then do
          let boss = spawnBoss gs
          put gs
            { gsEnemies     = [boss]
            , gsItems       = items'
            , gsRng         = genFinal
            , gsPhase       = BossFight
            , gsBossMsgTime = 3.0  
            }
        else
          put gs
            { gsEnemies = alive0
            , gsItems   = items'
            , gsRng     = genFinal
            }

    -- Fase de jefe: si muere el jefe, marcar como derrotado (no victoria inmediata)
    BossFight ->
      if null alive0
        then
          put gs
            { gsEnemies    = []
            , gsItems      = items'
            , gsRng        = genFinal
            , gsPhase      = Playing  -- Volver a Playing para buscar escalera
            , jefeDerrotado = True    -- Marcar jefe como derrotado
            }
        else
          put gs
            { gsEnemies = alive0
            , gsItems   = items'
            , gsRng     = genFinal
            }

    -- GameOver, Victory, etc: solo limpia y aplica drops
    _ ->
      put gs
        { gsEnemies = alive0
        , gsItems   = items'
        , gsRng     = genFinal
        }

spawnDrops :: [Enemy] -> StdGen -> ([(Vec2, Item)], StdGen)
spawnDrops [] gen = ([], gen)
spawnDrops (e:es) gen =
  let (r, gen1) = randomR (1,100 :: Int) gen
      (t, gen2) = randomR (1,3 :: Int) gen1  -- elige tipo de item 1..3
      item = case t of
               1 -> Heal 20
               2 -> BoostAtk 5
               _ -> BoostSpeed 10
      (restItems, gen3) = spawnDrops es gen2
      newItem = if r <= 30 then [ (ePos e, item) ] else []
  in (newItem ++ restItems, gen3)

-- Función de distancia para saber cuando el jugador esta cerca del item
distance :: Vec2 -> Vec2 -> Float
distance (x1,y1) (x2,y2) =
  sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- Verificar si el jugador está cerca de la escalera y puede transicionar
checkStairTransition :: State GameState ()
checkStairTransition = do
  gs <- get
  let playerPos = pPos (gsPlayer gs)
      stairPos  = posEscalera gs
      dist      = distance playerPos stairPos
      canTransition = dist < 30 && jefeDerrotado gs
  when canTransition $ do
    let newState = avanzarNivel gs
    put newState

-- Avanzar al siguiente nivel
avanzarNivel :: GameState -> GameState
avanzarNivel gs =
  let nivel = nivelActual gs
      nuevoNivel = nivel + 1
      oldGen = gsRng gs
      (mapGen, newGen) = split oldGen
      assets = gsAssets gs
      -- Generar nuevo mapa para el nuevo nivel (incluye posición del boss room)
      (newMap, startPos, enemies, bossRoomPos) = generateMap mapGen
  in if nuevoNivel > 3
       then gs { gsPhase = Victory }  -- Victoria si supera nivel 3
       else gs
         { gsPlayer      = initPlayerAtPos startPos (gsPlayer gs)  -- Spawn en inicio del nuevo mapa
         , gsEnemies     = enemies
         , gsItems       = []
         , gsMap         = newMap
         , gsRng         = newGen
         , gsPhase       = Playing
         , gsBossMsgTime = 0
         , nivelActual   = nuevoNivel
         , posEscalera   = bossRoomPos  -- Escalera en la sala del boss
         , jefeDerrotado = False
         }
