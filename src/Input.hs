-- src/Input.hs
module Input where

import Graphics.Gloss.Interface.Pure.Game
import Control.Monad.State (execState)
import Types
import Combat
import GameState (resetGame)

-- Procesa eventos del teclado y actualiza el estado del juego (movimiento, ataques, pantallas, reinicio).
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gs =
  case gsPhase gs of
    StartScreen    -> gs { gsPhase = LoreScreen }
    LoreScreen     -> gs { gsPhase = ControlsScreen }
    ControlsScreen -> gs { gsPhase = Playing }
    _              -> gs


--Reiniciar con la R
handleInput (EventKey (Char 'r') Down _ _) gs =
  resetGame gs


handleInput (EventKey (Char 'w') Down _ _) gs = 
  let p = gsPlayer gs
  in gs { gsKeys = setUp True gs, gsPlayer = p { pFacing = DirUp } }
handleInput (EventKey (Char 'w') Up   _ _) gs = gs { gsKeys = setUp False gs }

handleInput (EventKey (Char 's') Down _ _) gs = 
  let p = gsPlayer gs
  in gs { gsKeys = setDown True gs, gsPlayer = p { pFacing = DirDown } }
handleInput (EventKey (Char 's') Up   _ _) gs = gs { gsKeys = setDown False gs }

handleInput (EventKey (Char 'a') Down _ _) gs = 
  let p = gsPlayer gs
  in gs { gsKeys = setLeft True gs, gsPlayer = p { pFacing = DirLeft } }
handleInput (EventKey (Char 'a') Up   _ _) gs = gs { gsKeys = setLeft False gs }

handleInput (EventKey (Char 'd') Down _ _) gs = 
  let p = gsPlayer gs
  in gs { gsKeys = setRight True gs, gsPlayer = p { pFacing = DirRight } }
handleInput (EventKey (Char 'd') Up   _ _) gs = gs { gsKeys = setRight False gs }

handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = 
  let gs' = execState applyPlayerAttack gs
      p = gsPlayer gs'
  in gs' { gsPlayer = p { pAttackTimer = 0.3 } }  -- Activar animación por 0.3 segundos

handleInput _ gs = gs

-- Cambia el valor de la tecla "arriba" en el estado de teclas.
setUp :: Bool -> GameState -> KeysDown
setUp b gs    = let (_,d,l,r) = gsKeys gs in (b,d,l,r)

-- Cambia el valor de la tecla "abajo" en el estado de teclas.
setDown :: Bool -> GameState -> KeysDown
setDown b gs  = let (u,_,l,r) = gsKeys gs in (u,b,l,r)

-- Cambia el valor de la tecla "izquierda" en el estado de teclas.
setLeft :: Bool -> GameState -> KeysDown
setLeft b gs  = let (u,d,_,r) = gsKeys gs in (u,d,b,r)

-- Cambia el valor de la tecla "derecha" en el estado de teclas.
setRight :: Bool -> GameState -> KeysDown
setRight b gs = let (u,d,l,_) = gsKeys gs in (u,d,l,b)