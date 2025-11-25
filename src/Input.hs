-- src/Input.hs
module Input where

import Graphics.Gloss.Interface.Pure.Game
import Types
import Combat

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char 'w') Down _ _) gs = gs { gsKeys = setUp True gs }
handleInput (EventKey (Char 'w') Up   _ _) gs = gs { gsKeys = setUp False gs }

handleInput (EventKey (Char 's') Down _ _) gs = gs { gsKeys = setDown True gs }
handleInput (EventKey (Char 's') Up   _ _) gs = gs { gsKeys = setDown False gs }

handleInput (EventKey (Char 'a') Down _ _) gs = gs { gsKeys = setLeft True gs }
handleInput (EventKey (Char 'a') Up   _ _) gs = gs { gsKeys = setLeft False gs }

handleInput (EventKey (Char 'd') Down _ _) gs = gs { gsKeys = setRight True gs }
handleInput (EventKey (Char 'd') Up   _ _) gs = gs { gsKeys = setRight False gs }

handleInput (EventKey (SpecialKey KeySpace) Down _ _) gs = applyPlayerAttack gs

handleInput _ gs = gs

setUp :: Bool -> GameState -> KeysDown
setUp b gs    = let (_,d,l,r) = gsKeys gs in (b,d,l,r)

setDown :: Bool -> GameState -> KeysDown
setDown b gs  = let (u,_,l,r) = gsKeys gs in (u,b,l,r)

setLeft :: Bool -> GameState -> KeysDown
setLeft b gs  = let (u,d,_,r) = gsKeys gs in (u,d,b,r)

setRight :: Bool -> GameState -> KeysDown
setRight b gs = let (u,d,l,_) = gsKeys gs in (u,d,l,b)