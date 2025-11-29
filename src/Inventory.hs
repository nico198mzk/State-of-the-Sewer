--src/Inventory.hs
module Inventory where

import Types

-- Aplica el efecto de un ítem al jugador (curación, ataque o velocidad).
applyItem :: Player -> Item -> Player
applyItem p (Heal n) = p { pHP = min (pMaxHP p) (pHP p + n) }
applyItem p (BoostAtk n) = p { pAtk = pAtk p + n }
applyItem p (BoostSpeed s) = p { pSpeed = pSpeed p + s }

-- Usa un ítem del inventario del jugador y lo elimina de la lista.
useItem :: Player -> Int -> Player
useItem p idx =
  if idx < 0 || idx >= length (pInventory p)
    then p
    else let it = pInventory p !! idx
             p' = applyItem p it
         in p' { pInventory = removeAt idx (pInventory p) }

-- Elimina el elemento en un índice específico de una lista.
removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs