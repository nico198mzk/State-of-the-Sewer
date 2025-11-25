--src/Inventory.hs
module Inventory where

import Types

applyItem :: Player -> Item -> Player
applyItem p (Heal n) = p { pHP = min (pMaxHP p) (pHP p + n) }
applyItem p (BoostAtk n) = p { pAtk = pAtk p + n }
applyItem p (BoostSpeed s) = p { pSpeed = pSpeed p + s }

useItem :: Player -> Int -> Player
useItem p idx =
  if idx < 0 || idx >= length (pInventory p)
    then p
    else let it = pInventory p !! idx
             p' = applyItem p it
         in p' { pInventory = removeAt idx (pInventory p) }

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i+1) xs